# =============================================================================
# rag.R — GraphRAG 公共层（证据 / 引用 / 上下文 / 大模型）
#
# 与旧版的区别：数据源从 Neo4j / rag_bundle 换成 data/kg_store.rds，
# 检索逻辑搬到 rag_retrieve.R（实体链接 + 多步查询计划 + 证据融合）。
# 本文件只保留与"数据来自哪里"无关的部分，三种回答模式保持不变：
#   平台 Key / 用户自带 Key（BYOK）/ 无 Key（检索式回答）
# =============================================================================

# ---------------------------------------------------------------- 配置 -------
rag_env <- function(name, default = "") {
  v <- Sys.getenv(name, unset = "")
  if (!nzchar(v)) default else v
}

rag_config <- function() {
  list(
    embed_url    = sub("/+$", "", rag_env("EMBED_BASE_URL", "https://api.siliconflow.cn/v1")),
    embed_key    = trimws(rag_env("EMBED_API_KEY", "")),
    embed_model  = trimws(rag_env("EMBED_MODEL", "BAAI/bge-m3")),
    embed_dim    = as.integer(rag_env("EMBED_DIM", "1024")),
    top_chunks   = as.integer(rag_env("TOP_K_CHUNKS", "8")),
    vector_k     = as.integer(rag_env("VECTOR_K", "8")),
    max_evidence = as.integer(rag_env("MAX_EVIDENCE", "20")),
    llm_base     = rag_env("LLM_BASE_URL", ""),
    llm_model    = rag_env("LLM_MODEL", "deepseek-chat")
  )
}

# ------------------------------------------------------------ 证据与引用 ----
rag_ev <- function(kind, title, text, score = 1, citation = list()) {
  list(kind = kind, title = title, text = text, score = score, citation = citation)
}

# RRF：图证据与文本证据按排名融合（图证据权重 1.2）
rag_fuse <- function(graph_ev, text_lists, max_n = 20) {
  scores <- list()
  items <- list()
  add <- function(ev, rank, weight) {
    key <- if (!is.null(ev$citation$chunk_id)) {
      paste0("c:", ev$citation$chunk_id)
    } else {
      paste0("g:", ev$title)
    }
    scores[[key]] <<- (scores[[key]] %||% 0) + weight / (60 + rank)
    if (is.null(items[[key]])) items[[key]] <<- ev
  }
  for (i in seq_along(graph_ev)) add(graph_ev[[i]], i, 1.4)
  for (lst in text_lists) {
    for (i in seq_along(lst)) add(lst[[i]], i, 1.0)
  }
  if (!length(scores)) return(list())
  keys <- names(scores)[order(unlist(scores), decreasing = TRUE)]
  lapply(keys[seq_len(min(length(keys), max_n))], function(k) items[[k]])
}

rag_context <- function(evidence, max_chars = 20000) {
  if (!length(evidence)) return("")
  blocks <- vapply(seq_along(evidence), function(i) {
    ev <- evidence[[i]]
    cite <- if (ev$kind == "text") {
      sprintf("〔出处：%s · %s · 第%s行 · %s〕",
              ev$citation$doc %||% "", ev$citation$section %||% "",
              ev$citation$line %||% "", ev$citation$chunk_id %||% "")
    } else {
      sprintf("〔图谱证据：%s〕", ev$citation$source %||% "graph")
    }
    sprintf("[E%d] %s\n%s\n%s", i, ev$title, ev$text, cite)
  }, character(1))
  ctx <- paste(blocks, collapse = "\n\n")
  if (nchar(ctx) > max_chars) ctx <- paste0(substr(ctx, 1, max_chars), "\n…（证据已截断）")
  ctx
}

rag_citations <- function(evidence) {
  lapply(seq_along(evidence), function(i) {
    ev <- evidence[[i]]
    c(ev$citation, list(ref = paste0("E", i), title = ev$title, kind = ev$kind))
  })
}

rag_scale_line <- function() {
  lc <- tryCatch(kg_label_counts(), error = function(e) NULL)
  if (is.null(lc)) return("图谱规模：751 味药、5,229 首方剂、15,317 段原文。")
  g <- function(k, d) if (k %in% names(lc)) kg_num(lc[[k]]) else d
  sprintf("图谱规模：751 味药、%s 首方剂、%s 种化合物、%s 个主治病证、%s 段原文。",
          g("Formula", "5,229"), g("Compound", "3,864"),
          g("Indication", "1,701"), g("TextUnit", "15,317"))
}

rag_system_prompt <- paste(
  "你是《中华药海（精华本）》知识图谱的问答助手，服务于中药新药研发。",
  rag_scale_line(),
  "回答规则：",
  "1) 只使用【证据】回答；证据不足就明确说“图谱中没有足够证据”，不要补充常识；",
  "2) 每个结论后用 [E1] 标注来源；",
  "3) 原文证据保留“文件 · 章节 · 行号”，可回原文核对；",
  "4) 中文回答，先给结论再给明细，条目化；数字（剂量、含量、数量）原样引用；",
  "5) 多步问题按“① → ② → ③”的顺序写出推理链，每一步对应证据编号；",
  "6) 清单类问题（“哪些药”“共 N 味”）必须给出完整名单，不要只列前几个、也不要用“等”省略；名称照抄证据，用顿号连写即可，不必逐条展开；证据提示可下载时，末尾补一句“完整明细可在「检索链路」面板下载 CSV”；",
  "7) 涉及毒性、禁忌、妊娠、十八反十九畏时必须原样引用，并提示“需在医师/药师指导下使用”。",
  "8) 涉及分类归属时，只依据【分类归属】证据回答“属于/不属于”，不得用功效推断分类；",
  "   若某药的功效名称与分类同名（如百合“清心安神”与“安神药”），必须分别陈述并说明二者区别。",
  sep = "\n"
)

# 检索结果的"据实说明"：问到的药在图谱里没有独立条目
rag_answer_note <- function(retr) {
  u <- retr$unknown %||% ""
  if (!nzchar(u)) return("")
  sprintf(paste0("**据实说明：图谱中没有「%s」这味药的独立条目**（全书 751 味中未见）。",
                 "以下片段可能只是别处提及，不能当作该药的结论。"), u)
}

# ----------------------------- 检索式回答（无平台 Key / 用户主动选择仅检索） ----
# reason = "no_key"     ：平台没有配置大模型 Key，被动退回检索式回答
# reason = "user_choice"：用户在界面上主动选择「仅图谱检索」
rag_extractive <- function(retr, reason = c("no_key", "user_choice")) {
  reason <- match.arg(reason)
  note <- rag_answer_note(retr)
  if (!length(retr$evidence)) {
    msg <- "图谱中没有检索到相关证据。请换一种问法，或补充药名 / 病名 / 方名 / 成分名。"
    return(if (nzchar(note)) paste0(note, "\n\n", msg) else msg)
  }
  graph <- Filter(function(e) e$kind == "graph", retr$evidence)
  texts <- Filter(function(e) e$kind == "text", retr$evidence)
  parts <- c(if (identical(reason, "user_choice")) {
    "**仅检索结果（未调用大模型）**"
  } else {
    "**检索式回答（未启用大模型）**"
  }, "")
  if (nzchar(note)) parts <- c(parts, note, "")

  # 多步检索轨迹：让用户看到"查了哪几步"
  tr <- retr$trace
  if (length(tr)) {
    steps <- vapply(seq_along(tr), function(i) {
      sprintf("%d. %s（%d 条）", i, tr[[i]]$title, tr[[i]]$rows)
    }, "")
    parts <- c(parts, "**检索步骤**", "", steps, "")
  }

  if (length(graph)) {
    parts <- c(parts, "**图谱事实**", "")
    for (i in seq_along(graph)) {
      body <- gsub("\n", "；", graph[[i]]$text)
      # 清单类证据（"共 N 味"+"完整名单"）不截断，保证全部列出
      if (!isTRUE(graph[[i]]$citation$list) && nchar(body) > 1200) {
        body <- paste0(substr(body, 1, 1200), "…")
      }
      parts <- c(parts, sprintf("- **%s**：%s", graph[[i]]$title, body))
    }
    parts <- c(parts, "")
  }
  if (length(texts)) {
    parts <- c(parts, "**原文依据**", "")
    for (ev in utils::head(texts, 6)) {
      parts <- c(parts, sprintf("- %s：%s〔%s · %s · 第%s行〕",
                                ev$title, substr(gsub("\n", " ", ev$text), 1, 180),
                                ev$citation$doc %||% "", ev$citation$section %||% "",
                                ev$citation$line %||% ""))
    }
    parts <- c(parts, "")
  }
  footer <- if (identical(reason, "user_choice")) {
    "> 当前为「仅图谱检索」模式：以上为图谱与原文证据的结构化摘录，未调用大模型归纳。"
  } else {
    "> 当前为检索式回答（平台未配置大模型）：以上为图谱与原文证据的结构化摘录，未做额外归纳。"
  }
  paste(c(parts, footer), collapse = "\n")
}

# --------------------------------------------------------------- LLM --------
rag_chat_new <- function(api_key, cfg = rag_config()) {
  if (!nzchar(api_key)) stop("未提供 API Key")
  creds <- local({
    k <- api_key
    function() k
  })
  if (nzchar(cfg$llm_base)) {
    ellmer::chat_openai_compatible(
      base_url = cfg$llm_base, model = cfg$llm_model,
      credentials = creds, system_prompt = rag_system_prompt
    )
  } else {
    ellmer::chat_deepseek(
      model = cfg$llm_model, credentials = creds,
      system_prompt = rag_system_prompt
    )
  }
}

# 一次性回答（非流式），便于命令行测试
rag_answer <- function(question, api_key = "", cfg = rag_config()) {
  retr <- rag_retrieve(question, cfg)
  if (!length(retr$evidence)) {
    return(list(answer = "图谱中没有检索到相关证据。", mode = "none",
                citations = list(), evidence = list(), intent = retr$intent,
                trace = retr$trace))
  }
  if (!nzchar(api_key)) {
    return(list(answer = rag_extractive(retr), mode = "extractive",
                citations = rag_citations(retr$evidence),
                evidence = retr$evidence, intent = retr$intent, trace = retr$trace))
  }
  chat <- rag_chat_new(api_key, cfg)
  ans <- chat$chat(paste0(rag_context(retr$evidence),
                          "\n\n用户问题：", question))
  list(answer = ans, mode = "llm", citations = rag_citations(retr$evidence),
       evidence = retr$evidence, intent = retr$intent, trace = retr$trace)
}

# 给 shinychat 流式用：返回拼好的消息
rag_stream_message <- function(question, cfg = rag_config()) {
  retr <- rag_retrieve(question, cfg)
  if (!length(retr$evidence)) {
    list(message = NULL, retr = retr, empty = TRUE)
  } else {
    list(message = paste0(rag_context(retr$evidence), "\n\n用户问题：", question),
         retr = retr, empty = FALSE)
  }
}

# ------------------------------------------------------------ 健康检查 ------
rag_health <- function(cfg = rag_config()) {
  out <- tryCatch(kg_summary(), error = function(e) list(ok = FALSE,
                                                         error = conditionMessage(e)))
  out
}
