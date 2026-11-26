# =============================================================================
# rag_shiny.R — GraphRAG 对话界面（默认自动就绪 + 检索链路可视化）
#
# 与旧版的区别：
#   * 不再让用户选中药、不再输入 API Key：启动即用平台 Key，没有 Key 就用检索式回答
#   * 新增「回答方式」：默认「图谱检索 + 平台 Key」；可切换为「仅图谱检索」，
#     只返回图谱与原文证据，不调用大模型
#   * 不再需要"载入知识图谱"按钮：图谱在后端启动时已加载，输入框直接可用
#   * 新增「检索链路」折叠面板：问题解析 → 实体/条件 → 查询计划 → 逐步耗时 →
#     生成的 Cypher → 证据出处，全部可展开核对
# =============================================================================

rag_chat_ui <- function() {
  has_key <- nzchar(rag_env("DEEPSEEK_API_KEY", ""))
  tagList(
    tags$div(
      class = "zh-rag-mode",
      radioButtons(
        "ragGenMode", "回答方式",
        choices = c(
          "图谱检索 + 平台 Key（大模型生成）" = "llm",
          "仅图谱检索（不调用大模型）"       = "extractive"
        ),
        selected = if (has_key) "llm" else "extractive"
      ),
      tags$p(class = "text-muted", style = "font-size:.78rem;margin:-.35rem 0 .6rem;",
             if (has_key) {
               "选「仅检索」时只返回图谱事实与原文依据，不调用大模型归纳。"
             } else {
               "平台未配置大模型 Key，两种选项均只返回检索式结果。"
             })
    ),
    uiOutput("ragStatus"),
    uiOutput("ragPipeline"),
    tags$details(
      style = "margin-top:.5rem;",
      tags$summary(style = "cursor:pointer;color:#2c6e49;", "试试这些问法"),
      tags$ul(
        style = "padding-left:1.1rem;font-size:.88rem;margin-top:.4rem;",
        tags$li("哪些药物能治疗头痛，且温性，入肝经，用量小于6克？"),
        tags$li("温性且归肝经的药有哪些？"),
        tags$li("无毒的清热药有多少味？"),
        tags$li("孕妇禁忌的药有哪些？"),
        tags$li("整本书里治疗衄血的药有哪些？"),
        tags$li("地榆的方剂里还用了哪些药？这些药有什么功效？"),
        tags$li("含槲皮素的药中，哪些能止血？"),
        tags$li("清热解毒类药最常含的化合物是什么？"),
        tags$li("地榆与槐花有什么区别？"),
        tags$li("统计一下全书功效出现频次最高的 20 个"),
        tags$li("Which warm-natured herbs enter the Liver channel and are suitable for headache at doses below 6 g?")
      )
    )
  )
}

# 检索链路面板里用的小工具
rag_kv <- function(k, v) {
  tags$div(class = "d-flex gap-2", style = "font-size:.84rem;",
           tags$span(class = "text-muted", style = "min-width:5.2rem;", k),
           tags$span(v))
}

# 把清单类结果写成带 BOM 的 CSV（Excel 直接打开不乱码）
rag_write_list_csv <- function(df, file) {
  if (is.null(df) || !nrow(df)) {
    df <- data.frame(说明 = "本次提问没有清单类结果", stringsAsFactors = FALSE)
  }
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_excel_csv(df, file)
  } else {
    utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(file)
}

# 检索链路折叠面板（独立函数，便于单测）
rag_pipeline_ui <- function(retr) {
  if (is.null(retr)) {
    return(tags$details(
      style = "margin-top:.5rem;",
      tags$summary(style = "cursor:pointer;color:#2c6e49;",
                   "检索链路（提问后展示：条件解析 → 查询计划 → 逐步耗时 → 生成的 Cypher → 证据出处）"),
      tags$p(class = "text-muted small mt-2 mb-0", "还没有提问。")
    ))
  }

  ent <- if (nrow(retr$entities)) {
    paste(sprintf("%s：%s", retr$entities$label, retr$entities$name), collapse = "；")
  } else "（无）"

  f <- retr$filters %||% list()
  parts <- character(0)
  if (length(f$category)) parts <- c(parts, paste0("分类=", paste(f$category, collapse = "/")))
  if (length(f$meridian)) parts <- c(parts, paste0("归经=", paste(f$meridian, collapse = "/")))
  if (length(f$nature)) parts <- c(parts, paste0("四气=", paste(f$nature, collapse = "/")))
  if (length(f$taste)) parts <- c(parts, paste0("五味=", paste(f$taste, collapse = "/")))
  if (!is.null(f$toxicity)) parts <- c(parts, paste0("毒性=", f$toxicity))
  if (!is.null(f$dose_max)) parts <- c(parts, paste0("用量≤", f$dose_max, "克"))
  if (isTRUE(f$patent)) parts <- c(parts, "中成药")
  if (!is.null(f$contra_kind)) parts <- c(parts, paste0("禁忌类=", paste(f$contra_kind, collapse = "/")))
  if (isTRUE(f$has_contra)) parts <- c(parts, "有禁忌")
  if (!is.null(f$seven)) parts <- c(parts, paste0("七情=", paste(f$seven, collapse = "/")))
  if (isTRUE(f$pairs)) parts <- c(parts, "有配伍药对")
  filt_txt <- if (length(parts)) paste(parts, collapse = "；") else "（无）"

  plan_txt <- if (length(retr$plan)) paste(retr$plan, collapse = " → ") else "（无）"
  norm_line <- if (!identical(retr$question_raw %||% retr$question, retr$question)) {
    rag_kv("归一化", retr$question)
  }

  steps <- if (length(retr$trace)) {
    tags$table(
      class = "table table-sm table-borderless mb-2",
      style = "font-size:.84rem;",
      tags$thead(tags$tr(tags$th("#"), tags$th("步骤"), tags$th("证据"), tags$th("耗时"))),
      tags$tbody(lapply(seq_along(retr$trace), function(i) {
        tr <- retr$trace[[i]]
        tags$tr(tags$td(i), tags$td(tr$title), tags$td(tr$rows),
                tags$td(sprintf("%d ms", tr$ms %||% 0L)))
      }))
    )
  }

  sql_block <- if (length(retr$sql)) {
    tags$div(
      tags$div(class = "fw-semibold small mt-1", "生成的 Cypher（逻辑查询）"),
      tags$pre(style = paste0("font-size:.78rem;max-height:16rem;overflow:auto;",
                              "background:#f6f8fa;padding:.5rem;border-radius:.35rem;"),
               paste(retr$sql, collapse = "\n\n"))
    )
  }

  ev_items <- lapply(seq_along(retr$evidence), function(i) {
    e <- retr$evidence[[i]]
    loc <- if (identical(e$kind, "text")) {
      sprintf("%s · %s · 第%s行", e$citation$doc %||% "", e$citation$section %||% "",
              e$citation$line %||% "")
    } else {
      e$citation$source %||% "graph"
    }
    tags$li(sprintf("[E%d] %s — %s", i, e$title, loc))
  })

  tags$details(
    style = "margin-top:.5rem;",
    open = "open",
    tags$summary(style = "cursor:pointer;color:#2c6e49;",
                 sprintf("检索链路（意图：%s，%d 步，%.2fs，证据 %d 条）",
                         retr$intent, length(retr$trace), retr$elapsed %||% 0,
                         length(retr$evidence))),
    tags$div(class = "mt-2",
             tags$div(class = "fw-semibold small", "① 问题解析"),
             rag_kv("原始问题", retr$question_raw %||% retr$question),
             norm_line,
             rag_kv("识别实体", ent),
             rag_kv("解析条件", filt_txt),
             rag_kv("章节", if (nzchar(retr$section %||% "")) retr$section else "（未指定）"),
             rag_kv("意图", retr$intent),
             rag_kv("计划", plan_txt),
             if (nzchar(retr$unknown %||% "")) rag_kv("未知实体", retr$unknown),
             tags$div(class = "fw-semibold small mt-2", "② 执行步骤"),
             steps,
             sql_block,
             tags$div(class = "fw-semibold small mt-2", "③ 证据与出处"),
             tags$ul(style = "font-size:.82rem;padding-left:1.1rem;margin-bottom:0;", ev_items),
             if (!is.null(retr$list) && nrow(retr$list) > 0) {
               tags$div(class = "mt-2",
                        downloadButton("ragDownload",
                                       sprintf("下载完整名单（CSV，%d 行）", nrow(retr$list)),
                                       class = "btn-sm btn-outline-success"))
             })
  )
}

rag_chat_server <- function(input, output, session,
                             selected_herb = reactive(NULL),
                             chat_id = "chat") {
  has_key <- nzchar(rag_env("DEEPSEEK_API_KEY", ""))
  state <- reactiveValues(chat = NULL, last = NULL,
                          gen_mode = if (has_key) "llm" else "extractive",
                          key_source = if (has_key) "平台 Key" else "none")

  # 实际生效的回答方式：选了「平台 Key」但模型不可用时退回检索式
  effective_mode <- reactive({
    if (identical(state$gen_mode, "llm") && !is.null(state$chat)) "llm" else "extractive"
  })

  # ---- 启动即就绪：平台 Key 初始化 + 欢迎语（等前端渲染完再推消息） -------------
  session$onFlushed(function() {
    # 初始化失败也不能把整个会话带崩：只记录日志，聊天窗口仍可用（检索式回答兜底）
    tryCatch({
    # 注意：onFlushed 的回调不在 reactive 上下文里，必须先算到局部变量，
    # 再一次性写入 reactiveValues；不要写完后立刻回读 state$chat。
    key <- rag_env("DEEPSEEK_API_KEY", "")
    chat <- if (nzchar(key)) tryCatch(rag_chat_new(key), error = function(e) NULL) else NULL
    mode <- if (is.null(chat)) "extractive" else "llm"
    state$chat <- chat
    state$key_source <- if (nzchar(key)) "平台 Key" else "none"
    h <- tryCatch(rag_health(), error = function(e) list(ok = FALSE))
    size <- if (isTRUE(h$ok)) {
      sprintf("%s 个节点 / %s 条关系 / %s 段原文",
              format(h$nodes, big.mark = ","), format(h$relationships, big.mark = ","),
              format(h$chunks, big.mark = ","))
    } else "图谱未加载"
    chat_append(chat_id, if (identical(mode, "llm")) {
      sprintf(paste0("**《中华药海》知识图谱已就绪**（%s）。回答会带 [E1] 出处，检索过程可在右侧「检索链路」展开核对；",
                     "右侧「回答方式」可切换为「仅图谱检索」，只返回证据、不调用大模型。"), size)
    } else {
      sprintf("**《中华药海》知识图谱已就绪**（%s）。未配置平台大模型 Key，回答直接由图谱证据组织，仍带出处与检索链路。", size)
    })
    }, error = function(e) {
      message("GraphRAG 初始化失败：", conditionMessage(e))
    })
  }, once = TRUE)

  # ---- 状态栏 ---------------------------------------------------------------
  output$ragStatus <- renderUI({
    h <- tryCatch(rag_health(), error = function(e) list(ok = FALSE))
    graph_badge <- if (isTRUE(h$ok)) {
      sprintf("图谱：%s 味药 / %s 首方剂 / %s 段原文",
              format(h$herbs, big.mark = ","), format(h$formulas, big.mark = ","),
              format(h$chunks, big.mark = ","))
    } else "图谱未加载"

    vec <- tryCatch(rag_vec_status(), error = function(e) {
      list(enabled = FALSE, has_key = FALSE, has_index = FALSE, n = 0L, model = "BGE-M3")
    })
    vec_txt <- if (isTRUE(vec$enabled)) {
      sprintf("语义召回：已启用（%s · %s 段原文）", vec$model, format(vec$n, big.mark = ","))
    } else if (!isTRUE(vec$has_key)) {
      "语义召回：未启用（未配置 EMBED_API_KEY）"
    } else if (!isTRUE(vec$has_index)) {
      "语义召回：未启用（embeddings.rds 不可用）"
    } else "语义召回：未启用"
    vec_color <- if (isTRUE(vec$enabled)) "#2c6e49" else "#8a6d3b"
    tagList(
      if (identical(state$gen_mode, "extractive")) {
        tags$span(class = "badge bg-secondary", "GraphRAG：仅检索（不调用大模型）")
      } else if (!is.null(state$chat)) {
        tags$span(class = "badge bg-success", sprintf("GraphRAG：图谱检索 + %s", state$key_source))
      } else {
        tags$span(class = "badge bg-secondary", "GraphRAG：检索式回答（平台大模型不可用）")
      },
      tags$div(style = "margin-top:.35rem;font-size:.8rem;color:#666;", graph_badge),
      tags$div(
        style = paste0("margin-top:.2rem;font-size:.8rem;color:", vec_color, ";"),
        title = "语义召回为可选功能：需要 embeddings.rds 与 EMBED_API_KEY，缺失时自动跳过",
        vec_txt
      )
    )
  })

  # ---- 回答方式：默认图谱检索 + 平台 Key，可切换为仅检索 ----------------------
  observeEvent(input$ragGenMode, {
    state$gen_mode <- if (identical(input$ragGenMode, "extractive")) "extractive" else "llm"
  }, ignoreNULL = TRUE)

  # ---- 检索链路（折叠面板）---------------------------------------------------
  output$ragPipeline <- renderUI(rag_pipeline_ui(state$last))

  # ---- 下载完整名单（CSV）----------------------------------------------------
  output$ragDownload <- downloadHandler(
    filename = function() {
      sprintf("中华药海_检索名单_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      rag_write_list_csv(state$last$list, file)
    }
  )

  # ---- 提问 ---------------------------------------------------------------
  observeEvent(input$chat_user_input, {
    question <- trimws(input$chat_user_input)
    if (!nzchar(question)) return()

    retr <- tryCatch(rag_retrieve(question), error = function(e) {
      chat_append(chat_id, paste0("**检索失败：**", conditionMessage(e)))
      NULL
    })
    if (is.null(retr)) return()
    state$last <- retr

    if (!length(retr$evidence)) {
      chat_append(chat_id, rag_extractive(retr))
      return()
    }
    if (!identical(state$gen_mode, "llm") || is.null(state$chat)) {
      reason <- if (identical(state$gen_mode, "extractive")) "user_choice" else "no_key"
      chat_append(chat_id, rag_extractive(retr, reason))
      return()
    }
    note <- rag_answer_note(retr)
    msg <- paste0(if (nzchar(note)) paste0(note, "\n\n") else "",
                  rag_context(retr$evidence), "\n\n用户问题：", question)
    tryCatch(
      chat_append(chat_id, state$chat$stream_async(msg)),
      error = function(e) chat_append(chat_id, paste0("**请求失败：**", conditionMessage(e)))
    )
  })

  invisible(list(
    mode = effective_mode,
    gen_mode = reactive(state$gen_mode),
    last = reactive(state$last),
    evidence = reactive(state$last$evidence),
    trace = reactive(state$last$trace)
  ))
}
