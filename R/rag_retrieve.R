# =============================================================================
# rag_retrieve.R — 检索层：实体链接 → 条件解析 → 意图 → 多步查询计划 → 证据融合
#
# 数据：data/kg_store.rds（56,809 节点 / 299,331 关系 / 15,317 段原文）
# 引擎：内置 zhyhkg（R/kg/），所有图查询走 kg_cypher()，全文走 kg_fulltext_search()
#
# 相比旧版（rag_bundle + 单跳模板 + 2-gram 扫描）：
#   * 图查询升级为真正的 Cypher（变长路径、模式推导、OPTIONAL MATCH、聚合）
#   * 每个问题先出"查询计划"，再逐步执行，命中多步链（药→方剂→方中药→功效…）
#   * 全文检索用图库自带的 cjk 倒排索引（与 Neo4j 的 Lucene cjk 分析器一致）
#   * 综合查询支持任意条件组合（功效×病证×成分×分类×归经×性味×毒性×中成药）
#   * 分类归属核验（category_check）：回答“X 是不是 Y 类药”时核验分类名单，
#     并区分“分类归属”与“功效记载”（对应审稿人的“百合是否安神药”反例）
#   * 会话级指代继承：追问里的“它/该药”自动接上一轮实体（仅内存、按会话隔离）
# =============================================================================

# ---------------------------------------------------------------------------
# 1. 实体链接（最长匹配，词典来自图库本身）
# ---------------------------------------------------------------------------
rag_link_cache <- local({
  cache <- new.env(parent = emptyenv())
  function(terms = NULL) {
    if (is.null(terms)) return(cache)
    if (is.null(cache$env)) {
      # 用环境做 O(1) 查找（命名向量 [ ] 是线性扫描，20K 词条会明显变慢）
      cache$env <- list2env(
        stats::setNames(as.list(seq_len(nrow(terms))), terms$term),
        hash = TRUE, envir = new.env(parent = emptyenv())
      )
      cache$terms <- terms
      cache$maxlen <- max(nchar(terms$term))
    }
    cache
  }
})

rag_empty_link <- function() {
  data.frame(term = character(0), label = character(0), id = character(0),
             name = character(0), start = integer(0), stringsAsFactors = FALSE)
}

#' 从问题中识别人名地名……不，识别药/方/病证/功效/成分/著作/药理/分类等实体
rag_link <- function(text, terms = kg_terms()) {
  s <- gsub("[[:space:][:punct:]]", "", text)
  s <- gsub("[，。？！：；、“”‘’（）《》【】、]", "", s)
  n <- nchar(s)
  if (!nzchar(s)) return(rag_empty_link())
  cache <- rag_link_cache(terms)
  lookup <- cache$env
  maxlen <- min(cache$maxlen, n)
  hits <- list()
  i <- 1L
  while (i <= n) {
    matched <- NULL
    max_l <- min(maxlen, n - i + 1L)
    if (max_l >= 2L) {
      for (l in seq(max_l, 2L)) {
        cand <- substr(s, i, i + l - 1L)
        idx <- lookup[[cand]]
        if (!is.null(idx)) {
          matched <- list(term = cand, row = cache$terms[idx, , drop = FALSE])
          break
        }
      }
    }
    if (!is.null(matched)) {
      hits[[length(hits) + 1L]] <- data.frame(
        term = matched$term, label = matched$row$label,
        id = matched$row$id, name = matched$row$name, start = i,
        stringsAsFactors = FALSE
      )
      i <- i + nchar(matched$term)
    } else {
      i <- i + 1L
    }
  }
  if (!length(hits)) return(rag_empty_link())
  out <- do.call(rbind, hits)
  # "治疗""主治"这类通用词在原文里被当成方剂名，不应作为锚点
  generic_fm <- c("治疗", "主治", "用法", "用量", "组成", "功效", "禁忌", "附方",
                  "方剂", "制剂", "备注", "用法用量", "功能", "主治功能")
  out <- out[!(out$label == "Formula" & out$name %in% generic_fm), , drop = FALSE]
  if (!nrow(out)) return(rag_empty_link())
  out <- out[!duplicated(out$id), , drop = FALSE]

  # "治失眠"这类方剂名恰好和"治+病证"同形：补上病证链接，避免只命中方剂
  extra <- list()
  for (i in seq_len(nrow(out))) {
    if (!identical(out$label[i], "Formula") || !grepl("^治", out$name[i])) next
    suf <- sub("^治", "", out$name[i])
    if (!nzchar(suf) || suf == out$name[i]) next
    idx <- lookup[[suf]]
    if (is.null(idx)) next
    row <- cache$terms[idx, , drop = FALSE]
    if (!row$label %in% c("Indication", "Efficacy")) next
    extra[[length(extra) + 1L]] <- data.frame(
      term = suf, label = row$label, id = row$id, name = row$name,
      start = out$start[i] + 1L, stringsAsFactors = FALSE)
  }
  if (length(extra)) {
    out <- rbind(out, do.call(rbind, extra))
    out <- out[!duplicated(out$id), , drop = FALSE]
  }
  out
}

rag_entities <- function(link_df, label) {
  if (is.null(link_df) || !nrow(link_df)) return(rag_empty_link())
  link_df[link_df$label == label, , drop = FALSE]
}

rag_entity_names <- function(link_df, labels = NULL) {
  if (is.null(link_df) || !nrow(link_df)) return(character(0))
  d <- if (is.null(labels)) link_df else link_df[link_df$label %in% labels, , drop = FALSE]
  unique(d$name)
}

# ---------------------------------------------------------------------------
# 1b. 会话级指代继承（多轮追问）
# ---------------------------------------------------------------------------
# 检索层每题独立规划、不读历史，所以“它的用量是多少”这类追问拿不到锚点实体
# （改前实测：证据 0 条，并把“它的主要”误判成一味不存在的药）。
# 这里只保存“上一轮识别到的实体”，并且只在含代词、或极短的“呢/吗”追问时，
# 把它们并入本轮的实体链接；不改写问句本身，避免把药名里的“寒/温”误当筛选条件。
# 状态只在内存里，按会话（Shiny session token）隔离，进程重启即清空。

.rag_ctx <- new.env(parent = emptyenv())
RAG_CTX_MAX_SESSIONS <- 200L
RAG_PRONOUN <- paste0("(?<!其)它|该药|该方|此药|此方|这味药|这个药|这个方|",
                      "上述|前述|刚才|前面提到")

rag_ctx_key <- function(ctx = NULL) {
  if (!is.null(ctx) && length(ctx) == 1L && nzchar(as.character(ctx))) {
    return(as.character(ctx))
  }
  if (requireNamespace("shiny", quietly = TRUE)) {
    d <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
    if (!is.null(d) && !is.null(d$token) && nzchar(d$token)) return(paste0("s:", d$token))
  }
  "default"
}

rag_ctx_reset <- function(ctx = NULL) {
  k <- rag_ctx_key(ctx)
  if (exists(k, envir = .rag_ctx, inherits = FALSE)) rm(list = k, envir = .rag_ctx)
  invisible(TRUE)
}

rag_ctx_get <- function(ctx = NULL) {
  k <- rag_ctx_key(ctx)
  if (!exists(k, envir = .rag_ctx, inherits = FALSE)) return(NULL)
  get(k, envir = .rag_ctx, inherits = FALSE)
}

# 记住本轮实体：药 > 方剂 > 成分 > 病证 > 功效 > 分类，最多 6 个；
# extra 用于补记“被过滤条件吸收”的分类名（如“安神药有哪些”里的安神药）
rag_ctx_remember <- function(link_df, question, ctx = NULL, extra = character(0)) {
  prio <- c("Herb", "Formula", "Compound", "Indication", "Efficacy", "DrugCategory")
  ents <- character(0)
  if (!is.null(link_df) && nrow(link_df)) {
    d <- link_df[link_df$label %in% prio, , drop = FALSE]
    if (nrow(d)) {
      d <- d[order(match(d$label, prio)), , drop = FALSE]
      ents <- unique(utils::head(d$name, 6))
    }
  }
  extra <- extra[!is.na(extra) & nzchar(extra)]
  ents <- unique(c(ents, extra))
  if (!length(ents)) return(invisible(NULL))
  ents <- utils::head(ents, 6)
  k <- rag_ctx_key(ctx)
  # 会话数上限（按名字顺序淘汰，够用即可，不做 LRU）
  if (length(ls(.rag_ctx)) >= RAG_CTX_MAX_SESSIONS &&
      !exists(k, envir = .rag_ctx, inherits = FALSE)) {
    rm(list = ls(.rag_ctx)[1], envir = .rag_ctx)
  }
  assign(k, list(entities = ents, question = question,
                 time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), envir = .rag_ctx)
  invisible(TRUE)
}

rag_has_pronoun <- function(q) grepl(RAG_PRONOUN, q, perl = TRUE)

# 是否属于“追问”：有代词且本轮没有锚点实体；或极短、以“呢/吗”收尾且无锚点
rag_is_followup <- function(q, link_df) {
  if (rag_has_pronoun(q)) return(!rag_has_anchor(link_df))
  if (rag_has_anchor(link_df)) return(FALSE)
  # 清单/统计类问句即使很短也不是追问（“有毒的药有哪些呢”）
  if (grepl("哪些|哪几|有哪些|多少|都有|列出|名单|清单|统计|排行", q)) return(FALSE)
  n <- nchar(gsub("[[:punct:][:space:]]", "", q))
  n > 0L && n <= 8L && grepl("(呢|吗|么)[？?]?\\s*$", q)
}

# ---------------------------------------------------------------------------
# 1c. 异名归一（正名解析）
# ---------------------------------------------------------------------------
# 图谱里除 751 味专著药，还有 156 个只出现在方剂组成里的药名（瓜蒌、官桂、生地…）。
# 它们没有分类/性味/功效边，直接查会得到“未载”的空档案，方剂也会漏掉。
# 归一表分两层：
#   ① 图自带的 HerbVariant 边（35 条，如 瓜蒌→栝楼、生地→干地黄），来自建库时的
#      异名整理，可直接用；
#   ② 手工补充表：炮制品名、简称、OCR 变体，每条都核对过目标确实是专著药。
# 只在检索层归一，不改图库；输出里注明“按异名归一”，可逐条核对。
# 另有“一名对两药”的情况（芍药 → 白芍药/赤芍药），单列说明，不做单一归一。

RAG_ALIAS_MANUAL <- c(
  # 同药异写 / 简称
  "官桂" = "肉桂", "山栀" = "栀子", "黑山栀" = "栀子", "栀子仁" = "栀子",
  "白藓皮" = "白鲜皮", "赭石" = "代赭石",
  "桔皮" = "橘皮", "陈橘皮" = "橘皮", "广皮" = "橘皮",
  "瓜蒌仁" = "栝楼", "栝楼仁" = "栝楼", "瓜蒌根" = "天花粉",
  "葶苈" = "葶苈子", "熟干地黄" = "熟地黄", "败酱" = "败酱草", "蒡子" = "牛蒡子",
  "生地黄" = "干地黄", "石决" = "石决明",
  # OCR 变体
  "廑虫" = "䗪虫", "鏖虫" = "䗪虫",
  # 炮制品名（同一药物的不同炮制规格）
  "熟大黄" = "大黄", "熟附子" = "附子", "大附子" = "附子", "炮姜" = "干姜",
  "炙草" = "甘草", "粉甘草" = "甘草",
  # 同药异名 / 简称
  "莲肉" = "莲子", "甘菊花" = "菊花", "白菊花" = "菊花", "滁菊花" = "菊花",
  "双钩藤" = "钩藤", "钩藤皮" = "钩藤",
  "飞滑石" = "滑石", "白龙骨" = "龙骨", "朱茯神" = "茯神"
)

# 图谱把“橘红”连到“化橘红”，但方书里的橘红多指橘皮去白，学理存疑 → 暂不启用
RAG_ALIAS_DENY <- c("橘红")

# 一名对两药：本书分列两条，方剂里的写法无法判定具体品种
RAG_ALIAS_AMBIG <- list(
  "芍药" = c("白芍药", "赤芍药")
)

.rag_alias <- new.env(parent = emptyenv())

rag_alias_map <- function() {
  if (!is.null(.rag_alias$map)) return(.rag_alias$map)
  m <- character(0)
  g <- tryCatch(kg_store_get(), error = function(e) NULL)
  if (!is.null(g) && !is.null(g$nodes)) {
    # 直接读内存图库的列，比走 Cypher 快一个量级（这张表只在进程内建一次）
    vid <- g$label_ids[["HerbVariant"]] %||% integer(0)
    hid <- g$label_ids[["Herb"]] %||% integer(0)
    if (length(vid) && length(hid)) {
      alias <- as.character(g$nodes$name[vid])
      target <- as.character(g$nodes$herb_name[vid])
      inc <- g$nodes$in_corpus[hid] %in% c(TRUE, "true")
      mono <- as.character(g$nodes$name[hid])[inc]
      keep <- !is.na(alias) & nzchar(alias) & !is.na(target) &
        !(alias %in% RAG_ALIAS_DENY)
      m <- stats::setNames(target[keep], alias[keep])
      m <- m[m %in% mono]
    }
  }
  m <- c(m, RAG_ALIAS_MANUAL)
  if (!length(m)) {
    # 兜底：图库结构异常时走 Cypher
    v <- tryCatch(
      rag_q("MATCH (v:HerbVariant) RETURN v.name AS alias, v.herb_name AS target"),
      error = function(e) data.frame())
    if (nrow(v)) {
      keep <- !is.na(v$alias) & nzchar(v$alias) & !is.na(v$target) &
        !(v$alias %in% RAG_ALIAS_DENY)
      m <- stats::setNames(as.character(v$target[keep]), as.character(v$alias[keep]))
    }
    m <- c(m, RAG_ALIAS_MANUAL)
  }
  # 目标必须是带专著的药（in_corpus），否则归一没有落点
  mono <- tryCatch({
    gg <- kg_store_get()
    hid <- gg$label_ids[["Herb"]] %||% integer(0)
    as.character(gg$nodes$name[hid])[gg$nodes$in_corpus[hid] %in% c(TRUE, "true")]
  }, error = function(e) character(0))
  if (length(mono)) m <- m[m %in% mono]
  m <- m[!duplicated(names(m))]
  .rag_alias$map <- m
  m
}

# 异名 → 正名（歧义名原样返回；未收录的原样返回）
rag_canon <- function(name) {
  if (length(name) != 1L || is.na(name) || !nzchar(name)) return(name)
  if (name %in% names(RAG_ALIAS_AMBIG)) return(name)
  m <- rag_alias_map()
  if (name %in% names(m)) unname(m[[name]]) else name
}

# 该药在方剂里可能出现的全部写法（自身 + 正名 + 归一到它的异名）
rag_herb_names <- function(name) {
  if (length(name) != 1L || is.na(name) || !nzchar(name)) return(character(0))
  if (name %in% names(RAG_ALIAS_AMBIG)) {
    return(unique(c(name, RAG_ALIAS_AMBIG[[name]])))
  }
  m <- rag_alias_map()
  cn <- rag_canon(name)
  unique(c(name, cn, names(m)[m == cn]))
}

# 给输出加一句“按异名归一”的可核对说明
rag_alias_note <- function(name) {
  if (length(name) != 1L || is.na(name)) return("")
  if (name %in% names(RAG_ALIAS_AMBIG)) {
    return(sprintf("〔本书将「%s」分列为 %s，方剂中未注明品种〕", name,
                   paste(RAG_ALIAS_AMBIG[[name]], collapse = "、")))
  }
  cn <- rag_canon(name)
  if (identical(cn, name)) "" else sprintf("〔按异名归一：%s → %s〕", name, cn)
}

# ---------------------------------------------------------------------------
# 2. 条件解析（分类 / 归经 / 性味 / 毒性 / 中成药 / 剂量）
# ---------------------------------------------------------------------------
RAG_TOX <- c("大毒", "小毒", "有毒", "无毒", "微毒", "剧毒")
RAG_NATURE <- c("寒", "热", "温", "凉", "平")
RAG_TASTE <- c("酸", "苦", "甘", "辛", "咸", "涩", "淡")
RAG_MERIDIAN <- c("心包", "心", "肝", "脾", "肺", "肾", "胃", "大肠",
                  "小肠", "膀胱", "胆", "三焦")

RAG_NATURE_ALIAS <- c("微温" = "温", "大寒" = "寒", "微寒" = "寒", "大热" = "热",
                      "寒凉" = "寒", "温热" = "温")

rag_filters <- function(q) {
  out <- list()

  # 1) 药物分类：直接匹配图里的 DrugCategory 名称
  cats <- kg_terms()
  cat_terms <- cats$name[cats$label == "DrugCategory"]
  hit <- cat_terms[vapply(cat_terms, function(c) grepl(c, q, fixed = TRUE), logical(1))]
  if (length(hit)) out$category <- unique(hit)

  # 先把已识别的分类名从问句里去掉，避免“清热药”被误读成“热性药”
  q2 <- q
  for (cc in (out$category %||% character(0))) q2 <- gsub(cc, "", q2, fixed = TRUE)

  # 2) 归经：“肝经”“归肝、肾经”“入肝经”
  m <- regmatches(q2, gregexpr("(心包|心|肝|脾|肺|肾|胃|胆|小肠|大肠|膀胱|三焦)经", q2))[[1]]
  if (length(m)) out$meridian <- unique(sub("经$", "", m))

  # 3) 四气：“微温”“温性”“性温”“温药”
  nat <- character(0)
  for (x in regmatches(q2, gregexpr("[微大][寒热温凉]|[微大]?[寒热温凉平]性|性[微大]?[寒热温凉平]|[微大]?[寒热温凉平]药", q2))[[1]]) {
    v <- gsub("性|药", "", x)
    nat <- c(nat, if (v %in% names(RAG_NATURE_ALIAS)) RAG_NATURE_ALIAS[[v]] else v)
  }
  nat <- nat[nat %in% RAG_NATURE]
  if (length(nat)) out$nature <- unique(nat)

  # 4) 五味：“苦味”“味苦”“辛甘味”
  tas <- character(0)
  for (x in regmatches(q2, gregexpr("[酸苦甘辛咸涩淡]{1,4}味|味[酸苦甘辛咸涩淡]{1,4}", q2))[[1]]) {
    tas <- c(tas, strsplit(gsub("味", "", x), "")[[1]])
  }
  tas <- tas[tas %in% RAG_TASTE]
  if (length(tas)) out$taste <- unique(tas)

  # 5) 毒性：“有毒”=任意毒性（不等于“无毒”）
  tox <- RAG_TOX[vapply(RAG_TOX, function(t) grepl(t, q, fixed = TRUE), logical(1))]
  if (length(tox)) out$toxicity <- tox[1]

  # 6) 配伍 / 禁忌
  if (grepl("孕妇|妊娠|胎", q)) {
    out$contra_kind <- "妊娠禁忌"
  } else if (grepl("配伍禁忌", q, fixed = TRUE)) {
    # 配伍禁忌在抽取时多归入"七情"类，这里两类一起查
    out$contra_kind <- c("七情", "配伍禁忌")
  } else if (grepl("病证禁忌", q, fixed = TRUE)) {
    out$contra_kind <- "病证禁忌"
  } else if (grepl("禁忌", q, fixed = TRUE)) {
    out$has_contra <- TRUE
  }
  sev <- c("相恶", "相反", "相畏", "相杀", "相使", "相须")
  hit_sev <- sev[vapply(sev, function(x) grepl(x, q, fixed = TRUE), logical(1))]
  if (grepl("十八反", q, fixed = TRUE)) hit_sev <- unique(c(hit_sev, "相反"))
  if (grepl("十九畏", q, fixed = TRUE)) hit_sev <- unique(c(hit_sev, "相畏"))
  if (length(hit_sev)) out$seven <- hit_sev
  else if (grepl("七情", q, fixed = TRUE)) out$seven <- "any"
  if (grepl("药对|配伍应用|配伍的药|哪些配伍|有什么配伍", q)) out$pairs <- TRUE

  # 6b) 剂量上限："低于6克""6克以下""不超过6g""≤6克"
  dm <- regmatches(q, regexpr("(?:低于|小于|少于|不超过|不足|≤|<)\\s*[0-9]+(?:\\.[0-9]+)?\\s*克", q, perl = TRUE))
  if (!length(dm)) {
    dm <- regmatches(q, regexpr("[0-9]+(?:\\.[0-9]+)?\\s*克\\s*(?:以下|以内|之下)", q, perl = TRUE))
  }
  if (length(dm)) {
    out$dose_max <- as.numeric(sub(".*?([0-9]+(?:\\.[0-9]+)?).*", "\\1", dm[1]))
  }

  # 7) 中成药 / 已上市
  out$patent <- grepl("中成药|已上市|成药", q)
  # 7) 内服 / 外用
  if (grepl("外用", q, fixed = TRUE)) out$route <- "外用"
  # 8) “或”：同一维度多个取值时按并集而不是交集
  out$or <- grepl("或", q)
  out
}

# 条件个数（用于意图判定）
rag_n_filters <- function(f) {
  if (is.null(f)) return(0L)
  length(f$category) + length(f$meridian) + length(f$nature) +
    length(f$taste) + (!is.null(f$toxicity)) +
    (!is.null(f$contra_kind)) + isTRUE(f$has_contra) +
    (!is.null(f$seven)) + isTRUE(f$pairs) + (!is.null(f$dose_max)) + isTRUE(f$patent)
}

rag_has_filters <- function(f) rag_n_filters(f) > 0L

# 是否能作为"问某味药/某首方/某个成分/某个病证/某个功效"的锚点实体
# （Book、Section 之类的链接是误命中，不应阻止词表反查）
rag_has_anchor <- function(link_df) {
  !is.null(link_df) && nrow(link_df) > 0L &&
    any(link_df$label %in% c("Herb", "Formula", "Compound", "Indication", "Efficacy"))
}

# ---------------------------------------------------------------------------
# 3. 意图
# ---------------------------------------------------------------------------
RAG_SAFETY_KW <- c("禁忌", "毒性", "有毒", "无毒", "孕妇", "妊娠", "相反", "相恶",
                   "相畏", "相杀", "十八反", "十九畏", "副作用", "安全", "配伍禁忌",
                   "不能同用", "慎用")
RAG_FORMULA_KW <- c("方剂", "方", "中成药", "复方", "出处", "出自", "组成", "由哪些药",
                    "配方", "制剂", "丸", "散", "汤")
RAG_COMPOUND_KW <- c("成分", "化合物", "含有", "含", "药理", "作用机制", "有效成分",
                     "含量", "化学")
RAG_REVERSE_KW <- c("哪些药", "什么药", "有哪些", "哪味药", "哪几种", "用什么药",
                    "哪些中药", "都治", "都能", "用于哪些")
RAG_STATS_KW <- c("统计", "多少", "数量", "排行", "排行", "最多", "最高", "占比",
                  "分布", "频次", "top", "前几", "整本书", "全书", "全库", "总体",
                  "一共", "共有多", "概况", "概述")
RAG_COMPARE_KW <- c("区别", "对比", "异同", "哪个更", "鉴别", "不同", "差异")
RAG_MULTIHOP_KW <- c("方中", "方剂里", "组成", "这些药", "其中", "进而", "以及",
                     "同时", "结合", "配伍", "含有这些", "所用")

# ---------------------------------------------------------------------------
# 3b. 分类归属 / 成员核验（对应审稿人的“百合反例”）
# ---------------------------------------------------------------------------
# 旧版的问题不是“检索不到百合”，而是把“功效（清心安神）”当成了“分类（安神药）”。
# 这里把“X 是不是 Y 类药”路由到一个专门步骤：先查该药的分类边，再查 Y 的名单，
# 给出“在/不在”的核验结论，并显式区分“分类归属”与“功效记载”。
RAG_CAT_CUE <- paste(
  "属于", "算是", "算不算", "算作", "当作", "作为", "用作", "是否为", "是不是",
  "能不能当", "可否当", "可不可以当", "归为", "归入", "列为", "列入", "分为",
  "有没有", sep = "|")
RAG_CAT_CLASS_CUE <- c("哪一类", "哪类药", "什么类", "何种类型", "类别",
                       "分类归属", "是什么药")
RAG_CAT_LIST_CUE <- c("哪些", "哪几", "有哪些", "多少味", "都有", "列出",
                      "名单", "清单", "排行")

rag_category_question <- function(q, link_df, filters = list()) {
  herbs <- rag_entities(link_df, "Herb")
  # 只处理“单味药 + 归属判断”；清单类（哪些/多少味）走词表反查
  if (nrow(herbs) != 1L) return(FALSE)
  if (any(vapply(RAG_CAT_LIST_CUE, function(k) grepl(k, q, fixed = TRUE), logical(1)))) {
    return(FALSE)
  }
  is_cat_sentence <- grepl("是[^，。？?]{0,10}药(吗|嘛|么|不|\\?|？|$)", q)
  has_cat <- length(filters$category %||% character(0)) > 0L
  if (has_cat) return(grepl(RAG_CAT_CUE, q) || is_cat_sentence)
  any(vapply(RAG_CAT_CLASS_CUE, function(k) grepl(k, q, fixed = TRUE), logical(1))) ||
    grepl("是[^，。？?]{0,10}药(吗|嘛|么|\\?|？|$)", q)
}

rag_any_kw <- function(q, kws) {
  any(vapply(kws, function(k) grepl(k, q, fixed = TRUE), logical(1)))
}

rag_intent <- function(q, link_df, filters = list()) {
  herbs <- rag_entities(link_df, "Herb")
  labels <- unique(link_df$label)
  n_f <- rag_n_filters(filters)

  # 只有"筛药材"的词表条件、没有点名具体实体 → 词表反查
  # （“温性药有哪些”“归肝经的药有多少味”“有毒的清热药”都走这里）
  if (n_f >= 1L && !rag_has_anchor(link_df)) return("vocab")

  # “X 是不是 Y 类药”必须先核验分类名单，不能落进普通事实问答
  if (rag_category_question(q, link_df, filters)) return("category")

  if (rag_any_kw(q, RAG_STATS_KW) && n_f == 0L && !rag_has_anchor(link_df) &&
      !rag_any_kw(q, RAG_SAFETY_KW)) return("stats")
  if (rag_any_kw(q, RAG_COMPARE_KW) && nrow(herbs) >= 2) return("compare")
  if (rag_any_kw(q, RAG_SAFETY_KW)) return("safety")
  if (rag_any_kw(q, RAG_FORMULA_KW) && (nrow(herbs) || "Formula" %in% labels)) return("formula")
  if ("Compound" %in% labels || rag_any_kw(q, RAG_COMPOUND_KW)) return("compound")
  if (!("Herb" %in% labels) && any(c("Indication", "Efficacy") %in% labels)) return("reverse")
  if (rag_any_kw(q, RAG_REVERSE_KW)) return("reverse")
  if (rag_any_kw(q, RAG_MULTIHOP_KW)) return("multihop")
  "fact"
}

# ---------------------------------------------------------------------------
# 4. 小工具
# ---------------------------------------------------------------------------
rag_trunc <- function(x, n = 200) {
  x <- gsub("\n", " ", rag_cell(x))
  if (nchar(x) > n) paste0(substr(x, 1, n), "…") else x
}

rag_join <- function(x, sep = "、", max_n = Inf) {
  x <- x[nzchar(x)]
  if (is.finite(max_n)) x <- utils::head(x, max_n)
  paste(x, collapse = sep)
}

# data.frame 行 → 文本行
rag_fmt_rows <- function(df, cols, fmt, limit = 40) {
  if (!nrow(df)) return(character(0))
  df <- utils::head(df, limit)
  vapply(seq_len(nrow(df)), function(i) {
    vals <- lapply(cols, function(cn) if (cn %in% names(df)) rag_cell(df[[cn]][i]) else "")
    names(vals) <- cols
    fmt(vals)
  }, "")
}

rag_graph_ev <- function(title, text, source, score = 1) {
  if (!length(text) || !any(nzchar(text))) return(NULL)
  rag_ev("graph", title, paste(text, collapse = "\n"), score,
         list(source = source))
}

# 名称清单：每行 per_line 个，避免几百字的超长单行
rag_name_lines <- function(x, per_line = 12) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character(0))
  idx <- seq(1L, length(x), by = per_line)
  vapply(idx, function(i) paste(x[i:min(i + per_line - 1L, length(x))], collapse = "、"), "")
}

# 清单类证据：正文给完整名称（精简），明细放 citation$rows 供 CSV 下载
rag_list_ev <- function(title, names, rows, source) {
  txt <- rag_name_lines(names)
  if (!length(txt)) return(NULL)
  txt <- c(txt, "（完整明细可在「检索链路」面板下载 CSV）")
  rag_ev("graph", title, paste(txt, collapse = "\n"), 1,
         list(source = source, list = TRUE, rows = rows))
}

# 明细表：把 Cypher 集合属性（list 列）压成字符串，CSV 才可读
rag_flat_df <- function(d) {
  for (cn in names(d)) {
    if (is.list(d[[cn]])) d[[cn]] <- vapply(d[[cn]], rag_cell, "")
  }
  d
}

# 多份清单合并：列取并集，用「来源」列区分
rag_bind_lists <- function(parts) {
  parts <- Filter(function(d) !is.null(d) && nrow(d) > 0, parts)
  if (!length(parts)) return(NULL)
  cols <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(d) {
    for (cn in setdiff(cols, names(d))) d[[cn]] <- ""
    d[cols]
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

# 合并多路文本证据并按 chunk 去重（避免同一块被提及/全文各算一次而虚高）
rag_dedup_ev <- function(evs) {
  if (!length(evs)) return(list())
  keys <- vapply(evs, function(e) e$citation$chunk_id %||% e$title %||% "", "")
  evs[!duplicated(keys)]
}

# 单步执行壳：捕获错误、计时、记录轨迹
rag_step <- function(title, fn) {
  t0 <- proc.time()[["elapsed"]]
  ev <- tryCatch(fn(), error = function(e) {
    warning("检索步骤「", title, "」失败：", conditionMessage(e), call. = FALSE)
    NULL
  })
  evs <- if (is.null(ev)) list() else Filter(function(x) is.list(x) && nzchar(x$text %||% ""),
                                             if (is.list(ev) && !is.null(ev$kind)) list(ev) else ev)
  list(evidence = evs,
       trace = list(title = title, rows = length(evs),
                    ms = round((proc.time()[["elapsed"]] - t0) * 1000)))
}

# ---------------------------------------------------------------------------
# 5. 单实体步骤
# ---------------------------------------------------------------------------

# 5.1 药物档案（属性 + 功效 + 主治，1–2 跳）
# 异名先归一到正名；「芍药」这种一名对两药的，把两条档案并列给出
step_herb_profile <- function(name) {
  amb <- if (name %in% names(RAG_ALIAS_AMBIG)) RAG_ALIAS_AMBIG[[name]] else NULL
  if (!is.null(amb)) {
    lines <- sprintf("「%s」在本书分列为 %s 两条，方剂组成里的“%s”未注明具体品种，两条档案并列如下。",
                     name, paste(amb, collapse = "、"), name)
    for (h in amb) {
      d <- rag_q(
        "MATCH (h:Herb {name:$name})
 RETURN h.category_raw AS category, h.taste_raw AS taste,
        h.meridian_raw AS meridian, h.toxicity_raw AS toxicity,
        h.dosage_raw AS dosage,
        [(h)-[:HAS_EFFICACY]->(e:Efficacy) | e.name] AS efficacies,
        [(h)-[:ALSO_TREATS]->(i:Indication) | i.name][..6] AS indications",
        list(name = h))
      if (!nrow(d)) next
      r <- d[1, ]
      lines <- c(lines, sprintf("· %s［%s］性味：%s；归经：%s；功效：%s；主治：%s；用法：%s",
        h, rag_or(r$category, "未分类"),
        gsub("[。；;，,]+$", "", rag_or(r$taste, "未载")),
        gsub("[。；;，,]+$", "", rag_or(r$meridian, "未载")),
        rag_or(r$efficacies, "—"), rag_or(r$indications, "—"), rag_or(r$dosage, "—")))
    }
    return(list(rag_graph_ev(paste0("药物档案：", name), lines, "graph:Herb")))
  }

  cn <- rag_canon(name)
  df <- rag_q(
    "MATCH (h:Herb {name:$name})
 RETURN h.name AS name, h.category_raw AS category, h.taste_raw AS taste,
        h.nature_raw AS nature, h.meridian_raw AS meridian, h.toxicity_raw AS toxicity,
        h.dosage_raw AS dosage, h.first_record AS first_record, h.latin_name AS latin,
        h.family AS family, h.part AS part,
        [(h)-[:HAS_EFFICACY]->(e:Efficacy) | e.name] AS efficacies,
        [(h)-[:ALSO_TREATS]->(i:Indication) | i.name][..10] AS indications",
    list(name = cn))
  if (!nrow(df)) return(list())
  r <- df[1, ]
  taste <- gsub("[。；;，,]+$", "", rag_or(r$taste, "未载"))
  meridian <- gsub("[。；;，,]+$", "", rag_or(r$meridian, "未载"))
  toxic <- rag_or(r$toxicity, "")
  lines <- c(
    sprintf("%s（%s）%s", rag_or(r$name, cn), rag_or(r$category, "未分类"),
            if (!identical(cn, name)) paste0("　", rag_alias_note(name)) else ""),
    sprintf("性味：%s；归经：%s%s", taste, meridian,
            if (nzchar(toxic)) paste0("；毒性：", toxic) else ""),
    sprintf("功效：%s", rag_or(r$efficacies, "—")),
    sprintf("主治：%s", rag_or(r$indications, "—")),
    sprintf("用法用量：%s", rag_or(r$dosage, "—")),
    sprintf("始载：%s；基原：%s；药用部位：%s",
            rag_or(r$first_record, "—"), rag_or(r$latin, "—"), rag_or(r$part, "—"))
  )
  rag_graph_ev(paste0("药物档案：", name), lines, "graph:Herb")
}

# 5.2 功效 → 主治（2 跳）
step_herb_efficacy_chain <- function(name) {
  cn <- rag_canon(name)
  df <- rag_q(
    "MATCH (h:Herb {name:$name})-[:HAS_EFFICACY]->(e:Efficacy)
 RETURN e.name AS eff, e.func_family AS func,
        [(e)-[:TREATS]->(i:Indication) | i.name][..5] AS inds
 ORDER BY eff", list(name = cn))
  if (!nrow(df)) return(list())
  lines <- rag_fmt_rows(df, c("eff", "func", "inds"), function(v) {
    inds <- rag_cell(v$inds)
    sprintf("%s%s%s", v$eff,
            if (nzchar(v$func)) paste0("（", v$func, "类）") else "",
            if (nzchar(inds)) paste0(" → ", inds) else "")
  })
  rag_graph_ev(sprintf("%s：功效 → 主治（2 步）%s", cn,
                       if (!identical(cn, name)) rag_alias_note(name) else ""),
               lines, "graph:HAS_EFFICACY>TREATS")
}

# 5.3 安全性（毒性 / 七情 / 禁忌 / 用法）
step_herb_safety <- function(name) {
  cn <- rag_canon(name)
  df <- rag_q(
    "MATCH (h:Herb {name:$name})
 RETURN [(h)-[:HAS_TOXICITY]->(t:Toxicity) | t.name] AS tox,
        [(h)-[r:SEVEN_RELATION]->(o:Herb) | o.name + '（' + r.relation + '）'] AS seven,
        [(h)-[:CONTRAINDICATED_IN]->(c:Contraindication) | left(c.text, 90)][..6] AS contra,
        [(h)-[:USED_BY_ROUTE]->(u:UsageRoute) | u.name] AS route,
        h.dosage_raw AS dosage", list(name = cn))
  if (!nrow(df)) return(list())
  r <- df[1, ]
  parts <- c(sprintf("毒性：%s", rag_or(r$tox, "未标注（一般视为无毒）")))
  seven <- rag_cell(r$seven)
  if (nzchar(seven)) parts <- c(parts, paste0("七情配伍：", seven))
  contra <- rag_cell(r$contra, "\n")
  if (nzchar(contra)) {
    items <- strsplit(contra, "\n", fixed = TRUE)[[1]]
    items <- items[nzchar(trimws(items))]
    parts <- c(parts, "使用禁忌：", paste0("  · ", items))
  }
  route <- rag_cell(r$route)
  if (nzchar(route)) parts <- c(parts, paste0("给药途径：", route))
  parts <- c(parts, "提示：毒性/禁忌须在医师或药师指导下使用。")
  rag_graph_ev(sprintf("安全性：%s%s", name,
                       if (!identical(cn, name)) paste0("（", cn, "）") else ""),
               parts, "graph:safety")
}

# 5.4 方剂（药 → 方剂 → 出处/组成）；异名（瓜蒌/瓜蒌仁…）合并统计
# 性能：`{name:$name}` 内联匹配走索引（约 30 ms），`WHERE h.name IN $names` 会退化成
# 逐边扫描（实测慢 30 倍），所以这里“逐写法取名单 + 按名单取详情”，不做 IN 过滤。
step_herb_formulas <- function(name, limit = 12) {
  names <- rag_herb_names(name)
  all_names <- character(0)
  for (nm in names) {
    d <- rag_q(
      "MATCH (f:Formula)-[:HAS_INGREDIENT]->(h:Herb {name:$name})
       RETURN DISTINCT f.name AS f", list(name = nm))
    if (nrow(d)) all_names <- c(all_names, as.character(d$f))
  }
  all_names <- sort(unique(all_names[!is.na(all_names) & nzchar(all_names)]))
  total <- length(all_names)
  if (!total) return(list())
  pick <- utils::head(all_names, limit)
  df <- rag_q(
    "MATCH (f:Formula) WHERE f.name IN $pick
 OPTIONAL MATCH (f)-[:FROM_BOOK]->(b:Book)
 RETURN DISTINCT f.name AS formula, b.name AS source, f.is_patent AS patent,
        left(f.indication_text, 40) AS indication,
        [(f)-[:HAS_INGREDIENT]->(x:Herb) | x.name][..8] AS ingredients
 ORDER BY formula", list(pick = as.list(pick)))
  if (!nrow(df)) return(list())
  # 同一首方可能有两个出处版本（FROM_BOOK 多行），按方名去重后再截断
  df <- df[!duplicated(df$formula), , drop = FALSE]
  df <- utils::head(df, limit)
  lines <- rag_fmt_rows(df, c("formula", "source", "patent", "indication", "ingredients"),
                        function(v) {
    ind <- sub("^[：:；;，,、\\s]+", "", v$indication)
    sprintf("%s（%s）%s%s", v$formula, if (nzchar(v$source)) v$source else "未注明出处",
            if (identical(toupper(v$patent), "TRUE")) "·中成药" else "",
            if (nzchar(ind)) paste0("：", ind) else "")
  })
  merged <- setdiff(names, name)
  merged_txt <- if (!length(merged)) "" else {
    shown <- utils::head(merged, 6)
    sprintf("〔已并入异名：%s%s〕", paste(shown, collapse = "、"),
            if (length(merged) > length(shown)) sprintf(" 等 %d 种", length(merged)) else "")
  }
  rag_graph_ev(sprintf("含 %s 的方剂（共 %d 首，按方名去重；列前 %d）%s", name, total, nrow(df),
                       merged_txt),
               lines, "graph:HAS_INGREDIENT")
}

# 5.4b 含某药、且主治文本含某病证的方剂
# 注意：全库只有约 1/4 的方剂带主治文本，本步骤是“有文本可查”的那部分，不宣称完整
step_formula_indication <- function(herb, indication, limit = 10) {
  names <- rag_herb_names(herb)
  hits <- list()
  for (nm in names) {
    d <- rag_q(
      "MATCH (f:Formula)-[:HAS_INGREDIENT]->(h:Herb {name:$name})
 WHERE f.indication_text CONTAINS $ind
 RETURN DISTINCT f.name AS formula, left(f.indication_text, 60) AS ind,
        [(f)-[:HAS_INGREDIENT]->(x:Herb) | x.name] AS ingredients
 ORDER BY formula LIMIT $limit",
      list(name = nm, ind = indication, limit = as.integer(limit)))
    if (nrow(d)) hits[[nm]] <- d
  }
  if (!length(hits)) {
    return(list(rag_graph_ev(
      sprintf("含 %s 且主治含「%s」的方剂（0 首）", herb, indication),
      "方剂主治文本里没有同时命中这味药与该病证的条目（全库仅约 1/4 方剂带主治文本，可能只是未载）。",
      "graph:formula_indication", 1.3)))
  }
  df <- do.call(rbind, hits)
  df <- df[!duplicated(df$formula), , drop = FALSE]
  df <- df[order(df$formula), , drop = FALSE]
  df <- utils::head(df, limit)
  lines <- rag_fmt_rows(df, c("formula", "ingredients", "ind"), function(v) {
    sprintf("%s：%s；主治：%s", v$formula, rag_cell(v$ingredients), v$ind)
  })
  rag_graph_ev(sprintf("含 %s 且主治含「%s」的方剂（%d 首；仅覆盖有主治文本的方剂）",
                       herb, indication, nrow(df)),
               lines, "graph:formula_indication", 1.3)
}

# 5.5 方剂组成链（3 跳：药 → 方剂 → 方中其他药）
step_formula_partners <- function(name, limit = 8) {
  names <- rag_herb_names(name)
  all_names <- character(0)
  for (nm in names) {
    d <- rag_q(
      "MATCH (f:Formula)-[:HAS_INGREDIENT]->(h:Herb {name:$name})
       RETURN DISTINCT f.name AS f", list(name = nm))
    if (nrow(d)) all_names <- c(all_names, as.character(d$f))
  }
  all_names <- sort(unique(all_names[!is.na(all_names) & nzchar(all_names)]))
  if (!length(all_names)) return(list())
  pick <- utils::head(all_names, limit)
  df <- rag_q(
    "MATCH (f:Formula) WHERE f.name IN $pick
 MATCH (f)-[:HAS_INGREDIENT]->(o:Herb)
 RETURN f.name AS formula, collect(DISTINCT o.name) AS partners
 ORDER BY formula", list(pick = as.list(pick)))
  if (!nrow(df)) return(list())
  lines <- vapply(seq_len(nrow(df)), function(i) {
    ps <- setdiff(as.character(unlist(df$partners[[i]])), names)
    ps <- ps[!is.na(ps) & nzchar(ps)]
    if (!length(ps)) return("")
    sprintf("%s：%s", df$formula[i], paste(utils::head(ps, 8), collapse = "、"))
  }, "")
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(list())
  rag_graph_ev(sprintf("%s：方剂 → 方中其他药（3 步）%s", name,
                       if (!identical(rag_canon(name), name)) rag_alias_note(name) else ""),
               lines, "graph:formula_chain")
}

# 5.6 成分（药 → 成分 → 化学分类）：数量类问题，不截断
step_herb_compounds <- function(name) {
  cn <- rag_canon(name)
  n <- rag_q("MATCH (h:Herb {name:$name})-[:CONTAINS]->(m:Compound)
 RETURN count(DISTINCT m) AS n", list(name = cn))
  df <- rag_q(
    "MATCH (h:Herb {name:$name})-[c:CONTAINS]->(m:Compound)
 OPTIONAL MATCH (m)-[:OF_CLASS]->(cc:ChemicalClass)
 RETURN DISTINCT m.name AS 成分, c.pct_min AS 含量, cc.name AS 化学分类
 ORDER BY 成分", list(name = cn))
  if (!nrow(df)) return(list())
  df <- df[!duplicated(df$成分), , drop = FALSE]
  total <- if (nrow(n)) as.integer(n$n[1]) else nrow(df)
  rag_list_ev(sprintf("%s 的化学成分（共 %d 种）", cn, total),
              df$成分, rag_flat_df(df), "graph:CONTAINS")
}

# 5.7 药理作用 → 实验模型（3 跳）
step_herb_pharm <- function(name) {
  cn <- rag_canon(name)
  df <- rag_q(
    "MATCH (h:Herb {name:$name})-[:HAS_PHARM_EFFECT]->(p:PharmEffect)
 RETURN p.name AS effect, left(p.raw_head, 40) AS detail,
        [(p)-[:TESTED_IN]->(m:ExperimentalModel) | m.name][..3] AS models
 LIMIT 12", list(name = cn))
  if (!nrow(df)) return(list())
  lines <- rag_fmt_rows(df, c("effect", "models"), function(v) {
    sprintf("%s%s", v$effect,
            if (nzchar(rag_cell(v$models))) paste0("（实验模型：", rag_cell(v$models), "）") else "")
  })
  rag_graph_ev(sprintf("%s 的药理作用（药理 → 实验模型）", cn), lines,
               "graph:HAS_PHARM_EFFECT")
}

# 5.8 配伍药对 / 鉴别
step_herb_pairs <- function(name) {
  cn <- rag_canon(name)
  out <- list()
  df <- rag_q(
    "MATCH (h:Herb {name:$name})-[r:PAIRS_WITH]->(o:Herb)
 RETURN o.name AS partner, r.effect AS effect, left(r.indication, 30) AS indication
 LIMIT 15", list(name = cn))
  if (nrow(df)) {
    lines <- rag_fmt_rows(df, c("partner", "effect", "indication"), function(v) {
      sprintf("配%s：%s%s", v$partner, v$effect,
              if (nzchar(v$indication)) paste0("；主治 ", v$indication) else "")
    })
    out[[length(out) + 1L]] <- rag_graph_ev(paste0(cn, " 的配伍药对"), lines,
                                            "graph:PAIRS_WITH", 0.9)
  }
  df2 <- rag_q(
    "MATCH (h:Herb {name:$name})-[r:DIFFERENTIATED_WITH]->(o:Herb)
 RETURN o.name AS other, left(r.text, 120) AS note LIMIT 6", list(name = cn))
  if (nrow(df2)) {
    lines <- rag_fmt_rows(df2, c("other", "note"), function(v) {
      sprintf("与 %s 鉴别：%s", v$other, v$note)
    })
    out[[length(out) + 1L]] <- rag_graph_ev(paste0(cn, " 的鉴别对比"), lines,
                                            "graph:DIFFERENTIATED_WITH", 0.85)
  }
  Filter(Negate(is.null), out)
}

# ---------------------------------------------------------------------------
# 6. 反查步骤（病证 / 功效 / 成分 → 药）
# ---------------------------------------------------------------------------
# 数量类问题不截断：给出全部名称；明细（分类/功效/性味/用量）走 CSV 下载
step_reverse_indication <- function(name) {
  n <- rag_q("MATCH (h:Herb)-[:ALSO_TREATS]->(i:Indication {name:$name})
 RETURN count(DISTINCT h) AS n", list(name = name))
  df <- rag_q(
    "MATCH (h:Herb)-[:ALSO_TREATS]->(i:Indication {name:$name})
 RETURN DISTINCT h.name AS 药名, h.category_raw AS 分类,
        [(h)-[:HAS_EFFICACY]->(e:Efficacy) | e.name][..5] AS 功效,
        h.taste_raw AS 性味, h.meridian_raw AS 归经,
        left(h.dosage_raw, 80) AS 用法用量
 ORDER BY 药名", list(name = name))
  if (!nrow(df)) return(list())
  df <- df[!duplicated(df$药名), , drop = FALSE]
  total <- if (nrow(n)) as.integer(n$n[1]) else nrow(df)
  rag_list_ev(sprintf("主治「%s」的中药（共 %d 味）", name, total),
              df$药名, rag_flat_df(df), "graph:ALSO_TREATS")
}

# 数量类问题不截断：给出全部名称；明细走 CSV 下载
step_reverse_efficacy <- function(name) {
  n <- rag_q("MATCH (h:Herb)-[:HAS_EFFICACY]->(e:Efficacy)
 WHERE e.name CONTAINS $name RETURN count(DISTINCT h) AS n", list(name = name))
  df <- rag_q(
    "MATCH (h:Herb)-[:HAS_EFFICACY]->(e:Efficacy)
 WHERE e.name CONTAINS $name
 RETURN DISTINCT h.name AS 药名, h.category_raw AS 分类,
        [(h)-[:HAS_EFFICACY]->(x:Efficacy) | x.name][..5] AS 功效,
        h.taste_raw AS 性味, h.meridian_raw AS 归经,
        left(h.dosage_raw, 80) AS 用法用量
 ORDER BY 药名", list(name = name))
  if (!nrow(df)) return(list())
  df <- df[!duplicated(df$药名), , drop = FALSE]
  total <- if (nrow(n)) as.integer(n$n[1]) else nrow(df)
  rag_list_ev(sprintf("具有「%s」功效的中药（共 %d 味）", name, total),
              df$药名, rag_flat_df(df), "graph:HAS_EFFICACY")
}

# 数量类问题不截断：给出全部名称；明细（含量/分类/功效）走 CSV 下载
step_reverse_compound <- function(name) {
  n <- rag_q("MATCH (h:Herb)-[:CONTAINS]->(m:Compound) WHERE m.name = $name
 RETURN count(DISTINCT h) AS n", list(name = name))
  df <- rag_q(
    "MATCH (h:Herb)-[c:CONTAINS]->(m:Compound) WHERE m.name = $name
 RETURN DISTINCT h.name AS 药名, h.category_raw AS 分类, c.pct_min AS 含量,
        [(h)-[:HAS_EFFICACY]->(e:Efficacy) | e.name][..5] AS 功效,
        h.taste_raw AS 性味, h.meridian_raw AS 归经,
        left(h.dosage_raw, 80) AS 用法用量
 ORDER BY 药名", list(name = name))
  if (!nrow(df)) return(list())
  df <- df[!duplicated(df$药名), , drop = FALSE]
  total <- if (nrow(n)) as.integer(n$n[1]) else nrow(df)
  rag_list_ev(sprintf("含「%s」的中药（共 %d 味）", name, total),
              df$药名, rag_flat_df(df), "graph:CONTAINS")
}

# ---------------------------------------------------------------------------
# 7. 综合约束求解（任意条件组合）
# ---------------------------------------------------------------------------
# conds: list(efficacy=chr[], indication=chr[], compound=chr[],
#             category=chr, meridian=chr, nature=chr, taste=chr,
#             toxicity=chr, patent=TRUE)
rag_build_constraint <- function(conds) {
  matches <- c("MATCH (h:Herb {in_corpus: true})")
  wheres <- character(0)
  params <- list()
  k <- 0L
  add_var <- function(prefix) { k <<- k + 1L; paste0(prefix, k) }

  for (e in conds$efficacy %||% character(0)) {
    v <- add_var("e")
    matches <- c(matches, sprintf("MATCH (h)-[:HAS_EFFICACY]->(%s:Efficacy)", v))
    wheres <- c(wheres, sprintf("%s.name CONTAINS $%s", v, v))
    params[[v]] <- e
  }
  for (i in conds$indication %||% character(0)) {
    v <- add_var("i")
    matches <- c(matches, sprintf("MATCH (h)-[:ALSO_TREATS]->(%s:Indication)", v))
    wheres <- c(wheres, sprintf("%s.name CONTAINS $%s", v, v))
    params[[v]] <- i
  }
  for (c in conds$compound %||% character(0)) {
    v <- add_var("m")
    matches <- c(matches, sprintf("MATCH (h)-[:CONTAINS]->(%s:Compound)", v))
    wheres <- c(wheres, sprintf("%s.name = $%s", v, v))
    params[[v]] <- c
  }
  if (!is.null(conds$category)) {
    for (cc in conds$category) {
      v <- add_var("c")
      matches <- c(matches, sprintf("MATCH (h)-[:HAS_CATEGORY]->(%s:DrugCategory)", v))
      wheres <- c(wheres, sprintf("%s.name = $%s", v, v))
      params[[v]] <- cc
    }
  }
  # 多维取值：默认 AND（“归肝、肾二经”=两经都归）；问句里有“或”时按并集
  add_multi <- function(rel, label, values, prefix) {
    values <- values %||% character(0)
    if (!length(values)) return(invisible(NULL))
    if (length(values) == 1L) {
      v <- add_var(prefix)
      matches <<- c(matches, sprintf("MATCH (h)-[:%s]->(%s:%s)", rel, v, label))
      wheres <<- c(wheres, sprintf("%s.name = $%s", v, v))
      params[[v]] <<- values
    } else if (isTRUE(conds$or)) {
      v <- add_var(prefix)
      matches <<- c(matches, sprintf("MATCH (h)-[:%s]->(%s:%s)", rel, v, label))
      wheres <<- c(wheres, sprintf("%s.name IN $%s", v, v))
      params[[v]] <<- as.list(values)
    } else {
      for (x in values) {
        v <- add_var(prefix)
        matches <<- c(matches, sprintf("MATCH (h)-[:%s]->(%s:%s)", rel, v, label))
        wheres <<- c(wheres, sprintf("%s.name = $%s", v, v))
        params[[v]] <<- x
      }
    }
    invisible(NULL)
  }
  add_multi("ENTERS_MERIDIAN", "Meridian", conds$meridian, "me")
  add_multi("HAS_NATURE", "Nature", conds$nature, "na")
  add_multi("HAS_TASTE", "Taste", conds$taste, "ta")

  if (!is.null(conds$toxicity)) {
    tox <- conds$toxicity
    if (identical(tox, "无毒")) {
      wheres <- c(wheres, "NOT (h)-[:HAS_TOXICITY]->(:Toxicity {name:'有毒'})",
                  "NOT (h)-[:HAS_TOXICITY]->(:Toxicity {name:'大毒'})",
                  "NOT (h)-[:HAS_TOXICITY]->(:Toxicity {name:'小毒'})",
                  "NOT (h)-[:HAS_TOXICITY]->(:Toxicity {name:'剧毒'})",
                  "NOT (h)-[:HAS_TOXICITY]->(:Toxicity {name:'微毒'})")
    } else if (identical(tox, "有毒")) {
      # “有毒的药”= 任意毒性等级（大毒/有毒/小毒/剧毒/微毒）
      v <- add_var("tx")
      matches <- c(matches, sprintf("MATCH (h)-[:HAS_TOXICITY]->(%s:Toxicity)", v))
      wheres <- c(wheres, sprintf("%s.name IN $%s", v, v))
      params[[v]] <- as.list(c("有毒", "大毒", "小毒", "剧毒", "微毒"))
    } else {
      v <- add_var("tx")
      matches <- c(matches, sprintf("MATCH (h)-[:HAS_TOXICITY]->(%s:Toxicity)", v))
      wheres <- c(wheres, sprintf("%s.name = $%s", v, v))
      params[[v]] <- tox
    }
  }
  # 禁忌类（kind 属于某类）
  if (!is.null(conds$contra_kind)) {
    v <- add_var("ck")
    matches <- c(matches, sprintf("MATCH (h)-[:CONTRAINDICATED_IN]->(%s:Contraindication)", v))
    wheres <- c(wheres, sprintf("%s.kind IN $%s", v, v))
    params[[v]] <- as.list(conds$contra_kind)
  }
  # 有没有任何禁忌
  if (isTRUE(conds$has_contra)) {
    matches <- c(matches, "MATCH (h)-[:CONTRAINDICATED_IN]->(:Contraindication)")
  }
  # 七情：任意 / 指定关系
  if (!is.null(conds$seven)) {
    if (identical(conds$seven, "any")) {
      matches <- c(matches, "MATCH (h)-[:SEVEN_RELATION]->(:Herb)")
    } else if (length(conds$seven) > 1L && isTRUE(conds$or)) {
      v <- add_var("sr")
      matches <- c(matches, sprintf("MATCH (h)-[%s:SEVEN_RELATION]->(:Herb)", v))
      wheres <- c(wheres, sprintf("%s.relation IN $%s", v, v))
      params[[v]] <- as.list(conds$seven)
    } else {
      for (x in conds$seven) {
        v <- add_var("sr")
        matches <- c(matches, sprintf("MATCH (h)-[%s:SEVEN_RELATION]->(:Herb)", v))
        wheres <- c(wheres, sprintf("%s.relation = $%s", v, v))
        params[[v]] <- x
      }
    }
  }
  # 有没有配伍药对
  if (isTRUE(conds$pairs)) {
    matches <- c(matches, "MATCH (h)-[:PAIRS_WITH]->(:Herb)")
  }
  # 剂量上限（dose_max ≤ X，单位为克）
  if (!is.null(conds$dose_max)) {
    v <- add_var("dr")
    matches <- c(matches, sprintf("MATCH (h)-[%s:USED_BY_ROUTE]->(:UsageRoute)", v))
    wheres <- c(wheres, sprintf("%s.dose_max <= $%s", v, v))
    params[[v]] <- as.numeric(conds$dose_max)
  }

  if (isTRUE(conds$patent)) {
    wheres <- c(wheres, "(h)<-[:HAS_INGREDIENT]-(:Formula {is_patent: true})")
  }
  list(matches = matches, wheres = wheres, params = params)
}

rag_cond_labels <- function(conds) {
  joiner <- if (isTRUE(conds$or)) " 或 " else " + "
  out <- character(0)
  if (length(conds$efficacy)) out <- c(out, paste0("功效 ", paste(conds$efficacy, collapse = joiner)))
  if (length(conds$indication)) out <- c(out, paste0("主治 ", paste(conds$indication, collapse = joiner)))
  if (length(conds$compound)) out <- c(out, paste0("成分 ", paste(conds$compound, collapse = joiner)))
  if (length(conds$category)) out <- c(out, paste0("分类 ", paste(conds$category, collapse = joiner)))
  if (length(conds$meridian)) out <- c(out, paste(paste0(conds$meridian, "经"), collapse = joiner))
  if (length(conds$nature)) out <- c(out, paste(paste0(conds$nature, "性"), collapse = joiner))
  if (length(conds$taste)) out <- c(out, paste(paste0(conds$taste, "味"), collapse = joiner))
  if (!is.null(conds$toxicity)) out <- c(out, conds$toxicity)
  if (!is.null(conds$contra_kind)) out <- c(out, paste0(conds$contra_kind, collapse = " / "))
  if (isTRUE(conds$has_contra)) out <- c(out, "有禁忌")
  if (!is.null(conds$seven)) {
    out <- c(out, if (identical(conds$seven, "any")) "七情" else paste(conds$seven, collapse = " / "))
  }
  if (isTRUE(conds$pairs)) out <- c(out, "有配伍药对")
  if (!is.null(conds$dose_max)) out <- c(out, paste0("用量≤", conds$dose_max, "克"))
  if (isTRUE(conds$patent)) out <- c(out, "已上市中成药")
  out
}

# 把复合条件拆成"单个条件"的列表，便于逐个求候选药材集合
rag_split_conds <- function(conds) {
  out <- list()
  push <- function(x) out[[length(out) + 1L]] <<- x
  for (e in conds$efficacy %||% character(0)) push(list(efficacy = e))
  for (i in conds$indication %||% character(0)) push(list(indication = i))
  for (m in conds$compound %||% character(0)) push(list(compound = m))
  dims <- list(category = conds$category, meridian = conds$meridian,
               nature = conds$nature, taste = conds$taste)
  for (nm in names(dims)) {
    vals <- dims[[nm]]
    if (is.null(vals) || !length(vals)) next
    if (length(vals) > 1L && isTRUE(conds$or)) {
      push(stats::setNames(list(vals, TRUE), c(nm, "or")))
    } else {
      for (x in vals) push(stats::setNames(list(x), nm))
    }
  }
  if (!is.null(conds$toxicity)) push(list(toxicity = conds$toxicity))
  if (!is.null(conds$dose_max)) push(list(dose_max = conds$dose_max))
  if (!is.null(conds$contra_kind)) push(list(contra_kind = conds$contra_kind))
  if (isTRUE(conds$has_contra)) push(list(has_contra = TRUE))
  if (!is.null(conds$seven)) push(list(seven = conds$seven))
  if (isTRUE(conds$pairs)) push(list(pairs = TRUE))
  if (isTRUE(conds$patent)) push(list(patent = TRUE))
  out
}

# 各条件候选药材 id 的交集（每个条件单独查，再取 intersect，避开慢的多模式 JOIN）
rag_constraint_ids <- function(conds) {
  atoms <- rag_split_conds(conds)
  if (!length(atoms)) return(character(0))
  ids <- NULL
  for (a in atoms) {
    b <- rag_build_constraint(a)
    sql <- paste(c(b$matches,
                   if (length(b$wheres)) paste0("WHERE ", paste(b$wheres, collapse = " AND ")) else NULL,
                   "RETURN DISTINCT h.id AS id"), collapse = "\n")
    s <- tryCatch(as.character(rag_q(sql, b$params)$id), error = function(e) character(0))
    ids <- if (is.null(ids)) s else intersect(ids, s)
    if (!length(ids)) return(character(0))
  }
  ids
}

step_constraint <- function(conds, limit = 25) {
  b <- rag_build_constraint(conds)
  if (!length(b$matches)) return(list())
  ids <- tryCatch(rag_constraint_ids(conds), error = function(e) character(0))
  if (!length(ids)) {
    cond_txt0 <- paste(rag_cond_labels(conds), collapse = " + ")
    return(list(rag_graph_ev(sprintf("满足「%s」的中药（0 味）", cond_txt0),
                             "图谱中没有同时满足这些条件的中药。可以放宽其中一个条件再试。",
                             "graph:constraint", 1.2)))
  }
  total <- length(ids)
  list_sql <- paste(c(
    "MATCH (h:Herb) WHERE h.id IN $ids",
    "RETURN DISTINCT h.name AS herb, h.category_raw AS category,",
    "       [(h)-[:HAS_EFFICACY]->(x:Efficacy) | x.name][..5] AS effs,",
    "       [(h)-[:ALSO_TREATS]->(x:Indication) | x.name][..4] AS inds,",
    "       left(h.dosage_raw, 60) AS dosage",
    "ORDER BY herb LIMIT $limit"), collapse = "\n")
  df <- rag_q(list_sql, list(ids = as.list(ids), limit = as.integer(limit)))
  cond_txt <- paste(rag_cond_labels(conds), collapse = " + ")
  if (!nrow(df)) {
    return(list(rag_graph_ev(sprintf("满足「%s」的中药（0 味）", cond_txt),
                             "图谱中没有同时满足这些条件的中药。可以放宽其中一个条件再试。",
                             "graph:constraint", 1.2)))
  }
  lines <- rag_fmt_rows(df, c("herb", "category", "effs", "inds", "dosage"), function(v) {
    sprintf("%s［%s］功效：%s%s%s", v$herb, v$category, rag_cell(v$effs),
            if (nzchar(rag_cell(v$inds))) paste0("；主治：", rag_cell(v$inds)) else "",
            if (nzchar(rag_cell(v$dosage))) paste0("；用法：", rag_cell(v$dosage)) else "")
  })
  all_rows <- tryCatch(rag_q(
    "MATCH (h:Herb) WHERE h.id IN $ids
     RETURN DISTINCT h.name AS 药名, h.category_raw AS 分类,
            [(h)-[:HAS_EFFICACY]->(x:Efficacy) | x.name][..5] AS 功效,
            [(h)-[:ALSO_TREATS]->(x:Indication) | x.name][..5] AS 主治,
            left(h.dosage_raw, 80) AS 用法用量,
            h.taste_raw AS 性味, h.meridian_raw AS 归经
     ORDER BY 药名", list(ids = as.list(ids))),
    error = function(e) NULL)
  if (total > nrow(df) && !is.null(all_rows) && nrow(all_rows)) {
    lines <- c(lines, sprintf("完整名单（共 %d 味）：%s", total,
                              paste(as.character(all_rows$药名), collapse = "、")))
  }
  ev <- rag_graph_ev(sprintf("满足「%s」的中药（共 %d 味，前 %d 味给明细）",
                             cond_txt, total, nrow(df)),
                     lines, "graph:constraint", 1.2)
  if (!is.null(ev)) {
    ev$citation$herbs <- as.list(utils::head(df$herb, 8))
    ev$citation$sql <- paste(c(b$matches, paste0("WHERE ", paste(b$wheres, collapse = " AND "))),
                             collapse = "\n")
    if (total > nrow(df)) ev$citation$list <- TRUE
    if (!is.null(all_rows) && nrow(all_rows)) ev$citation$rows <- all_rows
  }
  ev
}

# ---------------------------------------------------------------------------
# 7b. 词表反查（四气 / 五味 / 归经 / 毒性 / 分类 / 中成药，可多条件叠加）
# ---------------------------------------------------------------------------
step_vocab <- function(conds, limit = 40) {
  b <- rag_build_constraint(conds)
  if (!length(b$matches)) return(list())
  cond_txt <- paste(rag_cond_labels(conds), collapse = " + ")

  ids <- tryCatch(rag_constraint_ids(conds), error = function(e) character(0))
  if (!length(ids)) {
    return(list(rag_graph_ev(sprintf("「%s」的中药（0 味）", cond_txt),
                             "图谱中没有同时满足这些条件的中药。可以去掉一个条件再试。",
                             "graph:vocab", 1.2)))
  }
  total <- length(ids)

  # 禁忌/七情相关的反查，额外把命中的禁忌文本与七情对象带出来
  want_contra <- !is.null(conds$contra_kind) || isTRUE(conds$has_contra)
  want_seven <- !is.null(conds$seven)
  sel <- c("h.name AS herb", "h.category_raw AS category",
           "h.taste_raw AS taste", "h.meridian_raw AS meridians")
  cols <- c("herb", "category", "taste", "meridians")
  if (want_contra) {
    sel <- c(sel, "[(h)-[:CONTRAINDICATED_IN]->(c:Contraindication) | left(c.text, 32)][..2] AS contra")
    cols <- c(cols, "contra")
  }
  if (want_seven) {
    sel <- c(sel, "[(h)-[sr:SEVEN_RELATION]->(o:Herb) | o.name + '（' + sr.relation + '）'][..2] AS seven")
    cols <- c(cols, "seven")
  }
  list_sql <- paste(c(
    "MATCH (h:Herb) WHERE h.id IN $ids",
    paste0("RETURN DISTINCT ", paste(sel, collapse = ", ")),
    "ORDER BY herb LIMIT $limit"), collapse = "\n")
  df <- rag_q(list_sql, list(ids = as.list(ids), limit = as.integer(limit)))
  if (!nrow(df)) {
    return(list(rag_graph_ev(sprintf("「%s」的中药（0 味）", cond_txt),
                             "图谱中没有同时满足这些条件的中药。可以去掉一个条件再试。",
                             "graph:vocab", 1.2)))
  }
  shown <- nrow(df)
  # 完整名单（不受 LIMIT 限制），保证"符合条件的全部给出"
  all_rows <- tryCatch(rag_q(
    "MATCH (h:Herb) WHERE h.id IN $ids
     RETURN DISTINCT h.name AS 药名, h.category_raw AS 分类, h.taste_raw AS 性味,
            h.meridian_raw AS 归经, h.toxicity_raw AS 毒性, h.dosage_raw AS 用法用量
     ORDER BY 药名", list(ids = as.list(ids))),
    error = function(e) NULL)
  all_names <- if (!is.null(all_rows) && nrow(all_rows)) as.character(all_rows$药名) else as.character(df$herb)
  head_txt <- if (total > shown) {
    sprintf("「%s」的中药：共 %d 味（前 %d 味给明细，末尾附完整名单）", cond_txt, total, shown)
  } else {
    sprintf("「%s」的中药：共 %d 味（全部列出）", cond_txt, total)
  }
  clean <- function(x) gsub("[。；;，,\\s]+$", "", x)
  lines <- rag_fmt_rows(df, cols, function(v) {
    base <- sprintf("%s［%s］性味：%s；归经：%s", v$herb,
                    if (nzchar(v$category)) v$category else "未分类",
                    if (nzchar(v$taste)) clean(v$taste) else "未载",
                    if (nzchar(v$meridians)) clean(v$meridians) else "未载")
    extra <- character(0)
    if (!is.null(v$contra) && nzchar(rag_cell(v$contra))) {
      extra <- c(extra, paste0("禁忌：", gsub("\\n", " ", rag_cell(v$contra))))
    }
    if (!is.null(v$seven) && nzchar(rag_cell(v$seven))) {
      extra <- c(extra, paste0("七情：", rag_cell(v$seven)))
    }
    if (length(extra)) paste0(base, "；", paste(extra, collapse = "；")) else base
  }, limit = shown)
  ev1 <- rag_graph_ev(head_txt, lines, "graph:vocab", 1.35)
  if (!is.null(ev1)) {
    ev1$citation$herbs <- as.list(utils::head(df$herb, 8))
    bb <- rag_build_constraint(conds)
    ev1$citation$sql <- paste(c(bb$matches,
                                paste0("WHERE ", paste(bb$wheres, collapse = " AND "))),
                              collapse = "\n")
  }
  if (total > shown && length(all_names)) {
    lines <- c(lines, sprintf("完整名单（共 %d 味）：%s", total, paste(all_names, collapse = "、")))
    ev1$text <- paste(lines, collapse = "\n")
    ev1$citation$list <- TRUE
  }
  if (!is.null(all_rows) && nrow(all_rows)) ev1$citation$rows <- all_rows
  out <- list(ev1)

  # 分类分布：单条件时最能说明“这类药主要落在哪些分类”
  dist_sql <- paste(c("MATCH (h:Herb) WHERE h.id IN $ids",
                      "RETURN h.category_raw AS category, count(DISTINCT h) AS n",
                      "ORDER BY n DESC"), collapse = "\n")
  dd <- rag_q(dist_sql, list(ids = as.list(ids)))
  if (nrow(dd) > 1L) {
    out[[length(out) + 1L]] <- rag_graph_ev(
      sprintf("「%s」的药材分类分布", cond_txt),
      sprintf("%s %d 味", dd$category, dd$n), "graph:vocab", 1.05)
  }
  Filter(Negate(is.null), out)
}

# ---------------------------------------------------------------------------
# 7c. 分类归属核验（“X 是不是 Y 类药”）
# ---------------------------------------------------------------------------
# 输出三段：① 本书把 X 归为哪一类；② 目标分类的名单与“在/不在”的核验结论；
# ③ 若 X 的功效里恰好有与 Y 同名的功效词（百合的清心安神 vs 安神药），
#    单独说明“这是功效，不是分类”，避免模型用功效反推分类。
step_category_check <- function(name, category = NULL, limit = 40) {
  cn <- rag_canon(name)
  note <- if (!identical(cn, name)) paste0("（", rag_alias_note(name), "）") else ""
  cats <- rag_q(
    "MATCH (h:Herb {name:$name})-[:HAS_CATEGORY]->(c:DrugCategory)
     RETURN c.name AS c ORDER BY c.name", list(name = cn))
  my_cats <- unique(as.character(cats$c))
  my_cats <- my_cats[!is.na(my_cats) & nzchar(my_cats)]
  if (!length(my_cats)) {
    return(list(rag_graph_ev(
      sprintf("分类归属核验：%s", name),
      sprintf(paste0("图谱中「%s」没有药物分类记录：它多半是方剂里的异名或简称，",
                     "不能据此给出分类结论。"), name),
      "graph:category_check", 1.6)))
  }

  eff <- rag_q(
    "MATCH (h:Herb {name:$name})-[:HAS_EFFICACY]->(e:Efficacy)
     RETURN e.name AS e ORDER BY e", list(name = cn))
  eff_names <- unique(as.character(eff$e))
  eff_names <- eff_names[!is.na(eff_names) & nzchar(eff_names)]
  lines <- sprintf("【分类归属】《中华药海》将%s归为：%s%s", cn,
                   paste(my_cats, collapse = "、"), note)

  # 只问“属于哪一类”，没有指定目标分类
  target <- if (is.null(category) || !length(category)) "" else as.character(category[1])
  if (!nzchar(target)) {
    lines <- c(lines, sprintf("【说明】分类归属以原书“药物分类”为准；功效记载（%s）不改变分类。",
                              if (length(eff_names)) rag_join(utils::head(eff_names, 8), "、") else "无"))
    return(list(rag_graph_ev(sprintf("分类归属核验：%s", name), lines,
                             "graph:category_check", 1.6)))
  }

  roster <- rag_q(
    "MATCH (h:Herb)-[:HAS_CATEGORY]->(c:DrugCategory {name:$c})
     RETURN h.name AS herb ORDER BY h.name", list(c = target))
  roster_names <- unique(as.character(roster$herb))
  roster_names <- roster_names[!is.na(roster_names) & nzchar(roster_names)]
  if (!length(roster_names)) {
    all_cats <- rag_q("MATCH (c:DrugCategory) RETURN c.name AS c ORDER BY c.name")
    lines <- c(lines, sprintf("【目标分类】图谱中没有「%s」这一分类。可选分类：%s。",
                              target, rag_join(as.character(all_cats$c), "、", 20)))
    return(list(rag_graph_ev(sprintf("分类归属核验：%s", name), lines,
                             "graph:category_check", 1.6)))
  }

  in_roster <- cn %in% roster_names
  shown <- utils::head(roster_names, limit)
  more <- if (length(roster_names) > length(shown)) "……" else ""
  lines <- c(lines,
    sprintf("【目标分类】%s（全书共 %d 味）：%s%s", target, length(roster_names),
            rag_join(shown, "、"), more),
    sprintf("【核验结论】%s %s「%s」名单内。", name,
            if (in_roster) "在" else "不在", target))

  # 分类名去掉“药”字后作为功效关键词：百合—安神药→“安神”→ 清心安神
  kw <- sub("药$", "", target)
  if (!in_roster && nzchar(kw) && !identical(kw, target) && length(eff_names)) {
    hit <- eff_names[grepl(kw, eff_names, fixed = TRUE)]
    if (length(hit)) {
      lines <- c(lines, sprintf(paste0(
        "【需要区分】「%s」是分类名称，%s的功效项里确有「%s」——这是功效记载，",
        "不等于分类归属；回答“是不是%s”只能依据分类，不能依据功效。"),
        target, cn, paste(hit, collapse = "、"), target))
    }
  }

  ev <- rag_graph_ev(sprintf("分类归属核验：%s", name), lines,
                     "graph:category_check", 1.6)
  if (!is.null(ev)) ev$citation$category <- as.list(my_cats)
  ev
}

# ---------------------------------------------------------------------------
# 8. 两药对比
# ---------------------------------------------------------------------------
step_compare <- function(a, b) {
  ca <- rag_canon(a); cb <- rag_canon(b)
  df <- rag_q(
    "MATCH (a:Herb {name:$a}), (b:Herb {name:$b})
 RETURN [(a)-[:HAS_EFFICACY]->(e:Efficacy) | e.name] AS a_eff,
        [(b)-[:HAS_EFFICACY]->(e:Efficacy) | e.name] AS b_eff,
        [(a)-[:ALSO_TREATS]->(i:Indication) | i.name] AS a_ind,
        [(b)-[:ALSO_TREATS]->(i:Indication) | i.name] AS b_ind,
        [(a)-[:CONTAINS]->(m:Compound) | m.name] AS a_cmp,
        [(b)-[:CONTAINS]->(m:Compound) | m.name] AS b_cmp",
    list(a = ca, b = cb))
  if (!nrow(df)) return(list())
  r <- df[1, ]
  split_set <- function(x) {
    v <- rag_cell(x, "\u0001")
    if (!nzchar(v)) character(0) else strsplit(v, "\u0001", fixed = TRUE)[[1]]
  }
  ae <- split_set(r$a_eff); be <- split_set(r$b_eff)
  ai <- split_set(r$a_ind); bi <- split_set(r$b_ind)
  ac <- split_set(r$a_cmp); bc <- split_set(r$b_cmp)
  lines <- c(
    sprintf("共同功效：%s", rag_join(intersect(ae, be), "、", 10) %||% "无"),
    sprintf("%s 独有功效：%s", ca, rag_join(setdiff(ae, be), "、", 8) %||% "无"),
    sprintf("%s 独有功效：%s", cb, rag_join(setdiff(be, ae), "、", 8) %||% "无"),
    sprintf("共同主治：%s", rag_join(intersect(ai, bi), "、", 10) %||% "无"),
    sprintf("共同成分：%s", rag_join(intersect(ac, bc), "、", 12) %||% "无")
  )
  notes <- c(if (!identical(ca, a)) rag_alias_note(a), if (!identical(cb, b)) rag_alias_note(b))
  rag_graph_ev(sprintf("对比：%s vs %s%s", a, b,
                       if (length(notes)) paste0("　", paste(notes, collapse = "")) else ""),
               lines, "graph:compare", 1.1)
}

# ---------------------------------------------------------------------------
# 9. 全库统计（综合查询）
# ---------------------------------------------------------------------------
RAG_STAT_TOPICS <- list(
  efficacy   = c("功效"),
  indication = c("主治", "病证", "疾病", "证"),
  compound   = c("成分", "化合物"),
  formula    = c("方剂", "方"),
  category   = c("分类", "类型", "类别"),
  meridian   = c("归经", "经络"),
  nature     = c("四气", "寒热", "性"),
  taste      = c("五味", "味"),
  book       = c("著作", "书目", "出处", "典籍")
)

rag_stats_topic <- function(q) {
  # “多少味药 / 多少首方剂”这类问规模的问题，先给全库概览
  if (grepl("多少|一共|共有|总量|规模", q) &&
      !grepl("最多|最高|排行|top|前几|频次|占比|分布", q, ignore.case = TRUE)) {
    return("overview")
  }
  for (nm in names(RAG_STAT_TOPICS)) {
    if (rag_any_kw(q, RAG_STAT_TOPICS[[nm]])) return(nm)
  }
  "overview"
}

RAG_STAT_SQL <- list(
  efficacy = "MATCH (e:Efficacy)<-[:HAS_EFFICACY]-(h:Herb)
RETURN e.name AS 功效, count(DISTINCT h) AS 药数 ORDER BY 药数 DESC LIMIT 20",
  indication = "MATCH (i:Indication)<-[:ALSO_TREATS]-(h:Herb)
RETURN i.name AS 主治, count(DISTINCT h) AS 药数 ORDER BY 药数 DESC LIMIT 20",
  compound = "MATCH (c:Compound)<-[:CONTAINS]-(h:Herb)
RETURN c.name AS 成分, count(DISTINCT h) AS 药数 ORDER BY 药数 DESC LIMIT 20",
  formula = "MATCH (f:Formula)-[:HAS_INGREDIENT]->(h:Herb)
RETURN f.name AS 方剂, count(DISTINCT h) AS 药味数 ORDER BY 药味数 DESC LIMIT 15",
  category = "MATCH (h:Herb)-[:HAS_CATEGORY]->(c:DrugCategory)
RETURN c.name AS 分类, count(*) AS 药数 ORDER BY 药数 DESC",
  meridian = "MATCH (h:Herb)-[:ENTERS_MERIDIAN]->(m:Meridian)
RETURN m.name AS 归经, count(DISTINCT h) AS 药数 ORDER BY 药数 DESC",
  nature = "MATCH (h:Herb)-[:HAS_NATURE]->(n:Nature)
RETURN n.name AS 四气, count(*) AS 药数 ORDER BY 药数 DESC",
  taste = "MATCH (h:Herb)-[:HAS_TASTE]->(t:Taste)
RETURN t.name AS 五味, count(*) AS 药数 ORDER BY 药数 DESC",
  book = "MATCH (b:Book)<-[:FROM_BOOK]-(f:Formula)
RETURN b.name AS 著作, count(DISTINCT f) AS 方剂数 ORDER BY 方剂数 DESC LIMIT 15"
)

step_stats <- function(question, topic = NULL) {
  topic <- topic %||% rag_stats_topic(question)
  out <- list()

  # 全库规模：任何统计问题都先给"这本书有多大"（计数缓存，避免每次扫 5 万节点）
  lc <- kg_label_counts()
  g <- kg_store_get()
  n_corpus <- sum(g$nodes$label == "Herb" & (g$nodes$in_corpus %in% c(TRUE, "true")))
  if (n_corpus > 0) lc[["Herb"]] <- n_corpus
  key <- c(Herb = "中药", Formula = "方剂", Compound = "化合物", Indication = "主治病证",
           Efficacy = "功效", Book = "著作", TextUnit = "原文段落", HerbAlias = "别名",
           Quotation = "引文", Section = "章节", SourceDoc = "源文档")
  present <- names(key)[names(key) %in% names(lc)]
  headline <- paste(sprintf("%s %s", key[present],
                            kg_num(as.integer(lc[present]))),
                    collapse = "；")
  detail <- paste(sprintf("%s %s", names(lc), kg_num(as.integer(lc))),
                  collapse = "、")
  out[[length(out) + 1L]] <- rag_graph_ev(
    "全库规模（《中华药海》整本书）",
    sprintf("%s。\n全部节点类型：%s\n合计 %s 个节点、%s 条关系。",
            headline, detail, kg_num(sum(lc)),
            kg_num(kg_summary()$relationships)),
    "graph:stats", 1.2)

  if (identical(topic, "overview")) {
    for (x in list(
      c("关系类型分布", "__CACHED_REL__"),
      c("药物分类分布", RAG_STAT_SQL$category)
    )) {
      d <- if (identical(x[2], "__CACHED_REL__")) {
        rc <- kg_rel_counts()
        data.frame(k = names(rc)[seq_len(min(12, length(rc)))],
                   n = as.integer(rc)[seq_len(min(12, length(rc)))],
                   stringsAsFactors = FALSE)
      } else rag_q(x[2])
      if (!nrow(d)) next
      lines <- sprintf("%s：%s", d[[1]], kg_num(d[[2]]))
      out[[length(out) + 1L]] <- rag_graph_ev(x[1], lines, "graph:stats", 1.1)
    }
    return(Filter(Negate(is.null), out))
  }

  sql <- RAG_STAT_SQL[[topic]]
  if (is.null(sql)) return(Filter(Negate(is.null), out))
  df <- rag_q(sql)
  if (nrow(df)) {
    unit <- if (grepl("药数", names(df)[2])) " 味药" else if (grepl("方剂数", names(df)[2])) " 首方剂" else ""
    lines <- sprintf("%s：%s%s", df[[1]], kg_num(df[[2]]), unit)
    out[[length(out) + 1L]] <- rag_graph_ev(
      sprintf("全库统计：%s Top %d", names(df)[1], nrow(df)), lines, "graph:stats", 1.1)
  }
  Filter(Negate(is.null), out)
}

# ---------------------------------------------------------------------------
# 10. 多步链（功效/病证 → 药 → 成分；成分 → 药 → 功效）
# ---------------------------------------------------------------------------
step_chain_efficacy_compound <- function(eff, limit = 12) {
  df <- rag_q(
    "MATCH (h:Herb)-[:HAS_EFFICACY]->(e:Efficacy)
 WHERE e.name CONTAINS $eff
 MATCH (h)-[:CONTAINS]->(m:Compound)
 RETURN m.name AS compound, count(DISTINCT h) AS n, collect(DISTINCT h.name)[..3] AS herbs
 ORDER BY n DESC LIMIT $limit", list(eff = eff, limit = as.integer(limit)))
  if (!nrow(df)) return(list())
  lines <- rag_fmt_rows(df, c("compound", "n", "herbs"), function(v) {
    sprintf("%s：%s 味药（如 %s）", v$compound, v$n, rag_cell(v$herbs))
  })
  rag_graph_ev(sprintf("「%s」类药最常含的化合物（功效→药→成分，3 步）", eff),
               lines, "graph:chain_eff_compound")
}

step_chain_indication_compound <- function(ind, limit = 12) {
  df <- rag_q(
    "MATCH (h:Herb)-[:ALSO_TREATS]->(i:Indication {name:$ind})
 MATCH (h)-[:CONTAINS]->(m:Compound)
 RETURN m.name AS compound, count(DISTINCT h) AS n, collect(DISTINCT h.name)[..3] AS herbs
 ORDER BY n DESC LIMIT $limit", list(ind = ind, limit = as.integer(limit)))
  if (!nrow(df)) return(list())
  lines <- rag_fmt_rows(df, c("compound", "n", "herbs"), function(v) {
    sprintf("%s：%s 味药（如 %s）", v$compound, v$n, rag_cell(v$herbs))
  })
  rag_graph_ev(sprintf("治「%s」的药最常含的化合物（病证→药→成分，3 步）", ind),
               lines, "graph:chain_ind_compound")
}

step_compound_indication <- function(compound, indication) {
  df <- rag_q(
    "MATCH (h:Herb)-[:CONTAINS]->(m:Compound)
 WHERE m.name = $cmp
 MATCH (h)-[:ALSO_TREATS]->(i:Indication)
 WHERE i.name CONTAINS $ind
 RETURN h.name AS herb, h.category_raw AS category, i.name AS indication
 ORDER BY herb LIMIT 20", list(cmp = compound, ind = indication))
  if (!nrow(df)) return(list())
  lines <- rag_fmt_rows(df, c("herb", "category", "indication"), function(v) {
    sprintf("%s［%s］→ %s", v$herb, v$category, v$indication)
  })
  rag_graph_ev(sprintf("同时含「%s」且主治含「%s」的中药（2 步交集）", compound, indication),
               lines, "graph:chain_compound_indication")
}

step_formula_detail <- function(name) {
  df <- rag_q(
    "MATCH (f:Formula {name:$name})
OPTIONAL MATCH (f)-[:FROM_BOOK]->(b:Book)
RETURN f.name AS name, b.name AS source, f.dosage_text AS dosage, f.usage_text AS usage,
       left(f.indication_text, 100) AS indication, f.is_patent AS patent,
       [(f)-[:HAS_INGREDIENT]->(i:Herb) | i.name] AS ingredients
LIMIT 5",
    list(name = name))
  if (!nrow(df)) return(list())
  lines <- rag_fmt_rows(df, c("source", "ingredients", "indication", "dosage", "patent"),
                        function(v) {
    sprintf("《%s》%s\n  组成：%s\n  主治：%s\n  剂型用法：%s%s",
            if (nzchar(v$source)) v$source else "未注明出处",
            if (identical(toupper(v$patent), "TRUE")) "（中成药）" else "",
            rag_or(v$ingredients, "—"),
            sub("^[：:；;，,、\\s]+", "", v$indication),
            rag_or(v$dosage, "—"),
            "")
  })
  rag_graph_ev(sprintf("方剂：%s（%d 个出处版本）", name, nrow(df)),
               lines, "graph:Formula")
}

# ---------------------------------------------------------------------------
# 10a. 英文/中英混排提问 → 中文条件（词表级翻译，不依赖大模型）
# ---------------------------------------------------------------------------
RAG_EN_GLOSSARY <- c(
  # 四气 / 五味
  "warm-natured" = "温性", "cold-natured" = "寒性", "cool-natured" = "凉性",
  "hot-natured" = "热性", "neutral-natured" = "平性", "slightly warm" = "微温",
  "warm nature" = "温性", "cold nature" = "寒性", "warm" = "温", "cold" = "寒",
  "cool" = "凉", "hot" = "热", "neutral" = "平",
  "pungent" = "辛味", "spicy" = "辛味", "sweet" = "甘味", "sour" = "酸味",
  "bitter" = "苦味", "salty" = "咸味", "astringent" = "涩味", "bland" = "淡味",
  # 归经
  "liver channel" = "肝经", "liver meridian" = "肝经", "heart channel" = "心经",
  "heart meridian" = "心经", "spleen channel" = "脾经", "spleen meridian" = "脾经",
  "lung channel" = "肺经", "lung meridian" = "肺经", "kidney channel" = "肾经",
  "kidney meridian" = "肾经", "stomach channel" = "胃经", "stomach meridian" = "胃经",
  "gallbladder channel" = "胆经", "bladder channel" = "膀胱经",
  "large intestine channel" = "大肠经", "small intestine channel" = "小肠经",
  "pericardium channel" = "心包经", "triple burner channel" = "三焦经",
  # 结构词
  "enter the" = "归", "enters the" = "归", "entering" = "归", "enter" = "归",
  "channel" = "经", "meridian" = "经", "suitable for" = "主治",
  "indicated for" = "主治", "used for" = "主治", "treats" = "治", "treat" = "治",
  "at doses" = "剂量", "dosage" = "剂量", "dose" = "剂量",
  "below" = "低于", "less than" = "低于", "under" = "低于",
  "no more than" = "不超过", "not exceeding" = "不超过",
  "grams" = "克", "gram" = "克",
  # 常见主治
  "headache" = "头痛", "cough" = "咳嗽", "asthma" = "哮喘", "wheezing" = "喘",
  "diarrhea" = "泄泻", "dysentery" = "痢疾", "insomnia" = "失眠",
  "palpitation" = "心悸", "palpitations" = "心悸", "edema" = "水肿",
  "jaundice" = "黄疸", "vomiting" = "呕吐", "nausea" = "恶心",
  "constipation" = "便秘", "malaria" = "疟疾", "rheumatism" = "风湿",
  "joint pain" = "关节疼痛", "back pain" = "腰痛", "chest pain" = "胸痹",
  "abdominal pain" = "腹痛", "stomach pain" = "胃痛", "toothache" = "牙痛",
  "sore throat" = "咽喉肿痛", "fever" = "发热", "dizziness" = "眩晕",
  "vertigo" = "眩晕", "hypertension" = "高血压", "diabetes" = "消渴",
  "tumor" = "肿瘤", "cancer" = "癌", "inflammation" = "炎症", "ulcer" = "溃疡",
  "eczema" = "湿疹", "itching" = "瘙痒", "anemia" = "贫血", "fatigue" = "乏力",
  "dysmenorrhea" = "痛经", "amenorrhea" = "闭经", "leukorrhea" = "带下",
  "bleeding" = "出血", "hemorrhage" = "出血", "nosebleed" = "衄血",
  "hemoptysis" = "咯血", "hematemesis" = "呕血", "blood in stool" = "便血",
  "blood in urine" = "尿血", "boils" = "痈肿", "carbuncle" = "痈",
  "abscess" = "脓肿", "snake bite" = "蛇咬伤", "burns" = "烧伤", "burn" = "烧伤",
  "trauma" = "跌打损伤", "convulsion" = "抽搐", "epilepsy" = "癫痫",
  "stroke" = "中风", "paralysis" = "瘫痪", "dysuria" = "小便不利",
  "enuresis" = "遗尿", "spermatorrhea" = "遗精", "impotence" = "阳痿",
  "infertility" = "不孕", "miscarriage" = "堕胎", "postpartum" = "产后",
  "menstrual" = "月经", "phlegm" = "痰", "palpitation" = "心悸",
  "poor appetite" = "食欲不振", "indigestion" = "食积", "bloating" = "腹胀",
  "hiccup" = "呃逆", "tinnitus" = "耳鸣", "deafness" = "耳聋",
  "blurred vision" = "目昏", "red eyes" = "目赤", "night blindness" = "夜盲",
  "numbness" = "麻木", "tremor" = "震颤"
)

# 英文/中英混排问句 → 中文（只做词表替换，绝不臆造）
rag_normalize_question <- function(q) {
  if (!grepl("[A-Za-z]", q)) return(q)
  s <- tolower(q)
  s <- gsub("([0-9]+(?:\\.[0-9]+)?)\\s*(?:grams?|g)\\b", "\\1克", s, perl = TRUE)
  keys <- names(RAG_EN_GLOSSARY)
  keys <- keys[order(-nchar(keys))]
  for (k in keys) {
    s <- gsub(paste0("\\b", k, "\\b"), RAG_EN_GLOSSARY[[k]], s, perl = TRUE)
  }
  for (w in c("which", "what", "the", "a", "an", "and", "or", "of", "for", "with",
              "at", "in", "on", "are", "is", "be", "that", "please", "can", "you",
              "tell", "me", "any", "all", "some", "herbs", "herb", "medicine",
              "medicines", "drug", "drugs", "there", "their", "it", "its", "also",
              "have", "has", "do", "does", "would", "could", "should")) {
    s <- gsub(paste0("\\b", w, "\\b"), " ", s, perl = TRUE)
  }
  s <- gsub("[[:punct:]]+", " ", s)
  gsub("\\s+", " ", trimws(s))
}

# ---------------------------------------------------------------------------
# 10b. 章节识别与"直达章节原文块"
# ---------------------------------------------------------------------------
RAG_SECTION_KW <- list(
  "药理作用" = c("药理", "药效", "作用机制", "实验研究", "现代研究", "实验"),
  "加工炮制" = c("炮制", "炮炙", "加工", "制炭", "酒制", "醋制", "盐制", "蜜制",
                 "炒炭", "麸炒", "土炒", "煨", "蒸制", "净制", "切制"),
  "药材鉴别" = c("鉴别", "性状", "药材特征", "特征", "外观", "品质", "规格", "真伪", "长什么样"),
  "化学成分" = c("化学成分", "成分", "化合物", "含量", "化学"),
  "栽培采集" = c("栽培", "种植", "采收", "采集", "播种", "田间", "育苗", "生长习性"),
  "药物基原" = c("基原", "植物形态", "科属", "是什么植物", "产地", "生境", "分布"),
  "性味归经" = c("性味", "归经", "四气", "五味", "寒性", "温性", "凉性", "热性",
                 "平性", "归什么经", "归哪经", "归何经"),
  "用法用量" = c("用法", "用量", "剂量", "怎么用", "怎么吃", "内服", "外用",
                 "煎汤", "一次服", "每日"),
  "宜忌配伍" = c("禁忌", "宜忌", "配伍", "同用", "一起用", "合用", "孕妇", "妊娠",
                 "十八反", "十九畏", "相恶", "相反", "相畏", "相杀", "相使", "相须", "慎用"),
  "方剂制剂" = c("方剂", "制剂", "哪些方", "什么方", "中成药", "丸剂", "散剂", "汤剂"),
  "各家学说" = c("各家", "医家", "本草考证", "历代", "评价", "论述", "考证"),
  "药物名称" = c("别名", "异名", "处方用名", "拉丁", "学名", "又叫", "名称"),
  "功效主治" = c("功效", "主治", "能治", "治什么", "有什么作用", "作用")
)

# 问句开头像"某味药"，但图谱里没有这个词条 → 用于"据实说明没有证据"
# 问句开头的短语若不是图谱里的词条 → "书里没有这味药"
# 边界：的/有/能/是/怎么/如何/内服/外用/含/治/功效/性味/…（避免把"扛板归内服外用"整段当药名）
RAG_LEAD_BOUNDARY <- paste0("的|有|能|是|怎么|如何|内服|外用|含|治|需要|可以|功效|性味|",
                            "别名|基原|药理|炮制|禁忌|用法|用量|方剂|各家|栽培|鉴别|",
                            "成分|归经|性状|采收|配伍|用法用量|功效主治")

# 问句开头的谓词 / 动词前缀：剥掉后再判断“开头的短语是不是药名”。
# 改前“治疗消渴的人参方剂”会把“治疗消渴”当成一味不存在的药，就是缺了这一步。
RAG_STRIP_PREFIX <- c("用于治疗", "用于治", "用来治", "治疗", "主治", "医治",
                      "用于", "用来", "适合", "适宜", "能治", "可治", "专治", "善治",
                      "防治", "缓解", "改善", "调理", "对付", "针对",
                      "含", "有", "用", "吃", "服", "查", "找", "问",
                      "介绍", "说说", "讲讲", "看看")

rag_strip_prefix <- function(x) {
  if (!nzchar(x)) return(x)
  ps <- RAG_STRIP_PREFIX[order(-nchar(RAG_STRIP_PREFIX))]
  for (p in ps) {
    if (startsWith(x, p) && nchar(x) > nchar(p) + 1L) {
      return(substring(x, nchar(p) + 1L))
    }
  }
  x
}

rag_unknown_entity <- function(question, link_df = NULL) {
  hits <- if (is.null(link_df) || !nrow(link_df)) character(0) else
    link_df$name[link_df$label %in% c("Herb", "Formula", "Compound", "Indication", "Efficacy")]
  # 问句正好以已知实体开头 → 一定不是未知
  if (length(hits) && any(startsWith(question, hits))) return("")
  m <- regmatches(question, regexpr(paste0("^[\u4e00-\u9fa5A-Za-z]{2,8}?(?=", RAG_LEAD_BOUNDARY, ")"),
                                    question, perl = TRUE))
  cand <- if (length(m)) m else ""
  if (!nzchar(cand)) return("")
  if (cand %in% c("哪些药", "什么药", "哪些药物", "什么药物", "哪些中药", "哪些药材",
                  "整本书", "全书", "历代", "中药", "这些药", "哪些中")) return("")
  # 候选本身只是个谓词/动词（“用于治疗失眠…”截出“用于”）→ 不是药名
  if (cand %in% RAG_STRIP_PREFIX) return("")
  # 以“历代/医家/各家/本草…”“整本书/全书…”开头的问句，开头那段不是药名
  if (grepl("^(历代|医家|各家|本草|古代|现代|我国|中医|书中|原文|方书|经方|古方)", cand)) return("")
  if (grepl("^(整本书|全书|一共|共有|总共|全部|本书|这本书|图谱|数据库|全库)", cand)) return("")
  if (grepl("^(哪些|什么|哪几|哪种|哪个|多少|几味)", cand)) return("")
  # 代词/回指开头的片段不是药名（“它的主要功效是什么”里的“它的主要”）
  if (grepl("^(它|其|该|此|这|那|上述|前述|前面|刚才|这个|那个|它们)", cand)) return("")
  terms <- tryCatch(kg_terms(), error = function(e) NULL)
  # “治疗消渴”“用于头痛”“主治失眠”“能治咳嗽”这类问句的开头是谓词，不是药名；
  # 逐层剥掉后再看剩下的短语是不是图谱里的词条（药/方/病证/功效/成分都算）
  variants <- cand
  cur <- cand
  for (i in seq_len(3L)) {
    nxt <- rag_strip_prefix(cur)
    if (identical(nxt, cur)) break
    cur <- nxt
    variants <- unique(c(variants, cur))
  }
  if (!is.null(terms) && any(variants %in% terms$name)) return("")
  cur
}

rag_section <- function(q) {
  for (sec in names(RAG_SECTION_KW)) {
    if (rag_any_kw(q, RAG_SECTION_KW[[sec]])) return(sec)
  }
  ""
}

# 直接取某味药某章节的原文块（不依赖全文命中，保证"问哪节给哪节"）
# 原文块挂在正目药名下，异名（瓜蒌）先归一再取
step_section_chunk <- function(name, section, limit = 3) {
  if (!nzchar(section)) return(list())
  cn <- rag_canon(name)
  df <- tryCatch(rag_q(
    "MATCH (u:TextUnit {herb_name:$name})
     WHERE u.section = $sec
     RETURN u.id AS id ORDER BY u.char_start LIMIT $limit",
    list(name = cn, sec = section, limit = as.integer(limit))),
    error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(list())
  g <- kg_store_get()
  idx <- match(df$id, g$nodes$id)
  idx <- idx[!is.na(idx)]
  lapply(idx, function(j) {
    rag_ev("text", g$nodes$title_path[j] %||% g$nodes$id[j],
           substr(g$nodes$text[j] %||% "", 1, 800), 3,
           list(chunk_id = g$nodes$id[j], doc = g$nodes$doc_id[j],
                section = g$nodes$section[j], line = g$nodes$src_line[j],
                source = "text:section"))
  })
}

# ---------------------------------------------------------------------------
# 11. 原文检索（cjk 全文索引 / 向量 / 实体提及）
# ---------------------------------------------------------------------------
RAG_STOP <- c("哪些", "什么", "怎么", "如何", "是否", "可以", "有没有", "有哪", "请问",
              "一下", "帮我", "整本", "全书", "全库", "总体", "这味", "这种", "中药",
              "药材", "图谱", "知识", "查询", "统计", "分析", "介绍", "说明", "看看",
              "能够", "实现", "综合", "多步", "相关", "关于", "它们", "这些", "那些",
              # 领域高频词：留在全文检索里只会带来噪声
              "功效", "主治", "性味", "归经", "用量", "用法", "禁忌", "毒性", "有毒",
              "无毒", "成分", "药理", "作用", "方剂", "组成", "治疗", "区别", "对比",
              "相反", "相恶", "配伍", "鉴别", "含量", "化合物", "有效成分", "临床",
              "应用", "研究", "现代", "具有", "分别", "各自", "一下", "分别",
              # 统计类用词
              "统计", "多少", "一共", "共有", "数量", "频次", "最高", "最多",
              "排行", "占比", "分布", "规模", "全书", "整本")

rag_fulltext_query <- function(question, entity_names = character(0)) {
  s <- gsub("[[:punct:]，。？！：；、“”‘’（）《》【】、]", " ", question)
  # 章节名与问句虚词先按子串去掉，避免整句被当成一个"长词"去做 bigram 匹配
  drop <- c(RAG_STOP, names(RAG_SECTION_KW),
            "药材鉴别", "药物基原", "栽培采集", "加工炮制", "性味归经", "功效主治",
            "用法用量", "宜忌配伍", "方剂制剂", "各家学说", "药物名称",
            "什么", "哪些", "多少", "怎么", "如何", "请问", "有哪些", "是什么",
            "有什么", "是什么", "多少味", "吗", "呢", "的", "了", "能", "可以",
            "有", "和", "与", "及", "分别", "一下", "介绍", "说明")
  for (w in unique(drop)) s <- gsub(w, " ", s, fixed = TRUE)
  toks <- strsplit(gsub("\\s+", " ", s), " ")[[1]]
  toks <- toks[nchar(toks) >= 2]
  toks <- utils::head(toks, 6)
  paste(unique(c(entity_names, toks)), collapse = " ")
}

step_text <- function(question, entity_names = character(0), limit = 8, section = "") {
  q <- rag_fulltext_query(question, entity_names)
  if (!nzchar(trimws(q))) return(list())
  g <- kg_store_get()
  # 章节原文块由 step_section_chunk 直接取，全文池不必开太大
  pool <- if (nzchar(section)) limit * 4L else limit * 3L
  hits <- tryCatch(kg_fulltext_search(g, "chunk_text", q, limit = pool),
                   error = function(e) NULL)
  if (is.null(hits) || !nrow(hits)) return(list())
  idx <- hits$node
  sc <- as.numeric(hits$score)
  # 来自锚点药自身文档的块优先（例如问「地榆」时优先 地榆.txt 而不是提到地榆的其他药）
  anchor <- rep(FALSE, length(idx))
  if (length(entity_names)) {
    docs <- g$nodes$doc_id[idx]
    for (nm in entity_names) {
      anchor <- anchor | (!is.na(docs) & endsWith(docs, paste0("/", nm)))
    }
  }
  sec_hit <- if (nzchar(section)) (g$nodes$section[idx] %in% section) else rep(FALSE, length(idx))
  ord <- order(-as.integer(sec_hit), -as.integer(anchor), -sc)
  ord <- utils::head(ord, limit)
  idx <- idx[ord]; sc <- sc[ord]
  lapply(seq_along(idx), function(i) {
    j <- idx[i]
    rag_ev("text",
           g$nodes$title_path[j] %||% g$nodes$id[j],
           substr(g$nodes$text[j] %||% "", 1, 600),
           sc[i],
           list(chunk_id = g$nodes$id[j], doc = g$nodes$doc_id[j],
                section = g$nodes$section[j], line = g$nodes$src_line[j],
                source = "text:fulltext"))
  })
}

step_mentions <- function(ids, limit = 6, entity_names = character(0), section = "") {
  ids <- utils::head(ids, 6)
  if (!length(ids)) return(list())
  g <- kg_store_get()
  pos <- match(ids, g$nodes$id)
  pos <- pos[!is.na(pos)]
  if (!length(pos)) return(list())
  # 直接走邻接表（实体 -> 提及它的 TextUnit），避免全图扫 14 万条 MENTIONS
  tu <- integer(0)
  for (i in pos) {
    e <- kg_adj(g, i, "in", "MENTIONS")
    if (length(e)) tu <- c(tu, g$rels$src[e])
  }
  if (!length(tu)) return(list())
  tb <- table(tu)
  cand <- as.integer(names(tb))
  hits <- as.integer(tb)
  # 锚点药自己文档里的块优先
  anchor <- rep(FALSE, length(cand))
  if (length(entity_names)) {
    docs <- g$nodes$doc_id[cand]
    for (nm in entity_names) {
      anchor <- anchor | (!is.na(docs) & endsWith(docs, paste0("/", nm)))
    }
  }
  sec_hit <- if (nzchar(section)) (g$nodes$section[cand] %in% section) else rep(FALSE, length(cand))
  ord <- order(-as.integer(sec_hit), -as.integer(anchor), -hits)
  ord <- utils::head(ord, limit)
  cand <- cand[ord]; hits <- hits[ord]
  lapply(seq_along(cand), function(i) {
    j <- cand[i]
    rag_ev("text",
           g$nodes$title_path[j] %||% g$nodes$id[j],
           substr(g$nodes$text[j] %||% "", 1, 600),
           hits[i],
           list(chunk_id = g$nodes$id[j], doc = g$nodes$doc_id[j],
                section = g$nodes$section[j], line = g$nodes$src_line[j],
                source = "text:mentions"))
  })
}

# ---------------------------------------------------------------------------
# 12. 计划生成
# ---------------------------------------------------------------------------
rag_plan <- function(question, link_df, filters, intent) {
  plan <- list()
  add <- function(kind, ...) plan[[length(plan) + 1L]] <<- list(kind = kind, ...)

  herbs <- rag_entities(link_df, "Herb")
  formulas <- rag_entities(link_df, "Formula")
  compounds <- rag_entities(link_df, "Compound")
  efficacies <- rag_entities(link_df, "Efficacy")
  indications <- rag_entities(link_df, "Indication")

  want_safety <- intent == "safety" || rag_any_kw(question, RAG_SAFETY_KW)
  want_formula <- intent == "formula" || rag_any_kw(question, c("方", "方剂", "中成药", "组成"))
  want_compound <- intent == "compound" || rag_any_kw(question, RAG_COMPOUND_KW)
  want_pharm <- rag_any_kw(question, c("药理", "作用机制", "药效", "实验"))
  want_pairs <- rag_any_kw(question, c("配伍", "药对", "鉴别", "区别", "对比"))
  want_chain <- rag_any_kw(question, RAG_MULTIHOP_KW)
  # 单味药的泛问（没有特别focus）才补上安全性与方剂，避免干扰定向问题
  focused <- want_safety || want_pharm ||
    rag_any_kw(question, c("成分", "化合物", "药理", "方剂", "方中", "配伍", "禁忌",
                           "毒性", "鉴别", "区别", "对比", "统计"))
  generic_one <- nrow(herbs) == 1L && !focused

  # A. 分类归属核验（“X 是不是 Y 类药”）：核验名单优先，档案与功效链作补充
  if (identical(intent, "category")) {
    add("category_check", name = herbs$name[1], category = filters$category)
    add("profile", name = herbs$name[1])
    add("chain", name = herbs$name[1])
    if (want_safety) add("safety", name = herbs$name[1])
    return(plan)
  }

  # 条件集合（词表反查与综合约束共用）
  conds <- list(
    efficacy = efficacies$name,
    indication = indications$name,
    compound = compounds$name,
    category = filters$category,
    meridian = filters$meridian,
    nature = filters$nature,
    taste = filters$taste,
    toxicity = filters$toxicity,
    contra_kind = filters$contra_kind,
    has_contra = filters$has_contra,
    seven = filters$seven,
    pairs = filters$pairs,
    dose_max = filters$dose_max,
    patent = filters$patent,
    or = filters$or
  )
  n_cond <- length(conds$efficacy) + length(conds$indication) +
    length(conds$compound) + length(conds$category) +
    length(conds$meridian) + length(conds$nature) + length(conds$taste) +
    (!is.null(conds$toxicity)) + (!is.null(conds$contra_kind)) +
    isTRUE(conds$has_contra) + (!is.null(conds$seven)) + isTRUE(conds$pairs) +
    (!is.null(conds$dose_max)) + isTRUE(conds$patent)

  # A0. 词表反查：只有词表条件、没有点名具体实体
  if (identical(intent, "vocab")) {
    add("vocab", conds = conds)
    return(plan)
  }

  # A. 全库统计
  if (identical(intent, "stats")) {
    add("stats", topic = rag_stats_topic(question))
    return(plan)
  }

  # B. 两药对比（档案与鉴别由下面的"锚点药"循环补齐）
  if (identical(intent, "compare") && nrow(herbs) >= 2) {
    add("compare", a = herbs$name[1], b = herbs$name[2])
  }

  # C. 锚点药
  for (h in utils::head(herbs$name, 3)) {
    add("profile", name = h)
    add("chain", name = h)
    if (want_safety || generic_one) add("safety", name = h)
    if (want_formula || generic_one) add("formulas", name = h)
    if (want_pharm) add("pharm", name = h)
    if (want_compound) {
      add("compounds", name = h)
      if (!want_pharm) add("pharm", name = h)
    }
    if (want_pairs) add("pairs", name = h)
    if (want_formula && want_chain) add("formula_partners", name = h)
  }

  # D. 方剂锚点
  # “治失眠”这类以谓词开头的方名，在“哪些药能治…”的问句里是解析噪声，不作为锚点
  herbs_q <- rag_any_kw(question, c("哪些药", "什么药", "有哪些药", "药有哪些", "哪些中药"))
  for (f in utils::head(formulas$name, 3)) {
    if (identical(intent, "reverse") && herbs_q && grepl("^治", f)) next
    add("formula_detail", name = f)
  }

  # D2. 方剂 × 病证：既要含这味药、又要主治文本含这个病证
  if (nrow(herbs) && nrow(indications)) {
    add("formula_indication", herb = herbs$name[1], indication = indications$name[1])
  }

  # E. 反查
  # 问“最常含什么成分”时，整类药清单会淹没有效信息，直接跳过功效反查
  chain_focus <- want_compound &&
    grepl("最常|最多|最常见|主要|共同|都有|共有", question)
  for (i in utils::head(indications$name, 3)) add("reverse_indication", name = i)
  if (!(chain_focus && nrow(efficacies))) {
    for (e in utils::head(efficacies$name, 3)) add("reverse_efficacy", name = e)
  }
  for (c in utils::head(compounds$name, 3)) add("reverse_compound", name = c)

  # F. 多步链
  if (nrow(compounds) && nrow(indications)) {
    add("compound_indication", cmp = compounds$name[1], ind = indications$name[1])
  }
  # “某类药最常含什么成分”只在没有点名成分时才有意义
  if (!nrow(compounds) && nrow(efficacies) && want_compound) {
    add("chain_eff_compound", eff = efficacies$name[1])
  }
  if (!nrow(compounds) && nrow(indications) && want_compound) {
    add("chain_ind_compound", ind = indications$name[1])
  }

  # G. 综合约束：≥2 个条件，或"实体 + 过滤条件"
  # 多条件交集就是这个问题的直接答案，放在计划最前面（证据排名最高）
  if (n_cond >= 2) plan <- c(list(list(kind = "constraint", conds = conds)), plan)

  # H. 只有一个过滤条件、且没有实体（词表反查没接住的兜底，如“外用药”）
  if (n_cond == 1L && !nrow(herbs) && !nrow(compounds) && !nrow(indications) &&
      !nrow(efficacies) && !nrow(formulas)) {
    add("constraint", conds = conds)
  }

  # 去重：同一 (类型 + 参数) 只跑一次（例如对比两药时档案会被加两次）
  if (length(plan)) {
    sig <- vapply(plan, function(s) {
      paste(s$kind, s$name %||% "", s$a %||% "", s$b %||% "", s$cmp %||% "",
            s$ind %||% "", s$eff %||% "", sep = "\u0001")
    }, "")
    plan <- plan[!duplicated(sig)]
  }
  plan
}

# ---------------------------------------------------------------------------
# 13. 执行计划
# ---------------------------------------------------------------------------
rag_run_step <- function(step) {
  switch(step$kind,
    profile = rag_step(paste0("药物档案：", step$name),
                       function() step_herb_profile(step$name)),
    chain = rag_step(paste0(step$name, " 功效→主治"),
                     function() step_herb_efficacy_chain(step$name)),
    safety = rag_step(paste0("安全性：", step$name),
                      function() step_herb_safety(step$name)),
    formulas = rag_step(paste0(step$name, " 相关方剂"),
                        function() step_herb_formulas(step$name)),
    formula_partners = rag_step(paste0(step$name, " 方剂→方中药"),
                                function() step_formula_partners(step$name)),
    compounds = rag_step(paste0(step$name, " 化学成分"),
                         function() step_herb_compounds(step$name)),
    pharm = rag_step(paste0(step$name, " 药理作用"),
                     function() step_herb_pharm(step$name)),
    pairs = rag_step(paste0(step$name, " 配伍/鉴别"),
                     function() step_herb_pairs(step$name)),
    reverse_indication = rag_step(paste0("主治「", step$name, "」反查"),
                                  function() step_reverse_indication(step$name)),
    reverse_efficacy = rag_step(paste0("功效「", step$name, "」反查"),
                                function() step_reverse_efficacy(step$name)),
    reverse_compound = rag_step(paste0("成分「", step$name, "」反查"),
                                function() step_reverse_compound(step$name)),
    formula_detail = rag_step(paste0("方剂：", step$name),
                              function() step_formula_detail(step$name)),
    formula_indication = rag_step(
      paste0("含", step$herb, "且主治含「", step$indication, "」的方剂"),
      function() step_formula_indication(step$herb, step$indication)),
    compound_indication = rag_step("成分 × 病证 交集",
                                   function() step_compound_indication(step$cmp, step$ind)),
    chain_eff_compound = rag_step("功效→药→成分",
                                  function() step_chain_efficacy_compound(step$eff)),
    chain_ind_compound = rag_step("病证→药→成分",
                                  function() step_chain_indication_compound(step$ind)),
    compare = rag_step("两药对比", function() step_compare(step$a, step$b)),
    constraint = rag_step("多条件筛选", function() step_constraint(step$conds)),
    vocab = rag_step("词表反查", function() step_vocab(step$conds)),
    category_check = rag_step(paste0("分类归属核验：", step$name),
                              function() step_category_check(step$name, step$category)),
    stats = rag_step("全库统计", function() step_stats(question = "", topic = step$topic)),
    list(evidence = list(), trace = list(title = step$kind, rows = 0L, ms = 0L))
  )
}

# ---------------------------------------------------------------------------
# 14. 主入口
# ---------------------------------------------------------------------------
rag_retrieve <- function(question, cfg = rag_config(), ctx = NULL) {
  t0 <- proc.time()[["elapsed"]]
  question_raw <- trimws(question)
  # 英文/中英混排先按词表转成中文条件（不臆造，只做词表映射）
  question <- tryCatch(rag_normalize_question(question_raw), error = function(e) question_raw)

  filters <- rag_filters(question)
  # 分类名（如"清热药"）已作为筛选条件，做实体链接时先去掉，避免再被认成"清热"功效
  q_link <- question
  for (cc in (filters$category %||% character(0))) q_link <- gsub(cc, "", q_link, fixed = TRUE)
  link_df <- tryCatch(rag_link(q_link), error = function(e) rag_empty_link())

  # 多轮追问：本轮没接住锚点实体、而问题在回指上一轮 → 把上一轮实体并入链接。
  # 若继承到的是「分类」，同时补进 filters（这样“它里面有毒的有哪些”仍按分类筛）。
  carried <- character(0)
  if (rag_is_followup(question, link_df)) {
    prev <- rag_ctx_get(ctx)
    if (!is.null(prev) && length(prev$entities)) {
      carried <- prev$entities
      lk2 <- tryCatch(rag_link(paste(c(q_link, carried), collapse = " ")),
                      error = function(e) NULL)
      if (!is.null(lk2) && nrow(lk2)) link_df <- lk2
      cat_terms <- tryCatch({
        t <- kg_terms()
        t$name[t$label == "DrugCategory"]
      }, error = function(e) character(0))
      cats <- intersect(carried, cat_terms)
      # 只有本轮仍没有药名锚点时，才把继承来的分类补进筛选条件
      # （否则“百合是安神药吗”→“它的功效有哪些”会被安神药过滤带偏）
      if (length(cats) && !nrow(rag_entities(link_df, "Herb"))) {
        filters$category <- unique(c(filters$category %||% character(0), cats))
      }
    }
  }
  if (length(carried)) {
    trace_msg <- sprintf("指代继承：%s", paste(carried, collapse = "、"))
  } else {
    trace_msg <- ""
  }
  # 有代词、但会话里没有可继承实体：不猜，也不让全文检索拿噪声凑答案
  clarify <- length(carried) == 0L && rag_has_pronoun(question) &&
    !rag_has_anchor(link_df)

  intent <- tryCatch(rag_intent(question, link_df, filters), error = function(e) "fact")
  # 两个以上条件同时成立时，本质是"综合筛选"而不是安全/事实问题
  n_cond <- length(rag_entities(link_df, "Efficacy")$name) +
    length(rag_entities(link_df, "Indication")$name) +
    length(rag_entities(link_df, "Compound")$name) +
    length(filters$category) + length(filters$meridian) +
    length(filters$nature) + length(filters$taste) +
    (!is.null(filters$toxicity)) + (!is.null(filters$contra_kind)) +
    isTRUE(filters$has_contra) + (!is.null(filters$seven)) + isTRUE(filters$pairs) +
    isTRUE(filters$patent)
  # 只有"筛选型"条件（功效/病证/成分/分类/归经/性味/中成药）有两个以上，
  # 且点名了实体，才算综合筛选；毒性/禁忌这类过滤器不改变单味药的意图
  n_screen <- length(rag_entities(link_df, "Efficacy")$name) +
    length(rag_entities(link_df, "Indication")$name) +
    length(rag_entities(link_df, "Compound")$name) +
    length(filters$category) + length(filters$meridian) +
    length(filters$nature) + length(filters$taste) + isTRUE(filters$patent)
  if (n_screen >= 2L && rag_has_anchor(link_df) && !intent %in% c("stats", "compare", "vocab")) {
    intent <- "constraint"
  }
  plan <- tryCatch(rag_plan(question, link_df, filters, intent), error = function(e) list())

  runs <- lapply(plan, rag_run_step)
  graph_ev <- unlist(lapply(runs, `[[`, "evidence"), recursive = FALSE) %||% list()
  trace <- lapply(runs, `[[`, "trace")

  entity_ids <- utils::head(link_df$id, 6)
  entity_names <- utils::head(unique(c(link_df$name, vapply(link_df$name, rag_canon, ""))), 6)

  # 章节识别：问题问的是哪一节，就直接取该节的原文块（保证"问哪节给哪节"）
  section <- tryCatch(rag_section(question), error = function(e) "")
  graph_herbs <- unlist(lapply(graph_ev, function(e) {
    h <- e$citation$herbs
    if (is.null(h)) NULL else unlist(h, use.names = FALSE)
  }))
  if (length(graph_herbs)) entity_names <- unique(c(entity_names, utils::head(graph_herbs, 5)))
  unknown <- if (length(carried) || identical(intent, "stats") ||
                 rag_n_filters(filters) > 0L) "" else
    tryCatch(rag_unknown_entity(question, link_df), error = function(e) "")
  herbs <- rag_entities(link_df, "Herb")

  tt <- proc.time()[["elapsed"]]
  sec_ev <- if (nzchar(section) && nrow(herbs)) {
    tryCatch(step_section_chunk(herbs$name[1], section), error = function(e) list())
  } else list()
  t_sec <- round((proc.time()[["elapsed"]] - tt) * 1000)

  tt <- proc.time()[["elapsed"]]
  text_ev <- tryCatch(step_text(question, entity_names, cfg$top_chunks, section = section),
                      error = function(e) list())
  t_text <- round((proc.time()[["elapsed"]] - tt) * 1000)

  tt <- proc.time()[["elapsed"]]
  mention_ev <- tryCatch(step_mentions(entity_ids, entity_names = entity_names,
                                       section = section),
                         error = function(e) list())
  t_mention <- round((proc.time()[["elapsed"]] - tt) * 1000)

  # 向量召回只在图文证据不足时兜底：章节直达 + cjk 全文已经能命中时不必再走网络
  tt <- proc.time()[["elapsed"]]
  enough <- (length(sec_ev) + length(text_ev) + length(mention_ev)) >= 3L
  vec_ev <- if (enough) list() else
    tryCatch(rag_vec_evidence(question, cfg, cfg$vector_k), error = function(e) list())
  t_vec <- round((proc.time()[["elapsed"]] - tt) * 1000)

  if (length(sec_ev)) {
    trace[[length(trace) + 1L]] <- list(title = paste0("直达章节原文（", section, "）"),
                                        rows = length(sec_ev), ms = t_sec)
  }
  if (length(text_ev)) {
    trace[[length(trace) + 1L]] <- list(title = "原文全文检索（cjk）",
                                        rows = length(text_ev), ms = t_text)
  }
  if (length(mention_ev)) {
    trace[[length(trace) + 1L]] <- list(title = "实体提及定位",
                                        rows = length(mention_ev), ms = t_mention)
  }
  if (length(vec_ev)) {
    trace[[length(trace) + 1L]] <- list(title = "语义向量检索（BGE-M3）",
                                        rows = length(vec_ev), ms = t_vec)
  }

  evidence <- rag_fuse(graph_ev, list(rag_dedup_ev(c(sec_ev, mention_ev, text_ev, vec_ev))),
                       max_n = cfg$max_evidence)
  # 清单类结果：完整明细提到顶层，供“下载完整名单（CSV）”；
  # 同一问题命中多个清单时按列并集合并，“来源”列区分
  list_parts <- list()
  for (i in seq_along(evidence)) {
    rows_i <- evidence[[i]]$citation$rows
    if (!is.null(rows_i) && nrow(rows_i)) {
      list_parts[[length(list_parts) + 1L]] <- cbind(
        data.frame(来源 = evidence[[i]]$title, stringsAsFactors = FALSE),
        rag_flat_df(rows_i))
      evidence[[i]]$citation$rows <- NULL
      evidence[[i]]$citation$list_title <- evidence[[i]]$title
    }
  }
  list_df <- rag_bind_lists(list_parts)

  # 问到的药在图谱里没有条目：只保留真正提到它的片段，避免拿别的药的原文充数
  if (nzchar(unknown) && length(evidence)) {
    mentioned <- vapply(evidence, function(e) {
      identical(e$kind, "graph") || grepl(unknown, e$text %||% "", fixed = TRUE)
    }, logical(1))
    evidence <- evidence[mentioned]
  }
  # 只问一味药时，别的药的原文块不参与（避免用别人的"关键词"充当该药的证据）。
  # 方剂类问题例外：讲某药出现在哪些方里时，别的药条目下提到它的段落正是证据。
  apply_own_filter <- nrow(herbs) == 1L && length(evidence) &&
    !identical(intent, "formula") &&
    !grepl("方剂|方中|中成药|丸|散|汤|制剂", question)
  if (apply_own_filter) {
    # 正文块按“正名 + 异名组”匹配（瓜蒌的原文块存在 栝楼 名下）
    nms <- rag_herb_names(herbs$name[1])
    own <- vapply(evidence, function(e) {
      if (identical(e$kind, "graph")) return(TRUE)
      d <- e$citation$doc %||% ""
      !is.na(d) && nzchar(d) && any(endsWith(d, paste0("/", nms)))
    }, logical(1))
    if (sum(own) >= 4L) evidence <- evidence[own]
  }
  # 指代解析不出对象时，只给一条“请先说明指代对象”的证据，不用噪声凑答案
  if (clarify) {
    evidence <- list(rag_graph_ev(
      "指代未能解析",
      paste0("本问题用“它/该药”指代，但当前会话还没有可继承的实体。",
             "请先说明是哪一味药（或哪首方剂、哪个病证），再问“它……”；",
             "例如先问“人参的性味归经”，再问“它的用量”。"),
      "graph:clarify", 2))
    trace <- list(list(title = "指代未能解析（需要上一轮实体）", rows = 1L, ms = 0L))
    list_df <- NULL
  }

  # 记住本轮实体，供下一轮指代继承（只在内存、按会话隔离）；
  # 分类名在链接前被剥离，这里从 filters 补记一次
  tryCatch(rag_ctx_remember(link_df, question_raw, ctx = ctx,
                            extra = filters$category %||% character(0)),
           error = function(e) NULL)

  # 检索链路里显示“这一步是继承了上一轮”，便于核对
  if (length(carried)) {
    trace <- c(list(list(title = trace_msg, rows = length(carried), ms = 0L)), trace)
  }

  list(
    question = question,
    question_raw = question_raw,
    intent = intent,
    carried = carried,
    entities = link_df,
    filters = filters,
    plan = vapply(plan, function(s) s$kind, ""),
    section = section,
    unknown = unknown,
    sql = unique(unlist(lapply(graph_ev, function(e) e$citation$sql))),
    list = list_df,
    trace = trace,
    evidence = evidence,
    counts = list(
      graph = sum(vapply(evidence, function(e) e$kind == "graph", TRUE)),
      text = sum(vapply(evidence, function(e) e$kind == "text", TRUE))
    ),
    elapsed = round(proc.time()[["elapsed"]] - t0, 3)
  )
}
