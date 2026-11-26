# =============================================================================
# kg_boot.R — 启动时加载内置的 zhyhkg 引擎与《中华药海》图库
#
# 设计：
#   * 引擎源码放在 R/kg/（由 07_rgraph 同步，见 tools/sync_zhyhkg.R），
#     source() 加载，不做 install.packages —— Connect Cloud 只上传本目录。
#   * 图库在 data/kg_store.rds（28.6 MB，含 6 个 cjk 全文倒排索引）。
#   * 整库只加载一次，缓存在 .kg 环境里，供所有 Shiny 会话共享（只读）。
# =============================================================================

.kg <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# 1. 加载 zhyhkg 源码（顺序：schema 常量必须最先）
# ---------------------------------------------------------------------------
.kg_source_engine <- function(dir = file.path("R", "kg")) {
  files <- c(
    "zzz_schema_data.R",   # 本体常量（34 标签 / 43 关系 / 属性类型）
    "aaa_utils.R",         # NULL 语义、值句柄、CSV 写出
    "store.R",             # 属性图存储
    "index.R",             # 约束 / 索引 / 全文检索
    "import.R",            # CSV 导入（本 app 用不到，但 store 依赖其辅助函数）
    "cypher_lexer.R",
    "cypher_parser.R",
    "cypher_functions.R",
    "cypher_eval.R",
    "cypher_exec.R",
    "ops.R",
    "algo.R"
  )
  missing <- files[!file.exists(file.path(dir, files))]
  if (length(missing)) {
    stop("缺少 zhyhkg 引擎文件：", paste(missing, collapse = ", "),
         "（运行 tools/sync_zhyhkg.R 同步）")
  }
  for (f in files) sys.source(file.path(dir, f), envir = globalenv())
  invisible(TRUE)
}

if (!exists("kg_cypher", mode = "function")) .kg_source_engine()

# zhyhkg 的 `%||%` 只判 NULL；问答层需要「NA 也当缺失」，这里统一成 NA 安全版
# （zhyhkg 内部只对 NULL/原子向量使用，语义兼容）
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  if (length(x) == 1L && is.na(x)) return(y)
  x
}

# ---------------------------------------------------------------------------
# 2. 加载图库（每进程一次）
# ---------------------------------------------------------------------------
kg_store_path <- function() {
  p <- Sys.getenv("KG_STORE_FILE", "data/kg_store.rds")
  if (!file.exists(p)) {
    stop("缺少图库 data/kg_store.rds（在上游工程运行 07_rgraph/scripts/01_build_store.R ",
         "后再执行 tools/sync_zhyhkg.R）")
  }
  p
}

kg_store_get <- function() {
  if (is.null(.kg$g)) {
    t0 <- proc.time()[["elapsed"]]
    .kg$g <- kg_load(kg_store_path(), read_only = TRUE, verbose = FALSE)
    .kg$load_sec <- proc.time()[["elapsed"]] - t0
    .kg$terms <- kg_build_terms(.kg$g)
    .kg$terms_sec <- proc.time()[["elapsed"]] - t0 - .kg$load_sec
  }
  .kg$g
}

# 兼容旧代码里的 graph 引用
kg_graph <- function() kg_store_get()

# ---------------------------------------------------------------------------
# 3. 实体词典：从图库自动生成（替代旧的 rag_terms.rds）
#    同名跨标签时按 priority 取一个（Herb 最优先）
# ---------------------------------------------------------------------------
kg_build_terms <- function(g) {
  nd <- g$nodes
  pick <- function(label, field, prio) {
    ids <- g$label_ids[[label]] %||% integer(0)
    if (!length(ids) || !field %in% names(nd)) return(NULL)
    v <- nd[[field]][ids]
    ok <- !is.na(v) & nzchar(v)
    if (!any(ok)) return(NULL)
    data.frame(term = v[ok], label = label, id = nd$id[ids][ok],
               name = v[ok], priority = prio, stringsAsFactors = FALSE)
  }
  parts <- list(
    pick("Herb", "name", 1),
    pick("Formula", "name", 2),
    pick("Indication", "name", 3),
    pick("Efficacy", "name", 4),
    pick("Compound", "name", 5),
    pick("Book", "name", 6),
    pick("PharmEffect", "name", 7),
    pick("DrugCategory", "name", 8),
    pick("Species", "name_zh", 9),
    pick("Family", "name", 10),
    pick("Region", "name", 11),
    pick("MedicinalPart", "name", 12)
  )

  # 别名 / 炮制品名 → 归到对应的 Herb（按 herb_name 找主药）
  herb_ids <- g$label_ids[["Herb"]] %||% integer(0)
  herb_name <- nd$name[herb_ids]
  name2herb <- stats::setNames(herb_ids, herb_name)
  link_alias <- function(ids, field, parent_field, prio) {
    if (!length(ids)) return(NULL)
    v <- nd[[field]][ids]
    p <- nd[[parent_field]][ids]
    keep <- !is.na(v) & nzchar(v) & !is.na(p) & p %in% names(name2herb)
    if (!any(keep)) return(NULL)
    hid <- name2herb[p[keep]]
    data.frame(term = v[keep], label = "Herb", id = nd$id[hid],
               name = nd$name[hid], priority = prio, stringsAsFactors = FALSE)
  }
  parts <- c(parts, list(
    link_alias(g$label_ids[["HerbAlias"]] %||% integer(0), "alias", "herb_name", 2),
    link_alias(g$label_ids[["HerbVariant"]] %||% integer(0), "name", "herb_name", 3)
  ))

  terms <- do.call(rbind, Filter(Negate(is.null), parts))
  if (is.null(terms) || !nrow(terms)) {
    return(data.frame(term = character(0), label = character(0), id = character(0),
                      name = character(0), priority = integer(0),
                      stringsAsFactors = FALSE))
  }
  terms$term <- trimws(terms$term)
  terms <- terms[nzchar(terms$term) & nchar(terms$term) >= 2L, , drop = FALSE]
  terms <- terms[order(terms$priority, -nchar(terms$term)), , drop = FALSE]
  terms <- terms[!duplicated(terms$term), , drop = FALSE]
  rownames(terms) <- NULL
  terms
}

kg_terms <- function() {
  kg_store_get()
  .kg$terms
}

# 预热：R 的闭包首次执行要编译，先把几条常用查询跑一遍（否则第一个提问会多等 2–3 秒）
kg_warmup <- function() {
  invisible(tryCatch({
    g <- kg_store_get()
    kg_cypher(g, "MATCH (h:Herb {name:'地榆'})-[:HAS_EFFICACY]->(e:Efficacy)
                  RETURN e.name AS e LIMIT 1")
    kg_cypher(g, "MATCH (h:Herb {name:'地榆'})
                  RETURN [(h)-[:ALSO_TREATS]->(i:Indication) | i.name][..3] AS x")
    kg_cypher(g, "MATCH (h:Herb)-[:HAS_EFFICACY]->(e:Efficacy)
                  WHERE e.name CONTAINS '清热' RETURN count(*) AS n")
    kg_cypher(g, "MATCH (h:Herb)-[:HAS_EFFICACY]->(e:Efficacy)
                  MATCH (h)-[:CONTAINS]->(m:Compound)
                  WHERE e.name CONTAINS '止血' AND m.name = '槲皮素'
                  RETURN h.name AS herb LIMIT 3")
    kg_fulltext_search(g, "chunk_text", "地榆 止血", limit = 3)
    TRUE
  }, error = function(e) FALSE))
}

# 千分位（trim=TRUE，避免 format() 自动右对齐补空格）
kg_num <- function(x) {
  format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# 各标签节点数（静态，缓存一次；统计类问题不再每次扫 5 万节点）
kg_label_counts <- function() {
  g <- kg_store_get()
  if (is.null(.kg$label_counts)) {
    lc <- vapply(g$label_ids, length, integer(1))
    .kg$label_counts <- lc[order(-lc)]
  }
  .kg$label_counts
}

# 各关系类型条数（静态，缓存）
kg_rel_counts <- function() {
  g <- kg_store_get()
  if (is.null(.kg$rel_counts)) {
    rc <- vapply(g$rel_type_ids, length, integer(1))
    .kg$rel_counts <- rc[order(-rc)]
  }
  .kg$rel_counts
}

# 图谱规模（状态栏 / 健康检查用）
kg_summary <- function() {  g <- kg_store_get()
  list(
    ok = TRUE,
    backend = "kg_store",
    file = kg_store_path(),
    nodes = nrow(g$nodes),
    relationships = nrow(g$rels),
    labels = length(g$label_ids),
    rel_types = length(g$rel_type_ids),
    chunks = length(g$label_ids[["TextUnit"]] %||% integer(0)),
    # herbs 指“带专著的药味数”（与论文一致）；herb_nodes 是全部药名节点（含方剂异名）
    herbs = {
      hid <- g$label_ids[["Herb"]] %||% integer(0)
      if (length(hid)) sum(g$nodes$in_corpus[hid] %in% c(TRUE, "true")) else 0L
    },
    herb_nodes = length(g$label_ids[["Herb"]] %||% integer(0)),
    formulas = length(g$label_ids[["Formula"]] %||% integer(0)),
    fulltext_indexes = sum(g$indexes$type == "FULLTEXT"),
    terms = nrow(kg_terms()),
    load_sec = round(.kg$load_sec %||% 0, 2),
    terms_sec = round(.kg$terms_sec %||% 0, 2)
  )
}

# ---------------------------------------------------------------------------
# 4. 小工具
# ---------------------------------------------------------------------------

# 把 kg_cypher 结果里的 list 列（collect / 模式推导）压成字符串
rag_cell <- function(x, sep = "、") {
  if (is.null(x) || inherits(x, "kg_null")) return("")
  if (is.list(x)) {
    parts <- vapply(x, function(e) {
      if (is.null(e) || inherits(e, "kg_null")) return("")
      paste(as.character(e), collapse = sep)
    }, "")
    parts <- parts[nzchar(parts)]
    return(paste(parts, collapse = sep))
  }
  if (length(x) == 1L && is.na(x)) return("")
  paste(as.character(x), collapse = sep)
}

# 空串/NA 时取默认值
rag_or <- function(x, default = "—") {
  v <- rag_cell(x)
  if (!nzchar(v)) default else v
}

# kg_cypher 包装：出错返回空表，不打断检索
rag_q <- function(query, params = list()) {
  g <- kg_store_get()
  out <- tryCatch(kg_cypher(g, query, params = params),
                  error = function(e) {
                    warning("Cypher 失败：", conditionMessage(e), call. = FALSE)
                    NULL
                  })
  if (is.null(out)) return(data.frame())
  as.data.frame(out, stringsAsFactors = FALSE)
}

rag_q_rows <- function(query, params = list()) {
  df <- rag_q(query, params)
  if (!nrow(df)) return(list())
  lapply(seq_len(nrow(df)), function(i) df[i, , drop = FALSE])
}
