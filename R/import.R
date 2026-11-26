
# =============================================================================
# import.R — 从 Neo4j 导入用 CSV 构建 R 图存储
#
# 输入：03_graph/neo4j/{nodes,rels}/*.csv（UTF-8，带表头）
# 语义：完全复刻 10_load_nodes.cypher / 20_load_rels.cypher 的 MERGE 语义
#       * 以 id 去重合并（本数据集 id 全局唯一）
#       * 空串视为缺失（coalesce(nullif(row.x,''), n.x)）
#       * 关系两端必须存在且标签符合 (from)-[type]->(to)，否则该行跳过
#         （与 Neo4j MATCH 两端后 MERGE 的"静默跳过"一致，并计数上报）
# =============================================================================

kg_snake <- function(x) {
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x)
  tolower(x)
}

# 从 schema 描述里汇总属性类型（同一属性取首个声明，冲突则降级为 character）
kg_schema_prop_types <- function(blocks) {
  out <- character(0)
  for (b in blocks) {
    for (p in names(b$props)) {
      t <- b$props[[p]]
      if (!p %in% names(out)) out[[p]] <- t
      else if (!identical(unname(out[[p]]), unname(t))) {
        out[[p]] <- if (setequal(c(out[[p]], t), c("integer", "double"))) "double" else "character"
      }
    }
  }
  out
}

# CSV 读取：必须严格遵循 RFC4180（引号内的 "" 表示一个引号、引号内可含换行）。
# 实测 data.table::fread 在含制表符 + 双引号 + 跨行的大字段上会把 "" 当成两个引号，
# 导致 11 篇文档的 full_text 多出字符、A5 逐字比对失败；readr 与 base R 行为一致。
kg_read_csv <- function(path) {  if (requireNamespace("readr", quietly = TRUE)) {
    dt <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                          na = "", progress = FALSE, show_col_types = FALSE)
    data.table::setDT(dt)
    return(dt)
  }
  dt <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE,
                        colClasses = "character", na.strings = "", check.names = FALSE)
  data.table::setDT(dt)
  dt
}

# Neo4j 侧 35 个约束涉及的标签（34 类实体 + 元数据类 SchemaVersion）
KG_CONSTRAINT_LABELS <- c(KG_LABELS, "SchemaVersion")

#' 从 Neo4j CSV 目录导入
#'
#' @param g 目标图存储（为空则新建）
#' @param dir 03_graph/neo4j 目录
#' @param verbose 打印进度
#' @param build_fulltext 是否立即构建中文全文索引（约 10 秒）
#' @param schema 可选：任意 Neo4j CSV 数据集的 schema 描述，形如
#'   list(nodes = list(Label = list(file="nodes/x.csv", rows=NA,
#'                                  props=c(id="character", name="character"))),
#'        rels  = list(list(type="R", from="A", to="B", file="rels/r.csv",
#'                          rows=NA, props=c(w="double"))))
#'   为 NULL 时使用本项目的 TCM schema（并安装 35 约束 / 45 索引）。
kg_import_neo4j <- function(g = NULL, dir, verbose = TRUE, build_fulltext = TRUE,
                            validate_counts = TRUE, schema = NULL,
                            install_metadata = is.null(schema)) {
  node_schema <- if (is.null(schema)) KG_NODE_SCHEMA else schema$nodes
  rel_schema  <- if (is.null(schema)) KG_REL_SCHEMA else schema$rels
  node_types  <- kg_schema_prop_types(node_schema)
  rel_types   <- kg_schema_prop_types(rel_schema)
  if (!is.null(schema)) install_metadata <- isTRUE(install_metadata)
  if (is.null(g)) g <- kg_new_store(preload_schema = is.null(schema))
  kg_assert(inherits(g, "kg_store"), "不是 kg_store 对象")
  if (!dir.exists(dir)) kg_abort("目录不存在: ", dir)
  t_start <- proc.time()[["elapsed"]]

  # ------------------------------------------------------------------
  # 1. 节点
  # ------------------------------------------------------------------
  if (verbose) kg_msg("导入节点 ...")
  node_parts <- list()
  node_report <- list()
  for (lab in names(node_schema)) {
    spec <- node_schema[[lab]]
    path <- file.path(dir, spec$file)
    if (!file.exists(path)) kg_abort("缺少 CSV: ", path)
    dt <- kg_read_csv(path)
    if (validate_counts && !is.na(spec$rows) && nrow(dt) != spec$rows)
      kg_abort(sprintf("%s 行数不符: csv=%d, cypher 声明=%d", spec$file, nrow(dt), spec$rows))
    if (!"id" %in% names(dt)) kg_abort("缺少 id 列: ", path)
    cols <- intersect(c("id", names(spec$props)), names(dt))
    dt <- dt[, cols, with = FALSE]
    dt[, label := lab]
    dt[, extra_labels := rep(list(character(0)), .N)]
    node_parts[[lab]] <- dt
    node_report[[lab]] <- data.frame(label = lab, file = spec$file, expected = spec$rows,
                                     actual = nrow(dt), stringsAsFactors = FALSE)
    if (verbose) kg_msg(sprintf("  %-20s %6d", lab, nrow(dt)))
  }
  nodes <- data.table::rbindlist(node_parts, fill = TRUE, use.names = TRUE)
  rm(node_parts)
  n_nodes <- nrow(nodes)
  nodes[, idx := seq_len(.N)]
  nodes[, deleted := FALSE]

  # 类型统一
  for (p in setdiff(names(node_types), KG_NODE_RESERVED)) {
    type <- node_types[[p]]
    if (!p %in% names(nodes)) nodes[[p]] <- NA
    data.table::set(nodes, j = p, value = kg_coerce(nodes[[p]], type))
  }
  node_cols <- c("idx", "id", "label", "extra_labels", "deleted",
                 setdiff(names(node_types), c(KG_NODE_RESERVED, "id")))
  for (p in setdiff(node_cols, names(nodes))) nodes[[p]] <- NA
  data.table::setcolorder(nodes, node_cols)
  data.table::setkey(nodes, idx)

  g$nodes <- nodes
  g$rels <- g$rels[0]
  g$meta$node_count <- n_nodes
  g$meta$source <- normalizePath(dir, winslash = "/")
  kg_rebuild_derived(g, verbose = FALSE)

  # ------------------------------------------------------------------
  # 2. 关系
  # ------------------------------------------------------------------
  if (verbose) kg_msg("导入关系 ...")
  node_id <- nodes$id
  node_label <- nodes$label
  rel_parts <- list()
  rel_report <- list()
  skipped <- list()
  for (spec in rel_schema) {
    path <- file.path(dir, spec$file)
    if (!file.exists(path)) kg_abort("缺少 CSV: ", path)
    dt <- kg_read_csv(path)
    if (validate_counts && !is.na(spec$rows) && nrow(dt) != spec$rows)
      kg_abort(sprintf("%s 行数不符: csv=%d, cypher 声明=%d", spec$file, nrow(dt), spec$rows))
    if (!all(c("start_id", "end_id") %in% names(dt)))
      kg_abort("缺少 start_id/end_id: ", path)
    src <- match(as.character(dt$start_id), node_id)
    dst <- match(as.character(dt$end_id), node_id)
    bad_endpoint <- is.na(src) | is.na(dst)
    bad_label <- rep(FALSE, nrow(dt))
    if (any(!bad_endpoint)) {
      ok <- !bad_endpoint
      bad_label[ok] <- node_label[src[ok]] != spec$from | node_label[dst[ok]] != spec$to
    }
    drop <- bad_endpoint | bad_label
    if (any(drop)) {
      skipped[[length(skipped) + 1]] <- data.frame(
        file = spec$file, type = spec$type, rows = sum(drop),
        missing_endpoint = sum(bad_endpoint), wrong_label = sum(bad_label),
        stringsAsFactors = FALSE)
    }
    keep <- which(!drop)
    out <- data.table::data.table(src = as.integer(src[keep]),
                                  dst = as.integer(dst[keep]),
                                  type = spec$type, deleted = FALSE)
    for (p in names(spec$props)) {
      if (!p %in% names(dt)) { out[[p]] <- NA; next }
      data.table::set(out, j = p, value = kg_coerce(dt[[p]][keep], rel_types[[p]] %||% "character"))
    }
    rel_parts[[length(rel_parts) + 1]] <- out
    rel_report[[length(rel_report) + 1]] <- data.frame(
      type = spec$type, from = spec$from, to = spec$to, file = spec$file,
      expected = spec$rows, actual = length(keep), stringsAsFactors = FALSE)
  }
  rels <- data.table::rbindlist(rel_parts, fill = TRUE, use.names = TRUE)
  rm(rel_parts)
  rels[, idx := seq_len(.N)]
  for (p in names(rel_types)) {
    if (!p %in% names(rels)) rels[[p]] <- NA
    data.table::set(rels, j = p, value = kg_coerce(rels[[p]], rel_types[[p]]))
  }
  rel_cols <- c("idx", "src", "dst", "type", "deleted", names(rel_types))
  data.table::setcolorder(rels, rel_cols)
  data.table::setkey(rels, idx)
  g$rels <- rels
  g$meta$rel_count <- nrow(rels)
  kg_rebuild_derived(g, verbose = FALSE)

  # ------------------------------------------------------------------
  # 3. Neo4j 元数据：约束 + 索引
  # ------------------------------------------------------------------
  if (isTRUE(install_metadata)) kg_install_neo4j_metadata(g, verbose = verbose)

  # ------------------------------------------------------------------
  # 4. 全文索引
  # ------------------------------------------------------------------
  if (isTRUE(build_fulltext)) kg_index_build_all(g, verbose = verbose)

  g$import_report <- list(
    nodes = data.table::rbindlist(lapply(node_report, data.table::as.data.table)),
    rels = data.table::rbindlist(lapply(rel_report, data.table::as.data.table)),
    skipped = if (length(skipped)) data.table::rbindlist(lapply(skipped, data.table::as.data.table)) else NULL,
    elapsed = proc.time()[["elapsed"]] - t_start
  )
  if (verbose) {
    kg_msg(sprintf("导入完成：%d 节点 / %d 关系 / %.1fs",
                   nrow(g$nodes), nrow(g$rels), g$import_report$elapsed))
    if (!is.null(g$import_report$skipped)) {
      kg_msg("注意：有 ", sum(g$import_report$skipped$rows), " 行关系因端点缺失/标签不符被跳过")
    }
  }
  invisible(g)
}

#' 安装与 00_constraints.cypher / 30_indexes.cypher 等价的约束与索引
kg_install_neo4j_metadata <- function(g, verbose = TRUE) {
  # 35 个唯一约束
  for (lab in KG_CONSTRAINT_LABELS) {
    nm <- paste0(kg_snake(lab), "_id")
    kg_constraint_create(g, nm, "UNIQUE", lab, "id", if_not_exists = TRUE)
  }
  # 35 个标签查找索引（Neo4j: LOOKUP）
  for (lab in KG_CONSTRAINT_LABELS) {
    nm <- paste0(kg_snake(lab), "_label_lookup")
    kg_index_create(g, nm, lab, character(0), "LOOKUP", if_not_exists = TRUE)
  }
  # 4 个常规索引（与 30_indexes.cypher 同名）
  kg_index_create(g, "textunit_doc", "TextUnit", "doc_id", "RANGE", if_not_exists = TRUE)
  kg_index_create(g, "textunit_span", "TextUnit", "char_start", "RANGE", if_not_exists = TRUE)
  kg_index_create(g, "herb_category", "Herb", "category_raw", "RANGE", if_not_exists = TRUE)
  kg_index_create(g, "formula_source", "Formula", "source_book", "RANGE", if_not_exists = TRUE)
  # 6 个中文全文索引（cjk 分析器）
  ft <- list(
    chunk_text      = c("TextUnit", "text"),
    herb_name       = c("Herb", "name"),
    formula_name    = c("Formula", "name"),
    compound_name   = c("Compound", "name"),
    indication_name = c("Indication", "name"),
    book_name       = c("Book", "name")
  )
  for (nm in names(ft)) {
    kg_index_create(g, nm, ft[[nm]][1], ft[[nm]][2], "FULLTEXT",
                    options = list(`fulltext.analyzer` = "cjk"), if_not_exists = TRUE)
  }
  if (verbose) {
    kg_msg("安装约束 ", nrow(g$constraints), " 个 / 索引 ", nrow(g$indexes),
           " 个（全文 ", sum(g$indexes$type == "FULLTEXT"),
           " / 常规 ", sum(g$indexes$type == "RANGE"),
           " / 查找 ", sum(g$indexes$type == "LOOKUP"), "）")
  }
  invisible(g)
}

#' 导入对账报告（逐标签/逐关系类型）
kg_import_report <- function(g, write = NULL) {
  rep <- g$import_report
  if (is.null(rep)) kg_abort("该图存储没有导入报告（不是通过 kg_import_neo4j 构建的）")
  nodes <- rep$nodes
  nodes$status <- ifelse(nodes$actual == nodes$expected, "PASS", "FAIL")
  rels <- rep$rels
  rels$status <- ifelse(rels$actual == rels$expected, "PASS", "FAIL")
  if (!is.null(write)) {
    dir.create(dirname(write), recursive = TRUE, showWarnings = FALSE)
    kg_write_csv(nodes, file.path(dirname(write), "import_nodes.csv"), row.names = FALSE)
    kg_write_csv(rels, file.path(dirname(write), "import_rels.csv"), row.names = FALSE)
    if (!is.null(rep$skipped))
      kg_write_csv(rep$skipped, file.path(dirname(write), "import_skipped.csv"), row.names = FALSE)
  }
  list(nodes = nodes, rels = rels, skipped = rep$skipped, elapsed = rep$elapsed)
}

#' 只加载节点/关系表（从已有 store 重建派生索引）
kg_reload_from_tables <- function(g, verbose = TRUE) {
  kg_rebuild_derived(g, verbose = verbose)
  kg_index_build_all(g, verbose = verbose)
  invisible(g)
}
