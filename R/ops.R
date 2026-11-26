
# =============================================================================
# ops.R — 面向 R 用户的高层图操作 API
#
# 与 Cypher 的对应关系（完整表见 docs/操作对照表.md）：
#   MATCH (n:Herb {name:'地榆'})        kg_nodes(g, "Herb", name = "地榆")
#   MATCH (a)-[:R]->(b)                kg_expand(g, ids, "R")
#   CREATE (n:Herb {...})              kg_add_node(g, "Herb", list(...))
#   MERGE (n:Herb {id:...})            kg_merge_node(g, "Herb", list(id=...))
#   SET n.x = 1                        kg_set_node(g, id, list(x = 1))
#   DETACH DELETE n                    kg_drop_node(g, id)
#   CALL ...queryNodes(...)            kg_fulltext_search(g, ...)
#   整段 Cypher 脚本                     kg_cypher_file(g, "cypher/41_acceptance.cypher")
# =============================================================================

# ---------------------------------------------------------------------------
# 执行 .cypher 脚本（按 ; 分段，跳过注释）
# ---------------------------------------------------------------------------

#' 把 .cypher 文本拆成语句（正确跳过字符串与注释内的分号）
kg_split_statements <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  out <- character(0)
  buf <- character(0)
  i <- 1L
  quote <- ""
  in_line_comment <- FALSE
  in_block_comment <- FALSE
  while (i <= n) {
    ch <- chars[i]
    nxt <- if (i < n) chars[i + 1L] else ""
    if (in_line_comment) {
      if (ch == "\n") { in_line_comment <- FALSE; buf <- c(buf, "\n") }
      i <- i + 1L
      next
    }
    if (in_block_comment) {
      if (ch == "*" && nxt == "/") { in_block_comment <- FALSE; i <- i + 2L; next }
      i <- i + 1L
      next
    }
    if (nzchar(quote)) {
      buf <- c(buf, ch)
      if (ch == "\\" && i < n) { buf <- c(buf, nxt); i <- i + 2L; next }
      if (ch == quote) quote <- ""
      i <- i + 1L
      next
    }
    if (ch == "/" && nxt == "/") { in_line_comment <- TRUE; i <- i + 2L; next }
    if (ch == "/" && nxt == "*") { in_block_comment <- TRUE; i <- i + 2L; next }
    if (ch == "'" || ch == "\"") { quote <- ch; buf <- c(buf, ch); i <- i + 1L; next }
    if (ch == ";") {
      s <- trimws(paste(buf, collapse = ""))
      if (nzchar(s)) out <- c(out, s)
      buf <- character(0)
      i <- i + 1L
      next
    }
    buf <- c(buf, ch)
    i <- i + 1L
  }
  s <- trimws(paste(buf, collapse = ""))
  if (nzchar(s)) out <- c(out, s)
  out
}

#' 逐条执行 .cypher 文件（与 Neo4j Browser 中逐条执行等价）
#'
#' @return list(statements=, results=, errors=) —— results 为每条语句的 kg_result
kg_cypher_file <- function(g, path, params = list(), verbose = TRUE, stop_on_error = FALSE) {
  if (!file.exists(path)) kg_abort("文件不存在: ", path)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  stmts <- kg_split_statements(text)
  results <- vector("list", length(stmts))
  errors <- vector("list", length(stmts))
  n_ok <- 0L
  t0 <- proc.time()[["elapsed"]]
  for (i in seq_along(stmts)) {
    res <- tryCatch(kg_cypher(g, stmts[i], params = params),
                    error = function(e) e)
    if (inherits(res, "error")) {
      errors[[i]] <- conditionMessage(res)
      if (verbose) kg_msg(sprintf("  [%02d] ERROR %s", i, conditionMessage(res)))
      if (stop_on_error) break
    } else {
      results[[i]] <- res
      n_ok <- n_ok + 1L
      if (verbose) kg_msg(sprintf("  [%02d] %d 行  %.2fs  %s", i, nrow(res),
                                  attr(res, "elapsed") %||% 0,
                                  substr(gsub("\\s+", " ", stmts[i]), 1, 60)))
    }
  }
  if (verbose) kg_msg(sprintf("共 %d 条语句，成功 %d，耗时 %.2fs", length(stmts), n_ok,
                              proc.time()[["elapsed"]] - t0))
  invisible(list(statements = stmts, results = results,
                 errors = Filter(Negate(is.null), errors),
                 file = path, elapsed = proc.time()[["elapsed"]] - t0))
}

# ---------------------------------------------------------------------------
# 节点
# ---------------------------------------------------------------------------

#' 按标签/属性查节点（等价 MATCH (n:Label {...})）
kg_nodes <- function(g, label = NULL, ..., limit = Inf) {
  props <- list(...)
  ids <- kg_lookup_ids(g, label, NULL, NULL)
  if (length(props)) {
    for (p in names(props)) {
      if (!p %in% names(g$nodes)) { ids <- integer(0); break }
      col <- g$nodes[[p]][ids]
      v <- props[[p]]
      ids <- ids[!is.na(col) & col %in% v]
      if (!length(ids)) break
    }
  }
  if (is.finite(limit)) ids <- utils::head(ids, limit)
  ids
}

#' 取节点为 data.frame（属性展开）
kg_nodes_df <- function(g, label = NULL, ..., limit = Inf) {
  ids <- kg_nodes(g, label, ..., limit = limit)
  kg_nodes_to_df(g, ids, label = label)
}

kg_nodes_to_df <- function(g, ids, label = NULL) {
  if (!length(ids)) return(data.frame(id = character(0), labels = character(0)))
  cols <- c("id", "label", setdiff(names(g$nodes), c(KG_NODE_RESERVED, "id")))
  cols <- cols[cols %in% names(g$nodes)]
  df <- as.data.frame(g$nodes[ids, cols, with = FALSE], stringsAsFactors = FALSE)
  names(df)[names(df) == "label"] <- "primary_label"
  df
}

#' 按 id 取单个节点（等价 MATCH (n {id:$id})）
kg_node <- function(g, id) {
  idx <- kg_node_idx(g, id)
  if (is.na(idx)) return(NULL)
  kg_get_node(g, idx)
}

#' 按 id 判断是否存在
kg_has_node <- function(g, id) !is.na(kg_node_idx(g, id))

#' 新建节点（等价 CREATE (n:Label {...})）
kg_add_node <- function(g, label, props = list(), labels = NULL) {
  idx <- kg_insert_node(g, label, props, labels = labels)
  kg_node_value(idx)
}

#' 合并节点（等价 MERGE (n:Label {id:...}) ON CREATE SET / ON MATCH SET）
kg_merge_node <- function(g, label, props, on_create = list(), on_match = list()) {
  id <- props$id
  if (is.null(id)) kg_abort("MERGE 需要 props$id")
  idx <- kg_node_idx(g, id)
  if (!is.na(idx)) {
    for (p in names(on_match)) kg_set_node_prop(g, idx, p, on_match[[p]])
    return(list(node = kg_node_value(idx), created = FALSE))
  }
  for (p in names(on_create)) props[[p]] <- on_create[[p]]
  idx <- kg_insert_node(g, label, props)
  list(node = kg_node_value(idx), created = TRUE)
}

#' 设置属性（等价 SET n.prop = value）
kg_set_node <- function(g, id, props) {
  idx <- kg_node_idx(g, id)
  if (is.na(idx)) kg_abort("节点不存在: ", id)
  for (p in names(props)) kg_set_node_prop(g, idx, p, props[[p]])
  invisible(g)
}

#' 删除属性（等价 REMOVE n.prop）
kg_remove_node_prop_by_id <- function(g, id, prop) {
  idx <- kg_node_idx(g, id)
  if (is.na(idx)) kg_abort("节点不存在: ", id)
  kg_remove_node_prop(g, idx, prop)
  invisible(g)
}

#' 删除节点（等价 DETACH DELETE）
kg_drop_node <- function(g, id) {
  idx <- kg_node_idx(g, id)
  if (is.na(idx)) return(invisible(0L))
  res <- kg_delete_node(g, idx, detach = TRUE)
  invisible(res)
}

#' 节点属性（named list，等价 properties(n)）
kg_properties <- function(g, x) {
  if (is.character(x)) {
    idx <- kg_node_idx(g, x)
    if (is.na(idx)) return(NULL)
    return(kg_node_props(g, idx))
  }
  if (is_node_value(x)) return(kg_node_props(g, x$idx))
  if (is_rel_value(x)) return(kg_rel_props(g, x$idx))
  NULL
}

# ---------------------------------------------------------------------------
# 关系
# ---------------------------------------------------------------------------

#' 新建关系（等价 CREATE (a)-[:T]->(b)）
kg_add_rel <- function(g, from, to, type, props = list()) {
  a <- if (is.character(from)) kg_node_idx(g, from) else from$idx
  b <- if (is.character(to)) kg_node_idx(g, to) else to$idx
  if (is.na(a)) kg_abort("起点不存在: ", from)
  if (is.na(b)) kg_abort("终点不存在: ", to)
  kg_rel_value(kg_insert_rel(g, a, b, type, props))
}

#' 合并关系（等价 MERGE (a)-[r:T]->(b)）
kg_merge_rel <- function(g, from, to, type, props = list()) {
  a <- if (is.character(from)) kg_node_idx(g, from) else from$idx
  b <- if (is.character(to)) kg_node_idx(g, to) else to$idx
  if (is.na(a)) kg_abort("起点不存在: ", from)
  if (is.na(b)) kg_abort("终点不存在: ", to)
  edges <- kg_adj(g, a, "out", type)
  if (length(edges)) {
    hit <- edges[g$rels$dst[edges] == b]
    if (length(hit)) {
      for (p in names(props)) kg_set_rel_prop(g, hit[1], p, props[[p]])
      return(list(rel = kg_rel_value(hit[1]), created = FALSE))
    }
  }
  list(rel = kg_rel_value(kg_insert_rel(g, a, b, type, props)), created = TRUE)
}

#' 删除关系（按 id 或按 端点+类型）
kg_drop_rel <- function(g, rel) {
  idx <- if (is_rel_value(rel)) rel$idx else as.integer(rel)
  invisible(kg_delete_rel(g, idx))
}

#' 取关系（等价 MATCH ()-[r]->() WHERE id(r)=...）
kg_rel <- function(g, idx) kg_get_rel(g, as.integer(idx))

# ---------------------------------------------------------------------------
# 遍历
# ---------------------------------------------------------------------------

#' 1 跳扩展，返回邻居节点 idx
kg_expand <- function(g, ids, type = NULL, direction = "out") {
  ids <- as.integer(ids)
  out <- integer(0)
  for (i in ids) {
    edges <- kg_adj(g, i, direction, type)
    if (!length(edges)) next
    if (direction == "out") out <- c(out, g$rels$dst[edges])
    else if (direction == "in") out <- c(out, g$rels$src[edges])
    else out <- c(out, kg_rel_other(g, edges, i))
  }
  unique(out)
}

#' 邻居列表（data.frame：邻居 id/标签/属性 + 关系类型/属性）
kg_neighbors <- function(g, id, type = NULL, direction = "both", limit = Inf) {
  idx <- if (is.character(id)) kg_node_idx(g, id) else id$idx
  edges <- kg_adj(g, idx, direction, type)
  if (!length(edges)) {
    return(data.frame(id = character(0), label = character(0), rel_type = character(0),
                      rel_id = integer(0), stringsAsFactors = FALSE))
  }
  if (is.finite(limit)) edges <- utils::head(edges, limit)
  other <- kg_rel_other(g, edges, idx)
  data.frame(
    id = g$nodes$id[other],
    label = g$nodes$label[other],
    name = kg_node_display(g, other),
    rel_type = g$rels$type[edges],
    rel_id = edges,
    direction = ifelse(g$rels$src[edges] == idx, "out", "in"),
    stringsAsFactors = FALSE
  )
}

#' 多跳扩展（等价 MATCH (a)-[:T*1..k]->(b)）
kg_expand_n <- function(g, ids, type = NULL, depth = 2L, direction = "out") {
  frontier <- as.integer(ids)
  seen <- frontier
  for (d in seq_len(depth)) {
    frontier <- setdiff(kg_expand(g, frontier, type, direction), seen)
    if (!length(frontier)) break
    seen <- c(seen, frontier)
  }
  setdiff(seen, as.integer(ids))
}

#' 最短路径（等价 shortestPath）
#'
#' @param types 允许的关系类型（NULL = 全部）
kg_shortest_path <- function(g, from, to, types = NULL, type = NULL, direction = "both",
                             max_depth = 10L) {
  if (is.null(types)) types <- type
  a <- if (is.character(from)) kg_node_idx(g, from) else from$idx
  b <- if (is.character(to)) kg_node_idx(g, to) else to$idx
  if (is.na(a) || is.na(b)) kg_abort("端点不存在")
  tenv <- list2env(as.list(stats::setNames(TRUE, as.character(b))),
                   envir = new.env(parent = emptyenv(), hash = TRUE))
  res <- kg_bfs_shortest(g, a, tenv, direction, types, max_depth, all = FALSE)
  if (!length(res)) return(NULL)
  p <- res[[1]]
  list(nodes = g$nodes$id[p$nodes], rels = g$rels$type[p$rels],
       length = length(p$rels), node_idx = p$nodes, rel_idx = p$rels)
}

# ---------------------------------------------------------------------------
# 批量导入（等价 LOAD CSV）
# ---------------------------------------------------------------------------

#' 从 CSV 批量导入节点
#'
#' @param type_map 可选：命名向量/列表，显式指定列类型；NULL 时按取值自动推断
kg_import_csv_nodes <- function(g, path, label, id_col = "id", type_map = NULL,
                                verbose = TRUE) {
  dt <- kg_read_csv(path)
  if (!id_col %in% names(dt)) kg_abort("缺少列: ", id_col)
  types <- if (!is.null(type_map)) type_map else stats::setNames(
    vapply(dt, kg_infer_column_type, ""), names(dt))
  types[[id_col]] <- "character"
  for (p in names(dt)) {
    ty <- types[[p]] %||% "character"
    data.table::set(dt, j = p, value = kg_coerce(dt[[p]], ty))
  }
  dt[, label := label]
  # 可选的 extra_labels 列（分号分隔）—— 由 kg_export_csv 写出
  if ("extra_labels" %in% names(dt)) {
    dt[, extra_labels := lapply(strsplit(ifelse(is.na(extra_labels), "", extra_labels), ";",
                                         fixed = TRUE), function(x) x[nzchar(x)])]
  } else {
    dt[, extra_labels := rep(list(character(0)), .N)]
  }
  kg_insert_nodes_bulk(g, dt[, c("id", setdiff(names(dt), "id")), with = FALSE])
  if (verbose) kg_msg("导入节点 ", nrow(dt), " 个（", label, "）")
  invisible(g)
}

#' 从 CSV 批量导入关系
kg_import_csv_rels <- function(g, path, type, from_col = "start_id", to_col = "end_id",
                               verbose = TRUE) {
  dt <- kg_read_csv(path)
  src <- match(dt[[from_col]], g$nodes$id)
  dst <- match(dt[[to_col]], g$nodes$id)
  ok <- !is.na(src) & !is.na(dst)
  if (verbose) kg_msg("导入关系 ", sum(ok), " 条（", type, "），跳过 ", sum(!ok))
  prop_cols <- setdiff(names(dt), c(from_col, to_col))
  types <- stats::setNames(vapply(dt[, prop_cols, with = FALSE], kg_infer_column_type, ""),
                           prop_cols)
  ids <- integer(sum(ok))
  k <- 0L
  for (i in which(ok)) {
    k <- k + 1L
    props <- list()
    for (p in prop_cols) {
      v <- dt[[p]][i]
      if (is.na(v)) next
      props[[p]] <- kg_coerce(v, types[[p]])
    }
    ids[k] <- kg_insert_rel(g, src[i], dst[i], type, props)
  }
  invisible(ids[seq_len(k)])
}

# ---------------------------------------------------------------------------
# 便捷查询
# ---------------------------------------------------------------------------

#' 某个实体的"档案"：属性 + 各类关系计数（对标 40_verify 第 5 条）
kg_profile <- function(g, id) {
  idx <- kg_node_idx(g, id)
  if (is.na(idx)) return(NULL)
  edges <- kg_adj(g, idx, "both")
  rels <- data.frame(
    rel_type = g$rels$type[edges],
    direction = ifelse(g$rels$src[edges] == idx, "out", "in"),
    target = g$nodes$id[kg_rel_other(g, edges, idx)],
    target_label = g$nodes$label[kg_rel_other(g, edges, idx)],
    stringsAsFactors = FALSE
  )
  list(node = kg_get_node(g, idx),
       rel_count = nrow(rels),
       rels = rels)
}

#' 全文检索并返回节点信息（对标 CALL db.index.fulltext.queryNodes）
kg_search <- function(g, query, index = "chunk_text", limit = 10) {
  hits <- kg_fulltext_search(g, index, query, limit = limit)
  if (!nrow(hits)) return(data.frame())
  data.frame(node_id = g$nodes$id[hits$node],
             label = g$nodes$label[hits$node],
             text = substr(kg_node_text(g, hits$node), 1, 120),
             score = round(hits$score, 4),
             stringsAsFactors = FALSE)
}

#' 清空图（保留 schema）
kg_clear <- function(g) {
  kg_assert_writable(g)
  g$nodes <- g$nodes[0]
  g$rels <- g$rels[0]
  g$id_env <- new.env(parent = emptyenv(), hash = TRUE)
  g$label_ids <- list()
  g$rel_type_ids <- list()
  g$out_adj <- new.env(parent = emptyenv(), hash = TRUE)
  g$in_adj <- new.env(parent = emptyenv(), hash = TRUE)
  g$label_code <- integer(0)
  invisible(g)
}
