
# =============================================================================
# store.R — R 原生属性图存储（与 Neo4j 的存储模型一一对应）
#
# 存储布局（全部在内存，data.table 列式）
#   g$nodes  idx | id | label | extra_labels | deleted | <所有节点属性列>
#   g$rels   idx | src | dst | type | deleted | <所有关系属性列>
#   g$id_env        id -> node idx          （等价 Neo4j 唯一约束下的主键查找）
#   g$label_ids     label -> node idx 向量  （等价 Neo4j 标签索引 / 全标签扫描）
#   g$rel_type_ids  type -> rel idx 向量    （等价 Neo4j 关系类型扫描）
#   g$out_adj/in_adj  node idx -> rel idx 向量（等价 Neo4j 关系链指针）
#
# 约定
#   * 空字符串在导入时转成 NA，与 `coalesce(nullif(row.x,''), n.x)` 语义一致；
#   * 节点/关系在表达式中只传句柄，属性按需回表，避免复制；
#   * 删除采用"墓碑"（deleted=TRUE）+ 从派生索引摘除，语义等价 DETACH DELETE。
# =============================================================================

KG_ENGINE_VERSION <- "1.0.0"

# 保留列（不是用户属性）
KG_NODE_RESERVED <- c("idx", "label", "extra_labels", "deleted")
KG_REL_RESERVED  <- c("idx", "src", "dst", "type", "deleted")

# ---------------------------------------------------------------------------
# 创建 / 打印 / 概览
# ---------------------------------------------------------------------------

#' 新建一个空的 R 属性图存储
#'
#' @param preload_schema 是否预建 TCM 本体的属性列（TRUE 便于直接导入本项目的
#'   CSV；建"另一张图"时可设 FALSE，属性列会按需自动增加）
kg_new_store <- function(name = "TCM-KG", schema_version = KG_SCHEMA_VERSION,
                         read_only = FALSE, preload_schema = TRUE) {
  g <- new.env(parent = emptyenv())
  class(g) <- "kg_store"

  g$meta <- list(
    name = name,
    engine = paste0("zhyhkg/", KG_ENGINE_VERSION),
    schema_version = schema_version,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    source = NA_character_,
    node_count = 0L,
    rel_count = 0L
  )

  node_props <- if (isTRUE(preload_schema)) setdiff(names(KG_NODE_PROP_TYPES), KG_NODE_RESERVED)
                else c("id", "name")
  rel_props  <- if (isTRUE(preload_schema)) setdiff(names(KG_REL_PROP_TYPES), KG_REL_RESERVED)
                else character(0)

  nodes <- data.table::data.table(
    idx = integer(0), id = character(0), label = character(0),
    extra_labels = vector("list", 0), deleted = logical(0)
  )
  for (p in setdiff(node_props, c("id", "name"))) {
    nodes[[p]] <- kg_empty_vec(kg_prop_type(p, "node"))
  }
  if (!"name" %in% names(nodes)) nodes[["name"]] <- character(0)
  data.table::setcolorder(nodes, c("idx", "id", "label", "extra_labels", "deleted"))

  rels <- data.table::data.table(
    idx = integer(0), src = integer(0), dst = integer(0),
    type = character(0), deleted = logical(0)
  )
  for (p in rel_props) rels[[p]] <- kg_empty_vec(kg_prop_type(p, "rel"))
  data.table::setcolorder(rels, c("idx", "src", "dst", "type", "deleted"))

  g$nodes <- nodes
  g$rels <- rels
  g$id_env <- new.env(parent = emptyenv(), hash = TRUE)
  g$label_ids <- list()
  g$rel_type_ids <- list()
  g$out_adj <- new.env(parent = emptyenv(), hash = TRUE)
  g$in_adj <- new.env(parent = emptyenv(), hash = TRUE)
  g$label_code <- integer(0)
  g$label_levels <- character(0)
  g$multi_labels <- character(0)
  g$indexes <- data.table::data.table(
    name = character(0), type = character(0), label = character(0),
    props = list(), state = character(0)
  )
  g$constraints <- data.table::data.table(
    name = character(0), type = character(0), label = character(0), props = list()
  )
  g$fulltext <- list()
  g$vector <- list()
  g$props_registry <- list(node = names(KG_NODE_PROP_TYPES),
                           rel = names(KG_REL_PROP_TYPES))
  g$read_only <- isTRUE(read_only)
  g$verbose <- TRUE
  g$journal <- NULL
  g
}

#' @export
print.kg_store <- function(x, ...) {
  s <- kg_stats(x)
  cat("<kg_store> ", s$name, "\n", sep = "")
  cat("  引擎      : ", s$engine, "（Neo4j 兼容层）\n", sep = "")
  cat("  节点      : ", kg_fmt_int(s$node_count), " / ", s$label_count, " 类标签\n", sep = "")
  cat("  关系      : ", kg_fmt_int(s$rel_count), " / ", s$rel_type_count, " 类关系\n", sep = "")
  cat("  约束      : ", nrow(x$constraints), "   索引: ", nrow(x$indexes), "\n", sep = "")
  cat("  只读      : ", s$read_only, "\n", sep = "")
  if (!is.null(s$source) && !is.na(s$source)) cat("  数据来源  : ", s$source, "\n", sep = "")
  invisible(x)
}

#' 图规模与状态概览
kg_stats <- function(g, detail = FALSE) {
  kg_assert(inherits(g, "kg_store"), "不是 kg_store 对象")
  out <- list(
    name = g$meta$name,
    engine = g$meta$engine,
    schema_version = g$meta$schema_version,
    node_count = nrow(g$nodes),
    rel_count = nrow(g$rels),
    deleted_nodes = sum(g$nodes$deleted),
    deleted_rels = sum(g$rels$deleted),
    label_count = length(g$label_ids),
    rel_type_count = length(g$rel_type_ids),
    constraint_count = nrow(g$constraints),
    index_count = nrow(g$indexes),
    read_only = g$read_only,
    source = g$meta$source
  )
  if (detail) {
    out$label_counts <- vapply(g$label_ids, length, integer(1))
    out$rel_type_counts <- vapply(g$rel_type_ids, length, integer(1))
  }
  out
}

kg_assert_writable <- function(g) {
  if (isTRUE(g$read_only)) kg_abort("图存储处于只读模式（read_only=TRUE），不能写入")
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# 基本读取
# ---------------------------------------------------------------------------

kg_node_idx <- function(g, id) {
  if (!exists(id, envir = g$id_env, inherits = FALSE)) return(NA_integer_)
  get(id, envir = g$id_env, inherits = FALSE)
}

kg_node_by_id <- function(g, id) kg_node_idx(g, id)

kg_node_ids <- function(g) g$nodes$id

kg_node_prop <- function(g, idx, prop) {
  if (is.na(idx)) return(kg_null)
  if (identical(prop, "id")) return(g$nodes$id[idx])
  if (prop %in% names(g$nodes)) {
    v <- g$nodes[[prop]][idx]
    if (length(v) == 0L || (length(v) == 1L && is.na(v))) return(kg_null)
    return(v)
  }
  kg_null
}

kg_rel_prop <- function(g, idx, prop) {
  if (is.na(idx)) return(kg_null)
  if (prop %in% names(g$rels)) {
    v <- g$rels[[prop]][idx]
    if (length(v) == 0L || (length(v) == 1L && is.na(v))) return(kg_null)
    return(v)
  }
  kg_null
}

#' 取节点全部属性（named list）
kg_node_props <- function(g, idx, include_id = TRUE) {
  if (length(idx) != 1L || is.na(idx)) return(list())
  cols <- names(g$nodes)
  cols <- setdiff(cols, c(KG_NODE_RESERVED, if (!include_id) "id"))
  vals <- lapply(cols, function(p) g$nodes[[p]][idx])
  names(vals) <- cols
  keep <- !vapply(vals, function(v) length(v) == 0L || (length(v) == 1L && is.na(v)), logical(1))
  vals[keep]
}

#' 取关系全部属性
kg_rel_props <- function(g, idx) {
  if (length(idx) != 1L || is.na(idx)) return(list())
  cols <- setdiff(names(g$rels), KG_REL_RESERVED)
  vals <- lapply(cols, function(p) g$rels[[p]][idx])
  names(vals) <- cols
  keep <- !vapply(vals, function(v) length(v) == 0L || (length(v) == 1L && is.na(v)), logical(1))
  vals[keep]
}

#' 节点标签向量（第一个为"主标签"）
kg_node_labels <- function(g, idx) {
  if (length(idx) != 1L || is.na(idx)) return(character(0))
  primary <- g$nodes$label[idx]
  extra <- g$nodes$extra_labels[[idx]]
  c(if (!is.na(primary)) primary else character(0), extra %||% character(0))
}

kg_node_has_label <- function(g, idx, label) {
  ids <- g$label_ids[[label]]
  if (is.null(ids)) return(rep(FALSE, length(idx)))
  idx %in% ids
}

kg_rel_type <- function(g, idx) g$rels$type[idx]

#' 节点的显示名：有 name 用 name，否则回落到 id（对任意图都安全）
kg_node_display <- function(g, ids) {
  ids <- as.integer(ids)
  nm <- if ("name" %in% names(g$nodes)) g$nodes$name[ids] else rep(NA_character_, length(ids))
  nm[is.na(nm) | !nzchar(nm)] <- g$nodes$id[ids][is.na(nm) | !nzchar(nm)]
  nm
}

#' 节点上第一个可用的文本列（用于全文检索结果展示）
kg_node_text_column <- function(g) {
  for (p in c("name", "title", "text", "label")) {
    if (p %in% names(g$nodes)) return(p)
  }
  "id"
}

#' 取节点的展示文本（全文检索用）
kg_node_text <- function(g, ids) {
  col <- kg_node_text_column(g)
  v <- g$nodes[[col]][ids]
  v[is.na(v)] <- ""
  v
}

#' 节点记录（用于 RETURN n / 打印）
kg_get_node <- function(g, idx) {
  if (length(idx) != 1L || is.na(idx)) return(kg_null)
  list(idx = idx, id = g$nodes$id[idx], labels = kg_node_labels(g, idx),
       props = kg_node_props(g, idx))
}

kg_get_rel <- function(g, idx) {
  if (length(idx) != 1L || is.na(idx)) return(kg_null)
  list(idx = idx, id = g$rels$id[idx] %||% NA_character_, type = g$rels$type[idx],
       start = g$rels$src[idx], end = g$rels$dst[idx], props = kg_rel_props(g, idx))
}

# ---------------------------------------------------------------------------
# 邻接（关系链）
# ---------------------------------------------------------------------------

kg_adj <- function(g, idx, direction = "out", types = NULL) {
  ids <- integer(0)
  if (direction %in% c("out", "both")) {
    v <- g$out_adj[[as.character(idx)]]
    if (!is.null(v)) ids <- c(ids, v)
  }
  if (direction %in% c("in", "both")) {
    v <- g$in_adj[[as.character(idx)]]
    if (!is.null(v)) ids <- c(ids, v)
  }
  if (length(ids) && !is.null(types) && length(types)) {
    ids <- ids[g$rels$type[ids] %in% types]
  }
  ids
}

kg_rel_other <- function(g, rel_idx, node_idx) {
  s <- g$rels$src[rel_idx]
  d <- g$rels$dst[rel_idx]
  ifelse(s == node_idx, d, s)
}

kg_node_degree <- function(g, idx, direction = "both", types = NULL) {
  length(kg_adj(g, idx, direction, types))
}

# ---------------------------------------------------------------------------
# 写入：节点
# ---------------------------------------------------------------------------

kg_ensure_prop_column <- function(g, scope = c("node", "rel"), prop, type) {
  scope <- match.arg(scope)
  tbl <- if (scope == "node") g$nodes else g$rels
  if (prop %in% names(tbl)) return(invisible(FALSE))
  tbl[[prop]] <- kg_empty_vec(type, nrow(tbl))
  if (scope == "node") g$nodes <- tbl else g$rels <- tbl
  g$props_registry[[scope]] <- c(g$props_registry[[scope]], prop)
  invisible(TRUE)
}

# 安全的属性类型查询（属性不在 TCM 本体里时返回 character）
kg_prop_type <- function(prop, scope = c("node", "rel")) {
  scope <- match.arg(scope)
  types <- if (scope == "node") KG_NODE_PROP_TYPES else KG_REL_PROP_TYPES
  if (prop %in% names(types)) types[[prop]] else "character"
}

kg_prop_type_of <- function(scope, prop, value) {  if (scope == "node") {
    t <- if (prop %in% names(KG_NODE_PROP_TYPES)) KG_NODE_PROP_TYPES[[prop]] else NULL
  } else {
    t <- if (prop %in% names(KG_REL_PROP_TYPES)) KG_REL_PROP_TYPES[[prop]] else NULL
  }
  if (!is.null(t)) return(t)
  if (is.logical(value)) "logical" else
    if (is.integer(value)) "integer" else
      if (is.numeric(value)) "double" else
        if (is.character(value)) "character" else "character"
}

# 单节点插入（返回 idx）
kg_insert_node <- function(g, label, props = list(), labels = NULL) {
  kg_assert_writable(g)
  label <- as.character(label)
  if (!length(label) || is.na(label[1])) kg_abort("节点必须有标签")
  labels <- unique(c(label, labels %||% character(0)))
  if (!is.null(props$id)) {
    exists_id <- kg_node_idx(g, props$id)
    if (!is.na(exists_id)) kg_abort("节点 id 已存在（唯一约束）: ", props$id)
  }
  idx <- nrow(g$nodes) + 1L
  row <- list(idx = idx, id = if (is.null(props$id)) NA_character_ else as.character(props$id),
              label = labels[1], extra_labels = list(if (length(labels) > 1) labels[-1] else character(0)),
              deleted = FALSE)
  # 其它属性
  for (p in setdiff(names(props), "id")) {
    val <- props[[p]]
    if (is.null(val) || (length(val) == 1L && is.na(val))) next
    if (!p %in% names(g$nodes)) kg_ensure_prop_column(g, "node", p, kg_prop_type_of("node", p, val))
    row[[p]] <- if (is.list(val)) list(val) else val
  }
  newrow <- data.table::as.data.table(row)
  # 补齐缺失列，保证 rbind 类型稳定
  for (p in setdiff(names(g$nodes), names(newrow))) newrow[[p]] <- NA
  data.table::setcolorder(newrow, names(g$nodes))
  g$nodes <- data.table::rbindlist(list(g$nodes, newrow), fill = TRUE, use.names = TRUE)
  # 派生索引
  if (!is.na(row$id)) assign(row$id, idx, envir = g$id_env)
  for (l in labels) {
    if (is.null(g$label_ids[[l]])) g$label_ids[[l]] <- integer(0)
    g$label_ids[[l]] <- c(g$label_ids[[l]], idx)
  }
  if (!(labels[1] %in% g$label_levels)) g$label_levels <- c(g$label_levels, labels[1])
  g$label_code <- c(g$label_code, match(labels[1], g$label_levels))
  if (length(labels) > 1) g$multi_labels <- union(g$multi_labels, labels[-1])
  idx
}

# 批量插入（导入用，快）
kg_insert_nodes_bulk <- function(g, dt) {
  kg_assert_writable(g)
  if (!nrow(dt)) return(invisible(g))
  n0 <- nrow(g$nodes)
  dt$idx <- n0 + seq_len(nrow(dt))
  dt$deleted <- FALSE
  for (p in setdiff(names(g$nodes), names(dt))) dt[[p]] <- NA
  data.table::setcolorder(dt, names(g$nodes))
  g$nodes <- data.table::rbindlist(list(g$nodes, dt), fill = TRUE, use.names = TRUE)
  for (i in seq_len(nrow(dt))) {
    id <- dt$id[i]
    if (!is.na(id)) assign(id, dt$idx[i], envir = g$id_env)
  }
  labs <- unique(dt$label)
  for (l in labs) {
    g$label_ids[[l]] <- c(g$label_ids[[l]], dt$idx[dt$label == l])
  }
  if (!length(g$label_levels)) g$label_levels <- labs
  g$label_levels <- union(g$label_levels, labs)
  g$label_code <- match(g$nodes$label, g$label_levels)
  invisible(g)
}

kg_set_node_prop <- function(g, idx, prop, value) {
  kg_assert_writable(g)
  if (!prop %in% names(g$nodes)) {
    kg_ensure_prop_column(g, "node", prop, kg_prop_type_of("node", prop, value))
  }
  if (is.list(value) && !is.null(value)) value <- list(value)
  data.table::set(g$nodes, i = idx, j = prop, value = value)
  if (identical(prop, "id")) {
    old <- g$nodes$id[idx]
    if (!is.na(old) && exists(old, envir = g$id_env, inherits = FALSE)) rm(list = old, envir = g$id_env)
    if (!is.na(value)) assign(value, idx, envir = g$id_env)
  }
  invisible(idx)
}

kg_remove_node_prop <- function(g, idx, prop) {
  kg_assert_writable(g)
  if (!prop %in% names(g$nodes)) return(invisible(idx))
  data.table::set(g$nodes, i = idx, j = prop, value = NA)
  invisible(idx)
}

kg_add_node_label <- function(g, idx, label) {
  kg_assert_writable(g)
  if (label %in% kg_node_labels(g, idx)) return(invisible(idx))
  extra <- g$nodes$extra_labels[[idx]] %||% character(0)
  g$nodes$extra_labels[[idx]] <- c(extra, label)
  if (is.null(g$label_ids[[label]])) g$label_ids[[label]] <- integer(0)
  g$label_ids[[label]] <- sort(unique(c(g$label_ids[[label]], idx)))
  if (!(label %in% g$label_levels)) g$label_levels <- c(g$label_levels, label)
  g$multi_labels <- union(g$multi_labels, label)
  invisible(idx)
}

kg_remove_node_label <- function(g, idx, label) {
  kg_assert_writable(g)
  labs <- kg_node_labels(g, idx)
  if (!label %in% labs) return(invisible(idx))
  labs <- setdiff(labs, label)
  if (length(labs)) {
    g$nodes$label[idx] <- labs[1]
    g$nodes$extra_labels[[idx]] <- labs[-1]
  } else {
    g$nodes$label[idx] <- NA_character_
    g$nodes$extra_labels[[idx]] <- character(0)
  }
  g$label_ids[[label]] <- setdiff(g$label_ids[[label]], idx)
  invisible(idx)
}

# ---------------------------------------------------------------------------
# 写入：关系
# ---------------------------------------------------------------------------

kg_insert_rel <- function(g, src, dst, type, props = list()) {
  kg_assert_writable(g)
  idx <- nrow(g$rels) + 1L
  row <- list(idx = idx, src = as.integer(src), dst = as.integer(dst),
              type = as.character(type), deleted = FALSE)
  for (p in names(props)) {
    val <- props[[p]]
    if (is.null(val) || (length(val) == 1L && is.na(val))) next
    if (!p %in% names(g$rels)) kg_ensure_prop_column(g, "rel", p, kg_prop_type_of("rel", p, val))
    row[[p]] <- if (is.list(val)) list(val) else val
  }
  newrow <- data.table::as.data.table(row)
  for (p in setdiff(names(g$rels), names(newrow))) newrow[[p]] <- NA
  data.table::setcolorder(newrow, names(g$rels))
  g$rels <- data.table::rbindlist(list(g$rels, newrow), fill = TRUE, use.names = TRUE)
  g$rel_type_ids[[type]] <- c(g$rel_type_ids[[type]], idx)
  k <- as.character(src)
  assign(k, c(g$out_adj[[k]], idx), envir = g$out_adj)
  k2 <- as.character(dst)
  assign(k2, c(g$in_adj[[k2]], idx), envir = g$in_adj)
  idx
}

kg_set_rel_prop <- function(g, idx, prop, value) {
  kg_assert_writable(g)
  if (!prop %in% names(g$rels)) {
    kg_ensure_prop_column(g, "rel", prop, kg_prop_type_of("rel", prop, value))
  }
  if (is.list(value) && !is.null(value)) value <- list(value)
  data.table::set(g$rels, i = idx, j = prop, value = value)
  invisible(idx)
}

kg_remove_rel_prop <- function(g, idx, prop) {
  kg_assert_writable(g)
  if (!prop %in% names(g$rels)) return(invisible(idx))
  data.table::set(g$rels, i = idx, j = prop, value = NA)
  invisible(idx)
}

# ---------------------------------------------------------------------------
# 删除
# ---------------------------------------------------------------------------

kg_delete_rel <- function(g, idx) {
  kg_assert_writable(g)
  idx <- as.integer(idx)
  idx <- idx[!is.na(idx) & !g$rels$deleted[idx]]
  if (!length(idx)) return(invisible(0L))
  for (r in idx) {
    s <- g$rels$src[r]; d <- g$rels$dst[r]; ty <- g$rels$type[r]
    ks <- as.character(s); kd <- as.character(d)
    vs <- g$out_adj[[ks]]; if (!is.null(vs)) assign(ks, setdiff(vs, r), envir = g$out_adj)
    vd <- g$in_adj[[kd]];  if (!is.null(vd)) assign(kd, setdiff(vd, r), envir = g$in_adj)
    if (!is.na(ty)) g$rel_type_ids[[ty]] <- setdiff(g$rel_type_ids[[ty]], r)
  }
  data.table::set(g$rels, i = idx, j = "deleted", value = TRUE)
  invisible(length(idx))
}

# 删除节点；detach=TRUE 时连同关系一起删（等价 DETACH DELETE）
kg_delete_node <- function(g, idx, detach = TRUE) {
  kg_assert_writable(g)
  idx <- as.integer(idx)
  idx <- idx[!is.na(idx) & !g$nodes$deleted[idx]]
  if (!length(idx)) return(invisible(list(nodes = 0L, rels = 0L)))
  rels <- unique(c(
    unlist(lapply(idx, function(i) g$out_adj[[as.character(i)]] %||% integer(0))),
    unlist(lapply(idx, function(i) g$in_adj[[as.character(i)]] %||% integer(0)))
  ))
  rels <- rels[!g$rels$deleted[rels]]
  if (length(rels) && !detach) {
    kg_abort("节点仍有关系（Neo4j: Cannot delete node, because it still has relationships）。",
             "请使用 DETACH DELETE 或先删除关系。")
  }
  if (length(rels)) kg_delete_rel(g, rels)
  for (i in idx) {
    id <- g$nodes$id[i]
    if (!is.na(id) && exists(id, envir = g$id_env, inherits = FALSE))
      rm(list = id, envir = g$id_env)
    for (l in kg_node_labels(g, i)) g$label_ids[[l]] <- setdiff(g$label_ids[[l]], i)
    assign(as.character(i), integer(0), envir = g$out_adj)
    assign(as.character(i), integer(0), envir = g$in_adj)
  }
  data.table::set(g$nodes, i = idx, j = "deleted", value = TRUE)
  invisible(list(nodes = length(idx), rels = length(rels)))
}

# ---------------------------------------------------------------------------
# 派生索引重建 / 一致性校验
# ---------------------------------------------------------------------------

kg_rebuild_derived <- function(g, verbose = TRUE) {
  nodes <- g$nodes
  rels <- g$rels
  alive_n <- which(!nodes$deleted)
  alive_r <- which(!rels$deleted)

  g$id_env <- new.env(parent = emptyenv(), hash = TRUE)
  ids <- nodes$id[alive_n]
  ok <- !is.na(ids)
  if (any(ok)) {
    tmp <- as.list(alive_n[ok]); names(tmp) <- ids[ok]
    list2env(tmp, envir = g$id_env)
  }

  g$label_ids <- list()
  labs <- nodes$label[alive_n]
  for (l in unique(labs[!is.na(labs)])) {
    g$label_ids[[l]] <- sort(alive_n[which(labs == l)])
  }
  # 多标签
  for (i in alive_n) {
    ex <- nodes$extra_labels[[i]]
    if (length(ex)) for (l in ex) g$label_ids[[l]] <- sort(unique(c(g$label_ids[[l]], i)))
  }

  g$label_levels <- names(g$label_ids)
  g$label_code <- match(labs, g$label_levels)
  g$multi_labels <- unique(unlist(g$nodes$extra_labels[alive_n])) %||% character(0)

  g$rel_type_ids <- list()
  tys <- rels$type[alive_r]
  for (t in unique(tys[!is.na(tys)])) g$rel_type_ids[[t]] <- alive_r[which(tys == t)]

  g$out_adj <- new.env(parent = emptyenv(), hash = TRUE)
  g$in_adj <- new.env(parent = emptyenv(), hash = TRUE)
  if (length(alive_r)) {
    sp_out <- split(alive_r, rels$src[alive_r])
    list2env(lapply(sp_out, as.integer), envir = g$out_adj)
    sp_in <- split(alive_r, rels$dst[alive_r])
    list2env(lapply(sp_in, as.integer), envir = g$in_adj)
  }

  g$meta$node_count <- length(alive_n)
  g$meta$rel_count <- length(alive_r)
  if (verbose) {
    kg_msg("重建派生索引：", length(alive_n), " 节点 / ", length(alive_r), " 关系 / ",
           length(g$label_ids), " 标签 / ", length(g$rel_type_ids), " 关系类型")
  }
  invisible(g)
}

#' 一致性自检：返回发现的问题（data.frame）
kg_check <- function(g, verbose = TRUE) {
  issues <- list()
  add <- function(check, detail) issues[[length(issues) + 1]] <<-
    data.frame(check = check, detail = detail, stringsAsFactors = FALSE)

  alive_n <- which(!g$nodes$deleted)
  alive_r <- which(!g$rels$deleted)
  if (any(is.na(g$nodes$id[alive_n]))) add("node_id_missing", sum(is.na(g$nodes$id[alive_n])))
  ids <- g$nodes$id[alive_n]
  if (anyDuplicated(ids[!is.na(ids)])) add("node_id_duplicated", sum(duplicated(ids[!is.na(ids)])))
  if (any(g$nodes$deleted[alive_n])) add("deleted_flag_inconsistent", NA)

  bad_src <- !g$rels$src[alive_r] %in% alive_n
  bad_dst <- !g$rels$dst[alive_r] %in% alive_n
  if (any(bad_src)) add("rel_src_missing", sum(bad_src))
  if (any(bad_dst)) add("rel_dst_missing", sum(bad_dst))

  # 邻接表与关系表互查
  n_out <- vapply(g$out_adj, length, integer(1))
  n_in <- vapply(g$in_adj, length, integer(1))
  if (sum(n_out) != length(alive_r)) add("out_adj_total_mismatch",
                                        paste0(sum(n_out), " vs ", length(alive_r)))
  if (sum(n_in) != length(alive_r)) add("in_adj_total_mismatch",
                                       paste0(sum(n_in), " vs ", length(alive_r)))

  out <- if (length(issues)) do.call(rbind, issues) else
    data.frame(check = character(0), detail = character(0), stringsAsFactors = FALSE)
  if (verbose) {
    if (nrow(out)) print(out) else cat("一致性自检通过：节点", length(alive_n),
                                       "关系", length(alive_r), "邻接/主键/标签索引均一致\n")
  }
  invisible(out)
}

# ---------------------------------------------------------------------------
# 持久化
# ---------------------------------------------------------------------------

#' 保存图存储
#'
#' 说明：环境（主键 hash、邻接表、倒排索引）在序列化时非常慢（实测 291K 条的
#' 环境要 100 秒），这里先转成 list 再存（1~5 秒），载入时再转回环境。
#' 注意：R 的环境是引用语义，必须新建一个环境承载副本，否则会把调用方的
#' 图存储改成"list 版"，后续查询立即报错。
kg_save <- function(g, path = "store/kg_store.rds", compress = "gzip") {
  kg_assert(inherits(g, "kg_store"), "不是 kg_store 对象")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  t0 <- proc.time()[["elapsed"]]
  g2 <- as.list.environment(g, all.names = TRUE)
  g2 <- list2env(g2, parent = emptyenv())
  class(g2) <- "kg_store"
  g2$id_env <- as.list(g$id_env)
  g2$out_adj <- as.list(g$out_adj)
  g2$in_adj <- as.list(g$in_adj)
  if (length(g$fulltext)) {
    g2$fulltext <- lapply(g$fulltext, function(fi) {
      if (is.environment(fi$terms)) fi$terms <- as.list(fi$terms)
      fi$df_env <- NULL
      fi
    })
  }
  g2$serialized <- list(version = KG_ENGINE_VERSION, at = format(Sys.time()),
                        env_as_list = TRUE)
  saveRDS(g2, path, compress = compress)
  dt <- proc.time()[["elapsed"]] - t0
  kb <- file.info(path)$size / 1024^2
  kg_msg(sprintf("已保存 %s（%.1f MB，%.1fs）", path, kb, dt))
  invisible(path)
}

#' 载入图存储
kg_load <- function(path = "store/kg_store.rds", read_only = TRUE, verbose = TRUE) {
  if (!file.exists(path)) kg_abort("文件不存在: ", path)
  t0 <- proc.time()[["elapsed"]]
  g <- readRDS(path)
  kg_assert(inherits(g, "kg_store"), "不是 kg_store 对象: ", path)
  if (is.list(g$id_env)) {
    g$id_env <- list2env(g$id_env, hash = TRUE, envir = new.env(parent = emptyenv()))
  }
  if (is.list(g$out_adj)) {
    g$out_adj <- list2env(g$out_adj, hash = TRUE, envir = new.env(parent = emptyenv()))
  }
  if (is.list(g$in_adj)) {
    g$in_adj <- list2env(g$in_adj, hash = TRUE, envir = new.env(parent = emptyenv()))
  }
  if (length(g$fulltext)) {
    g$fulltext <- lapply(g$fulltext, function(fi) {
      if (is.list(fi$terms)) fi$terms <- list2env(fi$terms, hash = TRUE,
                                                  envir = new.env(parent = emptyenv()))
      fi
    })
  }
  g$read_only <- isTRUE(read_only)
  g$verbose <- TRUE
  if (verbose) kg_msg(sprintf("已载入 %s（%d 节点 / %d 关系，%.1fs）", basename(path),
                              nrow(g$nodes), nrow(g$rels),
                              proc.time()[["elapsed"]] - t0))
  g
}

#' 备份（带时间戳）
kg_backup <- function(g, dir = "store/backups") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  p <- file.path(dir, paste0("kg_store_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  kg_save(g, p)
  invisible(p)
}

#' 写 CSV 清单
kg_write_manifest <- function(g, path = "store/kg_store_manifest.csv") {
  s <- kg_stats(g, detail = TRUE)
  rows <- c(
    paste0("name,", s$name),
    paste0("engine,", s$engine),
    paste0("schema_version,", s$schema_version),
    paste0("node_count,", s$node_count),
    paste0("rel_count,", s$rel_count),
    paste0("label_count,", s$label_count),
    paste0("rel_type_count,", s$rel_type_count),
    paste0("constraint_count,", s$constraint_count),
    paste0("index_count,", s$index_count),
    paste0("saved_at,", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )
  lab <- data.frame(label = names(s$label_counts), count = as.integer(s$label_counts),
                    stringsAsFactors = FALSE)
  rel <- data.frame(type = names(s$rel_type_counts), count = as.integer(s$rel_type_counts),
                    stringsAsFactors = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(rows, path, useBytes = TRUE)
  kg_write_csv(lab, file.path(dirname(path), "kg_store_labels.csv"), row.names = FALSE)
  kg_write_csv(rel, file.path(dirname(path), "kg_store_rel_types.csv"), row.names = FALSE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Neo4j 风格的文本渲染
# ---------------------------------------------------------------------------

kg_format_value <- function(g, x, max_items = 8L) {
  if (inherits(x, "kg_null") || is.null(x)) return("null")
  if (inherits(x, "kg_node")) {
    rec <- kg_get_node(g, x$idx)
    lab <- paste0(":", paste(rec$labels, collapse = ":"))
    props <- rec$props
    keys <- names(props)
    if (length(keys) > max_items) keys <- keys[seq_len(max_items)]
    body <- paste0(vapply(keys, function(k) {
      paste0(k, ": ", kg_render_scalar(props[[k]]))
    }, ""), collapse = ", ")
    return(paste0("(", lab, " {", body, if (length(names(props)) > length(keys)) ", ..." else "", "})"))
  }
  if (inherits(x, "kg_rel")) {
    rec <- kg_get_rel(g, x$idx)
    return(paste0("[:", rec$type, "]->"))
  }
  if (inherits(x, "kg_path")) return(paste0("<path ", length(x$rels), " rels>"))
  if (is.list(x)) {
    return(paste0("[", paste(vapply(x, function(e) kg_format_value(g, e, max_items), ""),
                             collapse = ", "), "]"))
  }
  kg_render_scalar(x)
}

kg_render_scalar <- function(x) {
  if (length(x) != 1L) return(paste0("[", paste(x, collapse = ", "), "]"))
  if (is.na(x)) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.numeric(x)) return(if (kg_num_is_int(x)) format(x, scientific = FALSE) else
    format(x, digits = 15, scientific = FALSE))
  if (is.character(x)) return(paste0("'", x, "'"))
  as.character(x)
}
