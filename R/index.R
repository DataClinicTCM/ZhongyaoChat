
# =============================================================================
# index.R — 索引与约束（对齐 Neo4j 5 的索引类型）
#
#   LOOKUP    标签查找索引（Neo4j 的 label token lookup，每标签一个）
#   RANGE     单属性/复合属性索引（等值与范围查找）
#   FULLTEXT  中文全文索引（cjk 分析器：汉字二元组 + ASCII 词）
#   VECTOR    向量索引（余弦/欧氏，需外部写入 embedding）
#   TEXT      文本索引（Neo4j 5 的 TEXT，按词/前缀）
#
# 约束
#   UNIQUE    唯一约束（35 个标签的 id）
#   EXISTS    存在性约束
#   NODE_KEY  节点键约束
#
# 说明：列式内存存储本身对等值/范围扫描已经足够快（5 万节点 < 1ms），
#      RANGE 索引更多是"元数据兼容"，供 SHOW INDEXES / 语义等价使用；
#      真正影响性能的是 FULLTEXT 的倒排表与邻接表。
# =============================================================================

# ---------------------------------------------------------------------------
# 索引注册表
# ---------------------------------------------------------------------------

kg_index_list <- function(g) {
  idx <- data.table::copy(g$indexes)
  if (!nrow(idx)) return(idx)
  idx[, props := vapply(props, function(p) paste(p, collapse = ","), "")]
  idx
}

kg_index_get <- function(g, name) {
  i <- match(name, g$indexes$name)
  if (is.na(i)) return(NULL)
  list(name = g$indexes$name[i], type = g$indexes$type[i],
       label = g$indexes$label[i], props = g$indexes$props[[i]],
       state = g$indexes$state[i])
}

#' 创建索引
#'
#' @param g 图存储
#' @param name 索引名（Neo4j: CREATE INDEX <name>）
#' @param label 节点标签
#' @param props 属性名（向量，复合索引按顺序）
#' @param type LOOKUP / RANGE / FULLTEXT / TEXT / VECTOR
#' @param options 额外参数（fulltext.analyzer、vector.dimensions、vector.similarity_function）
kg_index_create <- function(g, name, label, props, type = c("RANGE", "LOOKUP", "FULLTEXT",
                                                             "TEXT", "VECTOR"),
                            options = list(), if_not_exists = TRUE) {
  type <- match.arg(type)
  kg_assert(inherits(g, "kg_store"), "不是 kg_store 对象")
  if (name %in% g$indexes$name) {
    if (if_not_exists) return(invisible(g))
    kg_abort("索引已存在: ", name)
  }
  if (is.null(label) || !nzchar(label)) kg_abort("索引必须指定标签")
  row <- data.table::data.table(name = name, type = type, label = label,
                                props = list(as.character(props)),
                                state = if (type == "FULLTEXT") "POPULATING" else "ONLINE")
  g$indexes <- data.table::rbindlist(list(g$indexes, row))
  if (type == "FULLTEXT") {
    g$fulltext[[name]] <- list(name = name, label = label, props = as.character(props),
                               options = options, built = FALSE)
  }
  if (type == "VECTOR") {
    g$vector[[name]] <- list(name = name, label = label, prop = as.character(props)[1],
                             options = options, built = FALSE)
  }
  invisible(g)
}

kg_index_drop <- function(g, name, if_exists = TRUE) {
  i <- match(name, g$indexes$name)
  if (is.na(i)) {
    if (if_exists) return(invisible(FALSE))
    kg_abort("索引不存在: ", name)
  }
  g$indexes <- g$indexes[-i]
  g$fulltext[[name]] <- NULL
  g$vector[[name]] <- NULL
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# 约束注册表
# ---------------------------------------------------------------------------

kg_constraint_list <- function(g) {
  ct <- data.table::copy(g$constraints)
  if (!nrow(ct)) return(ct)
  ct[, props := vapply(props, function(p) paste(p, collapse = ","), "")]
  ct
}

kg_constraint_create <- function(g, name, type = c("UNIQUE", "EXISTS", "NODE_KEY"),
                                 label, props, if_not_exists = TRUE) {
  type <- match.arg(type)
  kg_assert(inherits(g, "kg_store"), "不是 kg_store 对象")
  if (name %in% g$constraints$name) {
    if (if_not_exists) return(invisible(g))
    kg_abort("约束已存在: ", name)
  }
  props <- as.character(props)
  if (type %in% c("UNIQUE", "NODE_KEY")) {
    # 建立前先校验现有数据
    ids <- g$label_ids[[label]] %||% integer(0)
    if (length(ids) && length(props) == 1L) {
      vals <- g$nodes[[props[1]]][ids]
      vals <- vals[!is.na(vals)]
      if (anyDuplicated(vals)) kg_abort("无法创建唯一约束，已存在重复值: ", name)
    }
  }
  row <- data.table::data.table(name = name, type = type, label = label,
                                props = list(props))
  g$constraints <- data.table::rbindlist(list(g$constraints, row))
  invisible(g)
}

kg_constraint_drop <- function(g, name, if_exists = TRUE) {
  i <- match(name, g$constraints$name)
  if (is.na(i)) {
    if (if_exists) return(invisible(FALSE))
    kg_abort("约束不存在: ", name)
  }
  g$constraints <- g$constraints[-i]
  invisible(TRUE)
}

# 单属性唯一约束校验（写入前调用）
kg_check_unique <- function(g, label, props, values, exclude_idx = NA_integer_) {
  if (length(props) != 1L) return(invisible(TRUE))
  p <- props[1]
  v <- values[[p]]
  if (is.null(v) || length(v) != 1L || is.na(v)) return(invisible(TRUE))
  if (identical(p, "id")) {
    hit <- kg_node_idx(g, v)
    if (!is.na(hit) && !identical(hit, exclude_idx)) kg_abort("违反唯一约束: id=", v)
    return(invisible(TRUE))
  }
  ids <- g$label_ids[[label]] %||% integer(0)
  if (length(ids)) {
    hit <- ids[which(g$nodes[[p]][ids] == v)]
    if (length(hit) && !all(hit %in% exclude_idx)) kg_abort("违反唯一约束: ", p, "=", v)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# 中文分词（与 Lucene `cjk` 分析器对齐：汉字二元组 + ASCII 词）
# ---------------------------------------------------------------------------

kg_tokenize <- function(s) {
  if (is.na(s) || !nzchar(s)) return(character(0))
  s <- tolower(s)
  n <- nchar(s, type = "chars")
  if (n < 1L) return(character(0))
  bg <- if (n >= 2L) substring(s, 1:(n - 1L), 2:n) else character(0)
  w <- regmatches(s, gregexpr("[a-z0-9]+", s))[[1]]
  w <- w[nchar(w) >= 2L]
  c(bg, w)
}

kg_tokenize_query <- function(q) {
  if (is.na(q) || !nzchar(trimws(q))) return(character(0))
  terms <- strsplit(trimws(q), "\\s+")[[1]]
  out <- unlist(lapply(terms, kg_tokenize), use.names = FALSE)
  unique(out)
}

# ---------------------------------------------------------------------------
# 全文索引：构建 / 查询 / 删除
# ---------------------------------------------------------------------------

kg_index_build_fulltext <- function(g, name, verbose = TRUE) {
  spec <- kg_index_get(g, name)
  if (is.null(spec)) kg_abort("索引不存在: ", name)
  if (!identical(spec$type, "FULLTEXT")) kg_abort("不是全文索引: ", name)
  ids <- g$label_ids[[spec$label]] %||% integer(0)
  if (!length(ids)) {
    g$fulltext[[name]] <- list(name = name, label = spec$label, props = spec$props,
                               built = TRUE, terms = new.env(parent = emptyenv()),
                               ids = integer(0), n_terms = 0L, n_tokens = 0L,
                               n_docs = 0L, avg_len = 0)
    return(invisible(g))
  }
  t0 <- proc.time()[["elapsed"]]
  # 多属性拼接
  texts <- rep("", length(ids))
  for (p in spec$props) {
    v <- if (p %in% names(g$nodes)) g$nodes[[p]][ids] else rep(NA_character_, length(ids))
    v[is.na(v)] <- ""
    texts <- paste0(texts, " ", v)
  }
  toks <- lapply(texts, kg_tokenize)
  lens <- lengths(toks)
  all_toks <- unlist(toks, use.names = FALSE)
  rm(toks)
  doc_pos <- rep(seq_along(ids), lens)
  if (!length(all_toks)) {
    g$fulltext[[name]] <- list(name = name, label = spec$label, props = spec$props,
                               built = TRUE, terms = new.env(parent = emptyenv()),
                               ids = ids, n_terms = 0L, n_tokens = 0L,
                               n_docs = length(ids), avg_len = 0)
    return(invisible(g))
  }
  sp <- split(doc_pos, all_toks)
  rm(doc_pos, all_toks)
  term_ids <- lapply(sp, function(pos) ids[pos])
  nterms <- lengths(sp)
  env <- list2env(term_ids, hash = TRUE)
  g$fulltext[[name]] <- list(
    name = name, label = spec$label, props = spec$props, built = TRUE,
    terms = env, ids = ids, n_terms = length(env), n_tokens = sum(nterms),
    n_docs = length(ids), avg_len = mean(lens)
  )
  # 索引状态 ONLINE
  i <- match(name, g$indexes$name)
  if (!is.na(i)) data.table::set(g$indexes, i = i, j = "state", value = "ONLINE")
  if (verbose) {
    kg_msg(sprintf("全文索引 %s：%d 文档 / %d 词条 / %d tokens / %.1fs",
                   name, length(ids), length(env), sum(nterms),
                   proc.time()[["elapsed"]] - t0))
  }
  invisible(g)
}

kg_index_build_all <- function(g, verbose = TRUE) {
  ft <- g$indexes$name[g$indexes$type == "FULLTEXT"]
  for (nm in ft) {
    cur <- g$fulltext[[nm]]
    if (is.null(cur) || !isTRUE(cur$built)) kg_index_build_fulltext(g, nm, verbose = verbose)
  }
  invisible(g)
}

#' 全文检索（等价 CALL db.index.fulltext.queryNodes(name, query)）
kg_fulltext_search <- function(g, index_name, query, limit = Inf) {
  spec <- kg_index_get(g, index_name)
  if (is.null(spec)) kg_abort("全文索引不存在: ", index_name)
  fi <- g$fulltext[[index_name]]
  if (is.null(fi) || !isTRUE(fi$built)) {
    kg_index_build_fulltext(g, index_name, verbose = FALSE)
    fi <- g$fulltext[[index_name]]
  }
  toks <- kg_tokenize_query(query)
  if (!length(toks) || !length(fi$ids)) {
    return(data.frame(node = integer(0), score = numeric(0)))
  }
  n_docs <- fi$n_docs %||% length(fi$ids)
  max_idx <- max(fi$ids)
  score <- numeric(max_idx)
  hit_any <- logical(1)
  n <- 0L
  for (t in toks) {
    if (!exists(t, envir = fi$terms, inherits = FALSE)) next
    ids <- get(t, envir = fi$terms, inherits = FALSE)
    idf <- log(1 + n_docs / max(length(ids), 1))
    score[ids] <- score[ids] + idf
    n <- n + 1L
  }
  if (n == 0L) return(data.frame(node = integer(0), score = numeric(0)))
  hit <- which(score > 0)
  sc <- score[hit]
  ord <- order(-sc, hit)
  hit <- hit[ord]; sc <- sc[ord]
  if (is.finite(limit)) {
    keep <- seq_len(min(limit, length(hit)))
    hit <- hit[keep]; sc <- sc[keep]
  }
  data.frame(node = as.integer(hit), score = as.numeric(sc), stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# 向量索引（可选）
# ---------------------------------------------------------------------------

kg_index_build_vector <- function(g, name, embeddings, ids, verbose = TRUE) {
  spec <- kg_index_get(g, name)
  if (is.null(spec)) kg_abort("索引不存在: ", name)
  if (nrow(embeddings) != length(ids)) kg_abort("向量行数与 id 数不一致")
  opts <- g$vector[[name]]$options
  sim <- opts[["vector.similarity_function"]] %||% "cosine"
  m <- as.matrix(embeddings)
  if (sim == "cosine") {
    nrm <- sqrt(rowSums(m^2)); nrm[nrm == 0] <- 1
    m <- m / nrm
  }
  g$vector[[name]] <- list(name = name, label = spec$label, prop = spec$props[1],
                           built = TRUE, matrix = m, ids = as.integer(ids),
                           similarity = sim, dim = ncol(m))
  i <- match(name, g$indexes$name)
  if (!is.na(i)) data.table::set(g$indexes, i = i, j = "state", value = "ONLINE")
  if (verbose) kg_msg("向量索引 ", name, "：", nrow(m), " x ", ncol(m), " 维，", sim)
  invisible(g)
}

#' 向量检索（等价 CALL db.index.vector.queryNodes）
kg_vector_search <- function(g, index_name, vector, k = 10) {
  vi <- g$vector[[index_name]]
  if (is.null(vi) || !isTRUE(vi$built)) kg_abort("向量索引未构建: ", index_name)
  q <- as.numeric(vector)
  if (length(q) != vi$dim) kg_abort("查询向量维度不符：", length(q), " vs ", vi$dim)
  if (vi$similarity == "cosine") {
    nq <- sqrt(sum(q^2)); if (nq == 0) nq <- 1
    q <- q / nq
  }
  sims <- as.numeric(vi$matrix %*% q)
  if (vi$similarity == "euclidean") sims <- -sqrt(rowSums((vi$matrix - rep(q, each = nrow(vi$matrix)))^2))
  ord <- order(-sims)
  k <- min(k, length(ord))
  data.frame(node = vi$ids[ord[seq_len(k)]], score = sims[ord[seq_len(k)]],
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# 快速查找（规划器使用）
# ---------------------------------------------------------------------------

#' 按 标签 + 属性等值 取候选节点 idx
kg_lookup_ids <- function(g, label = NULL, prop = NULL, value = NULL) {
  if (!is.null(label)) {
    ids <- g$label_ids[[label]] %||% integer(0)
  } else {
    ids <- which(!g$nodes$deleted)
  }
  if (is.null(prop)) return(ids)
  if (identical(prop, "id")) {
    idx <- kg_node_idx(g, as.character(value))
    return(if (is.na(idx)) integer(0) else idx)
  }
  if (!prop %in% names(g$nodes)) return(integer(0))
  v <- g$nodes[[prop]][ids]
  if (is.null(value) || inherits(value, "kg_null")) return(ids[is.na(v)])
  ids[!is.na(v) & v == value]
}

#' 按 关系类型 取全部关系 idx
kg_rel_ids_of_type <- function(g, type = NULL) {
  if (is.null(type)) return(which(!g$rels$deleted))
  g$rel_type_ids[[type]] %||% integer(0)
}
