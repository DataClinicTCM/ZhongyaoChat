
# =============================================================================
# cypher_exec.R — Cypher 执行器（子句流水线）
#
# 执行模型：
#   stream = list(vars = 变量名向量,
#                 rows = 无名 list 的行列表（每行与 vars 按下标对齐）,
#                 used = 每行已用关系 idx 列表（同一 MATCH 内不重复走同一条边）)
#   每个子句消费一个 stream、产出一个 stream；RETURN 负责物化成 data.frame。
#
# 隐藏变量以 "." 开头（匿名节点/关系、OPTIONAL MATCH 标记），不进入结果。
# =============================================================================

# ---------------------------------------------------------------------------
# 公开入口
# ---------------------------------------------------------------------------

#' 执行一条 Cypher（Neo4j 语法子集）
#'
#' @param g kg_store
#' @param query Cypher 语句
#' @param params 命名 list（对应 $param）
#' @param verbose 打印耗时
#' @return data.frame（class kg_result），attr "summary" 为写入统计
kg_cypher <- function(g, query, params = list(), verbose = FALSE) {
  kg_assert(inherits(g, "kg_store"), "不是 kg_store 对象")
  t0 <- proc.time()[["elapsed"]]
  ast <- kg_parse(query)
  summ <- kg_summary_new()
  cache <- new.env(parent = emptyenv())
  st <- kg_exec_query(g, ast, params, summ, cache)
  # Cypher 语义：最后一条查询没有 RETURN 时返回空记录集（只有写入统计）
  last_q <- ast$queries[[length(ast$queries)]]
  has_return <- any(vapply(last_q$clauses, function(c) identical(c$type, "RETURN"), logical(1)))
  if (!has_return) st <- list(vars = character(0), rows = list(), used = list())
  df <- kg_materialize(g, st)
  elapsed <- proc.time()[["elapsed"]] - t0
  attr(df, "query") <- query
  attr(df, "summary") <- summ
  attr(df, "elapsed") <- elapsed
  attr(df, "g") <- g
  if (verbose) kg_msg(sprintf("查询完成：%d 行，%.3fs", nrow(df), elapsed))
  df
}

#' 查看执行计划（不执行）
kg_explain <- function(query) {
  ast <- kg_parse(query)
  plan <- character(0)
  for (q in ast$queries) {
    for (cl in q$clauses) {
      plan <- c(plan, switch(cl$type,
        MATCH = paste0(if (isTRUE(cl$optional)) "OPTIONAL MATCH " else "MATCH ",
                       paste(vapply(cl$patterns, kg_pattern_sig, ""), collapse = ", "),
                       if (!is.null(cl$where)) " + WHERE" else ""),
        WITH = paste0("WITH ", paste(vapply(cl$items, `[[`, "", "alias"), collapse = ", "),
                      if (isTRUE(cl$distinct)) " [DISTINCT]" else ""),
        RETURN = paste0("RETURN ", paste(vapply(cl$items, `[[`, "", "alias"), collapse = ", ")),
        UNWIND = paste0("UNWIND AS ", cl$var),
        CALL = paste0("CALL ", cl$proc),
        SHOW = paste0("SHOW ", cl$what),
        cl$type
      ))
    }
  }
  data.frame(step = seq_along(plan), plan = plan, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# 统计
# ---------------------------------------------------------------------------

kg_summary_new <- function() {
  list(nodes_created = 0L, nodes_deleted = 0L,
       relationships_created = 0L, relationships_deleted = 0L,
       properties_set = 0L, labels_added = 0L, labels_removed = 0L,
       indexes_added = 0L, indexes_removed = 0L,
       constraints_added = 0L, constraints_removed = 0L)
}

kg_summary_print <- function(s) {
  cat(sprintf("Nodes created: %d\nRelationships created: %d\nProperties set: %d\n",
              s$nodes_created, s$relationships_created, s$properties_set))
  if (s$nodes_deleted) cat(sprintf("Nodes deleted: %d\n", s$nodes_deleted))
  if (s$relationships_deleted) cat(sprintf("Relationships deleted: %d\n", s$relationships_deleted))
  if (s$labels_added) cat(sprintf("Labels added: %d\n", s$labels_added))
  if (s$labels_removed) cat(sprintf("Labels removed: %d\n", s$labels_removed))
  invisible(s)
}

#' 取查询的写入统计
kg_summary <- function(x) attr(x, "summary")

# ---------------------------------------------------------------------------
# 查询 / 子句调度
# ---------------------------------------------------------------------------

kg_exec_query <- function(g, ast, params, summ, cache) {
  if (!length(ast$queries)) kg_abort("空查询")
  res <- kg_exec_single(g, ast$queries[[1]], params, summ, cache)
  if (length(ast$queries) > 1L) {
    for (i in 2:length(ast$queries)) {
      st2 <- kg_exec_single(g, ast$queries[[i]], params, summ, cache)
      all_flag <- if (length(ast$union_all) >= (i - 1L)) ast$union_all[i - 1L] else FALSE
      res <- kg_stream_union(res, st2, all = all_flag)
    }
  }
  res
}

kg_stream_union <- function(st1, st2, all = FALSE) {
  if (!identical(st1$vars, st2$vars)) {
    vars <- unique(c(st1$vars, st2$vars))
    fix <- function(st) {
      idx <- match(vars, st$vars)
      rows <- lapply(st$rows, function(r) {
        out <- vector("list", length(vars))
        for (j in seq_along(vars)) out[[j]] <- if (is.na(idx[j])) kg_null else {
          v <- r[[idx[j]]]; if (is.null(v)) kg_null else v
        }
        out
      })
      list(vars = vars, rows = rows, used = lapply(rows, function(x) integer(0)))
    }
    st1 <- fix(st1); st2 <- fix(st2)
  }
  rows <- c(st1$rows, st2$rows)
  if (!all) {
    keys <- vapply(rows, kg_row_key, "")
    rows <- rows[!duplicated(keys)]
  }
  list(vars = st1$vars, rows = rows, used = lapply(rows, function(x) integer(0)))
}

kg_exec_single <- function(g, q, params, summ, cache) {
  st <- list(vars = character(0), rows = list(list()), used = list(integer(0)))
  for (cl in q$clauses) {
    st <- kg_exec_clause(g, cl, st, params, summ, cache)
  }
  st
}

kg_exec_clause <- function(g, cl, st, params, summ, cache) {
  switch(cl$type,
    MATCH = kg_exec_match(g, cl, st, params, cache),
    UNWIND = kg_exec_unwind(g, cl, st, params, cache),
    WITH = kg_exec_project(g, cl, st, params, cache),
    RETURN = kg_exec_project(g, cl, st, params, cache),
    CREATE = kg_exec_create(g, cl, st, params, summ),
    MERGE = kg_exec_merge(g, cl, st, params, summ, cache),
    SET = kg_exec_set(g, cl, st, params, summ),
    REMOVE = kg_exec_remove(g, cl, st, params, summ),
    DELETE = kg_exec_delete(g, cl, st, params, summ),
    CALL = kg_exec_call(g, cl, st, params, summ, cache),
    SHOW = kg_exec_show(g, cl, st, params),
    kg_abort("未实现的子句: ", cl$type)
  )
}

# ---------------------------------------------------------------------------
# MATCH
# ---------------------------------------------------------------------------

kg_exec_match <- function(g, cl, st, params, cache) {
  if (isTRUE(cl$optional)) {
    st <- kg_optional_match(g, cl, st, params, cache)
  } else {
    for (part in cl$patterns) {
      st <- if (identical(part$shortest, "none")) kg_match_part(g, st, part, params, cache)
            else kg_match_shortest(g, st, part, params, cache)
      if (!length(st$rows)) break
    }
    if (!is.null(cl$where)) st <- kg_filter_stream(g, st, cl$where, params, cache)
  }
  st
}

kg_optional_match <- function(g, cl, st, params, cache) {
  n0 <- length(st$rows)
  if (!n0) return(st)
  base_vars <- st$vars
  base_rows <- st$rows
  base_used <- st$used
  idx_var <- kg_unique_name(".opt", st$vars)
  st2 <- list(vars = c(base_vars, idx_var),
              rows = lapply(seq_len(n0), function(i) c(base_rows[[i]], list(i))),
              used = base_used)
  patt_vars <- unique(unlist(lapply(cl$patterns, function(pt) {
    c(vapply(pt$nodes, function(n) n$var %||% "", ""),
      vapply(pt$rels, function(r) r$var %||% "", ""))
  })))
  patt_vars <- setdiff(patt_vars[nzchar(patt_vars)], base_vars)
  for (part in cl$patterns) {
    st2 <- if (identical(part$shortest, "none")) kg_match_part(g, st2, part, params, cache)
           else kg_match_shortest(g, st2, part, params, cache)
    if (!length(st2$rows)) break
  }
  if (!is.null(cl$where) && length(st2$rows)) {
    st2 <- kg_filter_stream(g, st2, cl$where, params, cache)
  }
  ipos <- match(idx_var, st2$vars)
  seen <- if (length(st2$rows)) {
    unique(vapply(st2$rows, function(r) as.integer(r[[ipos]]), 0L))
  } else integer(0)
  new_vars <- setdiff(st2$vars, c(base_vars, idx_var))
  new_vars <- unique(c(new_vars, patt_vars))
  rows <- st2$rows
  used <- st2$used
  for (i in setdiff(seq_len(n0), seen)) {
    rows[[length(rows) + 1]] <- c(base_rows[[i]], rep(list(kg_null), length(new_vars)))
    used[[length(used) + 1]] <- base_used[[i]]
  }
  keep <- setdiff(seq_along(st2$vars), ipos)
  list(vars = st2$vars[keep], rows = lapply(rows, function(r) r[keep]), used = used)
}

# 匿名节点/关系变量：在整条查询内唯一，避免跨模式片段碰撞
kg_anon_name <- function(prefix, cache) {
  if (is.null(cache)) return(paste0(prefix, "_", sprintf("%06d", sample.int(999999, 1))))
  n <- get0(".anon_counter", envir = cache, ifnotfound = 0L) + 1L
  assign(".anon_counter", n, envir = cache)
  paste0(prefix, n)
}

# 快速通道：`()-[r]->()` / `()-[r:TYPE]->()` 这类"纯关系扫描"
# （两个匿名无约束端点 + 单个定长关系），直接从关系表出行，跳过 5 万次邻接查找。
kg_try_rel_scan <- function(g, st, part, params, cache) {
  if (length(st$rows) != 1L) return(NULL)
  if (length(part$nodes) != 2L || length(part$rels) != 1L) return(NULL)
  if (!is.null(part$path_var)) return(NULL)
  n0 <- part$nodes[[1]]; n1 <- part$nodes[[2]]; rel <- part$rels[[1]]
  if (!is.null(n0$var) || length(n0$labels) || length(n0$props$keys %||% character(0))) return(NULL)
  if (!is.null(n1$var) || length(n1$labels) || length(n1$props$keys %||% character(0))) return(NULL)
  if (isTRUE(rel$varlen)) return(NULL)
  rel_var <- rel$var %||% kg_anon_name(".r", cache)
  edges <- kg_rel_ids_of_type(g, if (length(rel$types)) rel$types else NULL)
  if (!length(edges)) return(list(vars = c(st$vars, rel_var), rows = list(), used = list()))
  if (!is.null(rel$props)) {
    pv <- kg_eval_props(g, rel$props, kg_ctx(g, st$vars, st$rows[[1]], params, cache = cache))
    for (p in names(pv)) {
      v <- pv[[p]]
      if (inherits(v, "kg_null")) next
      if (!p %in% names(g$rels)) { edges <- integer(0); break }
      col <- g$rels[[p]][edges]
      edges <- edges[!is.na(col) & col == v]
    }
  }
  rows <- lapply(edges, function(e) list(kg_rel_value(e)))
  used <- lapply(edges, function(e) e)
  list(vars = c(st$vars, rel_var), rows = rows, used = used)
}

# 展开一个模式片段（MATCH / 模式推导 / 模式存在判断共用）
kg_match_part <- function(g, st, part, params = list(), cache = NULL) {
  nodes <- part$nodes
  rels <- part$rels
  k <- length(nodes)
  if (!length(st$rows) || !k) return(st)

  fast <- kg_try_rel_scan(g, st, part, params, cache)
  if (!is.null(fast)) return(fast)

  nvars <- vapply(seq_len(k), function(i) {
    v <- nodes[[i]]$var
    if (is.null(v)) kg_anon_name(".n", cache) else v
  }, "")
  rvars <- if (length(rels)) vapply(seq_along(rels), function(i) {
    v <- rels[[i]]$var
    if (is.null(v)) kg_anon_name(".r", cache) else v
  }, "") else character(0)

  bound <- nvars %in% st$vars
  if (any(bound)) {
    s <- which(bound)[1]
  } else {
    score <- vapply(seq_len(k), function(i) {
      n <- nodes[[i]]
      sc <- 0
      if (length(n$labels)) sc <- sc + 2
      if (!is.null(n$props) && length(n$props$keys)) sc <- sc + 5
      sc
    }, numeric(1))
    s <- which.max(score)
  }

  # 起点：未绑定则播种，已绑定则按约束过滤
  if (!bound[s]) {
    npat_s <- nodes[[s]]
    # 主键直查：{id:'H:地榆'} 走 id 哈希，避免全标签扫描
    cand <- NULL
    if (!is.null(npat_s$props) && "id" %in% npat_s$props$keys) {
      cctx <- kg_ctx(g, st$vars, if (length(st$rows)) st$rows[[1]] else list(),
                     params, cache = cache)
      pv <- kg_eval_props(g, npat_s$props, cctx)
      if (is.character(pv$id) && length(pv$id) == 1L && !is.na(pv$id)) {
        idx <- kg_node_idx(g, pv$id)
        cand <- if (is.na(idx)) integer(0) else idx
      }
    }
    if (is.null(cand)) cand <- kg_seed_nodes(g, npat_s)
    st <- kg_seed_stream(g, st, nvars[s], cand)
    if (!length(st$rows)) return(st)
    if (!is.null(npat_s$props)) {
      st <- kg_filter_bound_node(g, st, match(nvars[s], st$vars), npat_s, params, cache)
    }
  } else {
    st <- kg_filter_bound_node(g, st, match(nvars[s], st$vars), nodes[[s]], params, cache)
  }
  if (!length(st$rows)) return(st)

  if (s < k) {
    for (i in s:(k - 1)) {
      st <- kg_step(g, st, nvars[i], rels[[i]], rvars[i], nvars[i + 1], nodes[[i + 1]],
                    params, cache, reverse = FALSE)
      if (!length(st$rows)) return(st)
    }
  }
  if (s > 1) {
    for (i in (s - 1):1) {
      st <- kg_step(g, st, nvars[i + 1], rels[[i]], rvars[i], nvars[i], nodes[[i]],
                    params, cache, reverse = TRUE)
      if (!length(st$rows)) return(st)
    }
  }
  if (!is.null(part$path_var) && length(st$rows)) st <- kg_bind_path(g, st, part, nvars, rvars)
  st
}

kg_bind_path <- function(g, st, part, nvars, rvars) {
  posn <- match(nvars, st$vars)
  posr <- match(rvars, st$vars)
  if (any(is.na(posn)) || any(is.na(posr))) return(st)
  rows <- lapply(st$rows, function(r) {
    ns <- vapply(posn, function(p) {
      v <- r[[p]]; if (is_node_value(v)) v$idx else NA_integer_
    }, 0L)
    rs <- vapply(posr, function(p) {
      v <- r[[p]]; if (is_rel_value(v)) v$idx else NA_integer_
    }, 0L)
    if (any(is.na(ns)) || any(is.na(rs))) c(r, list(kg_null))
    else c(r, list(kg_path_value(ns, rs)))
  })
  list(vars = c(st$vars, part$path_var), rows = rows, used = st$used)
}

kg_seed_nodes <- function(g, npat) {
  if (length(npat$labels)) {
    ids <- g$label_ids[[npat$labels[1]]] %||% integer(0)
    if (length(npat$labels) > 1) {
      for (l in npat$labels[-1]) ids <- intersect(ids, g$label_ids[[l]] %||% integer(0))
    }
  } else {
    ids <- which(!g$nodes$deleted)
  }
  ids
}

kg_seed_stream <- function(g, st, var, cand) {
  nr <- length(st$rows)
  if (!length(cand)) return(list(vars = c(st$vars, var), rows = list(), used = list()))
  total <- nr * length(cand)
  rows <- vector("list", total)
  used <- vector("list", total)
  m <- 0L
  for (i in seq_len(nr)) {
    r <- st$rows[[i]]
    ui <- st$used[[i]]
    for (id in cand) {
      m <- m + 1L
      rows[[m]] <- c(r, list(kg_node_value(id)))
      used[[m]] <- ui
    }
  }
  list(vars = c(st$vars, var), rows = rows, used = used)
}

kg_filter_bound_node <- function(g, st, pos, npat, params, cache) {
  if (!length(npat$labels) && is.null(npat$props)) return(st)
  ctx <- kg_ctx(g, st$vars, list(), params, cache = cache)
  rows <- vector("list", length(st$rows))
  used <- vector("list", length(st$rows))
  m <- 0L
  for (i in seq_along(st$rows)) {
    r <- st$rows[[i]]
    v <- r[[pos]]
    if (!is_node_value(v)) next
    if (!kg_nodes_match_mask(g, v$idx, npat, ctx)[1]) next
    m <- m + 1L
    rows[[m]] <- r
    used[[m]] <- st$used[[i]]
  }
  list(vars = st$vars, rows = rows[seq_len(m)], used = used[seq_len(m)])
}

# 单步扩展（定长 / 变长）
kg_step <- function(g, st, cur_var, rel, rel_var, next_var, node_pat, params, cache,
                    reverse = FALSE) {
  cur_pos <- match(cur_var, st$vars)
  next_pos <- match(next_var, st$vars)
  add_node <- is.na(next_pos)
  add_rel <- !(rel_var %in% st$vars)
  rel_pos <- if (add_rel) NA_integer_ else match(rel_var, st$vars)

  dir <- rel$dir
  if (reverse) dir <- switch(dir, out = "in", `in` = "out", both = "both")

  ctx <- kg_ctx(g, st$vars, list(), params, cache = cache)
  rel_props_vals <- NULL
  if (!is.null(rel$props) && length(rel$props$keys)) {
    rel_props_vals <- kg_eval_props(g, rel$props, ctx)
  }

  rows_out <- vector("list", length(st$rows))
  used_out <- vector("list", length(st$rows))

  for (i in seq_along(st$rows)) {
    r <- st$rows[[i]]
    cur <- r[[cur_pos]]
    if (!is_node_value(cur)) next
    used_i <- st$used[[i]]

    if (isTRUE(rel$varlen)) {
      paths <- kg_varlen_paths(g, cur$idx, dir, rel$types, rel$min, rel$max, used_i)
      if (!length(paths)) next
      out_i <- vector("list", length(paths))
      used_i_out <- vector("list", length(paths))
      m <- 0L
      for (p in paths) {
        tgt <- p$target
        if (add_node) {
          if (!kg_nodes_match_mask(g, tgt, node_pat, ctx)[1]) next
        } else {
          b <- r[[next_pos]]
          if (!is_node_value(b) || b$idx != tgt) next
        }
        if (!add_rel) {
          b <- r[[rel_pos]]
          if (!is_rel_value(b) || length(p$rels) != 1L || b$idx != p$rels[1]) next
        }
        vals <- list()
        if (add_rel) vals <- c(vals, list(lapply(p$rels, kg_rel_value)))
        if (add_node) vals <- c(vals, list(kg_node_value(tgt)))
        m <- m + 1L
        out_i[[m]] <- c(r, vals)
        used_i_out[[m]] <- c(used_i, p$rels)
      }
      if (!m) next
      rows_out[[i]] <- out_i[seq_len(m)]
      used_out[[i]] <- used_i_out[seq_len(m)]
      next
    }

    edges <- kg_adj(g, cur$idx, dir, if (length(rel$types)) rel$types else NULL)
    if (!length(edges)) next
    if (length(used_i)) edges <- edges[!edges %in% used_i]
    if (!length(edges)) next
    if (!add_rel) {
      b <- r[[rel_pos]]
      if (!is_rel_value(b)) next
      edges <- edges[edges == b$idx]
      if (!length(edges)) next
    }
    if (!is.null(rel_props_vals)) {
      for (p in names(rel_props_vals)) {
        v <- rel_props_vals[[p]]
        if (inherits(v, "kg_null")) next
        if (!p %in% names(g$rels)) { edges <- integer(0); break }
        col <- g$rels[[p]][edges]
        edges <- edges[!is.na(col) & col == v]
      }
      if (!length(edges)) next
    }
    targets <- kg_rel_other(g, edges, cur$idx)
    ok <- kg_nodes_match_mask(g, targets, node_pat, ctx)
    if (!add_node) {
      b <- r[[next_pos]]
      if (!is_node_value(b)) ok[] <- FALSE
      else ok <- ok & targets == b$idx
    }
    sel <- which(ok)
    if (!length(sel)) next
    out_i <- vector("list", length(sel))
    used_i_out <- vector("list", length(sel))
    for (j in seq_along(sel)) {
      e <- edges[sel[j]]; t <- targets[sel[j]]
      vals <- list()
      if (add_rel) vals <- c(vals, list(kg_rel_value(e)))
      if (add_node) vals <- c(vals, list(kg_node_value(t)))
      out_i[[j]] <- c(r, vals)
      used_i_out[[j]] <- c(used_i, e)
    }
    rows_out[[i]] <- out_i
    used_out[[i]] <- used_i_out
  }

  new_rows <- unlist(rows_out, recursive = FALSE, use.names = FALSE)
  new_used <- unlist(used_out, recursive = FALSE, use.names = FALSE)
  if (is.null(new_rows)) new_rows <- list()
  if (is.null(new_used)) new_used <- list()
  vars <- st$vars
  if (add_rel) vars <- c(vars, rel_var)
  if (add_node) vars <- c(vars, next_var)
  list(vars = vars, rows = new_rows, used = new_used)
}

kg_nodes_match_mask <- function(g, idx, npat, ctx) {
  n <- length(idx)
  if (!n) return(logical(0))
  has_lab <- length(npat$labels) > 0
  has_prop <- !is.null(npat$props) && length(npat$props$keys) > 0
  if (!has_lab && !has_prop) return(rep(TRUE, n))
  mask <- rep(TRUE, n)
  if (has_lab) {
    for (lab in npat$labels) {
      if (lab %in% g$multi_labels) {
        ids <- g$label_ids[[lab]]
        if (is.null(ids)) { mask[] <- FALSE; return(mask) }
        mask <- mask & (idx %in% ids)
      } else {
        code <- match(lab, g$label_levels)
        if (is.na(code)) { mask[] <- FALSE; return(mask) }
        lc <- g$label_code[idx]
        mask <- mask & !is.na(lc) & lc == code
      }
      if (!any(mask)) return(mask)
    }
  }
  if (has_prop) {
    for (j in seq_along(npat$props$keys)) {
      p <- npat$props$keys[j]
      v <- kg_eval(npat$props$vals[[j]], ctx)
      if (!p %in% names(g$nodes)) { mask[] <- FALSE; return(mask) }
      col <- g$nodes[[p]][idx]
      if (inherits(v, "kg_null")) {
        mask <- mask & is.na(col)
      } else if (is.list(v) && !is_node_value(v)) {
        keys <- vapply(v, kg_value_key, "")
        mask <- mask & !is.na(col) & vapply(col, kg_value_key, "") %in% keys
      } else {
        mask <- mask & !is.na(col) & col == v
      }
      if (!any(mask)) return(mask)
    }
  }
  mask
}

# 变长路径枚举（BFS，限制状态数）
kg_varlen_paths <- function(g, start, dir, types, min_depth, max_depth, used,
                            max_states = 20000L) {
  out <- list()
  frontier <- list(list(node = start, rels = integer(0)))
  depth <- 0L
  truncated <- FALSE
  while (length(frontier) && depth < max_depth) {
    depth <- depth + 1L
    nxt <- list()
    for (st in frontier) {
      edges <- kg_adj(g, st$node, dir, if (length(types)) types else NULL)
      if (!length(edges)) next
      if (length(st$rels) || length(used)) edges <- edges[!edges %in% c(st$rels, used)]
      if (!length(edges)) next
      for (e in edges) {
        rels2 <- c(st$rels, e)
        target <- kg_rel_other(g, e, st$node)
        if (depth >= min_depth) out[[length(out) + 1]] <- list(rels = rels2, target = target)
        if (depth < max_depth) nxt[[length(nxt) + 1]] <- list(node = target, rels = rels2)
        if (length(nxt) > max_states) { truncated <- TRUE; break }
      }
      if (truncated) break
    }
    if (truncated) break
    if (length(out) > max_states) { truncated <- TRUE; break }
    frontier <- nxt
  }
  if (truncated) warning("变长路径状态数超过上限，结果已截断", call. = FALSE)
  out
}

# shortestPath / allShortestPaths
kg_match_shortest <- function(g, st, part, params, cache) {
  if (length(part$nodes) != 2L || length(part$rels) != 1L)
    kg_abort("shortestPath 只支持两节点单关系模式")
  rel <- part$rels[[1]]
  n0 <- part$nodes[[1]]; n1 <- part$nodes[[2]]
  v0 <- n0$var %||% kg_anon_name(".n", cache)
  v1 <- n1$var %||% kg_anon_name(".n", cache)
  add0 <- !(v0 %in% st$vars)
  add1 <- !(v1 %in% st$vars)
  ctx <- kg_ctx(g, st$vars, list(), params, cache = cache)
  maxd <- if (is.finite(rel$max)) rel$max else 15L
  all_paths <- identical(part$shortest, "all")

  cand0 <- if (add0) kg_seed_nodes(g, n0) else NULL
  cand1 <- if (add1) kg_seed_nodes(g, n1) else NULL
  # 端点还要满足标签/属性约束
  filt_ctx <- kg_ctx(g, st$vars, if (length(st$rows)) st$rows[[1]] else list(), params,
                     cache = cache)
  if (!is.null(cand0) && length(cand0)) {
    cand0 <- cand0[kg_nodes_match_mask(g, cand0, n0, filt_ctx)]
  }
  if (!is.null(cand1) && length(cand1)) {
    cand1 <- cand1[kg_nodes_match_mask(g, cand1, n1, filt_ctx)]
  }
  rows <- list(); used <- list()
  for (i in seq_along(st$rows)) {
    r <- st$rows[[i]]
    starts <- if (add0) cand0 else {
      v <- r[[match(v0, st$vars)]]; if (is_node_value(v)) v$idx else integer(0)
    }
    targets <- if (add1) cand1 else {
      v <- r[[match(v1, st$vars)]]; if (is_node_value(v)) v$idx else integer(0)
    }
    if (!length(starts) || !length(targets)) next
    tenv <- list2env(as.list(stats::setNames(rep(TRUE, length(targets)), as.character(targets))),
                     envir = new.env(parent = emptyenv(), hash = TRUE))
    found <- list()
    for (s in starts) {
      ps <- kg_bfs_shortest(g, s, tenv, rel$dir, rel$types, maxd, all_paths)
      if (length(ps)) {
        found <- c(found, ps)
        if (!all_paths) break
      }
    }
    if (!length(found)) next
    for (p in found) {
      vals <- list()
      if (add0) vals <- c(vals, list(kg_node_value(p$nodes[1])))
      if (add1) vals <- c(vals, list(kg_node_value(p$nodes[length(p$nodes)])))
      if (!is.null(part$path_var)) vals <- c(vals, list(kg_path_value(p$nodes, p$rels)))
      rows[[length(rows) + 1]] <- c(r, vals)
      used[[length(used) + 1]] <- integer(0)
    }
  }
  vars <- st$vars
  if (add0) vars <- c(vars, v0)
  if (add1) vars <- c(vars, v1)
  if (!is.null(part$path_var)) vars <- c(vars, part$path_var)
  list(vars = vars, rows = rows, used = used)
}

kg_bfs_shortest <- function(g, start, target_env, dir, types, max_depth, all = FALSE) {
  if (exists(as.character(start), envir = target_env, inherits = FALSE)) {
    return(list(list(nodes = start, rels = integer(0))))
  }
  prev <- new.env(parent = emptyenv(), hash = TRUE)
  depth <- new.env(parent = emptyenv(), hash = TRUE)
  assign(as.character(start), -1L, envir = prev)
  assign(as.character(start), 0L, envir = depth)
  frontier <- start
  found_depth <- NA_integer_
  d <- 0L
  while (length(frontier) && d < max_depth && (is.na(found_depth) || d < found_depth)) {
    d <- d + 1L
    nxt <- integer(0)
    for (u in frontier) {
      edges <- kg_adj(g, u, dir, if (length(types)) types else NULL)
      for (e in edges) {
        w <- kg_rel_other(g, e, u)
        k <- as.character(w)
        if (!exists(k, envir = depth, inherits = FALSE)) {
          assign(k, d, envir = depth)
          assign(k, e, envir = prev)
          nxt <- c(nxt, w)
          if (exists(k, envir = target_env, inherits = FALSE) && is.na(found_depth)) {
            found_depth <- d
          }
        }
      }
    }
    frontier <- unique(nxt)
    if (!is.na(found_depth)) break
  }
  if (is.na(found_depth)) return(list())
  founds <- ls(envir = target_env)
  founds <- founds[vapply(founds, function(k) {
    exists(k, envir = depth, inherits = FALSE) &&
      get(k, envir = depth, inherits = FALSE) == found_depth
  }, logical(1))]
  results <- list()
  for (kk in founds) {
    node <- as.integer(kk)
    nodes <- node; rels <- integer(0)
    cur <- node
    repeat {
      e <- get(as.character(cur), envir = prev, inherits = FALSE)
      if (identical(e, -1L)) break
      rels <- c(e, rels)
      cur <- kg_rel_other(g, e, cur)
      nodes <- c(cur, nodes)
    }
    results[[length(results) + 1]] <- list(nodes = nodes, rels = rels)
    if (!all) break
  }
  results
}

# ---------------------------------------------------------------------------
# WHERE 过滤
# ---------------------------------------------------------------------------

kg_filter_stream <- function(g, st, where, params, cache) {
  if (!length(st$rows)) return(st)
  rows <- vector("list", length(st$rows))
  used <- vector("list", length(st$rows))
  m <- 0L
  for (i in seq_along(st$rows)) {
    ctx <- kg_ctx(g, st$vars, st$rows[[i]], params, cache = cache)
    if (isTRUE(kg_bool3(kg_eval(where, ctx)))) {
      m <- m + 1L
      rows[[m]] <- st$rows[[i]]
      used[[m]] <- st$used[[i]]
    }
  }
  list(vars = st$vars, rows = rows[seq_len(m)], used = used[seq_len(m)])
}

# ---------------------------------------------------------------------------
# UNWIND
# ---------------------------------------------------------------------------

kg_exec_unwind <- function(g, cl, st, params, cache) {
  rows <- list(); used <- list()
  for (i in seq_along(st$rows)) {
    ctx <- kg_ctx(g, st$vars, st$rows[[i]], params, cache = cache)
    v <- kg_eval(cl$expr, ctx)
    if (inherits(v, "kg_null")) next
    items <- if (is.list(v) && !is_node_value(v) && !is_rel_value(v) && !is_path_value(v)) v else list(v)
    for (it in items) {
      rows[[length(rows) + 1]] <- c(st$rows[[i]], list(it))
      used[[length(used) + 1]] <- st$used[[i]]
    }
  }
  list(vars = c(st$vars, cl$var), rows = rows, used = used)
}

# ---------------------------------------------------------------------------
# WITH / RETURN（投影 + 聚合 + 去重 + 排序 + 分页）
# ---------------------------------------------------------------------------

kg_exec_project <- function(g, cl, st, params, cache) {
  items <- cl$items
  is_wild <- vapply(items, function(it) identical(it$expr$t, "wildcard"), logical(1))
  if (any(is_wild)) {
    visible <- st$vars[!startsWith(st$vars, ".")]
    rest <- items[!is_wild]
    items <- c(lapply(visible, function(v) list(expr = list(t = "var", name = v), alias = v)), rest)
  }
  if (!length(items)) kg_abort("RETURN/WITH 至少需要一个表达式")

  aliases <- vapply(items, `[[`, "", "alias")
  aliases <- make.unique(aliases, sep = "_")
  aggs <- vapply(items, function(it) kg_expr_has_agg(it$expr), logical(1))
  # 常量投影（不引用任何变量、无聚合）：只求值一次，且不作为分组键
  is_const <- vapply(items, function(it) {
    !kg_expr_has_agg(it$expr) && length(kg_expr_vars(it$expr)) == 0L
  }, logical(1))
  const_vals <- vector("list", length(items))
  if (any(is_const)) {
    cctx <- kg_ctx(g, st$vars, if (length(st$rows)) st$rows[[1]] else list(), params,
                   cache = cache)
    for (j in which(is_const)) const_vals[[j]] <- kg_eval(items[[j]]$expr, cctx)
  }

  out_rows <- list(); src_rows <- list(); agg_list <- list()

  if (any(aggs)) {
    key_idx <- which(!aggs & !is_const)
    if (length(key_idx) == 1L) {
      it <- items[[key_idx]]
      keys <- kg_fast_key(it$expr, st, g, params, cache)
      if (is.null(keys)) {
        keys <- vapply(st$rows, function(r) {
          kg_value_key(kg_eval(it$expr, kg_ctx(g, st$vars, r, params, cache = cache)))
        }, "")
      }
      groups <- if (length(keys)) split(seq_along(st$rows), factor(keys, levels = unique(keys))) else list()
    } else if (length(key_idx) > 1L) {
      keys <- vapply(st$rows, function(r) {
        ctx <- kg_ctx(g, st$vars, r, params, cache = cache)
        kg_row_key(lapply(items[key_idx], function(it) kg_eval(it$expr, ctx)))
      }, "")
      groups <- if (length(keys)) split(seq_along(st$rows), factor(keys, levels = unique(keys))) else list()
    } else {
      groups <- if (length(st$rows)) list(seq_along(st$rows)) else list(integer(0))
    }
    null_row <- rep(list(kg_null), length(st$vars))
    for (gi in groups) {
      r0 <- if (length(gi)) st$rows[[gi[1]]] else null_row
      agg_rows <- if (length(gi)) st$rows[gi] else list()
      vals <- vector("list", length(items))
      for (j in seq_along(items)) {
        if (is_const[j]) { vals[[j]] <- const_vals[[j]]; next }
        ctx <- kg_ctx(g, st$vars, r0, params, cache = cache)
        if (aggs[j]) ctx$agg_rows <- agg_rows
        vals[[j]] <- kg_eval(items[[j]]$expr, ctx)
      }
      out_rows[[length(out_rows) + 1]] <- vals
      src_rows[[length(src_rows) + 1]] <- r0
      agg_list[[length(agg_list) + 1]] <- agg_rows
    }
  } else {
    for (i in seq_along(st$rows)) {
      ctx <- kg_ctx(g, st$vars, st$rows[[i]], params, cache = cache)
      vals <- vector("list", length(items))
      for (j in seq_along(items)) {
        vals[[j]] <- if (is_const[j]) const_vals[[j]] else kg_eval(items[[j]]$expr, ctx)
      }
      out_rows[[length(out_rows) + 1]] <- vals
      src_rows[[length(src_rows) + 1]] <- st$rows[[i]]
      agg_list[length(agg_list) + 1L] <- list(NULL)
    }
  }

  # WITH ... WHERE
  if (!is.null(cl$where) && length(out_rows)) {
    keep <- logical(length(out_rows))
    for (i in seq_along(out_rows)) {
      ctx <- kg_ctx(g, c(aliases, st$vars), c(out_rows[[i]], src_rows[[i]]), params,
                    agg_rows = agg_list[[i]], cache = cache)
      keep[i] <- isTRUE(kg_bool3(kg_eval(cl$where, ctx)))
    }
    out_rows <- out_rows[keep]; src_rows <- src_rows[keep]; agg_list <- agg_list[keep]
  }

  # DISTINCT
  if (isTRUE(cl$distinct) && length(out_rows)) {
    keys <- vapply(out_rows, kg_row_key, "")
    keep <- !duplicated(keys)
    out_rows <- out_rows[keep]; src_rows <- src_rows[keep]; agg_list <- agg_list[keep]
  }

  # ORDER BY
  ord <- cl$order
  if (length(ord) && length(out_rows) > 1L) {
    mat <- vapply(seq_along(out_rows), function(i) {
      ctx <- kg_ctx(g, c(aliases, st$vars), c(out_rows[[i]], src_rows[[i]]), params,
                    agg_rows = agg_list[[i]], cache = cache)
      vapply(ord, function(o) kg_sort_key(kg_eval(o$expr, ctx)), "")
    }, character(length(ord)))
    if (length(ord) == 1L) mat <- matrix(mat, ncol = 1L)
    else mat <- t(mat)
    perm <- seq_along(out_rows)
    for (j in rev(seq_along(ord))) {
      perm <- perm[order(mat[perm, j], decreasing = ord[[j]]$desc, method = "radix")]
    }
    out_rows <- out_rows[perm]; src_rows <- src_rows[perm]; agg_list <- agg_list[perm]
  }

  skip <- if (!is.null(cl$skip)) {
    v <- kg_to_number(kg_eval(cl$skip, kg_ctx(g, st$vars, list(), params)), integer = TRUE)
    if (inherits(v, "kg_null")) 0L else as.integer(v)
  } else 0L
  limit <- if (!is.null(cl$limit)) {
    v <- kg_to_number(kg_eval(cl$limit, kg_ctx(g, st$vars, list(), params)), integer = TRUE)
    if (inherits(v, "kg_null")) Inf else as.integer(v)
  } else Inf
  n <- length(out_rows)
  idx <- seq_len(n)
  if (skip > 0L) idx <- idx[idx > skip]
  if (is.finite(limit)) idx <- idx[idx <= skip + limit]
  out_rows <- out_rows[idx]

  list(vars = aliases, rows = out_rows, used = lapply(out_rows, function(x) integer(0)))
}

# 分组键的向量化快速路径：只覆盖最常见的三种形态
#   var / type(var) / var.prop
# 命中时返回与 st$rows 等长的键向量；未命中返回 NULL（走通用求值）。
kg_fast_key <- function(expr, st, g, params, cache) {
  n <- length(st$rows)
  key_of <- function(v) {
    if (inherits(v, "kg_null") || is.null(v)) return("\u0001N")
    if (is.character(v)) return(paste0("\u0001s", v))
    if (is.numeric(v)) return(paste0("\u0001d", sprintf("%.15g", v)))
    if (is_rel_value(v)) return(paste0("\u0001r", v$idx))
    if (is_node_value(v)) return(paste0("\u0001n", v$idx))
    if (is.logical(v)) return(if (is.na(v)) "\u0001N" else if (v) "\u0001bT" else "\u0001bF")
    kg_value_key(v)
  }
  if (identical(expr$t, "var")) {
    pos <- match(expr$name, st$vars)
    if (is.na(pos)) return(rep("\u0001N", n))
    return(vapply(st$rows, function(r) key_of(r[[pos]]), ""))
  }
  if (identical(expr$t, "call") && tolower(expr$name) == "type" &&
      length(expr$args) == 1L && identical(expr$args[[1]]$t, "var")) {
    pos <- match(expr$args[[1]]$name, st$vars)
    if (is.na(pos)) return(rep("\u0001N", n))
    return(vapply(st$rows, function(r) {
      v <- r[[pos]]
      if (is_rel_value(v)) g$rels$type[v$idx] else "\u0001N"
    }, ""))
  }
  if (identical(expr$t, "prop") && identical(expr$obj$t, "var")) {
    pos <- match(expr$obj$name, st$vars)
    if (is.na(pos)) return(rep("\u0001N", n))
    key <- expr$key
    return(vapply(st$rows, function(r) {
      v <- r[[pos]]
      if (is_node_value(v)) {
        if (identical(key, "id")) return(paste0("\u0001s", g$nodes$id[v$idx]))
        if (key %in% names(g$nodes)) {
          x <- g$nodes[[key]][v$idx]
          if (is.na(x)) return("\u0001N")
          if (is.character(x)) return(paste0("\u0001s", x))
          if (is.numeric(x)) return(paste0("\u0001d", sprintf("%.15g", x)))
          if (is.logical(x)) return(if (x) "\u0001bT" else "\u0001bF")
          return(kg_value_key(x))
        }
        return("\u0001N")
      }
      if (is_rel_value(v)) {
        if (key %in% names(g$rels)) {
          x <- g$rels[[key]][v$idx]
          if (is.na(x)) return("\u0001N")
          if (is.character(x)) return(paste0("\u0001s", x))
          if (is.numeric(x)) return(paste0("\u0001d", sprintf("%.15g", x)))
          if (is.logical(x)) return(if (x) "\u0001bT" else "\u0001bF")
        }
        return("\u0001N")
      }
      "\u0001N"
    }, ""))
  }
  NULL
}

kg_sort_key <- function(v) {  if (inherits(v, "kg_null") || is.null(v)) return("\u0002")
  if (is.numeric(v)) return(sprintf("\u0001%+030.10f", v))
  if (is.logical(v)) return(sprintf("\u0001%d", as.integer(v)))
  if (is.character(v)) return(paste0("\u0001", v))
  paste0("\u0003", kg_value_key(v))
}

# ---------------------------------------------------------------------------
# CREATE
# ---------------------------------------------------------------------------

kg_exec_create <- function(g, cl, st, params, summ) {
  kg_assert_writable(g)
  for (part in cl$patterns) st <- kg_create_part(g, st, part, params, summ)
  st
}

kg_create_part <- function(g, st, part, params, summ) {
  nodes <- part$nodes; rels <- part$rels
  nvars <- vapply(seq_along(nodes), function(i) nodes[[i]]$var %||% paste0(".n", i), "")
  rvars <- if (length(rels)) vapply(seq_along(rels), function(i) rels[[i]]$var %||% paste0(".r", i), "")
           else character(0)
  new_vars <- unique(c(nvars[!nvars %in% st$vars], rvars[!rvars %in% st$vars]))
  rows <- list()
  for (i in seq_along(st$rows)) {
    r <- st$rows[[i]]
    ctx <- kg_ctx(g, c(st$vars, new_vars), c(r, rep(list(kg_null), length(new_vars))), params)
    bind <- list()
    idxs <- integer(length(nodes))
    for (j in seq_along(nodes)) {
      v <- nvars[j]
      if (v %in% st$vars) {
        node <- r[[match(v, st$vars)]]
        if (!is_node_value(node)) kg_abort("CREATE 中的变量不是节点: ", v)
        idxs[j] <- node$idx
      } else {
        labels <- nodes[[j]]$labels
        if (!length(labels)) kg_abort("CREATE 新节点必须指定标签")
        pv <- kg_eval_props(g, nodes[[j]]$props, ctx)
        idx <- kg_insert_node(g, labels[1], pv, labels = labels[-1])
        idxs[j] <- idx
        bind[[v]] <- kg_node_value(idx)
        summ$nodes_created <- summ$nodes_created + 1L
        summ$properties_set <- summ$properties_set + length(pv)
      }
    }
    for (j in seq_along(rels)) {
      ty <- rels[[j]]$types[1]
      if (is.na(ty)) kg_abort("CREATE 关系必须指定类型")
      pv <- kg_eval_props(g, rels[[j]]$props, ctx)
      ridx <- kg_insert_rel(g, idxs[j], idxs[j + 1], ty, pv)
      bind[[rvars[j]]] <- kg_rel_value(ridx)
      summ$relationships_created <- summ$relationships_created + 1L
      summ$properties_set <- summ$properties_set + length(pv)
    }
    rows[[i]] <- c(r, lapply(new_vars, function(v) bind[[v]] %||% kg_null))
  }
  list(vars = c(st$vars, new_vars), rows = rows,
       used = lapply(rows, function(x) integer(0)))
}

# ---------------------------------------------------------------------------
# MERGE
# ---------------------------------------------------------------------------

kg_exec_merge <- function(g, cl, st, params, summ, cache) {
  kg_assert_writable(g)
  part <- cl$pattern
  rows_out <- list(); used_out <- list(); schema <- NULL
  for (i in seq_along(st$rows)) {
    r <- st$rows[[i]]
    st1 <- list(vars = st$vars, rows = list(r), used = list(integer(0)))
    m <- kg_match_part(g, st1, part, params, cache)
    if (length(m$rows)) {
      for (mr in m$rows) {
        if (length(cl$on_match)) {
          kg_apply_set(g, cl$on_match, kg_ctx(g, m$vars, mr, params), summ)
        }
        rows_out[[length(rows_out) + 1]] <- mr
        used_out[[length(used_out) + 1]] <- integer(0)
      }
      if (is.null(schema)) schema <- m$vars
    } else {
      cr <- kg_create_part(g, st1, part, params, summ)
      cr_row <- cr$rows[[1]]
      if (length(cl$on_create)) {
        kg_apply_set(g, cl$on_create, kg_ctx(g, cr$vars, cr_row, params), summ)
      }
      rows_out[[length(rows_out) + 1]] <- cr_row
      used_out[[length(used_out) + 1]] <- integer(0)
      if (is.null(schema)) schema <- cr$vars
    }
  }
  if (is.null(schema)) schema <- st$vars
  list(vars = schema, rows = rows_out, used = used_out)
}

# ---------------------------------------------------------------------------
# SET / REMOVE
# ---------------------------------------------------------------------------

kg_apply_set <- function(g, items, ctx, summ) {
  for (it in items) {
    v <- kg_row_get(ctx, it$var)
    if (inherits(v, "kg_null")) next
    if (it$kind == "label") {
      if (is_node_value(v)) {
        for (l in it$labels) {
          if (!l %in% kg_node_labels(g, v$idx)) {
            kg_add_node_label(g, v$idx, l)
            summ$labels_added <- summ$labels_added + 1L
          }
        }
      }
      next
    }
    val <- kg_eval(it$expr, ctx)
    if (it$kind == "prop") {
      if (is_node_value(v)) {
        kg_set_node_prop(g, v$idx, it$prop, val)
        summ$properties_set <- summ$properties_set + 1L
      } else if (is_rel_value(v)) {
        kg_set_rel_prop(g, v$idx, it$prop, val)
        summ$properties_set <- summ$properties_set + 1L
      }
      next
    }
    if (it$kind %in% c("replace", "merge")) {
      if (inherits(val, "kg_null") || !is.list(val) || is.null(names(val))) next
      if (it$kind == "replace") {
        if (is_node_value(v)) {
          for (p in setdiff(names(g$nodes), c(KG_NODE_RESERVED))) {
            if (!p %in% names(val) && !is.na(g$nodes[[p]][v$idx])) {
              kg_remove_node_prop(g, v$idx, p)
              summ$properties_set <- summ$properties_set + 1L
            }
          }
        } else if (is_rel_value(v)) {
          for (p in setdiff(names(g$rels), KG_REL_RESERVED)) {
            if (!p %in% names(val) && !is.na(g$rels[[p]][v$idx])) {
              kg_remove_rel_prop(g, v$idx, p)
              summ$properties_set <- summ$properties_set + 1L
            }
          }
        }
      }
      for (p in names(val)) {
        if (is_node_value(v)) kg_set_node_prop(g, v$idx, p, val[[p]])
        else if (is_rel_value(v)) kg_set_rel_prop(g, v$idx, p, val[[p]])
        summ$properties_set <- summ$properties_set + 1L
      }
    }
  }
  invisible(summ)
}

kg_exec_set <- function(g, cl, st, params, summ) {
  kg_assert_writable(g)
  for (r in st$rows) {
    kg_apply_set(g, cl$items, kg_ctx(g, st$vars, r, params), summ)
  }
  st
}

kg_exec_remove <- function(g, cl, st, params, summ) {
  kg_assert_writable(g)
  for (r in st$rows) {
    ctx <- kg_ctx(g, st$vars, r, params)
    for (it in cl$items) {
      v <- kg_row_get(ctx, it$var)
      if (inherits(v, "kg_null")) next
      if (it$kind == "prop") {
        if (is_node_value(v)) kg_remove_node_prop(g, v$idx, it$prop)
        else if (is_rel_value(v)) kg_remove_rel_prop(g, v$idx, it$prop)
        summ$properties_set <- summ$properties_set + 1L
      } else if (it$kind == "label" && is_node_value(v)) {
        for (l in it$labels) {
          kg_remove_node_label(g, v$idx, l)
          summ$labels_removed <- summ$labels_removed + 1L
        }
      }
    }
  }
  st
}

# ---------------------------------------------------------------------------
# DELETE
# ---------------------------------------------------------------------------

kg_exec_delete <- function(g, cl, st, params, summ) {
  kg_assert_writable(g)
  node_ids <- integer(0); rel_ids <- integer(0)
  for (r in st$rows) {
    ctx <- kg_ctx(g, st$vars, r, params)
    for (e in cl$exprs) {
      v <- kg_eval(e, ctx)
      if (is_node_value(v)) node_ids <- c(node_ids, v$idx)
      else if (is_rel_value(v)) rel_ids <- c(rel_ids, v$idx)
      else if (is.list(v) && !inherits(v, "kg_node") && !inherits(v, "kg_rel")) {
        for (x in v) {
          if (is_node_value(x)) node_ids <- c(node_ids, x$idx)
          else if (is_rel_value(x)) rel_ids <- c(rel_ids, x$idx)
        }
      }
    }
  }
  rel_ids <- unique(rel_ids); node_ids <- unique(node_ids)
  if (length(rel_ids)) {
    n <- kg_delete_rel(g, rel_ids)
    summ$relationships_deleted <- summ$relationships_deleted + n
  }
  if (length(node_ids)) {
    res <- kg_delete_node(g, node_ids, detach = isTRUE(cl$detach))
    summ$nodes_deleted <- summ$nodes_deleted + res$nodes
    summ$relationships_deleted <- summ$relationships_deleted + res$rels
  }
  st
}

# ---------------------------------------------------------------------------
# CALL（过程）
# ---------------------------------------------------------------------------

kg_exec_call <- function(g, cl, st, params, summ, cache) {
  if (!length(st$rows)) {
    proc <- tolower(cl$proc)
    res <- tryCatch(kg_call_proc(g, proc, list(), summ), error = function(e) NULL)
    return(list(vars = c(st$vars, if (!is.null(res)) res$vars else character(0)),
                rows = list(), used = list()))
  }
  proc <- tolower(cl$proc)
  rows <- list()
  res_vars <- character(0)
  for (i in seq_along(st$rows)) {
    r <- st$rows[[i]]
    ctx <- kg_ctx(g, st$vars, r, params, cache = cache)
    args <- lapply(cl$args, function(a) kg_eval(a, ctx))
    res <- kg_call_proc(g, proc, args, summ)
    res_vars <- res$vars
    if (!length(res$rows)) next
    for (j in seq_along(res$rows)) rows[[length(rows) + 1]] <- c(r, res$rows[[j]])
  }
  vars <- c(st$vars, res_vars)
  if (!is.null(cl$yields)) {
    if (!identical(cl$yields[[1]]$name, "*")) {
      keep_names <- vapply(cl$yields, `[[`, "", "name")
      keep_alias <- vapply(cl$yields, `[[`, "", "alias")
      pos <- match(keep_names, vars)
      if (any(is.na(pos))) kg_abort("YIELD 了不存在的列: ",
                                    paste(keep_names[is.na(pos)], collapse = ", "))
      rows <- lapply(rows, function(rw) rw[pos])
      vars <- keep_alias
    }
  }
  out <- list(vars = vars, rows = rows, used = lapply(rows, function(x) integer(0)))
  if (!is.null(cl$where)) out <- kg_filter_stream(g, out, cl$where, params, cache)
  out
}

kg_call_proc <- function(g, proc, args, summ) {
  a1 <- if (length(args) >= 1) args[[1]] else NULL
  a2 <- if (length(args) >= 2) args[[2]] else NULL
  a3 <- if (length(args) >= 3) args[[3]] else NULL
  mkrows <- function(vars, cols) {
    n <- if (length(cols)) length(cols[[1]]) else 0L
    rows <- vector("list", n)
    for (i in seq_len(n)) {
      rows[[i]] <- lapply(cols, function(cc) {
        v <- if (is.list(cc)) cc[[i]] else cc[i]
        if (is.null(v)) kg_null else v
      })
    }
    list(vars = vars, rows = rows)
  }
  switch(proc,
    "db.index.fulltext.querynodes" = {
      idx <- as.character(a1); q <- as.character(a2)
      lim <- if (!is.null(a3) && is.list(a3) && !is.null(a3[["limit"]])) a3[["limit"]] else Inf
      hits <- kg_fulltext_search(g, idx, q, limit = lim)
      mkrows(c("node", "score"), list(lapply(hits$node, kg_node_value), as.list(hits$score)))
    },
    "db.index.fulltext.queryrelationships" = kg_abort("暂未实现关系全文索引"),
    "db.index.vector.querynodes" = {
      idx <- as.character(a1); k <- as.integer(a2); vec <- as.numeric(unlist(a3))
      hits <- kg_vector_search(g, idx, vec, k)
      mkrows(c("node", "score"), list(lapply(hits$node, kg_node_value), as.list(hits$score)))
    },
    "db.labels" = mkrows("label", list(as.list(names(g$label_ids)))),
    "db.relationshiptypes" = mkrows("relationshipType", list(as.list(names(g$rel_type_ids)))),
    "db.propertykeys" = {
      keys <- unique(c(names(g$nodes), names(g$rels)))
      mkrows("propertyKey", list(as.list(setdiff(keys, c(KG_NODE_RESERVED, KG_REL_RESERVED)))))
    },
    "db.schema.visualization" = kg_abort("请使用 kg_schema_graph() 生成本体鸟瞰图"),
    "dbms.components" = mkrows(c("name", "versions", "edition"),
                               list("Neo4j Kernel (R 兼容实现)",
                                    list(paste0("5.x-compatible / zhyhkg ", KG_ENGINE_VERSION)),
                                    "community")),
    "dbms.procedures" = {
      procs <- c("db.index.fulltext.queryNodes", "db.index.vector.queryNodes",
                 "db.labels", "db.relationshipTypes", "db.propertyKeys",
                 "dbms.components", "dbms.procedures", "db.indexes", "db.constraints")
      mkrows(c("name", "signature", "description"),
             list(procs, rep("()", length(procs)), rep("zhyhkg 内置过程", length(procs))))
    },
    "db.indexes" = {
      ix <- kg_index_list(g)
      mkrows(c("name", "type", "entityType", "labelsOrTypes", "properties"),
             list(ix$name, ix$type, rep("NODE", nrow(ix)), as.list(ix$label),
                  strsplit(ifelse(nzchar(ix$props), ix$props, ""), ",")))
    },
    "db.constraints" = {
      ct <- kg_constraint_list(g)
      mkrows(c("name", "type", "entityType", "labelsOrTypes"),
             list(ct$name, ct$type, rep("NODE", nrow(ct)), as.list(ct$label)))
    },
    "db.awaitindexes" = mkrows(character(0), list()),
    kg_abort("不支持的过程: ", proc)
  )
}

# ---------------------------------------------------------------------------
# SHOW
# ---------------------------------------------------------------------------

kg_exec_show <- function(g, cl, st, params) {
  what <- toupper(cl$what)
  res <- switch(what,
    INDEXES = {
      ix <- kg_index_list(g)
      list(vars = c("name", "state", "type", "entityType", "labelsOrTypes",
                    "properties", "indexProvider", "owningConstraint"),
           rows = lapply(seq_len(nrow(ix)), function(i) list(
             ix$name[i], ix$state[i], ix$type[i], "NODE", list(ix$label[i]),
             if (nzchar(ix$props[i])) as.list(strsplit(ix$props[i], ",")[[1]]) else list(),
             "token-lookup-1.0", kg_null)))
    },
    CONSTRAINTS = {
      ct <- kg_constraint_list(g)
      list(vars = c("name", "type", "entityType", "labelsOrTypes", "properties", "ownedIndex"),
           rows = lapply(seq_len(nrow(ct)), function(i) list(
             ct$name[i], ct$type[i], "NODE", list(ct$label[i]), list(ct$props[i]),
             paste0(ct$name[i], "_index"))))
    },
    PROCEDURES = {
      procs <- c("db.index.fulltext.queryNodes", "db.index.vector.queryNodes",
                 "db.labels", "db.relationshipTypes", "db.propertyKeys",
                 "dbms.components", "dbms.procedures", "db.indexes", "db.constraints")
      list(vars = c("name", "signature", "description"),
           rows = lapply(procs, function(p) list(p, "()", "zhyhkg 内置过程")))
    },
    FUNCTIONS = {
      fns <- c("labels", "type", "id", "elementId", "properties", "keys", "size",
               "coalesce", "toString", "toInteger", "toFloat", "substring", "collect")
      list(vars = c("name", "category", "description"),
           rows = lapply(fns, function(f) list(f, "scalar", "zhyhkg 内置函数")))
    },
    DATABASES = list(vars = c("name", "address", "role", "currentStatus"),
                     rows = list(list("neo4j", "localhost:7687", "primary", "online"))),
    kg_abort("暂不支持 SHOW ", cl$what)
  )
  out <- res
  if (!is.null(cl$yields)) {
    keep_names <- vapply(cl$yields, `[[`, "", "name")
    keep_alias <- vapply(cl$yields, `[[`, "", "alias")
    pos <- match(keep_names, res$vars)
    if (any(is.na(pos))) kg_abort("YIELD 了不存在的列: ",
                                  paste(keep_names[is.na(pos)], collapse = ", "))
    out$rows <- lapply(res$rows, function(r) r[pos])
    out$vars <- keep_alias
  }
  if (!is.null(cl$where)) out <- kg_filter_stream(g, out, cl$where, params, NULL)
  out
}

# ---------------------------------------------------------------------------
# 结果物化
# ---------------------------------------------------------------------------

kg_materialize <- function(g, st) {
  vars <- st$vars
  keep <- !startsWith(vars, ".")
  vars <- vars[keep]
  if (!length(st$rows)) {
    df <- data.frame(row.names = integer(0))
    for (v in vars) df[[v]] <- character(0)
    return(structure(df, class = c("kg_result", "data.frame")))
  }
  cols <- vector("list", length(vars))
  names(cols) <- vars
  for (j in seq_along(vars)) {
    pos <- match(vars[j], st$vars)
    vals <- lapply(st$rows, function(r) {
      v <- r[[pos]]
      if (is.null(v)) kg_null else v
    })
    cols[[j]] <- kg_simplify_column(vals)
  }
  # 直接构造 data.frame（避免 as.data.frame 对零长列表列做行数推断）
  n <- length(st$rows)
  for (j in seq_along(cols)) {
    if (is.list(cols[[j]]) && !is.data.frame(cols[[j]])) {
      class(cols[[j]]) <- c("AsIs", class(cols[[j]]))
    }
    if (length(cols[[j]]) != n) cols[[j]] <- rep(list(kg_null), n)
  }
  structure(cols, names = vars, row.names = seq_len(n),
            class = c("kg_result", "data.frame"))
}

kg_simplify_column <- function(vals) {
  kinds <- vapply(vals, function(v) {
    if (inherits(v, "kg_null")) "null"
    else if (inherits(v, "kg_node")) "node"
    else if (inherits(v, "kg_rel")) "rel"
    else if (inherits(v, "kg_path")) "path"
    else if (is.list(v)) "list"
    else "scalar"
  }, "")
  if (all(kinds %in% c("scalar", "null"))) {
    idx <- which(kinds == "scalar")
    if (!length(idx)) return(rep(NA, length(vals)))
    types <- unique(vapply(vals[idx], function(v) class(v)[1], ""))
    types <- setdiff(types, "kg_null")
    if (length(types) == 1L) {
      t <- types[1]
      out <- switch(t,
        character = vapply(vals, function(v) if (inherits(v, "kg_null") || is.null(v)) NA_character_ else as.character(v), ""),
        integer = vapply(vals, function(v) if (inherits(v, "kg_null") || is.null(v)) NA_integer_ else as.integer(v), 0L),
        numeric = vapply(vals, function(v) if (inherits(v, "kg_null") || is.null(v)) NA_real_ else as.numeric(v), 0),
        logical = vapply(vals, function(v) if (inherits(v, "kg_null") || is.null(v)) NA else as.logical(v), NA),
        NULL
      )
      if (!is.null(out)) return(out)
    }
    return(vals)
  }
  vals
}

#' @export
print.kg_result <- function(x, n = 10L, ...) {
  summ <- attr(x, "summary")
  g <- attr(x, "g")
  cat("<kg_result> ", nrow(x), " 行 x ", ncol(x), " 列",
      if (!is.null(attr(x, "elapsed"))) sprintf("（%.3fs）", attr(x, "elapsed")) else "", "\n", sep = "")
  df <- as.data.frame(x)
  if (!nrow(df)) {
    cat("(空结果)\n")
    if (!is.null(attr(x, "query"))) cat("查询: ", gsub("\\s+", " ", attr(x, "query")), "\n", sep = "")
    return(invisible(x))
  }
  show <- utils::head(df, n)
  for (j in seq_along(show)) {
    col <- show[[j]]
    if (is.list(col) && length(col) && any(vapply(col, function(v)
      is_node_value(v) || is_rel_value(v) || is_path_value(v), logical(1)))) {
      show[[j]] <- vapply(col, function(v) {
        if (inherits(v, "kg_null")) "null"
        else if (is_node_value(v)) {
          if (!is.null(g)) kg_format_value(g, v, 3L) else paste0("(:node#", v$idx, ")")
        }
        else if (is_rel_value(v)) {
          if (!is.null(g)) paste0("[:", g$rels$type[v$idx], "]") else paste0("[:rel#", v$idx, "]")
        }
        else if (is_path_value(v)) paste0("<path ", length(v$rels), " rels>")
        else if (is.list(v)) paste0("[", paste(vapply(v, function(e)
          if (inherits(e, "kg_null")) "null" else as.character(e), ""), collapse = ", "), "]")
        else as.character(v)
      }, "")
    } else if (is.list(col)) {
      show[[j]] <- vapply(col, function(v) {
        if (inherits(v, "kg_null")) "null"
        else if (!is.null(names(v))) paste0("{", paste(names(v), collapse = ","), "}")
        else paste0("[", length(v), " items]")
      }, "")
    }
  }
  print(show, row.names = FALSE)
  if (nrow(df) > n) cat("... 共 ", nrow(df), " 行\n", sep = "")
  if (!is.null(summ) && (summ$nodes_created + summ$relationships_created + summ$properties_set +
                         summ$nodes_deleted + summ$relationships_deleted) > 0) {
    cat("---\n")
    kg_summary_print(summ)
  }
  invisible(x)
}
