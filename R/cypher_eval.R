
# =============================================================================
# cypher_eval.R — 表达式求值
#
# 求值上下文 ctx：
#   g         图存储
#   vars      当前流（stream）的变量名（与 row 按下标对齐）
#   row       当前行（无名 list，与 vars 等长）
#   params    查询参数（$name）
#   agg_rows  聚合上下文：所在分组的全部行；NULL 表示非聚合求值
#   cache     查询级缓存（环境），用于模式存在判断的向量化
#
# 值域：NULL 用 kg_null 单例；节点/关系/路径用轻量句柄；列表用无名 list；
#       映射用命名 list。
# =============================================================================

kg_ctx <- function(g, vars, row, params = list(), agg_rows = NULL, cache = NULL) {
  list(g = g, vars = vars, row = row, params = params,
       agg_rows = agg_rows, cache = cache)
}

kg_ctx_with_row <- function(ctx, row) {
  ctx$row <- row
  ctx
}

kg_row_get <- function(ctx, var) {
  i <- match(var, ctx$vars)
  if (is.na(i) || i > length(ctx$row)) return(kg_null)
  v <- ctx$row[[i]]
  if (is.null(v)) kg_null else v
}

# ---------------------------------------------------------------------------
# 主入口
# ---------------------------------------------------------------------------

kg_eval <- function(e, ctx) {
  if (is.null(e)) return(kg_null)
  switch(e$t,
    lit = e$v,
    var = kg_row_get(ctx, e$name),
    param = {
      v <- ctx$params[[e$name]]
      if (is.null(v)) kg_null else v
    },
    wildcard = kg_null,
    prop = {
      obj <- kg_eval(e$obj, ctx)
      kg_prop_access(ctx$g, obj, e$key)
    },
    index = {
      obj <- kg_eval(e$obj, ctx)
      i <- kg_eval(e$idx, ctx)
      kg_index_access(obj, i)
    },
    slice = {
      obj <- kg_eval(e$obj, ctx)
      from <- if (is.null(e$from)) NULL else kg_eval(e$from, ctx)
      to <- if (is.null(e$to)) NULL else kg_eval(e$to, ctx)
      kg_slice_value(obj, from, to)
    },
    list = lapply(e$items, function(x) kg_eval(x, ctx)),
    map = {
      out <- lapply(e$vals, function(x) kg_eval(x, ctx))
      names(out) <- e$keys
      out
    },
    bin = kg_eval_bin(e, ctx),
    not = kg_not(kg_eval(e$e, ctx)),
    neg = kg_neg(kg_eval(e$e, ctx)),
    isnull = inherits(kg_eval(e$e, ctx), "kg_null"),
    notnull = !inherits(kg_eval(e$e, ctx), "kg_null"),
    haslabel = {
      v <- kg_eval(e$e, ctx)
      if (!is_node_value(v)) kg_null else kg_node_has_label(ctx$g, v$idx, e$label)
    },
    `in` = kg_in_list(kg_eval(e$l, ctx), kg_eval(e$r, ctx)),
    case = kg_eval_case(e, ctx),
    call = kg_eval_call(e, ctx),
    agg = {
      if (is.null(ctx$agg_rows)) {
        # 非分组上下文（例如 WHERE 中误用聚合）
        kg_eval_agg(e, ctx, list(ctx$row))
      } else {
        kg_eval_agg(e, ctx, ctx$agg_rows)
      }
    },
    listcomp = kg_eval_listcomp(e, ctx),
    patterncomp = kg_eval_patterncomp(e, ctx),
    patternexists = kg_eval_patternexists(e, ctx),
    kg_null
  )
}

kg_prop_access <- function(g, obj, key) {
  if (inherits(obj, "kg_null") || is.null(obj)) return(kg_null)
  if (is_node_value(obj)) return(kg_node_prop(g, obj$idx, key))
  if (is_rel_value(obj)) return(kg_rel_prop(g, obj$idx, key))
  if (is.list(obj) && !is.null(names(obj))) {
    v <- obj[[key]]
    if (is.null(v)) return(kg_null)
    return(v)
  }
  kg_null
}

kg_index_access <- function(obj, i) {
  if (inherits(obj, "kg_null") || is.null(obj)) return(kg_null)
  if (is.list(obj) && !is_node_value(obj) && !is_rel_value(obj) && !is_path_value(obj)) {
    if (is.character(i) && !is.null(names(obj))) {
      v <- obj[[i]]
      if (is.null(v)) return(kg_null)
      return(v)
    }
    return(kg_index_value(obj, i))
  }
  if (is.character(obj)) {
    if (!is.numeric(i)) return(kg_null)
    return(kg_index_value(strsplit(obj, "", fixed = TRUE)[[1]], i))
  }
  kg_null
}

# ---------------------------------------------------------------------------
# 二元运算符
# ---------------------------------------------------------------------------

kg_eval_bin <- function(e, ctx) {
  op <- e$op
  a <- kg_eval(e$l, ctx)
  if (op == "AND") {
    if (isFALSE(kg_bool3(a))) return(FALSE)
    return(kg_and(a, kg_eval(e$r, ctx)))
  }
  if (op == "OR") {
    if (isTRUE(kg_bool3(a))) return(TRUE)
    return(kg_or(a, kg_eval(e$r, ctx)))
  }
  b <- kg_eval(e$r, ctx)
  switch(op,
    XOR = kg_xor(a, b),
    "=" = kg_equal(a, b),
    "<>" = {
      v <- kg_equal(a, b)
      if (inherits(v, "kg_null")) kg_null else !v
    },
    "<" = , "<=" = , ">" = , ">=" = kg_compare_op(op, a, b),
    "+" = kg_add(a, b),
    "-" = kg_sub(a, b),
    "*" = kg_mul(a, b),
    "/" = kg_div(a, b),
    "%" = kg_mod(a, b),
    "^" = kg_pow(a, b),
    "=~" = {
      sa <- kg_str(a); sb <- kg_str(b)
      if (is.null(sa) || is.null(sb)) kg_null else {
        full <- if (substr(sb, 1, 1) == "^") sb else paste0("^(?:", sb, ")$")
        isTRUE(tryCatch(grepl(full, sa, perl = TRUE), error = function(err) NA))
      }
    },
    "CONTAINS" = {
      sa <- kg_str(a); sb <- kg_str(b)
      if (is.null(sa) || is.null(sb)) kg_null else grepl(sb, sa, fixed = TRUE)
    },
    "STARTS WITH" = {
      sa <- kg_str(a); sb <- kg_str(b)
      if (is.null(sa) || is.null(sb)) kg_null else startsWith(sa, sb)
    },
    "ENDS WITH" = {
      sa <- kg_str(a); sb <- kg_str(b)
      if (is.null(sa) || is.null(sb)) kg_null else endsWith(sa, sb)
    },
    kg_null
  )
}

# ---------------------------------------------------------------------------
# CASE
# ---------------------------------------------------------------------------

kg_eval_case <- function(e, ctx) {
  if (!is.null(e$expr)) {
    base <- kg_eval(e$expr, ctx)
    for (w in e$whens) {
      v <- kg_equal(base, kg_eval(w$when, ctx))
      if (isTRUE(v)) return(kg_eval(w$then, ctx))
    }
  } else {
    for (w in e$whens) {
      v <- kg_bool3(kg_eval(w$when, ctx))
      if (isTRUE(v)) return(kg_eval(w$then, ctx))
    }
  }
  if (!is.null(e$els)) return(kg_eval(e$els, ctx))
  kg_null
}

# ---------------------------------------------------------------------------
# 列表推导 / 模式推导
# ---------------------------------------------------------------------------

kg_eval_listcomp <- function(e, ctx) {
  src <- kg_eval(e$source, ctx)
  if (inherits(src, "kg_null") || !is.list(src)) return(list())
  out <- vector("list", length(src))
  k <- 0L
  for (i in seq_along(src)) {
    ctx2 <- ctx
    ctx2$vars <- c(ctx$vars, e$var)
    ctx2$row <- c(ctx$row, list(src[[i]]))
    if (!is.null(e$where)) {
      ok <- kg_bool3(kg_eval(e$where, ctx2))
      if (!isTRUE(ok)) next
    }
    k <- k + 1L
    out[[k]] <- if (is.null(e$map)) src[[i]] else kg_eval(e$map, ctx2)
  }
  out[seq_len(k)]
}

kg_eval_patterncomp <- function(e, ctx) {
  st <- list(vars = ctx$vars, rows = list(ctx$row), used = list(integer(0)))
  res <- kg_match_part(ctx$g, st, e$pattern, ctx$params, ctx$cache)
  if (!length(res$rows)) return(list())
  out <- vector("list", length(res$rows))
  for (i in seq_along(res$rows)) {
    ctx2 <- kg_ctx(ctx$g, res$vars, res$rows[[i]], ctx$params, cache = ctx$cache)
    if (!is.null(e$where)) {
      ok <- kg_bool3(kg_eval(e$where, ctx2))
      if (!isTRUE(ok)) next
    }
    out[[i]] <- kg_eval(e$map, ctx2)
  }
  Filter(function(x) !is.null(x), out)
}

kg_eval_patternexists <- function(e, ctx) {
  pat <- e$pattern
  # 优化：2 节点 1 关系定长模式的向量化存在性
  fast <- kg_pattern_fastset(ctx, pat)
  if (!is.null(fast)) return(fast)
  st <- list(vars = ctx$vars, rows = list(ctx$row), used = list(integer(0)))
  res <- tryCatch(kg_match_part(ctx$g, st, pat, ctx$params, ctx$cache),
                  error = function(err) NULL)
  if (is.null(res)) return(kg_null)
  length(res$rows) > 0L
}

# 2 节点 1 关系模式：预计算"锚点节点集合"，之后逐行 O(1) 判断
kg_pattern_fastset <- function(ctx, pat) {
  if (length(pat$nodes) != 2L || length(pat$rels) != 1L) return(NULL)
  rel <- pat$rels[[1]]
  if (isTRUE(rel$varlen)) return(NULL)
  n0 <- pat$nodes[[1]]; n1 <- pat$nodes[[2]]
  v0 <- n0$var; v1 <- n1$var
  i0 <- if (is.null(v0)) NA_integer_ else match(v0, ctx$vars)
  i1 <- if (is.null(v1)) NA_integer_ else match(v1, ctx$vars)
  bound0 <- !is.na(i0); bound1 <- !is.na(i1)
  if (!bound0 && !bound1) return(NULL)
  anchor_is_0 <- bound0

  cache <- ctx$cache
  key <- paste0("\u0001pex:", kg_pattern_sig(pat),
                if (anchor_is_0) ":0" else ":1")
  if (!is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
    valid_mask <- get(key, envir = cache, inherits = FALSE)
  } else {
    g <- ctx$g
    edges <- kg_rel_ids_of_type(g, if (length(rel$types)) rel$types else NULL)
    valid_mask <- logical(nrow(g$nodes))
    if (length(edges)) {
      # 关系属性过滤（仅字面量）
      if (!is.null(rel$props)) {
        pv <- kg_eval_props(g, rel$props, ctx)
        keep <- rep(TRUE, length(edges))
        for (p in names(pv)) {
          v <- pv[[p]]
          if (inherits(v, "kg_null") || is.null(v)) next
          if (!p %in% names(g$rels)) { keep[] <- FALSE; break }
          keep <- keep & !is.na(g$rels[[p]][edges]) & g$rels[[p]][edges] == v
        }
        edges <- edges[keep]
      }
      if (length(edges)) {
        from <- g$rels$src[edges]; to <- g$rels$dst[edges]
        if (identical(rel$dir, "in")) { tmp <- from; from <- to; to <- tmp }
        if (identical(rel$dir, "both")) {
          from <- c(from, g$rels$dst[edges]); to <- c(to, g$rels$src[edges])
        }
        ok1 <- kg_nodes_match_mask(g, to, n1, ctx)
        ok0 <- kg_nodes_match_mask(g, from, n0, ctx)
        valid <- if (anchor_is_0) unique(from[ok1]) else unique(to[ok0])
        valid_mask[valid] <- TRUE
      }
    }
    if (!is.null(cache)) assign(key, valid_mask, envir = cache)
  }
  node <- kg_row_get(ctx, if (anchor_is_0) v0 else v1)
  if (!is_node_value(node)) return(kg_null)
  isTRUE(valid_mask[node$idx])
}

kg_pattern_sig <- function(pat) {
  # 属性必须带值：(:Toxicity {name:'有毒'}) 与 {name:'小毒'} 不能共用缓存
  prop_sig <- function(props) {
    if (is.null(props) || !length(props)) return("")
    vals <- vapply(props, function(v) {
      if (inherits(v, "kg_null") || is.null(v)) "NULL" else paste(as.character(v), collapse = ",")
    }, "")
    paste(names(props), vals, sep = "=", collapse = ",")
  }
  parts <- character(0)
  for (i in seq_along(pat$nodes)) {
    n <- pat$nodes[[i]]
    parts <- c(parts, paste0("(", n$var %||% "", ":", paste(n$labels, collapse = "&"),
                             "{", prop_sig(n$props), "}"))
    if (i <= length(pat$rels)) {
      r <- pat$rels[[i]]
      parts <- c(parts, paste0("[", paste(r$types, collapse = "|"), r$dir,
                               if (isTRUE(r$varlen)) "*" else "",
                               "{", prop_sig(r$props), "}]"))
    }
  }
  paste(parts, collapse = "")
}

# ---------------------------------------------------------------------------
# 函数调用
# ---------------------------------------------------------------------------

kg_call_function <- function(name, args, ctx, node) {
  lname <- tolower(name)
  g <- ctx$g
  a1 <- if (length(args) >= 1) args[[1]] else kg_null
  a2 <- if (length(args) >= 2) args[[2]] else kg_null
  a3 <- if (length(args) >= 3) args[[3]] else kg_null

  switch(lname,
    # ---- 转换 ----
    tostring = kg_to_string(a1),
    tointeger = kg_to_number(a1, integer = TRUE),
    tofloat = kg_to_number(a1, integer = FALSE),
    toboolean = kg_to_boolean(a1),
    # ---- 字符串 ----
    toupper = , upper = { s <- kg_str(a1); if (is.null(s)) kg_null else toupper(s) },
    tolower = , lower = { s <- kg_str(a1); if (is.null(s)) kg_null else tolower(s) },
    trim = { s <- kg_str(a1); if (is.null(s)) kg_null else trimws(s, "both") },
    ltrim = { s <- kg_str(a1); if (is.null(s)) kg_null else trimws(s, "left") },
    rtrim = { s <- kg_str(a1); if (is.null(s)) kg_null else trimws(s, "right") },
    replace = kg_replace(a1, a2, a3),
    split = kg_split(a1, a2),
    substring = kg_substring(a1, a2, if (length(args) >= 3) a3 else NULL),
    left = kg_left(a1, a2),
    right = kg_right(a1, a2),
    reverse = kg_reverse(a1),
    normalize = { s <- kg_str(a1); if (is.null(s)) kg_null else s },
    # ---- 数学 ----
    abs = kg_math1(a1, abs),
    ceil = kg_math1(a1, ceiling),
    floor = kg_math1(a1, floor),
    round = {
      mode <- if (length(args) >= 2 && is.character(a2)) a2 else "HALF_UP"
      dig <- if (length(args) >= 2 && is.numeric(a2)) a2 else if (length(args) >= 3) a3 else 0
      kg_round(a1, mode, dig)
    },
    sign = kg_math1(a1, sign),
    sqrt = kg_math1(a1, sqrt),
    exp = kg_math1(a1, exp),
    log = kg_math1(a1, log),
    log10 = kg_math1(a1, log10),
    sin = kg_math1(a1, sin),
    cos = kg_math1(a1, cos),
    tan = kg_math1(a1, tan),
    asin = kg_math1(a1, asin),
    acos = kg_math1(a1, acos),
    atan = kg_math1(a1, atan),
    atan2 = kg_num2(a1, a2, atan2),
    degrees = kg_math1(a1, function(x) x * 180 / pi),
    radians = kg_math1(a1, function(x) x * pi / 180),
    pi = pi,
    e = exp(1),
    rand = stats::runif(1),
    # ---- 列表 ----
    size = kg_size(a1),
    length = {
      if (is_path_value(a1)) length(a1$rels) else kg_size(a1)
    },
    char_length = kg_size(a1),
    head = kg_head(a1),
    last = kg_last(a1),
    tail = kg_tail(a1),
    range = {
      if (length(args) >= 3) kg_range(a1, a2, a3) else kg_range(a1, a2, 1)
    },
    keys = {
      if (inherits(a1, "kg_null")) kg_null
      else if (is_node_value(a1)) as.list(names(kg_node_props(g, a1$idx)))
      else if (is_rel_value(a1)) as.list(names(kg_rel_props(g, a1$idx)))
      else if (is.list(a1) && !is.null(names(a1))) as.list(names(a1))
      else list()
    },
    # ---- 图 ----
    labels = {
      if (is_node_value(a1)) as.list(kg_node_labels(g, a1$idx)) else if (inherits(a1, "kg_null")) kg_null else list()
    },
    type = {
      if (is_rel_value(a1)) g$rels$type[a1$idx] else if (inherits(a1, "kg_null")) kg_null else kg_null
    },
    id = {
      if (is_node_value(a1)) a1$idx else if (is_rel_value(a1)) a1$idx else kg_null
    },
    elementid = {
      if (is_node_value(a1)) paste0("4:kg:", a1$idx)
      else if (is_rel_value(a1)) paste0("5:kg:", a1$idx)
      else kg_null
    },
    properties = {
      if (is_node_value(a1)) kg_node_props(g, a1$idx)
      else if (is_rel_value(a1)) kg_rel_props(g, a1$idx)
      else kg_null
    },
    startnode = {
      if (is_rel_value(a1)) kg_node_value(g$rels$src[a1$idx]) else kg_null
    },
    endnode = {
      if (is_rel_value(a1)) kg_node_value(g$rels$dst[a1$idx]) else kg_null
    },
    nodes = {
      if (is_path_value(a1)) lapply(a1$nodes, kg_node_value) else kg_null
    },
    relationships = {
      if (is_path_value(a1)) lapply(a1$rels, kg_rel_value) else kg_null
    },
    coalesce = {
      for (v in args) if (!inherits(v, "kg_null")) return(v)
      kg_null
    },
    nullif = {
      eq <- kg_equal(a1, a2)
      if (isTRUE(eq)) kg_null else a1
    },
    exists = !inherits(a1, "kg_null"),
    isempty = {
      if (is.character(a1) && !is.na(a1)) return(!nzchar(a1))
      kg_size(a1) == 0L
    },
    # ---- 时间（简化：以 ISO 字符串 / 毫秒时间戳表示）----
    timestamp = as.numeric(as.POSIXct(Sys.time())) * 1000,
    date = format(Sys.time(), "%Y-%m-%d"),
    datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    localdatetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    localtime = format(Sys.time(), "%H:%M:%OS3"),
    # ---- 其它 ----
    kg_null
  )
}

# ---------------------------------------------------------------------------
# 函数调用：高频图函数走快路径（避免 80 分支 switch 的线性匹配开销）
# ---------------------------------------------------------------------------

kg_eval_call <- function(e, ctx) {
  lname <- tolower(e$name)
  n <- length(e$args)
  if (n == 1L) {
    if (lname == "type") {
      v <- kg_eval(e$args[[1]], ctx)
      return(if (is_rel_value(v)) ctx$g$rels$type[v$idx] else kg_null)
    }
    if (lname == "labels") {
      v <- kg_eval(e$args[[1]], ctx)
      if (is_node_value(v)) return(as.list(kg_node_labels(ctx$g, v$idx)))
      return(if (inherits(v, "kg_null")) kg_null else list())
    }
    if (lname == "id" || lname == "elementid") {
      v <- kg_eval(e$args[[1]], ctx)
      if (!is_node_value(v) && !is_rel_value(v)) return(kg_null)
      if (lname == "id") return(v$idx)
      return(if (is_node_value(v)) paste0("4:kg:", v$idx) else paste0("5:kg:", v$idx))
    }
    if (lname == "size") return(kg_size(kg_eval(e$args[[1]], ctx)))
    if (lname == "tostring") return(kg_to_string(kg_eval(e$args[[1]], ctx)))
    if (lname == "tointeger") return(kg_to_number(kg_eval(e$args[[1]], ctx), integer = TRUE))
    if (lname == "tofloat") return(kg_to_number(kg_eval(e$args[[1]], ctx), integer = FALSE))
  }
  args <- lapply(e$args, function(x) kg_eval(x, ctx))
  kg_call_function(e$name, args, ctx, e)
}

# ---------------------------------------------------------------------------
# 聚合函数
# ---------------------------------------------------------------------------

kg_eval_agg <- function(e, ctx, rows) {
  name <- tolower(e$name)
  n_rows <- length(rows)
  if (isTRUE(e$star)) {
    if (name == "count") return(as.integer(n_rows))
    return(kg_null)
  }
  # 快路径：count(var) / collect(var)
  arg <- e$args[[1]]
  if (identical(arg$t, "var")) {
    pos <- match(arg$name, ctx$vars)
    vals <- if (is.na(pos)) rep(list(kg_null), n_rows) else lapply(rows, function(r) {
      v <- r[[pos]]; if (is.null(v)) kg_null else v
    })
  } else {
    vals <- lapply(rows, function(r) {
      ctx2 <- ctx; ctx2$row <- r; ctx2$agg_rows <- NULL
      kg_eval(arg, ctx2)
    })
  }
  nn <- !vapply(vals, function(v) inherits(v, "kg_null"), logical(1))
  vals_nn <- vals[nn]
  if (isTRUE(e$distinct)) {
    keys <- vapply(vals_nn, kg_value_key, "")
    vals_nn <- vals_nn[!duplicated(keys)]
  }
  switch(name,
    count = as.integer(length(vals_nn)),
    sum = {
      nums <- Filter(function(v) is.numeric(v) && length(v) == 1L && !is.na(v), vals_nn)
      if (!length(nums)) 0L else sum(unlist(nums))
    },
    avg = {
      nums <- Filter(function(v) is.numeric(v) && length(v) == 1L && !is.na(v), vals_nn)
      if (!length(nums)) kg_null else mean(unlist(nums))
    },
    min = kg_agg_extreme(vals_nn, TRUE),
    max = kg_agg_extreme(vals_nn, FALSE),
    collect = vals_nn,
    stdev = {
      nums <- unlist(Filter(function(v) is.numeric(v), vals_nn))
      if (length(nums) < 2L) 0 else stats::sd(nums)
    },
    stdevp = {
      nums <- unlist(Filter(function(v) is.numeric(v), vals_nn))
      if (!length(nums)) 0 else sqrt(mean((nums - mean(nums))^2))
    },
    percentilecont = {
      nums <- sort(unlist(Filter(function(v) is.numeric(v), vals_nn)))
      p <- as.numeric(ctx$params[["p"]] %||% 0.5)
      if (!length(nums)) kg_null else stats::quantile(nums, p, names = FALSE, type = 7)
    },
    percentiledisc = {
      nums <- sort(unlist(Filter(function(v) is.numeric(v), vals_nn)))
      if (!length(nums)) kg_null else nums[ceiling(length(nums) / 2)]
    },
    kg_null
  )
}

kg_agg_extreme <- function(vals, minimum = TRUE) {
  vals <- Filter(function(v) !inherits(v, "kg_null") && !is.list(v), vals)
  if (!length(vals)) return(kg_null)
  best <- vals[[1]]
  if (length(vals) > 1) {
    for (i in 2:length(vals)) {
      c <- kg_compare(vals[[i]], best)
      if (inherits(c, "kg_null")) next
      if ((minimum && c < 0L) || (!minimum && c > 0L)) best <- vals[[i]]
    }
  }
  best
}

# ---------------------------------------------------------------------------
# 属性映射求值
# ---------------------------------------------------------------------------

kg_eval_props <- function(g, props, ctx) {
  if (is.null(props) || !length(props$vals)) return(list())
  out <- lapply(props$vals, function(x) kg_eval(x, ctx))
  names(out) <- props$keys
  out
}

kg_props_literal <- function(props) {
  if (is.null(props) || !length(props$vals)) return(TRUE)
  all(vapply(props$vals, function(x) x$t %in% c("lit", "param", "list", "map"), logical(1)))
}
