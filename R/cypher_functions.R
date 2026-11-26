
# =============================================================================
# cypher_functions.R — Cypher 标量函数 / 类型转换 / 三值逻辑
#
# 这些函数只处理"值"，不依赖图存储；图相关函数（labels/type/id/nodes/…）
# 在 cypher_eval.R 中实现。
# =============================================================================

# ---------------------------------------------------------------------------
# 类型转换
# ---------------------------------------------------------------------------

kg_to_string <- function(x) {
  if (inherits(x, "kg_null") || is.null(x)) return(kg_null)
  if (is.character(x)) return(x)
  if (is.logical(x)) return(if (is.na(x)) kg_null else if (x) "true" else "false")
  if (is.numeric(x)) {
    if (is.na(x)) return(kg_null)
    if (kg_num_is_int(x)) return(format(x, scientific = FALSE, trim = TRUE))
    return(format(x, digits = 15, scientific = FALSE, trim = TRUE))
  }
  if (is.list(x)) return(paste0("[", paste(vapply(x, function(e) {
    s <- kg_to_string(e); if (inherits(s, "kg_null")) "null" else s
  }, ""), collapse = ", "), "]"))
  as.character(x)
}

kg_to_number <- function(x, integer = FALSE) {
  if (inherits(x, "kg_null") || is.null(x)) return(kg_null)
  if (is.numeric(x)) {
    if (integer) return(as.integer(x))
    return(as.numeric(x))
  }
  if (is.logical(x)) return(if (is.na(x)) kg_null else as.numeric(x))
  if (is.character(x)) {
    s <- trimws(x)
    if (!nzchar(s)) return(kg_null)
    v <- suppressWarnings(as.numeric(s))
    if (is.na(v)) return(kg_null)
    if (integer) return(as.integer(v))
    return(v)
  }
  kg_null
}

kg_to_boolean <- function(x) {
  if (inherits(x, "kg_null") || is.null(x)) return(kg_null)
  if (is.logical(x)) return(x)
  if (is.character(x)) {
    s <- tolower(trimws(x))
    if (s == "true") return(TRUE)
    if (s == "false") return(FALSE)
    return(kg_null)
  }
  kg_null
}

# ---------------------------------------------------------------------------
# 三值逻辑
# ---------------------------------------------------------------------------

kg_bool3 <- function(x) {
  # 返回 TRUE / FALSE / NA(表示 null)
  if (inherits(x, "kg_null") || is.null(x)) return(NA)
  if (is.logical(x)) return(if (is.na(x)) NA else x)
  NA
}

kg_and <- function(a, b) {
  x <- kg_bool3(a); y <- kg_bool3(b)
  if (isFALSE(x) || isFALSE(y)) return(FALSE)
  if (is.na(x) || is.na(y)) return(kg_null)
  TRUE
}

kg_or <- function(a, b) {
  x <- kg_bool3(a); y <- kg_bool3(b)
  if (isTRUE(x) || isTRUE(y)) return(TRUE)
  if (is.na(x) || is.na(y)) return(kg_null)
  FALSE
}

kg_xor <- function(a, b) {
  x <- kg_bool3(a); y <- kg_bool3(b)
  if (is.na(x) || is.na(y)) return(kg_null)
  xor(x, y)
}

kg_not <- function(a) {
  x <- kg_bool3(a)
  if (is.na(x)) return(kg_null)
  !x
}

kg_is_true <- function(x) isTRUE(kg_bool3(x))

# ---------------------------------------------------------------------------
# 比较
# ---------------------------------------------------------------------------

kg_compare <- function(a, b) {
  # 返回 -1 / 0 / 1，不可比返回 NULL(kg_null)
  if (inherits(a, "kg_null") || inherits(b, "kg_null")) return(kg_null)
  if (inherits(a, "kg_node") || inherits(a, "kg_rel") || inherits(a, "kg_path") ||
      inherits(b, "kg_node") || inherits(b, "kg_rel") || inherits(b, "kg_path")) {
    ka <- kg_value_key(a); kb <- kg_value_key(b)
    return(if (ka < kb) -1L else if (ka > kb) 1L else 0L)
  }
  if (is.list(a) || is.list(b)) return(kg_null)
  if (is.logical(a) && is.logical(b)) {
    if (is.na(a) || is.na(b)) return(kg_null)
    return(if (a == b) 0L else if (a) 1L else -1L)
  }
  if (is.numeric(a) && is.numeric(b)) {
    if (is.na(a) || is.na(b)) return(kg_null)
    return(if (a < b) -1L else if (a > b) 1L else 0L)
  }
  if (is.character(a) && is.character(b)) {
    if (is.na(a) || is.na(b)) return(kg_null)
    return(if (a < b) -1L else if (a > b) 1L else 0L)
  }
  kg_null
}

kg_equal <- function(a, b) {
  if (inherits(a, "kg_null") || inherits(b, "kg_null")) return(kg_null)
  if (is.list(a) && is.list(b) && !inherits(a, "kg_node") && !inherits(b, "kg_node")) {
    if (length(a) != length(b)) return(FALSE)
    res <- Map(kg_equal, a, b)
    if (any(vapply(res, function(x) isFALSE(x), logical(1)))) return(FALSE)
    if (any(vapply(res, function(x) inherits(x, "kg_null"), logical(1)))) return(kg_null)
    return(TRUE)
  }
  c <- kg_compare(a, b)
  if (inherits(c, "kg_null")) {
    # 数字与字符串不可比，但在 Cypher 中 '1' = 1 为 false（不报错）
    if ((is.character(a) && is.numeric(b)) || (is.numeric(a) && is.character(b))) return(FALSE)
    return(kg_null)
  }
  identical(c, 0L)
}

kg_in_list <- function(x, lst) {
  if (inherits(x, "kg_null") || inherits(lst, "kg_null")) return(kg_null)
  if (!is.list(lst)) return(FALSE)
  if (!length(lst)) return(FALSE)
  hit_null <- FALSE
  for (e in lst) {
    r <- kg_equal(x, e)
    if (isTRUE(r)) return(TRUE)
    if (inherits(r, "kg_null")) hit_null <- TRUE
  }
  if (hit_null) kg_null else FALSE
}

kg_compare_op <- function(op, a, b) {
  c <- kg_compare(a, b)
  if (inherits(c, "kg_null")) return(kg_null)
  switch(op,
    "<" = c < 0L, "<=" = c <= 0L, ">" = c > 0L, ">=" = c >= 0L,
    "=" = c == 0L, "<>" = c != 0L,
    kg_null
  )
}

# ---------------------------------------------------------------------------
# 算术
# ---------------------------------------------------------------------------

kg_is_num <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)

kg_add <- function(a, b) {
  if (inherits(a, "kg_null") || inherits(b, "kg_null")) return(kg_null)
  if (is.list(a) && !inherits(a, "kg_node")) {
    if (is.list(b) && !inherits(b, "kg_node")) return(c(a, b))
    return(c(a, list(b)))
  }
  if (is.list(b) && !inherits(b, "kg_node")) return(c(list(a), b))
  if (is.character(a) || is.character(b)) {
    sa <- kg_to_string(a); sb <- kg_to_string(b)
    if (inherits(sa, "kg_null") || inherits(sb, "kg_null")) return(kg_null)
    return(paste0(sa, sb))
  }
  if (is.numeric(a) && is.numeric(b)) {
    if (is.na(a) || is.na(b)) return(kg_null)
    v <- a + b
    if (is.integer(a) && is.integer(b) && !is.na(v) && v <= .Machine$integer.max) return(as.integer(v))
    return(v)
  }
  kg_null
}

kg_num2 <- function(a, b, f) {
  if (!kg_is_num(a) || !kg_is_num(b)) return(kg_null)
  f(a, b)
}

kg_sub <- function(a, b) kg_num2(a, b, function(x, y) x - y)
kg_mul <- function(a, b) kg_num2(a, b, function(x, y) x * y)
kg_div <- function(a, b) {
  if (!kg_is_num(a) || !kg_is_num(b)) return(kg_null)
  if (b == 0) return(kg_null)
  a / b
}
kg_mod <- function(a, b) {
  if (!kg_is_num(a) || !kg_is_num(b) || b == 0) return(kg_null)
  a %% b
}
kg_pow <- function(a, b) kg_num2(a, b, function(x, y) x^y)
kg_neg <- function(a) if (!kg_is_num(a)) kg_null else -a

# ---------------------------------------------------------------------------
# 字符串
# ---------------------------------------------------------------------------

kg_str <- function(x) {
  s <- kg_to_string(x)
  if (inherits(s, "kg_null")) NULL else s
}

kg_substring <- function(s, start, len = NULL) {
  ss <- kg_str(s)
  if (is.null(ss)) return(kg_null)
  n <- nchar(ss, type = "chars")
  st <- kg_to_number(start, integer = TRUE)
  if (inherits(st, "kg_null")) return(kg_null)
  st <- as.integer(st)
  if (st < 0) st <- 0L
  if (st > n) return("")
  if (is.null(len) || inherits(len, "kg_null")) {
    return(substr(ss, st + 1L, n))
  }
  ln <- as.integer(kg_to_number(len, integer = TRUE))
  if (is.na(ln) || ln <= 0) return("")
  substr(ss, st + 1L, min(n, st + ln))
}

kg_left <- function(s, n) {
  ss <- kg_str(s); if (is.null(ss)) return(kg_null)
  n <- kg_to_number(n, integer = TRUE)
  if (inherits(n, "kg_null")) return(kg_null)
  substr(ss, 1L, max(0L, as.integer(n)))
}

kg_right <- function(s, n) {
  ss <- kg_str(s); if (is.null(ss)) return(kg_null)
  n <- kg_to_number(n, integer = TRUE)
  if (inherits(n, "kg_null")) return(kg_null)
  nn <- nchar(ss, type = "chars"); k <- max(0L, as.integer(n))
  if (k == 0L) return("")
  substr(ss, max(1L, nn - k + 1L), nn)
}

kg_split <- function(s, sep) {
  ss <- kg_str(s); sp <- kg_str(sep)
  if (is.null(ss) || is.null(sp)) return(kg_null)
  strsplit(ss, sp, fixed = TRUE)[[1]]
}

kg_replace <- function(s, search, repl) {
  ss <- kg_str(s); se <- kg_str(search); rp <- kg_str(repl)
  if (is.null(ss) || is.null(se) || is.null(rp)) return(kg_null)
  gsub(se, rp, ss, fixed = TRUE)
}

kg_reverse <- function(x) {
  if (inherits(x, "kg_null")) return(kg_null)
  if (is.character(x)) {
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    return(paste(rev(chars), collapse = ""))
  }
  if (is.list(x)) return(rev(x))
  kg_null
}

kg_size <- function(x) {
  if (inherits(x, "kg_null") || is.null(x)) return(0L)
  if (is.list(x)) return(length(x))
  if (is.character(x)) return(nchar(x, type = "chars"))
  if (is.numeric(x) || is.logical(x)) return(1L)
  0L
}

# ---------------------------------------------------------------------------
# 列表
# ---------------------------------------------------------------------------

kg_head <- function(x) {
  if (!is.list(x) || !length(x)) return(kg_null)
  x[[1]]
}
kg_last <- function(x) {
  if (!is.list(x) || !length(x)) return(kg_null)
  x[[length(x)]]
}
kg_tail <- function(x) {
  if (!is.list(x) || length(x) <= 1L) return(list())
  x[-1]
}
kg_range <- function(start, end, step = 1) {
  s <- kg_to_number(start, integer = TRUE); e <- kg_to_number(end, integer = TRUE)
  st <- kg_to_number(step, integer = TRUE)
  if (inherits(s, "kg_null") || inherits(e, "kg_null")) return(kg_null)
  if (inherits(st, "kg_null") || st == 0) st <- 1L
  if ((s > e && st > 0) || (s < e && st < 0)) return(list())
  as.list(seq(as.integer(s), as.integer(e), by = as.integer(st)))
}

kg_index_value <- function(x, i) {
  if (inherits(x, "kg_null") || is.null(x)) return(kg_null)
  if (is.list(x)) {
    if (inherits(x, "kg_node") || inherits(x, "kg_rel") || inherits(x, "kg_path")) return(kg_null)
    n <- length(x)
    ii <- kg_to_number(i, integer = TRUE)
    if (inherits(ii, "kg_null")) return(kg_null)
    ii <- as.integer(ii)
    if (ii < 0) ii <- n + ii
    if (ii < 0 || ii >= n) return(kg_null)
    return(x[[ii + 1L]])
  }
  if (is.character(x) && is.character(i)) return(kg_null)  # 映射下标在 eval 中处理
  kg_null
}

kg_slice_value <- function(x, from, to) {
  if (inherits(x, "kg_null") || is.null(x)) return(kg_null)
  if (is.character(x)) {
    n <- nchar(x, type = "chars")
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    idx <- kg_slice_index(n, from, to)
    return(paste(chars[idx], collapse = ""))
  }
  if (is.list(x)) {
    if (inherits(x, "kg_node") || inherits(x, "kg_rel") || inherits(x, "kg_path")) return(kg_null)
    n <- length(x)
    idx <- kg_slice_index(n, from, to)
    return(x[idx])
  }
  kg_null
}

kg_slice_index <- function(n, from, to) {
  f <- if (is.null(from)) 0L else {
    v <- kg_to_number(from, integer = TRUE)
    if (inherits(v, "kg_null")) 0L else as.integer(v)
  }
  t <- if (is.null(to)) n else {
    v <- kg_to_number(to, integer = TRUE)
    if (inherits(v, "kg_null")) n else as.integer(v)
  }
  if (f < 0) f <- max(0L, n + f)
  if (t < 0) t <- max(0L, n + t)
  f <- min(f, n); t <- min(t, n)
  if (t <= f) return(integer(0))
  # 列表切片包含 to（Cypher [0..2] 取 3 个元素? 实际 [0..2] 取 2 个）
  seq.int(f + 1L, t)
}

# ---------------------------------------------------------------------------
# 数学
# ---------------------------------------------------------------------------

kg_math1 <- function(x, f) {
  v <- kg_to_number(x)
  if (inherits(v, "kg_null")) return(kg_null)
  f(v)
}

kg_round <- function(x, mode = "HALF_UP", digits = 0) {
  v <- kg_to_number(x)
  d <- kg_to_number(digits, integer = TRUE)
  if (inherits(v, "kg_null")) return(kg_null)
  if (!inherits(d, "kg_null") && d > 0) return(round(v, as.integer(d)))
  switch(toupper(mode),
    HALF_UP = floor(v + 0.5),
    CEILING = ceiling(v),
    FLOOR = floor(v),
    DOWN = trunc(v),
    UP = if (v >= 0) ceiling(v) else floor(v),
    floor(v + 0.5)
  )
}
