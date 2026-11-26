
# =============================================================================
# aaa_utils.R — 基础工具：NULL 语义、值句柄、断言、计时、日志
#
# 设计要点
#   * Cypher 的 NULL 与 R 的 NULL 语义不同：Cypher 的 NULL 可以作为值参与
#     比较、可以放进列表、可以被返回。R 的 NULL 会从 list 中"消失"，
#     因此这里用单例对象 kg_null 表示 Cypher NULL。
#   * 节点/关系/路径在表达式求值过程中只传"句柄"（idx），属性按需回表查询，
#     避免在 29 万行结果里复制属性。
# =============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# data.table 的 `[` 方法只有在 data.table 命名空间已载入、且调用方声明 aware 时
# 才会被 S3 派发；否则会退化成 `[.data.frame`（`with = FALSE` 直接报错）。
.datatable.aware <- TRUE
if (requireNamespace("data.table", quietly = TRUE)) {
  # 触发命名空间载入与 S3 方法注册（源码模式 / 包模式都安全）
  invisible(data.table::data.table())
}

# ---------------------------------------------------------------------------
# Cypher NULL 单例
# ---------------------------------------------------------------------------
kg_null <- structure(list(), class = "kg_null")
is_kg_null <- function(x) inherits(x, "kg_null")
null_if <- function(x) if (is.null(x)) kg_null else x

# ---------------------------------------------------------------------------
# 值句柄
# ---------------------------------------------------------------------------
kg_node_value <- function(idx) structure(list(idx = as.integer(idx)), class = "kg_node")
kg_rel_value  <- function(idx) structure(list(idx = as.integer(idx)), class = "kg_rel")
kg_path_value <- function(nodes, rels) {
  structure(list(nodes = as.integer(nodes), rels = as.integer(rels)), class = "kg_path")
}
is_node_value <- function(x) inherits(x, "kg_node")
is_rel_value  <- function(x) inherits(x, "kg_rel")
is_path_value <- function(x) inherits(x, "kg_path")

kg_value_kind <- function(x) {
  if (inherits(x, "kg_null")) return("null")
  if (inherits(x, "kg_node")) return("node")
  if (inherits(x, "kg_rel")) return("rel")
  if (inherits(x, "kg_path")) return("path")
  if (is.logical(x) && length(x) == 1L) return("boolean")
  if (is.numeric(x) && length(x) == 1L) return("number")
  if (is.character(x)) return("string")
  if (is.list(x)) return("list")
  "other"
}

# 用于 GROUP BY / DISTINCT 的稳定键
kg_value_key <- function(x) {
  if (inherits(x, "kg_null") || is.null(x)) return("\u0001N")
  if (inherits(x, "kg_node")) return(paste0("\u0001n", x$idx))
  if (inherits(x, "kg_rel"))  return(paste0("\u0001r", x$idx))
  if (inherits(x, "kg_path")) return(paste0("\u0001p", paste(x$nodes, collapse = ","),
                                            "|", paste(x$rels, collapse = ",")))
  if (is.character(x)) {
    if (length(x) != 1L) return(paste0("\u0001S", paste(x, collapse = "\u0002")))
    if (is.na(x)) return("\u0001N")
    return(paste0("\u0001s", x))
  }
  if (is.numeric(x)) {
    if (length(x) != 1L) return(paste0("\u0001D", paste(sprintf("%.15g", x), collapse = ",")))
    if (is.na(x)) return("\u0001N")
    return(paste0("\u0001d", sprintf("%.15g", x)))
  }
  if (is.logical(x)) {
    if (length(x) != 1L) return(paste0("\u0001B", paste(as.integer(x), collapse = ",")))
    if (is.na(x)) return("\u0001N")
    return(if (x) "\u0001bT" else "\u0001bF")
  }
  if (is.list(x)) {
    if (!length(x)) return("\u0001L")
    return(paste0("\u0001l", paste(vapply(x, kg_value_key, ""), collapse = "\u0002")))
  }
  paste0("\u0001o", paste(as.character(x), collapse = ","))
}

# 把行内多个值拼成分组键
kg_row_key <- function(vals) {
  n <- length(vals)
  if (n == 0L) return("")
  if (n == 1L) return(kg_value_key(vals[[1]]))
  paste(vapply(vals, kg_value_key, ""), collapse = "\u0003")
}

# ---------------------------------------------------------------------------
# 断言 / 错误 / 提示
# ---------------------------------------------------------------------------
kg_abort <- function(...) stop(paste0(...), call. = FALSE)
kg_assert <- function(cond, ...) if (!isTRUE(cond)) kg_abort(...)

kg_msg <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message(paste0(...))
}

kg_time <- function(expr, verbose = TRUE, label = NULL) {
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  dt <- proc.time()[["elapsed"]] - t0
  if (verbose) kg_msg(sprintf("%s%.2fs%s", if (is.null(label)) "" else paste0(label, " "),
                              dt, ""))
  attr(value, "elapsed") <- dt
  value
}

kg_fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")

# ---------------------------------------------------------------------------
# 类型转换（导入用）：空串 -> NA，与 Neo4j `coalesce(nullif(c,''), ...)` 等价
# ---------------------------------------------------------------------------
kg_blank_to_na <- function(x) {
  if (is.character(x)) {
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    return(x)
  }
  x
}

kg_coerce <- function(x, type) {
  switch(type,
    character = {
      if (!is.character(x)) x <- as.character(x)
      kg_blank_to_na(x)
    },
    integer = {
      if (is.character(x)) { x <- kg_blank_to_na(x); suppressWarnings(x <- as.integer(x)) }
      else suppressWarnings(x <- as.integer(x))
      x
    },
    double = {
      if (is.character(x)) { x <- kg_blank_to_na(x); suppressWarnings(x <- as.numeric(x)) }
      else suppressWarnings(x <- as.numeric(x))
      x
    },
    logical = {
      if (is.logical(x)) return(x)
      v <- tolower(trimws(as.character(x)))
      out <- rep(NA, length(v))
      out[v %in% c("true", "t", "1", "yes", "y")] <- TRUE
      out[v %in% c("false", "f", "0", "no", "n")] <- FALSE
      out
    },
    stop("未知类型: ", type)
  )
}

#' 按取值推断一列的类型（用于导入任意 CSV）
kg_infer_column_type <- function(v) {
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return("character")
  if (all(grepl("^(true|false)$", v, ignore.case = TRUE))) return("logical")
  if (all(grepl("^-?[0-9]+$", v))) return("integer")
  if (all(grepl("^-?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", v))) return("double")
  "character"
}

kg_empty_vec <- function(type, n = 0L) {  switch(type,
    character = character(n),
    integer = integer(n),
    double = numeric(n),
    logical = logical(n),
    list = vector("list", n),
    stop("未知类型: ", type)
  )
}

# ---------------------------------------------------------------------------
# 杂项
# ---------------------------------------------------------------------------
kg_is_scalar <- function(x) length(x) == 1L

kg_escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

kg_unique_name <- function(prefix, existing) {
  i <- 1L
  repeat {
    nm <- paste0(prefix, i)
    if (!nm %in% existing) return(nm)
    i <- i + 1L
  }
}

kg_head_df <- function(df, n = 5L) utils::head(df, n)

# 数值是否可视为整数（用于 toString 输出 1 而不是 1.0）
kg_num_is_int <- function(x) is.finite(x) && abs(x - round(x)) < 1e-12

# ---------------------------------------------------------------------------
# CSV 写出
#
# 必须用二进制方式写：Windows 的文本连接会把字段内的 "\n" 翻译成 "\r\n"，
# 导出 Neo4j CSV 后再读回来，full_text / text 会多出 \r（往返不一致）。
# ---------------------------------------------------------------------------
#' 把 data.frame 的 list 列压平成字符串（CSV 友好）
kg_flatten_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  one <- function(v) {
    if (inherits(v, "kg_null") || is.null(v)) return("")
    if (is_node_value(v)) return(paste0("node#", v$idx))
    if (is_rel_value(v)) return(paste0("rel#", v$idx))
    if (is.list(v)) {
      if (!is.null(names(v))) {
        return(paste0("{", paste(names(v), vapply(v, one, ""), sep = "=", collapse = ", "), "}"))
      }
      return(paste(vapply(v, one, ""), collapse = " | "))
    }
    paste(as.character(v), collapse = " | ")
  }
  for (j in seq_along(df)) {
    if (is.list(df[[j]]) && !is.data.frame(df[[j]])) {
      df[[j]] <- vapply(df[[j]], one, "")
    }
  }
  df
}

kg_write_csv <- function(x, file, row.names = FALSE, na = "", fileEncoding = "",
                         bom = FALSE, ...) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  x <- kg_flatten_df(x)
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(x, file, na = na, bom = bom,
                       quote = "auto", showProgress = FALSE)
  } else {
    con <- base::file(file, open = "wb")
    on.exit(close(con))
    utils::write.csv(x, con, row.names = row.names, na = na)
  }
  invisible(file)
}
