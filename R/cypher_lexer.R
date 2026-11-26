
# =============================================================================
# cypher_lexer.R — Cypher 词法分析
#
# 把查询串切成 token（ident / number / string / param / op / eof），
# 注释（// 与 /* */）直接丢弃。中文变量名、反引号标识符、Unicode 字符串均支持。
# =============================================================================

KG_LEX_MASTER <- paste0(
  "(?s)",
  "\\s+",                                   # 空白
  "|//[^\\n]*",                            # 行注释
  "|/\\*.*?\\*/",                           # 块注释
  "|'(?:\\\\.|[^'\\\\])*'",                  # 单引号字符串
  "|\"(?:\\\\.|[^\"\\\\])*\"",                # 双引号字符串
  "|`[^`]*`",                              # 反引号标识符
  "|\\$[A-Za-z_][A-Za-z0-9_]*|\\$[0-9]+",    # 参数
  "|(?:[0-9]+\\.[0-9]*|\\.[0-9]+)(?![.0-9])", # 浮点
  "|[0-9]+[eE][-+]?[0-9]+",                  # 科学计数
  "|[0-9]+",                                # 整数
  "|[A-Za-z_\\p{L}][A-Za-z0-9_\\p{L}]*",     # 标识符
  "|\\.\\.",                                # 范围 ..
  "|<>|!=|<=|>=|=~|\\+=|-=",                 # 双字符运算符
  "|\\+|-|\\*|/|%|\\^|=|<|>",                # 单字符运算符
  "|\\(|\\)|\\[|\\]|\\{|\\}",                 # 括号
  "|\\.|,|:|;|\\||&|!"                       # 其它标点
)

kg_unescape_string <- function(s) {
  if (!grepl("\\\\", s)) return(s)
  s <- gsub("\\\\n", "\n", s, fixed = TRUE)
  s <- gsub("\\\\t", "\t", s, fixed = TRUE)
  s <- gsub("\\\\r", "\r", s, fixed = TRUE)
  s <- gsub('\\\\"', '"', s, fixed = TRUE)
  s <- gsub("\\\\'", "'", s, fixed = TRUE)
  s <- gsub("\\\\\\\\", "\\\\", s, fixed = TRUE)
  # \uXXXX
  if (grepl("\\\\u[0-9a-fA-F]{4}", s)) {
    m <- gregexpr("\\\\u[0-9a-fA-F]{4}", s, perl = TRUE)
    regmatches(s, m) <- list(vapply(regmatches(s, m)[[1]], function(x) {
      intToUtf8(strtoi(substr(x, 3, 6), 16L))
    }, ""))
  }
  s
}

#' 词法分析，返回 token 流（环境，可当作游标使用）
kg_lex <- function(query) {
  q <- as.character(query)
  if (!nzchar(trimws(q))) kg_abort("空查询")
  m <- gregexpr(KG_LEX_MASTER, q, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) kg_abort("无法解析的查询")
  lens <- attr(m, "match.length")
  raw <- substring(q, m, m + lens - 1L)

  types <- character(length(raw))
  vals <- vector("list", length(raw))
  poss <- integer(length(raw))
  lensv <- integer(length(raw))
  k <- 0L
  for (i in seq_along(raw)) {
    s <- raw[i]
    if (grepl("^\\s+$", s)) next
    if (startsWith(s, "//")) next
    if (startsWith(s, "/*")) next
    k <- k + 1L
    poss[k] <- m[i]
    lensv[k] <- nchar(s, type = "chars")
    if (startsWith(s, "'") || startsWith(s, "\"")) {
      types[k] <- "string"
      vals[[k]] <- kg_unescape_string(substr(s, 2, nchar(s) - 1L))
    } else if (startsWith(s, "`")) {
      types[k] <- "ident"
      vals[[k]] <- gsub("``", "`", substr(s, 2, nchar(s) - 1L), fixed = TRUE)
    } else if (startsWith(s, "$")) {
      types[k] <- "param"
      vals[[k]] <- substr(s, 2, nchar(s))
    } else if (grepl("^[0-9]", s) || grepl("^\\.[0-9]", s)) {
      types[k] <- "number"
      vals[[k]] <- s
    } else if (grepl("^[A-Za-z_\\p{L}]", s, perl = TRUE)) {
      types[k] <- "ident"
      vals[[k]] <- s
    } else if (nchar(s) <= 2L && grepl("^[[:punct:]]+$", s)) {
      types[k] <- "op"
      vals[[k]] <- s
    } else {
      types[k] <- "op"
      vals[[k]] <- s
    }
  }
  # 末尾哨兵
  k <- k + 1L
  types[k] <- "eof"
  vals[[k]] <- ""
  poss[k] <- nchar(q) + 1L
  lensv[k] <- 0L

  p <- new.env(parent = emptyenv())
  p$type <- types[seq_len(k)]
  p$val <- vals[seq_len(k)]
  p$pos <- poss[seq_len(k)]
  p$len <- lensv[seq_len(k)]
  p$i <- 1L
  p$query <- q
  class(p) <- "kg_parser"
  p
}

# ---------------------------------------------------------------------------
# 游标操作
# ---------------------------------------------------------------------------

kg_tok <- function(p) list(type = p$type[p$i], val = p$val[[p$i]], pos = p$pos[p$i])
kg_tok_type <- function(p) p$type[p$i]
kg_tok_val <- function(p) p$val[[p$i]]

kg_adv <- function(p) {
  if (p$type[p$i] != "eof") p$i <- p$i + 1L
  invisible(p)
}

kg_peek_k <- function(p, k = 1L) {
  j <- p$i + k - 1L
  if (j > length(p$type)) j <- length(p$type)
  list(type = p$type[j], val = p$val[[j]], pos = p$pos[j])
}

kg_eof <- function(p) p$type[p$i] == "eof"

kg_is_kw <- function(p, kw, k = 1L) {
  t <- kg_peek_k(p, k)
  t$type == "ident" && toupper(t$val) == toupper(kw)
}

kg_accept_kw <- function(p, kw) {
  if (kg_is_kw(p, kw)) { kg_adv(p); return(TRUE) }
  FALSE
}

kg_expect_kw <- function(p, kw) {
  if (!kg_accept_kw(p, kw)) {
    t <- kg_tok(p)
    kg_abort(sprintf("语法错误（位置 %d）：期望 %s，实际 '%s'", t$pos, kw, t$val))
  }
  invisible(TRUE)
}

kg_is_op <- function(p, s, k = 1L) {
  t <- kg_peek_k(p, k)
  t$type == "op" && t$val == s
}

kg_accept_op <- function(p, s) {
  if (kg_is_op(p, s)) { kg_adv(p); return(TRUE) }
  FALSE
}

kg_expect_op <- function(p, s) {
  if (!kg_accept_op(p, s)) {
    t <- kg_tok(p)
    kg_abort(sprintf("语法错误（位置 %d）：期望 '%s'，实际 '%s'", t$pos, s, t$val))
  }
  invisible(TRUE)
}

kg_expect_ident <- function(p, what = "标识符") {
  if (p$type[p$i] != "ident") {
    t <- kg_tok(p)
    kg_abort(sprintf("语法错误（位置 %d）：期望%s，实际 '%s'", t$pos, what, t$val))
  }
  v <- p$val[[p$i]]
  kg_adv(p)
  v
}

kg_expect_string <- function(p) {
  if (p$type[p$i] != "string") {
    t <- kg_tok(p)
    kg_abort(sprintf("语法错误（位置 %d）：期望字符串，实际 '%s'", t$pos, t$val))
  }
  v <- p$val[[p$i]]
  kg_adv(p)
  v
}

kg_expect_number <- function(p) {
  if (p$type[p$i] != "number") {
    t <- kg_tok(p)
    kg_abort(sprintf("语法错误（位置 %d）：期望数字，实际 '%s'", t$pos, t$val))
  }
  v <- kg_number_value(p$val[[p$i]])
  kg_adv(p)
  v
}

kg_number_value <- function(s) {
  if (grepl("[.eE]", s)) as.numeric(s) else {
    v <- suppressWarnings(as.integer(s))
    if (is.na(v)) as.numeric(s) else v
  }
}

# 记录起始位置，用于取"原文表达式文本"作为默认列名
kg_mark <- function(p) p$i
kg_span_text <- function(p, from) {
  to <- p$i - 1L
  if (to < from) return("")
  q <- p$query
  start_pos <- p$pos[from]
  end_pos <- p$pos[to] + p$len[to] - 1L
  if (end_pos < start_pos) return("")
  substr(q, start_pos, end_pos)
}
