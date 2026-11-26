
# =============================================================================
# cypher_parser.R — Cypher 递归下降语法分析
#
# 支持子集（覆盖 Neo4j 日常 95% 的用法）：
#   MATCH / OPTIONAL MATCH / WHERE
#   RETURN / WITH [DISTINCT] … ORDER BY / SKIP / LIMIT
#   UNWIND / CREATE / MERGE (ON CREATE|ON MATCH SET) / SET / REMOVE
#   DELETE / DETACH DELETE / CALL … YIELD / SHOW / UNION [ALL]
#   模式：单跳、多跳链、多模式、变长 (*min..max)、shortestPath、路径变量 p =
#   表达式：字面量、属性、下标/切片、CASE、列表/映射、列表推导、模式推导、
#           模式存在判断、聚合函数、标签判断 (:Label)、IN / CONTAINS /
#           STARTS WITH / ENDS WITH / =~ / IS [NOT] NULL、AND/OR/XOR/NOT
# =============================================================================

KG_KEYWORDS <- c(
  "MATCH", "OPTIONAL", "WHERE", "RETURN", "WITH", "UNWIND", "CREATE", "MERGE",
  "SET", "DELETE", "DETACH", "REMOVE", "CALL", "YIELD", "ORDER", "BY", "SKIP",
  "LIMIT", "ASC", "ASCENDING", "DESC", "DESCENDING", "DISTINCT", "AS", "AND",
  "OR", "XOR", "NOT", "IN", "IS", "NULL", "TRUE", "FALSE", "CONTAINS", "STARTS",
  "ENDS", "CASE", "WHEN", "THEN", "ELSE", "END", "UNION", "ALL", "ON", "FOREACH",
  "EXPLAIN", "PROFILE", "SHOW", "CONSTRAINTS", "INDEXES", "PROCEDURES",
  "FUNCTIONS", "SHORTESTPATH", "ALLSHORTESTPATHS", "EXISTS", "OPTIONAL"
)

KG_AGG_FUNCS <- c("count", "sum", "avg", "min", "max", "collect", "stdev",
                  "stdevp", "percentilecont", "percentiledisc")

# ---------------------------------------------------------------------------
# 入口
# ---------------------------------------------------------------------------

#' 解析 Cypher 查询串
kg_parse <- function(query) {
  p <- kg_lex(query)
  explain <- FALSE
  if (kg_accept_kw(p, "EXPLAIN") || kg_accept_kw(p, "PROFILE")) explain <- TRUE
  queries <- list()
  union_all <- logical(0)
  repeat {
    q <- kg_parse_single(p)
    queries[[length(queries) + 1]] <- q
    if (kg_accept_kw(p, "UNION")) {
      union_all <- c(union_all, kg_accept_kw(p, "ALL"))
      next
    }
    break
  }
  if (!kg_eof(p) && !kg_accept_op(p, ";")) {
    if (!kg_eof(p)) {
      t <- kg_tok(p)
      kg_abort(sprintf("语法错误（位置 %d）：多余的 '%s'", t$pos, t$val))
    }
  }
  list(type = "query", queries = queries, union_all = union_all,
       explain = explain, text = query)
}

kg_parse_single <- function(p) {
  clauses <- list()
  repeat {
    if (kg_eof(p)) break
    if (kg_is_kw(p, "UNION")) break
    if (kg_is_op(p, ";")) break
    cl <- kg_parse_clause(p)
    if (is.null(cl)) break
    clauses[[length(clauses) + 1]] <- cl
  }
  if (!length(clauses)) kg_abort("空查询")
  list(clauses = clauses)
}

kg_parse_clause <- function(p) {
  if (kg_is_kw(p, "OPTIONAL")) {
    kg_adv(p); kg_expect_kw(p, "MATCH")
    return(kg_parse_match(p, optional = TRUE))
  }
  if (kg_is_kw(p, "MATCH")) { kg_adv(p); return(kg_parse_match(p, optional = FALSE)) }
  if (kg_is_kw(p, "UNWIND")) {
    kg_adv(p)
    e <- kg_parse_expr(p)
    kg_expect_kw(p, "AS")
    v <- kg_expect_ident(p)
    return(list(type = "UNWIND", expr = e, var = v))
  }
  if (kg_is_kw(p, "WITH")) { kg_adv(p); return(kg_parse_projection(p, "WITH")) }
  if (kg_is_kw(p, "RETURN")) { kg_adv(p); return(kg_parse_projection(p, "RETURN")) }
  if (kg_is_kw(p, "CREATE")) {
    kg_adv(p)
    return(list(type = "CREATE", patterns = kg_parse_pattern(p)))
  }
  if (kg_is_kw(p, "MERGE")) { kg_adv(p); return(kg_parse_merge(p)) }
  if (kg_is_kw(p, "SET")) { kg_adv(p); return(list(type = "SET", items = kg_parse_set_items(p))) }
  if (kg_is_kw(p, "REMOVE")) { kg_adv(p); return(list(type = "REMOVE", items = kg_parse_set_items(p, remove = TRUE))) }
  if (kg_is_kw(p, "DETACH")) {
    kg_adv(p); kg_expect_kw(p, "DELETE")
    return(list(type = "DELETE", detach = TRUE, exprs = kg_parse_expr_list(p)))
  }
  if (kg_is_kw(p, "DELETE")) {
    kg_adv(p)
    return(list(type = "DELETE", detach = FALSE, exprs = kg_parse_expr_list(p)))
  }
  if (kg_is_kw(p, "CALL")) { kg_adv(p); return(kg_parse_call(p)) }
  if (kg_is_kw(p, "SHOW")) { kg_adv(p); return(kg_parse_show(p)) }
  if (kg_is_kw(p, "FOREACH")) kg_abort("暂不支持 FOREACH（可用 UNWIND 改写）")
  if (kg_is_kw(p, "LOAD")) kg_abort("暂不支持 LOAD CSV（请用 kg_import_csv()）")
  t <- kg_tok(p)
  kg_abort(sprintf("无法识别的子句（位置 %d）：'%s'", t$pos, t$val))
}

kg_parse_expr_list <- function(p) {
  out <- list(kg_parse_expr(p))
  while (kg_accept_op(p, ",")) out[[length(out) + 1]] <- kg_parse_expr(p)
  out
}

# ---------------------------------------------------------------------------
# MATCH / 模式
# ---------------------------------------------------------------------------

kg_parse_match <- function(p, optional = FALSE) {
  patterns <- kg_parse_pattern(p)
  where <- NULL
  if (kg_accept_kw(p, "WHERE")) where <- kg_parse_expr(p)
  list(type = "MATCH", optional = optional, patterns = patterns, where = where)
}

kg_parse_pattern <- function(p) {
  parts <- list(kg_parse_pattern_part(p))
  while (kg_accept_op(p, ",")) parts[[length(parts) + 1]] <- kg_parse_pattern_part(p)
  parts
}

kg_parse_pattern_part <- function(p) {
  path_var <- NULL
  # 路径变量： p = (a)-[:R]->(b)  或  p = shortestPath((a)-[*]-(b))
  if (p$type[p$i] == "ident" && kg_is_op(p, "=", 2L) &&
      (kg_is_op(p, "(", 3L) ||
       (kg_peek_k(p, 3L)$type == "ident" &&
        kg_is_kw(p, "shortestPath", 3L) || kg_is_kw(p, "allShortestPaths", 3L)))) {
    path_var <- p$val[[p$i]]
    p$i <- p$i + 2L
  }
  shortest <- "none"
  if (kg_is_kw(p, "shortestPath")) { kg_adv(p); kg_expect_op(p, "("); shortest <- "shortest" }
  else if (kg_is_kw(p, "allShortestPaths")) { kg_adv(p); kg_expect_op(p, "("); shortest <- "all" }

  nodes <- list(kg_parse_node_pattern(p))
  rels <- list()
  repeat {
    if (kg_is_op(p, "-") || kg_is_op(p, "<")) {
      rel <- kg_parse_rel_pattern(p)
      node <- kg_parse_node_pattern(p)
      rels[[length(rels) + 1]] <- rel
      nodes[[length(nodes) + 1]] <- node
    } else break
  }
  if (shortest != "none") kg_expect_op(p, ")")
  list(path_var = path_var, nodes = nodes, rels = rels, shortest = shortest)
}

kg_parse_node_pattern <- function(p) {
  kg_expect_op(p, "(")
  var <- NULL; labels <- character(0); props <- NULL
  if (p$type[p$i] == "ident" && !kg_is_kw(p, "WHERE")) {
    var <- p$val[[p$i]]; kg_adv(p)
  }
  while (kg_accept_op(p, ":")) {
    labels <- c(labels, kg_expect_ident(p, "标签"))
  }
  if (kg_is_op(p, "{")) props <- kg_parse_map(p)
  kg_expect_op(p, ")")
  list(var = var, labels = labels, props = props)
}

kg_parse_rel_pattern <- function(p) {
  left <- FALSE
  if (kg_accept_op(p, "<")) { kg_expect_op(p, "-"); left <- TRUE }
  else kg_expect_op(p, "-")

  var <- NULL; types <- character(0); varlen <- FALSE; min <- 1L; max <- 1L
  props <- NULL; where <- NULL

  if (kg_accept_op(p, "[")) {
    if (p$type[p$i] == "ident") { var <- p$val[[p$i]]; kg_adv(p) }
    if (kg_accept_op(p, ":")) {
      types <- c(types, kg_expect_ident(p, "关系类型"))
      while (kg_accept_op(p, "|")) {
        kg_accept_op(p, ":")
        types <- c(types, kg_expect_ident(p, "关系类型"))
      }
    }
    if (kg_accept_op(p, "*")) {
      varlen <- TRUE
      min <- 1L; max <- Inf
      if (p$type[p$i] == "number") {
        min <- as.integer(kg_expect_number(p))
        max <- min
      }
      if (kg_accept_op(p, "..")) {
        if (p$type[p$i] == "number") max <- as.integer(kg_expect_number(p)) else max <- Inf
      }
      if (!is.finite(max)) max <- 15L  # 安全上限，避免全图笛卡尔爆炸
      if (min < 0L) min <- 0L
      if (max < min) kg_abort("变长关系上界小于下界")
    }
    if (kg_is_op(p, "{")) props <- kg_parse_map(p)
    if (kg_accept_kw(p, "WHERE")) where <- kg_parse_expr(p)
    kg_expect_op(p, "]")
  }

  kg_expect_op(p, "-")
  right <- kg_accept_op(p, ">")
  dir <- if (left && !right) "in" else if (!left && right) "out" else if (!left && !right) "both" else
    kg_abort("关系方向非法：<-[...]-> ")

  list(var = var, types = types, dir = dir, min = min, max = max,
       props = props, where = where, varlen = varlen)
}

# ---------------------------------------------------------------------------
# PROFILE 类投影子句（WITH / RETURN）
# ---------------------------------------------------------------------------

kg_parse_projection <- function(p, kind) {
  distinct <- kg_accept_kw(p, "DISTINCT")
  items <- list()
  repeat {
    if (kg_accept_op(p, "*")) {
      items[[length(items) + 1]] <- list(expr = list(t = "wildcard"), alias = "*")
    } else {
      from <- kg_mark(p)
      e <- kg_parse_expr(p)
      txt <- kg_span_text(p, from)
      alias <- if (kg_accept_kw(p, "AS")) kg_expect_ident(p) else txt
      items[[length(items) + 1]] <- list(expr = e, alias = alias)
    }
    if (!kg_accept_op(p, ",")) break
  }
  order <- list()
  if (kg_accept_kw(p, "ORDER")) {
    kg_expect_kw(p, "BY")
    repeat {
      e <- kg_parse_expr(p)
      desc <- FALSE
      if (kg_accept_kw(p, "DESC") || kg_accept_kw(p, "DESCENDING")) desc <- TRUE
      else if (kg_accept_kw(p, "ASC") || kg_accept_kw(p, "ASCENDING")) desc <- FALSE
      order[[length(order) + 1]] <- list(expr = e, desc = desc)
      if (!kg_accept_op(p, ",")) break
    }
  }
  skip <- NULL; limit <- NULL
  if (kg_accept_kw(p, "SKIP")) skip <- kg_parse_expr(p)
  if (kg_accept_kw(p, "LIMIT")) limit <- kg_parse_expr(p)
  where <- NULL
  if (kind == "WITH" && kg_accept_kw(p, "WHERE")) where <- kg_parse_expr(p)
  list(type = kind, kind = kind, distinct = distinct, items = items, order = order,
       skip = skip, limit = limit, where = where)
}

# ---------------------------------------------------------------------------
# MERGE / SET / REMOVE
# ---------------------------------------------------------------------------

kg_parse_merge <- function(p) {
  pattern <- kg_parse_pattern_part(p)
  on_create <- list(); on_match <- list()
  repeat {
    if (!kg_accept_kw(p, "ON")) break
    if (kg_accept_kw(p, "CREATE")) {
      kg_expect_kw(p, "SET")
      on_create <- c(on_create, kg_parse_set_items(p))
    } else if (kg_accept_kw(p, "MATCH")) {
      kg_expect_kw(p, "SET")
      on_match <- c(on_match, kg_parse_set_items(p))
    } else {
      kg_abort("MERGE 之后只支持 ON CREATE SET / ON MATCH SET")
    }
  }
  list(type = "MERGE", pattern = pattern, on_create = on_create, on_match = on_match)
}

kg_parse_set_items <- function(p, remove = FALSE) {
  items <- list()
  repeat {
    if (p$type[p$i] != "ident") kg_abort("SET/REMOVE 需要变量名")
    var <- p$val[[p$i]]
    # n:a:b
    if (kg_is_op(p, ":", 2L)) {
      kg_adv(p)
      labels <- character(0)
      while (kg_accept_op(p, ":")) labels <- c(labels, kg_expect_ident(p, "标签"))
      items[[length(items) + 1]] <- list(kind = "label", var = var, labels = labels)
    } else if (kg_is_op(p, ".", 2L)) {
      kg_adv(p); kg_expect_op(p, ".")
      prop <- kg_expect_ident(p, "属性名")
      if (remove) {
        items[[length(items) + 1]] <- list(kind = "prop", var = var, prop = prop, expr = NULL)
      } else {
        kg_expect_op(p, "=")
        items[[length(items) + 1]] <- list(kind = "prop", var = var, prop = prop,
                                           expr = kg_parse_expr(p))
      }
    } else if (remove) {
      t <- kg_tok(p)
      kg_abort(sprintf("REMOVE 语法错误（位置 %d）：'%s'", t$pos, t$val))
    } else if (kg_is_op(p, "+=", 2L)) {
      kg_adv(p); kg_adv(p)
      items[[length(items) + 1]] <- list(kind = "merge", var = var, expr = kg_parse_expr(p))
    } else if (kg_is_op(p, "=", 2L)) {
      kg_adv(p); kg_adv(p)
      items[[length(items) + 1]] <- list(kind = "replace", var = var, expr = kg_parse_expr(p))
    } else {
      t <- kg_tok(p)
      kg_abort(sprintf("SET/REMOVE 语法错误（位置 %d）：'%s'", t$pos, t$val))
    }
    if (!kg_accept_op(p, ",")) break
  }
  items
}

# ---------------------------------------------------------------------------
# CALL / SHOW
# ---------------------------------------------------------------------------

kg_parse_call <- function(p) {
  if (kg_is_op(p, "{")) kg_abort("暂不支持 CALL { ... } 子查询")
  parts <- kg_expect_ident(p, "过程名")
  while (kg_is_op(p, ".")) { kg_adv(p); parts <- c(parts, kg_expect_ident(p, "过程名")) }
  proc <- paste(parts, collapse = ".")
  kg_expect_op(p, "(")
  args <- list()
  if (!kg_is_op(p, ")")) args <- kg_parse_expr_list(p)
  kg_expect_op(p, ")")
  yields <- NULL; where <- NULL
  if (kg_accept_kw(p, "YIELD")) {
    if (kg_accept_op(p, "*")) {
      yields <- list(list(name = "*", alias = "*"))
    } else {
      yields <- list()
      repeat {
        nm <- kg_expect_ident(p, "输出列名")
        al <- if (kg_accept_kw(p, "AS")) kg_expect_ident(p) else nm
        yields[[length(yields) + 1]] <- list(name = nm, alias = al)
        if (!kg_accept_op(p, ",")) break
      }
    }
    if (kg_accept_kw(p, "WHERE")) where <- kg_parse_expr(p)
  }
  list(type = "CALL", proc = proc, args = args, yields = yields, where = where)
}

kg_parse_show <- function(p) {
  what <- kg_expect_ident(p, "SHOW 对象")
  what <- toupper(what)
  yields <- NULL; where <- NULL
  if (kg_accept_kw(p, "YIELD")) {
    yields <- list()
    repeat {
      nm <- kg_expect_ident(p, "列名")
      al <- if (kg_accept_kw(p, "AS")) kg_expect_ident(p) else nm
      yields[[length(yields) + 1]] <- list(name = nm, alias = al)
      if (!kg_accept_op(p, ",")) break
    }
    if (kg_accept_kw(p, "WHERE")) where <- kg_parse_expr(p)
  }
  list(type = "SHOW", what = what, yields = yields, where = where)
}

# ---------------------------------------------------------------------------
# 表达式
# ---------------------------------------------------------------------------

kg_parse_expr <- function(p) kg_parse_or(p)

kg_parse_or <- function(p) {
  l <- kg_parse_and(p)
  while (kg_accept_kw(p, "OR")) l <- list(t = "bin", op = "OR", l = l, r = kg_parse_and(p))
  l
}

kg_parse_and <- function(p) {
  l <- kg_parse_xor(p)
  while (kg_accept_kw(p, "AND")) l <- list(t = "bin", op = "AND", l = l, r = kg_parse_xor(p))
  l
}

kg_parse_xor <- function(p) {
  l <- kg_parse_not(p)
  while (kg_accept_kw(p, "XOR")) l <- list(t = "bin", op = "XOR", l = l, r = kg_parse_not(p))
  l
}

kg_parse_not <- function(p) {
  if (kg_accept_kw(p, "NOT")) return(list(t = "not", e = kg_parse_not(p)))
  kg_parse_comparison(p)
}

kg_parse_comparison <- function(p) {
  l <- kg_parse_additive(p)
  repeat {
    if (kg_is_op(p, "=") || kg_is_op(p, "<>") || kg_is_op(p, "!=") ||
        kg_is_op(p, "<") || kg_is_op(p, "<=") || kg_is_op(p, ">") || kg_is_op(p, ">=") ||
        kg_is_op(p, "=~")) {
      op <- p$val[[p$i]]; kg_adv(p)
      if (op == "!=") op <- "<>"
      r <- kg_parse_additive(p)
      l <- list(t = "bin", op = op, l = l, r = r)
    } else if (kg_is_kw(p, "IN")) {
      kg_adv(p); r <- kg_parse_additive(p)
      l <- list(t = "in", l = l, r = r)
    } else if (kg_is_kw(p, "CONTAINS")) {
      kg_adv(p); r <- kg_parse_additive(p)
      l <- list(t = "bin", op = "CONTAINS", l = l, r = r)
    } else if (kg_is_kw(p, "STARTS")) {
      kg_adv(p); kg_expect_kw(p, "WITH")
      r <- kg_parse_additive(p)
      l <- list(t = "bin", op = "STARTS WITH", l = l, r = r)
    } else if (kg_is_kw(p, "ENDS")) {
      kg_adv(p); kg_expect_kw(p, "WITH")
      r <- kg_parse_additive(p)
      l <- list(t = "bin", op = "ENDS WITH", l = l, r = r)
    } else if (kg_is_kw(p, "IS")) {
      kg_adv(p)
      if (kg_accept_kw(p, "NOT")) {
        kg_expect_kw(p, "NULL"); l <- list(t = "notnull", e = l)
      } else {
        kg_expect_kw(p, "NULL"); l <- list(t = "isnull", e = l)
      }
    } else break
  }
  l
}

kg_parse_additive <- function(p) {
  l <- kg_parse_multiplicative(p)
  while (kg_is_op(p, "+") || kg_is_op(p, "-")) {
    op <- p$val[[p$i]]; kg_adv(p)
    r <- kg_parse_multiplicative(p)
    l <- list(t = "bin", op = op, l = l, r = r)
  }
  l
}

kg_parse_multiplicative <- function(p) {
  l <- kg_parse_power(p)
  while (kg_is_op(p, "*") || kg_is_op(p, "/") || kg_is_op(p, "%")) {
    op <- p$val[[p$i]]; kg_adv(p)
    r <- kg_parse_power(p)
    l <- list(t = "bin", op = op, l = l, r = r)
  }
  l
}

kg_parse_power <- function(p) {
  l <- kg_parse_unary(p)
  if (kg_accept_op(p, "^")) {
    r <- kg_parse_power(p)
    return(list(t = "bin", op = "^", l = l, r = r))
  }
  l
}

kg_parse_unary <- function(p) {
  if (kg_accept_op(p, "-")) return(list(t = "neg", e = kg_parse_unary(p)))
  if (kg_accept_op(p, "+")) return(kg_parse_unary(p))
  kg_parse_postfix(p)
}

kg_parse_postfix <- function(p) {
  e <- kg_parse_atom(p)
  repeat {
    if (kg_accept_op(p, ".")) {
      key <- kg_expect_ident(p, "属性名")
      e <- list(t = "prop", obj = e, key = key)
    } else if (kg_is_op(p, "[")) {
      kg_adv(p)
      if (kg_is_op(p, "..")) {
        # [..to]
        kg_adv(p)
        to <- if (kg_is_op(p, "]")) NULL else kg_parse_expr(p)
        kg_expect_op(p, "]")
        e <- list(t = "slice", obj = e, from = NULL, to = to)
      } else {
        from <- kg_parse_expr(p)
        if (kg_accept_op(p, "..")) {
          to <- if (kg_is_op(p, "]")) NULL else kg_parse_expr(p)
          kg_expect_op(p, "]")
          e <- list(t = "slice", obj = e, from = from, to = to)
        } else {
          kg_expect_op(p, "]")
          e <- list(t = "index", obj = e, idx = from)
        }
      }
    } else if (kg_is_op(p, ":")) {
      # n:Label 标签判断
      kg_adv(p)
      lab <- kg_expect_ident(p, "标签")
      e <- list(t = "haslabel", e = e, label = lab)
    } else break
  }
  e
}

kg_parse_atom <- function(p) {
  t <- kg_tok(p)
  if (t$type == "number") { kg_adv(p); return(list(t = "lit", v = kg_number_value(t$val))) }
  if (t$type == "string") { kg_adv(p); return(list(t = "lit", v = t$val)) }
  if (t$type == "param") { kg_adv(p); return(list(t = "param", name = t$val)) }
  if (t$type == "op") {
    if (t$val == "(") {
      # 判断是模式还是普通括号表达式
      if (kg_looks_like_pattern(p)) return(list(t = "patternexists", pattern = kg_parse_pattern_part(p)))
      kg_adv(p)
      e <- kg_parse_expr(p)
      kg_expect_op(p, ")")
      return(e)
    }
    if (t$val == "[") return(kg_parse_bracket(p))
    if (t$val == "{") return(kg_parse_map(p))
  }
  if (t$type == "ident") {
    up <- toupper(t$val)
    if (up == "TRUE") { kg_adv(p); return(list(t = "lit", v = TRUE)) }
    if (up == "FALSE") { kg_adv(p); return(list(t = "lit", v = FALSE)) }
    if (up == "NULL") { kg_adv(p); return(list(t = "lit", v = kg_null)) }
    if (up == "CASE") { kg_adv(p); return(kg_parse_case(p)) }
    if (up == "NOT") { kg_adv(p); return(list(t = "not", e = kg_parse_not(p))) }
    if (up == "EXISTS" && kg_is_op(p, "(", 2L)) {
      kg_adv(p); kg_expect_op(p, "(")
      if (kg_looks_like_pattern(p)) {
        pat <- kg_parse_pattern_part(p)
        kg_expect_op(p, ")")
        return(list(t = "patternexists", pattern = pat))
      }
      e <- kg_parse_expr(p)
      kg_expect_op(p, ")")
      return(list(t = "call", name = "exists", args = list(e), distinct = FALSE, star = FALSE))
    }
    if (kg_is_op(p, "(", 2L)) return(kg_parse_function_call(p))
    kg_adv(p)
    return(list(t = "var", name = t$val))
  }
  kg_abort(sprintf("语法错误（位置 %d）：意外的 '%s'", t$pos, t$val))
}

# 从当前位置的 "(" 开始，匹配括号后看是否紧跟 - / < ，判断是否为模式
kg_looks_like_pattern <- function(p) {
  if (!kg_is_op(p, "(")) return(FALSE)
  depth <- 0L
  j <- p$i
  n <- length(p$type)
  while (j <= n) {
    if (p$type[j] == "op" && p$val[[j]] == "(") depth <- depth + 1L
    else if (p$type[j] == "op" && p$val[[j]] == ")") {
      depth <- depth - 1L
      if (depth == 0L) {
        nx <- if (j + 1L <= n) p$val[[j + 1L]] else ""
        return(nx %in% c("-", "<"))
      }
    }
    j <- j + 1L
  }
  FALSE
}

kg_parse_function_call <- function(p) {
  name <- p$val[[p$i]]; kg_adv(p)
  kg_expect_op(p, "(")
  lname <- tolower(name)
  distinct <- FALSE
  args <- list()
  star <- FALSE
  if (kg_accept_op(p, "*")) {
    star <- TRUE
  } else if (!kg_is_op(p, ")")) {
    if (kg_accept_kw(p, "DISTINCT")) distinct <- TRUE
    args <- kg_parse_expr_list(p)
  }
  kg_expect_op(p, ")")
  is_agg <- lname %in% KG_AGG_FUNCS
  list(t = if (is_agg) "agg" else "call", name = name, args = args,
       distinct = distinct, star = star)
}

kg_parse_case <- function(p) {
  base <- NULL
  if (!kg_is_kw(p, "WHEN")) base <- kg_parse_expr(p)
  whens <- list()
  while (kg_accept_kw(p, "WHEN")) {
    w <- kg_parse_expr(p)
    kg_expect_kw(p, "THEN")
    th <- kg_parse_expr(p)
    whens[[length(whens) + 1]] <- list(when = w, then = th)
  }
  els <- NULL
  if (kg_accept_kw(p, "ELSE")) els <- kg_parse_expr(p)
  kg_expect_kw(p, "END")
  list(t = "case", expr = base, whens = whens, els = els)
}

kg_parse_map <- function(p) {
  kg_expect_op(p, "{")
  keys <- character(0); vals <- list()
  if (!kg_is_op(p, "}")) {
    repeat {
      if (p$type[p$i] == "ident") { keys <- c(keys, p$val[[p$i]]); kg_adv(p) }
      else if (p$type[p$i] == "string") { keys <- c(keys, p$val[[p$i]]); kg_adv(p) }
      else kg_abort("映射字面量的键必须是标识符或字符串")
      kg_expect_op(p, ":")
      vals[[length(vals) + 1]] <- kg_parse_expr(p)
      if (!kg_accept_op(p, ",")) break
    }
  }
  kg_expect_op(p, "}")
  list(t = "map", keys = keys, vals = vals)
}

# [ ... ]：空列表 / 普通列表 / 列表推导 / 模式推导
kg_parse_bracket <- function(p) {
  kg_expect_op(p, "[")
  if (kg_accept_op(p, "]")) return(list(t = "list", items = list()))
  # 模式推导
  if (kg_is_op(p, "(") && kg_looks_like_pattern(p)) {
    pat <- kg_parse_pattern_part(p)
    where <- NULL
    if (kg_accept_kw(p, "WHERE")) where <- kg_parse_expr(p)
    kg_expect_op(p, "|")
    map <- kg_parse_expr(p)
    kg_expect_op(p, "]")
    return(list(t = "patterncomp", pattern = pat, where = where, map = map))
  }
  # 列表推导 [x IN list WHERE pred | expr]
  if (p$type[p$i] == "ident" && kg_is_kw(p, "IN", 2L)) {
    v <- p$val[[p$i]]; kg_adv(p); kg_adv(p)
    src <- kg_parse_expr(p)
    where <- NULL
    if (kg_accept_kw(p, "WHERE")) where <- kg_parse_expr(p)
    map <- NULL
    if (kg_accept_op(p, "|")) map <- kg_parse_expr(p)
    kg_expect_op(p, "]")
    return(list(t = "listcomp", var = v, source = src, where = where, map = map))
  }
  items <- list(kg_parse_expr(p))
  while (kg_accept_op(p, ",")) {
    if (kg_is_op(p, "]")) break
    items[[length(items) + 1]] <- kg_parse_expr(p)
  }
  kg_expect_op(p, "]")
  list(t = "list", items = items)
}

# ---------------------------------------------------------------------------
# 辅助：AST 遍历
# ---------------------------------------------------------------------------

kg_expr_walk <- function(e, f) {
  f(e)
  kids <- switch(e$t,
    bin = list(e$l, e$r),
    not = list(e$e),
    neg = list(e$e),
    isnull = list(e$e),
    notnull = list(e$e),
    haslabel = list(e$e),
    "in" = list(e$l, e$r),
    prop = list(e$obj),
    index = list(e$obj, e$idx),
    slice = list(e$obj, e$from, e$to),
    call = e$args,
    agg = e$args,
    list = e$items,
    map = e$vals,
    case = c(list(e$expr), unlist(lapply(e$whens, function(w) list(w$when, w$then)), recursive = FALSE), list(e$els)),
    listcomp = list(e$source, e$where, e$map),
    patterncomp = list(e$where, e$map),
    list()
  )
  kids <- Filter(Negate(is.null), kids)
  for (k in kids) kg_expr_walk(k, f)
  invisible(NULL)
}

kg_expr_has_agg <- function(e) {
  found <- FALSE
  kg_expr_walk(e, function(x) if (identical(x$t, "agg")) found <<- TRUE)
  found
}

kg_expr_has_var <- function(e, vars) {
  found <- FALSE
  kg_expr_walk(e, function(x) if (identical(x$t, "var") && x$name %in% vars) found <<- TRUE)
  found
}

kg_expr_vars <- function(e) {
  out <- character(0)
  kg_expr_walk(e, function(x) if (identical(x$t, "var")) out <<- c(out, x$name))
  unique(out)
}
