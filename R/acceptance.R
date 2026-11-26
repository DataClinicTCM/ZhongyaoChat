
# =============================================================================
# acceptance.R — 与 Neo4j 完全对齐的导入验收（A1–A15）
#
# 两种执行方式：
#   kg_acceptance(g)            内置检查（用 Cypher 实现，逐条给出 PASS/FAIL）
#   kg_acceptance_cypher(g)     直接把 Neo4j 的 cypher/41_acceptance.cypher 丢给 R 执行
# 报告：reports/acceptance.csv、reports/acceptance.md
# =============================================================================

KG_ACCEPTANCE_EXPECT <- list(
  nodes = KG_LABEL_COUNTS,
  rels = KG_REL_COUNTS,
  total_nodes = sum(KG_LABEL_COUNTS),
  total_rels = sum(KG_REL_COUNTS),
  constraints = 35L,
  fulltext_index = 6L,
  range_index = 4L,
  lookup_index = 35L
)

# 单条检查的执行器
kg_acc_check <- function(g, id, title, expected, fun, note = "") {
  t0 <- proc.time()[["elapsed"]]
  actual <- tryCatch(fun(), error = function(e) paste0("ERROR: ", conditionMessage(e)))
  elapsed <- proc.time()[["elapsed"]] - t0
  if (inherits(actual, "kg_check")) {
    status <- if (isTRUE(actual$pass)) "PASS" else "FAIL"
    text <- as.character(actual$text)
  } else if (inherits(actual, "kg_result") || is.data.frame(actual)) {
    status <- "INFO"
    text <- paste(utils::capture.output(print(utils::head(as.data.frame(actual), 3))),
                  collapse = " / ")
  } else {
    status <- if (is.character(actual) && startsWith(actual, "ERROR")) "ERROR" else "INFO"
    text <- paste(as.character(actual), collapse = "; ")
  }
  list(id = id, check = title, expected = expected, actual = text,
       status = status, note = note, elapsed = elapsed)
}

kg_num <- function(x) {
  if (inherits(x, "kg_result")) x <- as.data.frame(x)
  if (is.data.frame(x)) as.numeric(x[[1]][1]) else as.numeric(x)
}

#' 内置验收（A1–A15）
kg_acceptance <- function(g, verbose = TRUE, write = NULL) {
  checks <- list()
  add <- function(...) checks[[length(checks) + 1]] <<- kg_acc_check(g, ...)

  # A1 节点逐类计数
  add("A1", "节点数量对账（34 类逐类）", paste0(length(KG_LABEL_COUNTS), " 类全部相等"),
      function() {
        got <- vapply(names(KG_LABEL_COUNTS), function(l) length(g$label_ids[[l]] %||% integer(0)),
                      integer(1))
        bad <- names(got)[got != KG_LABEL_COUNTS[names(got)]]
        r <- structure(list(pass = length(bad) == 0L,
                            text = if (length(bad)) paste0("不符: ", paste(bad, collapse = ","))
                                   else paste0(length(got), " 类全部一致，合计 ", sum(got))),
                       class = "kg_check")
        r
      })

  # A2 关系逐类计数
  add("A2", "关系数量对账（43 类逐类）", paste0(length(KG_REL_COUNTS), " 类全部相等"),
      function() {
        got <- vapply(names(KG_REL_COUNTS), function(t) length(g$rel_type_ids[[t]] %||% integer(0)),
                      integer(1))
        bad <- names(got)[got != KG_REL_COUNTS[names(got)]]
        structure(list(pass = length(bad) == 0L,
                       text = if (length(bad)) paste0("不符: ", paste(bad, collapse = ","))
                              else paste0(length(got), " 类全部一致，合计 ", sum(got))),
                  class = "kg_check")
      })

  # A3 / A3b 整类缺失
  add("A3", "是否有整类标签缺失", "missing_labels 为空",
      function() {
        have <- names(g$label_ids)
        miss <- setdiff(names(KG_LABEL_COUNTS), have)
        structure(list(pass = !length(miss),
                       text = if (length(miss)) paste(miss, collapse = ",") else "无缺失"),
                  class = "kg_check")
      })
  add("A3b", "是否有整类关系类型缺失", "missing_rel_types 为空",
      function() {
        miss <- setdiff(names(KG_REL_COUNTS), names(g$rel_type_ids))
        structure(list(pass = !length(miss),
                       text = if (length(miss)) paste(miss, collapse = ",") else "无缺失"),
                  class = "kg_check")
      })

  # A4 字符覆盖率（用 Cypher，真正走执行器）
  add("A4", "每篇文档 chunk 字符和 == 原文字符数", "0",
      function() {
        r <- kg_cypher(g, "MATCH (d:SourceDoc)-[:HAS_TEXT_UNIT]->(u:TextUnit)
WITH d, sum(u.char_len) AS covered, d.char_len AS total
WHERE covered <> total
RETURN count(*) AS docs_with_bad_coverage")
        v <- kg_num(r)
        structure(list(pass = identical(as.integer(v), 0L), text = as.character(as.integer(v))),
                  class = "kg_check")
      })

  # A5 逐字一致
  add("A5", "chunk.text == 原文切片（substring）", "0",
      function() {
        r <- kg_cypher(g, "MATCH (d:SourceDoc)-[:HAS_TEXT_UNIT]->(u:TextUnit)
WHERE substring(d.full_text, u.char_start, u.char_len) <> u.text
RETURN count(*) AS mismatched_chunks")
        v <- kg_num(r)
        structure(list(pass = identical(as.integer(v), 0L), text = as.character(as.integer(v))),
                  class = "kg_check")
      })

  # A6 关系总数
  add("A6", "关系总数", kg_fmt_int(sum(KG_REL_COUNTS)),
      function() {
        r <- kg_cypher(g, "MATCH ()-[r]->() WHERE type(r) <> 'SCHEMA_LINK' RETURN count(r) AS relationships")
        v <- kg_num(r)
        structure(list(pass = v == KG_ACCEPTANCE_EXPECT$total_rels,
                       text = kg_fmt_int(as.integer(v))), class = "kg_check")
      })

  # A7 孤立节点
  add("A7", "孤立节点（只允许 Formula，且 ≤ 6）", "仅 Formula，≤ 6",
      function() {
        r <- kg_cypher(g, "MATCH (n) WHERE NOT (n)--() AND NOT n:Section
RETURN labels(n)[0] AS label, count(*) AS c")
        d <- as.data.frame(r)
        ok <- all(d$label == "Formula") && sum(d$c) <= 6
        structure(list(pass = ok,
                       text = paste0(paste(d$label, d$c, sep = "="), collapse = ", ")),
                  class = "kg_check")
      })

  # A8 约束
  add("A8", "唯一约束数量", "35",
      function() {
        r <- kg_cypher(g, "SHOW CONSTRAINTS YIELD name RETURN count(*) AS constraints")
        v <- kg_num(r)
        structure(list(pass = v == KG_ACCEPTANCE_EXPECT$constraints, text = as.character(as.integer(v))),
                  class = "kg_check")
      })

  # A9 索引
  add("A9", "索引（全文 6 / 常规 4 / 查找 35）", "FULLTEXT=6, RANGE=4, LOOKUP=35",
      function() {
        r <- as.data.frame(kg_cypher(g, "SHOW INDEXES YIELD name, type RETURN type, count(*) AS c ORDER BY type"))
        got <- stats::setNames(r$c, r$type)
        ok <- identical(as.integer(got["FULLTEXT"]), KG_ACCEPTANCE_EXPECT$fulltext_index) &&
          identical(as.integer(got["RANGE"]), KG_ACCEPTANCE_EXPECT$range_index) &&
          identical(as.integer(got["LOOKUP"]), KG_ACCEPTANCE_EXPECT$lookup_index)
        structure(list(pass = ok,
                       text = paste(names(got), got, sep = "=", collapse = ", ")),
                  class = "kg_check")
      })

  # A10 抽样：地榆
  add("A10", "抽样：地榆 功效/主治/引文", "2 / 15 / 5",
      function() {
        r <- as.data.frame(kg_cypher(g, "MATCH (h:Herb {name:'地榆'})
RETURN size([(h)-[:HAS_EFFICACY]->(x) | x]) AS efficacies,
       size([(h)-[:ALSO_TREATS]->(x) | x]) AS indications,
       size([(h)-[:HAS_QUOTATION]->(x) | x]) AS quotations"))
        got <- paste(r$efficacies, r$indications, r$quotations, sep = " / ")
        structure(list(pass = identical(got, "2 / 15 / 5"), text = got), class = "kg_check")
      })

  # A11 同名异物
  add("A11", "同名异物：草石蚕（2 个基原 + 1 个汇总节点）", "3 行（骨碎补科 / 唇形科 / 汇总）",
      function() {
        r <- as.data.frame(kg_cypher(g, "MATCH (h:Herb {name:'草石蚕'})
RETURN h.id AS id, h.family AS family, h.category_raw AS category"))
        fam <- r$family[!is.na(r$family)]
        ok <- nrow(r) == 3L && length(fam) == 2L
        structure(list(pass = ok,
                       text = paste0(nrow(r), " 行：", paste(fam, collapse = " / "))),
                  class = "kg_check")
      },
      note = "41_acceptance.cypher 原文期望 2 行；实测 herb.csv 中 name='草石蚕' 有 3 个节点（1 个汇总 + 2 个基原），Neo4j 同样返回 3 行。")

  # A12 溯源
  add("A12", "本文药材必须带 src_doc/src_section", "0",
      function() {
        v <- kg_num(kg_cypher(g, "MATCH (h:Herb)
WHERE h.in_corpus = true AND (h.src_doc IS NULL OR h.src_section IS NULL)
RETURN count(*) AS herbs_missing_provenance"))
        structure(list(pass = identical(as.integer(v), 0L), text = as.character(as.integer(v))),
                  class = "kg_check")
      })

  # A13 中文全文索引
  add("A13", "cjk 全文索引可用性（咯血 止血）", "> 0",
      function() {
        v <- kg_num(kg_cypher(g, "CALL db.index.fulltext.queryNodes('chunk_text', '咯血 止血') YIELD node, score
RETURN count(*) AS fulltext_hits"))
        structure(list(pass = v > 0, text = kg_fmt_int(as.integer(v))), class = "kg_check")
      })

  # A14 chunk 挂载
  add("A14", "每个 chunk 都有父文档与章节", "0",
      function() {
        v <- kg_num(kg_cypher(g, "MATCH (u:TextUnit)
WHERE NOT (u)-[:IN_SECTION]->(:Section) OR NOT ()-[:HAS_TEXT_UNIT]->(u)
RETURN count(*) AS textunits_without_parent"))
        structure(list(pass = identical(as.integer(v), 0L), text = as.character(as.integer(v))),
                  class = "kg_check")
      })

  # A15 药材分类挂载
  add("A15", "本文药材必须属于一个分类", "0",
      function() {
        v <- kg_num(kg_cypher(g, "MATCH (h:Herb)
WHERE h.in_corpus = true AND NOT (h)-[:HAS_CATEGORY]->(:DrugCategory)
RETURN count(*) AS herbs_without_category"))
        structure(list(pass = identical(as.integer(v), 0L), text = as.character(as.integer(v))),
                  class = "kg_check")
      })

  df <- do.call(rbind, lapply(checks, function(x) data.frame(
    id = x$id, check = x$check, expected = x$expected, actual = x$actual,
    status = x$status, note = x$note, elapsed = round(x$elapsed, 3),
    stringsAsFactors = FALSE)))
  attr(df, "pass") <- sum(df$status == "PASS")
  attr(df, "fail") <- sum(df$status != "PASS")
  if (verbose) print(df[, c("id", "check", "expected", "actual", "status")], row.names = FALSE)
  if (!is.null(write)) kg_acceptance_report(df, write)
  invisible(df)
}

#' 直接把 Neo4j 的 41_acceptance.cypher 交给 R 执行
kg_acceptance_cypher <- function(g, cypher_dir =
                                   file.path(KG_PKG_ROOT %||% ".", "..", "03_graph", "neo4j", "cypher"),
                                 verbose = TRUE) {
  path <- file.path(cypher_dir, "41_acceptance.cypher")
  if (!file.exists(path)) kg_abort("找不到 ", path)
  res <- kg_cypher_file(g, path, verbose = verbose)
  n_pass <- 0L
  for (r in Filter(Negate(is.null), res$results)) {
    d <- as.data.frame(r)
    if ("status" %in% names(d)) n_pass <- n_pass + sum(d$status == "PASS")
  }
  list(result = res, pass_rows = n_pass, errors = length(res$errors),
       statements = length(res$statements))
}

#' 写验收报告（CSV + Markdown）
kg_acceptance_report <- function(df, path = "reports/acceptance") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  kg_write_csv(df, paste0(path, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  md <- c(
    "# 导入验收报告（R 图库 vs Neo4j）", "",
    paste0("- 生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("- 检查项：", nrow(df), " 条；PASS ", sum(df$status == "PASS"),
           "；FAIL ", sum(df$status != "PASS")),
    "", "| 编号 | 检查 | 期望 | 实测 | 结论 |", "|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s |", df$id, df$check, df$expected, df$actual, df$status),
    ""
  )
  if (any(nzchar(df$note))) {
    md <- c(md, "## 说明", "", paste0("- ", df$id[df$id == "A11"], "：",
                                      df$note[df$id == "A11"]), "")
  }
  writeLines(md, paste0(path, ".md"), useBytes = TRUE)
  invisible(path)
}

#' 一致性校验（40_verify.cypher）
kg_verify <- function(g, cypher_dir =
                        file.path(KG_PKG_ROOT %||% ".", "..", "03_graph", "neo4j", "cypher"),
                      verbose = TRUE) {
  path <- file.path(cypher_dir, "40_verify.cypher")
  if (!file.exists(path)) kg_abort("找不到 ", path)
  kg_cypher_file(g, path, verbose = verbose)
}

#' 示例查询（50_queries_examples.cypher），逐条保存结果
#'
#' 注：原脚本第 1 条 A 的模式 `(e:Efficacy)-[:TREATS]->(i:Indication)<-[:HAS_EFFICACY]-(h:Herb)`
#' 与本体不符（HAS_EFFICACY 指向 Efficacy 而不是 Indication），在 Neo4j 中同样返回 0 行。
#' 这里额外给出修正版（Herb -HAS_EFFICACY-> Efficacy -TREATS-> Indication）并落盘。
kg_examples <- function(g, cypher_dir =
                          file.path(KG_PKG_ROOT %||% ".", "..", "03_graph", "neo4j", "cypher"),
                        out_dir = "reports/examples", verbose = TRUE) {
  path <- file.path(cypher_dir, "50_queries_examples.cypher")
  if (!file.exists(path)) kg_abort("找不到 ", path)
  res <- kg_cypher_file(g, path, verbose = verbose)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  keys <- c("A_病证反查中药", "B_成分药效关联", "C_方剂出处", "D_配伍药对", "E_安全性禁忌", "F_候选新药筛选")
  for (i in seq_along(res$results)) {
    r <- res$results[[i]]
    if (is.null(r)) next
    fn <- file.path(out_dir, sprintf("%s.csv", keys[i] %||% paste0("Q", i)))
    kg_write_csv(as.data.frame(r), fn)
  }
  # 修正版 A
  fixed <- kg_cypher(g, "MATCH (h:Herb)-[:HAS_EFFICACY]->(e:Efficacy)-[:TREATS]->(i:Indication {name:'衄血'})
RETURN h.name AS herb, e.name AS efficacy, h.category_raw AS category
ORDER BY herb")
  kg_write_csv(as.data.frame(fixed), file.path(out_dir, "A2_病证反查中药_修正版.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  if (verbose) {
    kg_msg("示例 A（原脚本）返回 ", nrow(res$results[[1]] %||% data.frame()),
           " 行；修正版返回 ", nrow(fixed), " 行（原脚本模式与本体不符，见 docs/说明）")
  }
  attr(res, "fixed_a") <- fixed
  res
}
