
# =============================================================================
# export.R — 导出：回到 Neo4j（Cypher / CSV / neo4j-admin / GraphML / JSON）
#
# 这是"R 图库 ⇄ Neo4j"可逆性的证据：
#   kg_export_csv()   生成与 03_graph/neo4j/nodes|rels 完全同构的 CSV
#   kg_export_cypher() 生成可直接粘进 Browser 的 MERGE 脚本
#   kg_export_admin()  生成 neo4j-admin database import 所需的 :ID/:START_ID/:TYPE 头
# =============================================================================

kg_cypher_quote <- function(x) {
  if (is.logical(x)) return(if (is.na(x)) "null" else if (x) "true" else "false")
  if (is.numeric(x)) {
    if (is.na(x)) return("null")
    return(format(x, scientific = FALSE, trim = TRUE, digits = 15))
  }
  if (is.list(x)) {
    return(paste0("[", paste(vapply(x, kg_cypher_quote, ""), collapse = ", "), "]"))
  }
  s <- as.character(x)
  if (is.na(s)) return("null")
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("'", "\\\\'", s)
  s <- gsub("\n", "\\\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\\\t", s, fixed = TRUE)
  paste0("'", s, "'")
}

#' 导出为 Cypher MERGE 脚本（节点 + 关系 + 约束/索引）
kg_export_cypher <- function(g, path = "store/kg_export.cypher", include_schema = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  if (include_schema) {
    writeLines(c("// ===== constraints & indexes（由 07_rgraph 导出）=====",
                 kg_export_schema_cypher(g)), con)
  }
  labs <- names(g$label_ids)
  for (lab in labs) {
    ids <- g$label_ids[[lab]]
    ids <- ids[!g$nodes$deleted[ids]]
    if (!length(ids)) next
    writeLines(sprintf("\n// %s: %d rows", lab, length(ids)), con)
    props <- setdiff(names(g$nodes), KG_NODE_RESERVED)
    for (i in ids) {
      vals <- vapply(props, function(p) kg_cypher_quote(g$nodes[[p]][i]), "")
      vals <- vals[vals != "null"]
      body <- paste(paste0(names(vals), ": ", vals), collapse = ", ")
      writeLines(paste0("MERGE (n:", lab, " {id: ", kg_cypher_quote(g$nodes$id[i]), "})",
                        if (nzchar(body)) paste0(" SET n += {", body, "}") else "", ";"), con)
    }
  }
  types <- names(g$rel_type_ids)
  for (ty in types) {
    ids <- g$rel_type_ids[[ty]]
    ids <- ids[!g$rels$deleted[ids]]
    if (!length(ids)) next
    writeLines(sprintf("\n// %s: %d rows", ty, length(ids)), con)
    props <- setdiff(names(g$rels), KG_REL_RESERVED)
    for (i in ids) {
      vals <- vapply(props, function(p) kg_cypher_quote(g$rels[[p]][i]), "")
      vals <- vals[vals != "null"]
      body <- paste(paste0(names(vals), ": ", vals), collapse = ", ")
      writeLines(paste0(
        "MATCH (a {id: ", kg_cypher_quote(g$nodes$id[g$rels$src[i]]), "}), ",
        "(b {id: ", kg_cypher_quote(g$nodes$id[g$rels$dst[i]]), "}) ",
        "MERGE (a)-[r:", ty, "]->(b)",
        if (nzchar(body)) paste0(" SET r += {", body, "}") else "", ";"), con)
    }
  }
  kg_msg("已导出 Cypher: ", path, "（", file.info(path)$size / 1024^2, " MB）")
  invisible(path)
}

#' 生成约束/索引 DDL（与 00_constraints / 30_indexes 同构）
kg_export_schema_cypher <- function(g) {
  out <- character(0)
  for (i in seq_len(nrow(g$constraints))) {
    ct <- g$constraints[i]
    out <- c(out, sprintf("CREATE CONSTRAINT %s IF NOT EXISTS FOR (n:%s) REQUIRE n.%s IS UNIQUE;",
                          ct$name, ct$label, ct$props[[1]]))
  }
  for (i in seq_len(nrow(g$indexes))) {
    ix <- g$indexes[i]
    if (ix$type == "FULLTEXT") {
      out <- c(out, sprintf(paste0("CREATE FULLTEXT INDEX %s IF NOT EXISTS FOR (n:%s) ",
                                   "ON EACH [%s] OPTIONS { indexConfig: ",
                                   "{ `fulltext.analyzer`: 'cjk' } };"),
                            ix$name, ix$label,
                            paste(paste0("n.", ix$props[[1]]), collapse = ", ")))
    } else if (ix$type == "RANGE") {
      out <- c(out, sprintf("CREATE INDEX %s IF NOT EXISTS FOR (n:%s) ON (%s);",
                            ix$name, ix$label,
                            paste(paste0("n.", ix$props[[1]]), collapse = ", ")))
    }
  }
  out
}

#' 导出 CSV（与 03_graph/neo4j/nodes、rels 完全同构，可直接喂给 Neo4j LOAD CSV）
#'
#' TCM 本体内的标签/关系类型沿用原始文件名（herb.csv、has_efficacy.csv…），
#' 其它标签/类型按 `<snake_case>.csv` 命名；TCM 标签即使没有数据也写出表头，
#' 这样导出的目录可以直接替换 03_graph/neo4j/{nodes,rels} 使用。
kg_export_csv <- function(g, dir = "store/neo4j_csv", verbose = TRUE,
                          tcm_layout = NULL) {
  nd <- file.path(dir, "nodes")
  rd <- file.path(dir, "rels")
  dir.create(nd, recursive = TRUE, showWarnings = FALSE)
  dir.create(rd, recursive = TRUE, showWarnings = FALSE)
  # 是否按 TCM 的 34+48 文件布局导出（空标签也写表头）。
  # 用本包建"另一张图"时自动只导出实际存在的标签/关系类型。
  if (is.null(tcm_layout)) tcm_layout <- any(names(g$label_ids) %in% names(KG_NODE_SCHEMA))

  labs <- if (isTRUE(tcm_layout)) unique(c(names(KG_NODE_SCHEMA), names(g$label_ids)))
          else names(g$label_ids)
  for (lab in labs) {
    spec <- KG_NODE_SCHEMA[[lab]]
    if (is.null(spec) && !length(g$label_ids[[lab]] %||% integer(0))) next
    cols <- if (!is.null(spec)) c("id", setdiff(names(spec$props), "id"))
            else c("id", setdiff(names(g$nodes), c(KG_NODE_RESERVED, "id")))
    cols <- cols[cols %in% names(g$nodes)]
    # 只按"主标签"导出，多标签写进 extra_labels 列（避免同一节点出现在多个文件）
    ids <- which(!g$nodes$deleted & g$nodes$label == lab)
    if (!length(ids) && !isTRUE(tcm_layout)) next   # 非 TCM 布局不写空文件
    has_extra <- length(ids) && any(lengths(g$nodes$extra_labels[ids]) > 0)
    if (length(ids)) {
      df <- as.data.frame(g$nodes[ids, cols, with = FALSE], stringsAsFactors = FALSE)
      if (has_extra) {
        df[["extra_labels"]] <- vapply(g$nodes$extra_labels[ids],
                                       function(x) paste(x, collapse = ";"), "")
      }
    } else {
      df <- stats::setNames(data.frame(matrix(nrow = 0, ncol = length(cols))), cols)
      for (p in cols) df[[p]] <- kg_empty_vec(kg_prop_type(p, "node"))
    }
    fname <- if (!is.null(spec)) basename(spec$file) else paste0(kg_snake(lab), ".csv")
    kg_write_csv(df, file.path(nd, fname))
  }

  tcm_types <- vapply(KG_REL_SCHEMA, `[[`, "", "type")
  rel_specs <- if (isTRUE(tcm_layout)) KG_REL_SCHEMA else
    Filter(function(sp) sp$type %in% names(g$rel_type_ids), KG_REL_SCHEMA)
  for (spec in rel_specs) {
    ids <- g$rel_type_ids[[spec$type]] %||% integer(0)
    ids <- ids[!g$rels$deleted[ids]]
    if (length(ids)) {
      src_lab <- g$nodes$label[g$rels$src[ids]]
      dst_lab <- g$nodes$label[g$rels$dst[ids]]
      ids <- ids[src_lab == spec$from & dst_lab == spec$to]
    }
    cols <- names(spec$props)
    if (length(ids)) {
      df <- data.frame(start_id = g$nodes$id[g$rels$src[ids]],
                       end_id = g$nodes$id[g$rels$dst[ids]],
                       stringsAsFactors = FALSE)
      for (p in cols) {
        df[[p]] <- if (p %in% names(g$rels)) g$rels[[p]][ids] else NA
      }
    } else {
      df <- data.frame(start_id = character(0), end_id = character(0),
                       stringsAsFactors = FALSE)
      for (p in cols) df[[p]] <- kg_empty_vec(kg_prop_type(p, "rel"))
    }
    fname <- sub("^rels/", "", spec$file)
    kg_write_csv(df, file.path(rd, fname))
  }
  # 非 TCM 关系类型（用本包建的其它图）按类型单独成文件
  for (ty in setdiff(names(g$rel_type_ids), tcm_types)) {
    ids <- g$rel_type_ids[[ty]]
    ids <- ids[!g$rels$deleted[ids]]
    if (!length(ids)) next
    cols <- setdiff(names(g$rels), KG_REL_RESERVED)
    df <- data.frame(start_id = g$nodes$id[g$rels$src[ids]],
                     end_id = g$nodes$id[g$rels$dst[ids]],
                     stringsAsFactors = FALSE)
    for (p in cols) df[[p]] <- g$rels[[p]][ids]
    kg_write_csv(df, file.path(rd, paste0(kg_snake(ty), ".csv")))
  }
  if (verbose) kg_msg("已导出 CSV 到 ", dir, "（nodes/ 与 rels/ 与 Neo4j 导入格式一致）")
  invisible(dir)
}

#' 导出 neo4j-admin 批量导入格式（:ID / :LABEL / :START_ID / :END_ID / :TYPE）
kg_export_admin <- function(g, dir = "store/neo4j_admin", verbose = TRUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  labs <- names(g$label_ids)
  nodes_all <- list()
  for (lab in labs) {
    # 只按主标签导出，多标签合并进 :LABEL（分号分隔，neo4j-admin 支持）
    ids <- which(!g$nodes$deleted & g$nodes$label == lab)
    if (!length(ids)) next
    cols <- c("id", setdiff(names(g$nodes), c(KG_NODE_RESERVED, "id")))
    df <- as.data.frame(g$nodes[ids, cols, with = FALSE], stringsAsFactors = FALSE)
    names(df)[1] <- ":ID"
    df[[":LABEL"]] <- vapply(seq_along(ids), function(i) {
      paste(c(lab, g$nodes$extra_labels[[ids[i]]]), collapse = ";")
    }, "")
    nodes_all[[lab]] <- df
  }
  all_nodes <- data.table::rbindlist(lapply(nodes_all, data.table::as.data.table),
                                     fill = TRUE, use.names = TRUE)
  kg_write_csv(all_nodes, file.path(dir, "nodes_all.csv"), row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  rels <- which(!g$rels$deleted)
  df <- data.frame(`:START_ID` = g$nodes$id[g$rels$src[rels]],
                   `:END_ID` = g$nodes$id[g$rels$dst[rels]],
                   `:TYPE` = g$rels$type[rels],
                   stringsAsFactors = FALSE, check.names = FALSE)
  for (p in setdiff(names(g$rels), KG_REL_RESERVED)) {
    df[[p]] <- g$rels[[p]][rels]
  }
  kg_write_csv(df, file.path(dir, "rels_all.csv"), row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  script <- c(
    "REM 由 07_rgraph 生成：neo4j-admin database import",
    "neo4j-admin database import full \\",
    paste0("  --nodes=nodes_all.csv \\"),
    paste0("  --relationships=rels_all.csv \\"),
    paste0("  --ignore-empty-strings=true --trim-strings=false neo4j")
  )
  writeLines(script, file.path(dir, "import.cmd"), useBytes = TRUE)
  if (verbose) kg_msg("已导出 neo4j-admin 导入目录: ", dir)
  invisible(dir)
}

#' 导出 GraphML（可被 Gephi / yEd / igraph 读取）
kg_export_graphml <- function(g, path = "store/kg.graphml", nodes = NULL, types = NULL) {
  if (!requireNamespace("igraph", quietly = TRUE)) kg_abort("需要 igraph 包")
  ig <- kg_to_igraph(g, nodes, types, directed = TRUE)
  igraph::write_graph(ig, path, format = "graphml")
  kg_msg("已导出 GraphML: ", path)
  invisible(path)
}

#' 导出 node-link JSON（D3 / vis.js 可直接用）
kg_export_json <- function(g, path = "store/kg.json", nodes = NULL, types = NULL) {
  if (is.null(nodes)) nodes <- which(!g$nodes$deleted) else nodes <- as.integer(nodes)
  edges <- which(!g$rels$deleted & g$rels$src %in% nodes & g$rels$dst %in% nodes)
  if (!is.null(types)) edges <- edges[g$rels$type[edges] %in% types]
  n <- lapply(nodes, function(i) {
    p <- kg_node_props(g, i)
    list(id = g$nodes$id[i], label = g$nodes$label[i],
         properties = p[setdiff(names(p), c("full_text", "text"))])
  })
  e <- lapply(edges, function(i) {
    list(source = g$nodes$id[g$rels$src[i]], target = g$nodes$id[g$rels$dst[i]],
         type = g$rels$type[i])
  })
  json <- paste0('{"nodes":[',
                 paste(vapply(n, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), ""),
                       collapse = ","),
                 '],"links":[',
                 paste(vapply(e, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), ""),
                       collapse = ","), "]}")
  writeLines(json, path, useBytes = TRUE)
  kg_msg("已导出 JSON: ", path)
  invisible(path)
}

#' 导出为 R 原生结构（list of data.frame，便于 readRDS 后二次加工）
kg_export_tables <- function(g) {
  list(
    nodes = data.table::copy(g$nodes[!g$nodes$deleted]),
    rels = data.table::copy(g$rels[!g$rels$deleted]),
    meta = g$meta,
    schema = list(labels = KG_LABEL_SCHEMA, rels = KG_REL_SCHEMA)
  )
}
