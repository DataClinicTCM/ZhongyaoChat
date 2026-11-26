
# =============================================================================
# viz.R — 可视化（对标 Neo4j Bloom / Browser 的图形视图）
#
#   kg_plot_schema()     本体鸟瞰（35 类节点 / 43 类关系，等价 61_bloom_schema_graph）
#   kg_plot_subgraph()   子图（等价 Bloom 的 Perspective）
#   kg_vis_network()     交互式 HTML（visNetwork）
#   kg_plot_degree()     度数分布
#   kg_plot_top()        度数 Top-N 条形图
# =============================================================================

KG_LABEL_COLORS <- c(
  Herb = "#2E7D32", HerbAlias = "#81C784", HerbVariant = "#A5D6A7",
  DrugCategory = "#1B5E20", Family = "#66BB6A", Species = "#43A047",
  MedicinalPart = "#7CB342", Compound = "#1565C0", ChemicalClass = "#1E88E5",
  PharmEffect = "#5E35B1", ExperimentalModel = "#7E57C2", Efficacy = "#00897B",
  Indication = "#00ACC1", Pathogenesis = "#26C6DA", Formula = "#EF6C00",
  DosageForm = "#FB8C00", Book = "#8D6E63", Quotation = "#A1887F",
  ProcessingMethod = "#6D4C41", Taste = "#C0CA33", Nature = "#9E9D24",
  Toxicity = "#F4511E", Meridian = "#FDD835", UsageRoute = "#FFB300",
  Contraindication = "#E53935", CultivationTechnique = "#7CB342",
  Harvest = "#9CCC65", Habitat = "#AED581", Region = "#4FC3F7",
  AppearanceFeature = "#4DB6AC", QualityCriterion = "#009688",
  Section = "#90A4AE", SourceDoc = "#546E7A", TextUnit = "#B0BEC5",
  SchemaVersion = "#CFD8DC"
)

kg_color_for <- function(label) {
  cols <- KG_LABEL_COLORS[label]
  if (is.na(cols)) "#9E9E9E" else unname(cols)
}

#' 本体鸟瞰图（节点=标签，边=关系类型聚合）
kg_plot_schema <- function(g, path = NULL, width = 2400, height = 1800,
                           edge_label_cex = 0.5, min_edge_count = 1,
                           niter = 4000) {
  if (!requireNamespace("igraph", quietly = TRUE)) kg_abort("需要 igraph 包")
  sg <- kg_schema_graph(g)
  if (!nrow(sg$nodes)) kg_abort("图为空")
  nodes <- sg$nodes
  edf <- sg$edges[sg$edges$count >= min_edge_count, , drop = FALSE]
  ig <- igraph::graph_from_data_frame(
    data.frame(from = edf$from, to = edf$to, label = edf$type, weight = log1p(edf$count),
               stringsAsFactors = FALSE),
    directed = TRUE,
    vertices = data.frame(name = nodes$label, size = 6 + 6 * log1p(nodes$count) / max(log1p(nodes$count)),
                          color = vapply(nodes$label, kg_color_for, ""),
                          stringsAsFactors = FALSE))
  igraph::V(ig)$label <- igraph::V(ig)$name
  igraph::V(ig)$frame.color <- "white"
  igraph::V(ig)$label.color <- "black"
  igraph::V(ig)$label.cex <- 1.05
  igraph::E(ig)$arrow.size <- 0.3
  igraph::E(ig)$color <- "grey65"
  igraph::E(ig)$curved <- 0.15
  igraph::E(ig)$label.cex <- edge_label_cex
  igraph::E(ig)$label.color <- "grey30"
  lay <- igraph::layout_with_fr(ig, niter = niter,
                                weights = igraph::E(ig)$weight)
  if (!is.null(path)) grDevices::png(path, width = width, height = height, res = 150)
  op <- graphics::par(mar = c(0, 0, 2, 0))
  on.exit({graphics::par(op); if (!is.null(path)) grDevices::dev.off()}, add = TRUE)
  plot(ig, layout = lay,
       main = paste0("TCM-KG 本体鸟瞰：", nrow(nodes), " 类节点 / ", nrow(edf), " 类关系"),
       cex.main = 1.3)
  graphics::legend("bottomleft", legend = c("节点大小 ~ 实体数量（对数）"), bty = "n", cex = 0.9)
  invisible(ig)
}

#' 子图可视化
kg_plot_subgraph <- function(g, nodes, path = NULL, types = NULL,
                             max_nodes = 300L, width = 1400, height = 1100) {
  if (!requireNamespace("igraph", quietly = TRUE)) kg_abort("需要 igraph 包")
  nodes <- as.integer(nodes)
  if (length(nodes) > max_nodes) nodes <- utils::head(nodes, max_nodes)
  ig <- kg_to_igraph(g, nodes, types, directed = TRUE)
  if (igraph::vcount(ig) == 0) return(invisible(NULL))
  igraph::V(ig)$color <- vapply(igraph::V(ig)$label, kg_color_for, "")
  lab <- ifelse(is.na(igraph::V(ig)$label_name) | !nzchar(igraph::V(ig)$label_name),
                igraph::V(ig)$label, igraph::V(ig)$label_name)
  igraph::V(ig)$label <- lab
  igraph::V(ig)$label.cex <- 0.7
  igraph::V(ig)$label.color <- "black"
  igraph::V(ig)$frame.color <- "white"
  igraph::V(ig)$size <- 6
  igraph::E(ig)$arrow.size <- 0.25
  igraph::E(ig)$color <- "grey75"
  igraph::E(ig)$label <- igraph::E(ig)$type
  igraph::E(ig)$label.cex <- 0.5
  igraph::E(ig)$label.color <- "grey30"
  if (!is.null(path)) grDevices::png(path, width = width, height = height, res = 110)
  op <- graphics::par(mar = c(0, 0, 2, 0))
  on.exit({graphics::par(op); if (!is.null(path)) grDevices::dev.off()}, add = TRUE)
  plot(ig, layout = igraph::layout_with_fr(ig), main = "TCM-KG 子图")
  invisible(ig)
}

#' 交互式网络（visNetwork）
kg_vis_network <- function(g, nodes, types = NULL, height = "700px", physics = TRUE) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) kg_abort("需要 visNetwork 包")
  nodes <- as.integer(nodes)
  edges <- which(g$rels$src %in% nodes & g$rels$dst %in% nodes)
  if (!is.null(types)) edges <- edges[g$rels$type[edges] %in% types]
  keep <- unique(c(g$rels$src[edges], g$rels$dst[edges], nodes))
  lab <- kg_node_display(g, keep)
  nd <- data.frame(
    id = as.character(keep),
    label = lab,
    group = g$nodes$label[keep],
    title = paste0(g$nodes$label[keep], "<br/>", g$nodes$id[keep]),
    stringsAsFactors = FALSE
  )
  ed <- data.frame(
    from = as.character(g$rels$src[edges]),
    to = as.character(g$rels$dst[edges]),
    label = g$rels$type[edges],
    arrows = "to",
    stringsAsFactors = FALSE
  )
  vis <- visNetwork::visNetwork(nd, ed, height = height, width = "100%") |>
    visNetwork::visNodes(font = list(size = 14), shape = "dot") |>
    visNetwork::visEdges(font = list(size = 10, align = "middle"), color = "#BDBDBD") |>
    visNetwork::visPhysics(enabled = physics, stabilization = list(iterations = 120))
  # 配色统一走 KG_LABEL_COLORS；只对子图里出现的标签建组，未收录的标签沿用 visNetwork 默认色
  for (lb in intersect(names(KG_LABEL_COLORS), unique(nd$group))) {
    vis <- visNetwork::visGroups(vis, groupname = lb, color = KG_LABEL_COLORS[[lb]])
  }
  visNetwork::visLegend(vis, useGroups = TRUE, position = "right")
}

#' 度数分布图
kg_plot_degree <- function(g, label = NULL, path = NULL) {
  dd <- kg_degree_distribution(g, label)
  if (!is.null(path)) grDevices::png(path, width = 1200, height = 600, res = 110)
  op <- graphics::par(mar = c(4, 4, 3, 1))
  on.exit({graphics::par(op); if (!is.null(path)) grDevices::dev.off()}, add = TRUE)
  graphics::barplot(dd$count, names.arg = dd$degree, log = "y",
                    xlab = "度数", ylab = "节点数（对数）",
                    main = paste0(if (is.null(label)) "全图" else label, " 度数分布"),
                    col = "#66BB6A", border = NA)
  invisible(dd)
}

#' Top-N 高度数节点（label 只看某一类实体，等价 kg_degree_distribution() 的 label）
kg_plot_top <- function(g, n = 20, direction = "both", types = NULL, path = NULL,
                        label = NULL) {
  d <- kg_top_nodes(g, n, direction, types, label)
  main <- paste0(if (is.null(label)) "全图" else label, " · Top ", nrow(d), " 高度数节点")
  if (!nrow(d)) {
    graphics::plot.new()
    graphics::title(main = main)
    graphics::text(0.5, 0.5, "该类型下没有节点", col = "grey40")
    return(invisible(d))
  }
  d$name <- ifelse(is.na(d$name) | !nzchar(d$name), d$id, d$name)
  if (!is.null(path)) grDevices::png(path, width = 1300, height = 700, res = 110)
  op <- graphics::par(mar = c(4, 10, 3, 2))
  on.exit({graphics::par(op); if (!is.null(path)) grDevices::dev.off()}, add = TRUE)
  graphics::barplot(rev(d$degree), names.arg = rev(d$name), horiz = TRUE, las = 1,
                    col = "#42A5F5", border = NA, xlab = "度数", main = main)
  invisible(d)
}
