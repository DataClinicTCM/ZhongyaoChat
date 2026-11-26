
# =============================================================================
# algo.R — 图算法（对标 Neo4j Graph Data Science / Cypher 路径查询）
#
# 覆盖：度中心性、PageRank、连通分量、社区发现、最短路径、k 跳邻域、
#       子图抽取、三角形计数、本体 schema 聚合、度数分布。
# 底层用 igraph（若未安装则退化为纯 R 实现的关键算法）。
# =============================================================================

#' 把（子）图转成 igraph 对象
kg_to_igraph <- function(g, nodes = NULL, types = NULL, directed = TRUE) {
  if (!requireNamespace("igraph", quietly = TRUE))
    kg_abort("需要 igraph 包：install.packages('igraph')")
  if (is.null(nodes)) nodes <- which(!g$nodes$deleted) else nodes <- as.integer(nodes)
  edges <- which(!g$rels$deleted & g$rels$src %in% nodes & g$rels$dst %in% nodes)
  if (!is.null(types)) edges <- edges[g$rels$type[edges] %in% types]
  vdf <- data.frame(name = as.character(nodes),
                    label = g$nodes$label[nodes],
                    label_name = kg_node_display(g, nodes),
                    stringsAsFactors = FALSE)
  if (!length(edges)) {
    return(igraph::graph_from_data_frame(
      data.frame(from = character(0), to = character(0), type = character(0)),
      directed = directed, vertices = vdf))
  }
  edf <- data.frame(from = as.character(g$rels$src[edges]),
                    to = as.character(g$rels$dst[edges]),
                    type = g$rels$type[edges],
                    stringsAsFactors = FALSE)
  ig <- igraph::graph_from_data_frame(edf, directed = directed, vertices = vdf)
  ig
}

#' 度中心性表
kg_degree <- function(g, nodes = NULL, direction = "both", types = NULL) {
  if (is.null(nodes)) nodes <- which(!g$nodes$deleted)
  nodes <- as.integer(nodes)
  outd <- vapply(nodes, function(i) length(kg_adj(g, i, "out", types)), integer(1))
  ind <- vapply(nodes, function(i) length(kg_adj(g, i, "in", types)), integer(1))
  data.frame(node = nodes, id = g$nodes$id[nodes], label = g$nodes$label[nodes],
             name = kg_node_display(g, nodes),
             out_degree = outd, in_degree = ind,
             degree = if (direction == "both") outd + ind else
               if (direction == "out") outd else ind,
             stringsAsFactors = FALSE)
}

#' 度数最高的节点
kg_top_nodes <- function(g, n = 20, direction = "both", types = NULL, label = NULL) {
  nodes <- if (is.null(label)) which(!g$nodes$deleted) else g$label_ids[[label]] %||% integer(0)
  d <- kg_degree(g, nodes, direction, types)
  d <- d[order(-d$degree), ]
  utils::head(d, n)
}

#' PageRank（幂迭代；等价 gds.pageRank）
kg_pagerank <- function(g, nodes = NULL, types = NULL, damping = 0.85, iterations = 30L) {
  if (is.null(nodes)) nodes <- which(!g$nodes$deleted)
  nodes <- as.integer(nodes)
  n <- length(nodes)
  if (!n) return(data.frame())
  idx_map <- stats::setNames(seq_len(n), as.character(nodes))
  edges <- which(!g$rels$deleted & g$rels$src %in% nodes & g$rels$dst %in% nodes)
  if (!is.null(types)) edges <- edges[g$rels$type[edges] %in% types]
  src <- unname(idx_map[as.character(g$rels$src[edges])])
  dst <- unname(idx_map[as.character(g$rels$dst[edges])])
  src <- src[!is.na(src)]; dst <- dst[!is.na(dst)]
  outdeg <- tabulate(src, nbins = n)
  pr <- rep(1 / n, n)
  for (it in seq_len(iterations)) {
    contrib <- ifelse(outdeg[src] > 0, pr[src] / outdeg[src], 0)
    acc <- numeric(n)
    if (length(dst)) {
      agg <- rowsum(contrib, group = dst, reorder = TRUE)
      acc[as.integer(rownames(agg))] <- as.numeric(agg[, 1])
    }
    dangling <- sum(pr[outdeg == 0]) / n
    new <- (1 - damping) / n + damping * (acc + dangling)
    if (sum(abs(new - pr)) < 1e-10) { pr <- new; break }
    pr <- new
  }
  out <- data.frame(node = nodes, id = g$nodes$id[nodes], label = g$nodes$label[nodes],
                    name = kg_node_display(g, nodes),
                    pagerank = as.numeric(pr), stringsAsFactors = FALSE)
  out[order(-out$pagerank), ]
}

#' 弱连通分量
kg_components <- function(g, nodes = NULL, types = NULL, mode = "weak") {
  ig <- kg_to_igraph(g, nodes, types, directed = (mode == "strong"))
  comp <- igraph::components(ig, mode = mode)
  vn <- as.integer(igraph::V(ig)$name)
  data.frame(node = vn,
             id = g$nodes$id[vn],
             label = g$nodes$label[vn],
             component = as.integer(comp$membership),
             size = as.integer(comp$csize[comp$membership]),
             stringsAsFactors = FALSE)
}

#' 社区发现（Louvain / Walktrap / Fast Greedy）
kg_communities <- function(g, nodes = NULL, types = NULL,
                           algorithm = c("louvain", "walktrap", "fast_greedy")) {
  algorithm <- match.arg(algorithm)
  ig <- kg_to_igraph(g, nodes, types, directed = FALSE)
  cl <- switch(algorithm,
    louvain = igraph::cluster_louvain(ig),
    walktrap = igraph::cluster_walktrap(ig),
    fast_greedy = igraph::cluster_fast_greedy(ig))
  vn <- as.integer(igraph::V(ig)$name)
  data.frame(node = vn,
             id = g$nodes$id[vn],
             label = g$nodes$label[vn],
             name = kg_node_display(g, vn),
             community = as.integer(igraph::membership(cl)),
             stringsAsFactors = FALSE)
}

#' 中介中心性（igraph，采样）
kg_betweenness <- function(g, nodes = NULL, types = NULL, sample_size = 500) {
  ig <- kg_to_igraph(g, nodes, types, directed = FALSE)
  vn <- igraph::vcount(ig)
  b <- if (vn > sample_size)
    igraph::estimate_betweenness(ig, vids = igraph::V(ig), cutoff = 3,
                                 directed = FALSE) else
    igraph::betweenness(ig, directed = FALSE)
  idx <- as.integer(igraph::V(ig)$name)
  data.frame(node = idx,
             id = g$nodes$id[idx],
             label = g$nodes$label[idx],
             betweenness = as.numeric(b),
             stringsAsFactors = FALSE)[order(-b), ]
}

#' 三角形计数
kg_triangle_count <- function(g, nodes = NULL, types = NULL) {
  ig <- kg_to_igraph(g, nodes, types, directed = FALSE)
  igraph::count_triangles(ig)
}

#' k 跳邻域（等价 Cypher 变长路径）
kg_khop <- function(g, id, k = 2L, types = NULL, direction = "both") {
  idx <- if (is.character(id)) kg_node_idx(g, id) else id$idx
  frontier <- idx
  seen <- idx
  levels <- list(idx)
  for (d in seq_len(k)) {
    nxt <- setdiff(kg_expand(g, frontier, types, direction), seen)
    if (!length(nxt)) break
    levels[[length(levels) + 1]] <- nxt
    seen <- c(seen, nxt)
    frontier <- nxt
  }
  list(nodes = seen, levels = levels,
       edges = which(g$rels$src %in% seen & g$rels$dst %in% seen))
}

#' 抽取子图（保留指定节点及其内部关系）
kg_subgraph <- function(g, nodes, types = NULL) {
  nodes <- as.integer(nodes)
  edges <- which(g$rels$src %in% nodes & g$rels$dst %in% nodes)
  if (!is.null(types)) edges <- edges[g$rels$type[edges] %in% types]
  list(nodes = nodes, edges = edges,
       node_ids = g$nodes$id[nodes], rel_types = table(g$rels$type[edges]))
}

#' 本体 schema 聚合（对标 61_bloom_schema_graph.cypher）
kg_schema_graph <- function(g) {
  edges <- which(!g$rels$deleted)
  if (!length(edges)) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  from_lab <- g$nodes$label[g$rels$src[edges]]
  to_lab <- g$nodes$label[g$rels$dst[edges]]
  key <- paste(g$rels$type[edges], from_lab, to_lab, sep = "\u0001")
  tb <- table(key)
  parts <- do.call(rbind, strsplit(names(tb), "\u0001", fixed = TRUE))
  e <- data.frame(type = parts[, 1], from = parts[, 2], to = parts[, 3],
                  count = as.integer(tb), stringsAsFactors = FALSE)
  e <- e[order(-e$count), ]
  labs <- names(g$label_ids)
  counts <- vapply(g$label_ids, length, integer(1))
  n <- data.frame(label = labs, count = as.integer(counts),
                  out_degree = as.integer(table(factor(e$from, levels = labs))),
                  in_degree = as.integer(table(factor(e$to, levels = labs))),
                  stringsAsFactors = FALSE)
  n <- n[order(-n$count), ]
  list(nodes = n, edges = e)
}

#' 度数分布
kg_degree_distribution <- function(g, label = NULL, direction = "both") {
  nodes <- if (is.null(label)) which(!g$nodes$deleted) else g$label_ids[[label]] %||% integer(0)
  d <- kg_degree(g, nodes, direction)$degree
  tb <- table(d)
  data.frame(degree = as.integer(names(tb)), count = as.integer(tb),
             stringsAsFactors = FALSE)
}

#' 图算法总览（一次跑完常用指标）
kg_algo_summary <- function(g, nodes = NULL, types = NULL) {
  ig <- kg_to_igraph(g, nodes, types, directed = TRUE)
  list(
    nodes = igraph::vcount(ig),
    edges = igraph::ecount(ig),
    density = igraph::edge_density(ig),
    mean_degree = mean(igraph::degree(ig, mode = "all")),
    max_degree = max(igraph::degree(ig, mode = "all")),
    components = igraph::components(ig, mode = "weak")$no,
    diameter = tryCatch(igraph::diameter(ig, directed = FALSE), error = function(e) NA_real_),
    transitivity = tryCatch(igraph::transitivity(ig, type = "global"), error = function(e) NA_real_),
    triangles = tryCatch(sum(igraph::count_triangles(ig)) / 3, error = function(e) NA_real_)
  )
}
