# =============================================================================
# map_bivariate.R — 省份双变量地图（ggplot2 + sf + ggiraph）
#
# 风格参考：Timogrossenbacher 的 bivariate maps with ggplot2 and sf
#   https://timogrossenbacher.ch/bivariate-maps-with-ggplot2-and-sf/
# 组成：地形阴影底图（Esri World Hillshade，中国境外已透明）+ 3×3 双变量配色 +
#       白色省界 + 3×3 图例 + 引线标注 + 极简主题
# 交互：ggiraph（hover 提示 / 点击选中省份），Shiny 里读 input$<id>_selected
# =============================================================================

# 3×3 双变量色板：行 = 收录数量（低→高），列 = 资源密度（低→高）
KG_BIVAR_COLORS <- matrix(
  c("#e7e3dd", "#dcb8b0", "#c8877f",
    "#bcc9da", "#b1a3c1", "#98688a",
    "#7f9dc7", "#6a6aa8", "#3d3878"),
  nrow = 3, byrow = TRUE
)

# 读地形底图并统一成 3 通道 RGB 数组（terra 写出的 PNG 可能带 alpha）
kg_read_hillshade <- function(path) {
  if (!file.exists(path)) return(NULL)
  h <- png::readPNG(path)
  if (length(dim(h)) == 2) return(array(rep(h, 3), c(dim(h), 3)))
  if (dim(h)[3] == 2) {
    g <- h[, , 1]
    return(array(rep(g, 3), c(dim(g), 3)))
  }
  h[, , 1:3, drop = FALSE]
}

# 把一列数值按排名分成 3 组（1=低，3=高），避免并列值导致分箱不均
kg_tercile <- function(x) {
  r <- rank(x, ties.method = "first", na.last = "keep")
  as.integer(cut(r, breaks = 3, labels = 1:3))
}

# ---- 南海诸岛附图（中国地图通用做法：主图右下角放小图）--------------------
# 附图经纬度范围与它在主图中的落位长宽比一致，避免附图被拉伸
KG_SEA_INSET_RANGE <- c(xmin = 105.5, xmax = 124.2, ymin = 2.6, ymax = 24.8)
KG_SEA_INSET_BOX   <- c(xmin = 124.6, xmax = 135.6, ymin = 16.9, ymax = 30.0)

kg_south_china_sea <- function(china, d = NULL) {
  is_land <- !is.na(china$province) & nzchar(china$province)
  land <- china[is_land, c("province", "geometry")]
  nine <- china[!is_land, "geometry"]   # DataV 数据中无名的那组多边形即九段线

  # 附图只强调海南省，其余陆地用统一的浅色底，避免小图上颜色成块
  fill <- rep("#e9e6e0", nrow(land))
  idx <- if (is.null(d)) NA_integer_ else match("海南", land$province)
  if (!is.na(idx)) fill[idx] <- d$fill_col[match("海南", d$province)]

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = land, fill = fill, colour = "white", linewidth = 0.22) +
    ggplot2::geom_sf(data = nine, fill = "#3c4a5a", colour = NA) +
    ggplot2::annotate("text", x = 118.6, y = 4.2, label = "南海诸岛",
                      size = 2.9, colour = "#3c4a5a") +
    ggplot2::coord_sf(xlim = KG_SEA_INSET_RANGE[1:2], ylim = KG_SEA_INSET_RANGE[3:4],
                      crs = 4326, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#f7f5f0", colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "#8a8f98", linewidth = 0.45),
      plot.background = ggplot2::element_rect(fill = "#f7f5f0", colour = NA),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

#' 构建省份双变量地图
#' @param stats data.frame（province/n_herb/density/n_toxic/top_herbs/center_x/center_y）
#' @param china sf 对象（www/china.json，已加 province 列）
#' @param hill  png::readPNG 读入的灰度地形图（可为 NULL）
#' @param hill_ext 地形图范围 c(xmin,xmax,ymin,ymax)（可为 NULL）
build_province_map <- function(stats, china, hill = NULL, hill_ext = NULL) {
  # 省界对象可能用 name（DataV 原名）而不是 province，这里统一成 province
  if (!"province" %in% names(china)) {
    china$province <- gsub("特别行政区|维吾尔自治区|壮族自治区|回族自治区|自治区|省|市", "",
                           china$name)
  }
  d <- stats
  d$g_n <- kg_tercile(d$n_herb)
  d$g_d <- kg_tercile(d$density)
  d$fill_col <- KG_BIVAR_COLORS[cbind(d$g_n, d$g_d)]
  d$tip <- sprintf("%s\n收录 %d 味（密度 %.1f 味/万km²）\n其中标注有毒 %d 味",
                   d$province, d$n_herb, d$density, d$n_toxic)
  if ("daodi_n" %in% names(d)) {
    d$tip <- paste0(d$tip, sprintf("\n道地药材 %d 味", d$daodi_n))
  }

  # 九段线（无 province 的要素）只在右下角附图里画，不进主图
  land <- china[!is.na(china$province) & nzchar(china$province), c("province", "geometry")]
  m <- merge(land, d, by = "province", all.x = TRUE)
  m$fill_col[is.na(m$fill_col)] <- "#efefef"
  m <- m[order(m$g_n, m$g_d), ]

  p <- ggplot2::ggplot()
  if (!is.null(hill) && !is.null(hill_ext)) {
    p <- p + ggplot2::annotation_raster(hill, hill_ext[1], hill_ext[2],
                                        hill_ext[3], hill_ext[4], interpolate = TRUE)
  }
  p <- p + ggiraph::geom_sf_interactive(
    data = m,
    ggplot2::aes(fill = fill_col, data_id = province, tooltip = tip),
    colour = "white", linewidth = 0.4, alpha = 0.92, inherit.aes = FALSE
  ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_sf(xlim = c(63.5, 136.5), ylim = c(16.5, 54.8), expand = FALSE,
                      crs = 4326) +
    ggplot2::labs(
      title = "中国中药资源分布：收录数量 × 资源密度",
      subtitle = paste0("《中华药海（精华本）》751 味药按省份归属（一味药可跨省）；",
                        "密度 = 收录数 / 省域面积（味/万km²）")
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#f7f5f0", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#f7f5f0", colour = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 19, colour = "#1f2d3d",
                                         margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 10.5, colour = "#5b6770",
                                            margin = ggplot2::margin(b = 12)),
      plot.margin = ggplot2::margin(14, 18, 10, 10)
    )

  if (!is.null(hill)) {
    p <- p + ggplot2::annotate("rect", xmin = 66.4, xmax = 73.2, ymin = 18.6, ymax = 28.0,
                               fill = "#f7f5f0", colour = NA, alpha = 0.95)
  }

  # ---- 3×3 图例 ----
  lx <- 67.8; ly <- 20.2; cw <- 1.3; ch <- 1.3
  tiles <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(x = lx + (0:2) * cw, y = ly + (i - 1) * ch,
               col = KG_BIVAR_COLORS[i, ], stringsAsFactors = FALSE)
  }))
  p <- p +
    ggplot2::geom_rect(
      data = tiles, inherit.aes = FALSE,
      ggplot2::aes(xmin = x, xmax = x + cw, ymin = y, ymax = y + ch, fill = col),
      colour = "#f7f5f0", linewidth = 0.6
    ) +
    ggplot2::annotate("text", x = lx + 1.5 * cw, y = ly + 3 * ch + 1.1,
                      label = "资源密度 →", size = 3.4, colour = "#3c4a5a") +
    ggplot2::annotate("text", x = lx - 0.95, y = ly + 1.5 * ch, label = "收录数量 ↑",
                      size = 3.4, colour = "#3c4a5a", angle = 90) +
    ggplot2::annotate("text", x = lx + 1.5 * cw, y = ly - 0.9, label = "低  →  高",
                      size = 3.2, colour = "#6b7785") +
    ggplot2::annotate("text", x = lx - 0.6, y = ly + 3 * ch + 3.0, hjust = 0,
                      label = "颜色 = 收录数量 × 资源密度", size = 3.3, colour = "#3c4a5a")

  # ---- 南海诸岛附图（右下角）：主图只画大陆与海南岛，附图补全南海诸岛 ----
  p <- p + ggplot2::annotation_custom(
    ggplot2::ggplotGrob(kg_south_china_sea(china, d)),
    xmin = KG_SEA_INSET_BOX[1], xmax = KG_SEA_INSET_BOX[2],
    ymin = KG_SEA_INSET_BOX[3], ymax = KG_SEA_INSET_BOX[4]
  )

  # ---- 引线标注：数量最多 / 密度最高 / 面积大但密度低 ----
  mk_note <- function(prov, dx, dy, label) {
    r <- d[d$province == prov, ]
    if (!nrow(r)) return(NULL)
    list(
      ggplot2::annotate("segment", x = r$center_x, y = r$center_y,
                        xend = r$center_x + dx, yend = r$center_y + dy,
                        colour = "#4a4a4a", linewidth = 0.35,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.4, "mm"), type = "closed")),
      ggplot2::annotate("label", x = r$center_x + dx, y = r$center_y + dy, label = label,
                        size = 3.0, colour = "#2b2b2b", fill = "#f7f5f0",
                        linewidth = 0.15, label.padding = ggplot2::unit(0.35, "mm"))
    )
  }
  notes <- list(
    mk_note("四川", -7.5, -7.0, "四川：收录最多"),
    mk_note("浙江", 6.2, 4.6, "浙江：密度最高"),
    mk_note("新疆", 2.0, 7.0, "新疆：面积最大、密度最低")
  )
  notes <- Filter(Negate(is.null), notes)
  if (length(notes)) p <- p + notes

  p
}
