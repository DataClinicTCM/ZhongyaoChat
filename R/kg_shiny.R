# =============================================================================
# kg_shiny.R — 「知识图谱」页：把 R/kg/viz.R 的图谱可视化接到网页上
#
# 页面两块，分别对应 viz.R 里的函数：
#   关系网络     kg_vis_network()                    交互式 visNetwork；点节点看档案，可换中心下钻
#   本体总览     kg_plot_schema()                    35 类节点 / 43 类关系的聚合鸟瞰
#
# 图库以只读方式加载（data/kg_store.rds），本页不做任何写操作。
# 子图从起点按「跳数 + 关系类型」扩展；超过节点上限时，第 1 跳按关系类型轮流取，
# 避免截断后只剩单一关系类型、把图看偏。
# =============================================================================

kg_viz_ready <- function() {
  requireNamespace("visNetwork", quietly = TRUE) && requireNamespace("igraph", quietly = TRUE)
}

# 匹配多个同名实体时，谁排在前面（原书正文里药名最常指代 Herb）
KG_MATCH_PRIORITY <- c("Herb", "Formula", "Compound", "Efficacy", "Indication",
                       "PharmEffect", "Book", "HerbAlias", "HerbVariant")

# ---------------------------------------------------------------------------
# 1. 起点解析
# ---------------------------------------------------------------------------
# 先在实体词典里做最长匹配（rag_link：支持别名、异名、处方用名），
# 词典失手时退回「名字里包含关键词」的模糊匹配。
kg_seed_matches <- function(g, query, limit = 20L) {
  q <- trimws(as.character(query %||% ""))
  if (!nzchar(q)) return(NULL)

  out <- NULL
  if (exists("rag_link", mode = "function")) {
    lk <- tryCatch(rag_link(q), error = function(e) NULL)
    if (!is.null(lk) && nrow(lk)) {
      out <- data.frame(term = lk$term, label = lk$label, id = lk$id,
                        name = lk$name, stringsAsFactors = FALSE)
    }
  }

  if (is.null(out) || !nrow(out)) {
    nm <- g$nodes$name
    hit <- which(!is.na(nm) & nzchar(nm) & grepl(q, nm, fixed = TRUE))
    if (length(hit)) {
      # 精确同名的排最前，其余按名字长度（越短越像正名）
      rank <- ifelse(nm[hit] == q, 0L, 1L)
      hit <- hit[order(rank, nchar(nm[hit]), nm[hit])]
      hit <- utils::head(hit, limit)
      out <- data.frame(term = q, label = g$nodes$label[hit], id = g$nodes$id[hit],
                        name = nm[hit], stringsAsFactors = FALSE)
    }
  }

  if (is.null(out) || !nrow(out)) return(NULL)
  prio <- match(out$label, KG_MATCH_PRIORITY)
  prio[is.na(prio)] <- length(KG_MATCH_PRIORITY) + 1L
  out <- out[order(prio, nchar(out$name), out$name), , drop = FALSE]
  out <- out[!duplicated(out$id), , drop = FALSE]
  utils::head(out, limit)
}

# ---------------------------------------------------------------------------
# 2. 子图抽取
# ---------------------------------------------------------------------------
# 第 1 跳按关系类型轮流取，保证截断后各关系类型都有代表
kg_pick_nodes <- function(g, center, ordered, max_nodes) {
  if (length(ordered) <= max_nodes) return(ordered)
  keep <- center
  adj <- kg_adj(g, center, "both")
  other <- kg_rel_other(g, adj, center)
  typ <- g$rels$type[adj]
  if (length(adj)) {
    ok <- !is.na(other) & other %in% ordered
    other <- other[ok]
    typ <- typ[ok]
    buckets <- split(other, typ)
    buckets <- lapply(buckets, unique)
    repeat {
      added <- FALSE
      for (t in names(buckets)) {
        b <- setdiff(buckets[[t]], keep)
        if (!length(b)) next
        keep <- c(keep, b[1])
        buckets[[t]] <- b[-1]
        added <- TRUE
        if (length(keep) >= max_nodes) break
      }
      if (!added || length(keep) >= max_nodes) break
    }
  }
  # 还有空位就按广度优先顺序补齐（第 2 跳及以后）
  if (length(keep) < max_nodes) {
    rest <- ordered[!ordered %in% keep]
    keep <- c(keep, utils::head(rest, max_nodes - length(keep)))
  }
  keep
}

kg_build_view <- function(g, center, k = 1L, types = NULL, max_nodes = 150L) {
  idx <- kg_node_idx(g, center)
  if (is.na(idx)) return(NULL)
  # kg_khop() 只接受「节点 id 或节点值对象」，不接受裸索引，所以这里传 id
  kh <- kg_khop(g, g$nodes$id[idx], k = max(1L, as.integer(k)), types = types)
  ordered <- unique(unlist(kh$levels))
  ordered <- ordered[!is.na(ordered)]
  nodes <- kg_pick_nodes(g, idx, ordered, max_nodes)
  edges <- which(!g$rels$deleted & g$rels$src %in% nodes & g$rels$dst %in% nodes)
  if (!is.null(types)) edges <- edges[g$rels$type[edges] %in% types]
  list(center = idx, center_id = g$nodes$id[idx],
       nodes = nodes, edges = edges,
       found = length(ordered), truncated = length(ordered) > length(nodes),
       depth = length(kh$levels) - 1L,
       n_rel_types = length(unique(g$rels$type[edges])))
}

# visNetwork 的节点 id 是「图节点行号」的字符串（前端载荷小、不掺中文冒号），
# 点击事件回传的也是行号，这里换回真实节点 id 才能查档案、能当新的中心
kg_vis_node_id <- function(g, vis_id) {
  n <- suppressWarnings(as.integer(as.character(vis_id %||% "")))
  if (length(n) != 1L || is.na(n) || n < 1L || n > nrow(g$nodes)) return(NULL)
  g$nodes$id[n]
}

# ---------------------------------------------------------------------------
# 3. 小部件
# ---------------------------------------------------------------------------
kg_focus_button <- function(id, label, hint = NULL, class = "kg-match") {
  js <- sprintf("Shiny.setInputValue('kgFocusPick', '%s', {priority: 'event'}); return false;",
                gsub("'", "&#39;", as.character(id), fixed = TRUE))
  tags$button(
    type = "button", class = class, onclick = js,
    tags$span(label),
    if (!is.null(hint)) tags$span(class = "kg-match-label", hint)
  )
}

kg_chip <- function(...) tags$span(class = "kg-chip", ...)

# 节点的「档案」面板：属性 + 关系构成 + 邻居
kg_node_detail_ui <- function(g, id, max_props = 12L, max_neighbors = 12L) {
  pr <- tryCatch(kg_profile(g, id), error = function(e) NULL)
  if (is.null(pr) || is.null(pr$node)) {
    return(tags$p(class = "zh-empty", "节点不存在"))
  }
  nd <- pr$node
  props <- nd$props
  nm <- kg_node_display(g, nd$idx)

  prop_rows <- lapply(utils::head(names(props), max_props), function(k) {
    v <- as.character(props[[k]])
    v <- paste(v, collapse = " / ")
    if (nchar(v) > 220) v <- paste0(substr(v, 1, 220), " …")
    tags$div(
      class = "kg-prop",
      tags$span(class = "kg-prop-k", k),
      tags$span(class = "kg-prop-v", v)
    )
  })

  rels <- pr$rels
  # 关系构成：按「类型 × 出入方向」计数（只出现单方向时也要有 0 列）
  by_type <- if (nrow(rels)) {
    lv <- names(sort(table(rels$rel_type), decreasing = TRUE))
    data.frame(
      type = lv,
      out = as.integer(table(factor(rels$rel_type[rels$direction == "out"], levels = lv))),
      inn = as.integer(table(factor(rels$rel_type[rels$direction == "in"], levels = lv))),
      stringsAsFactors = FALSE
    )
  } else NULL

  nb <- if (nrow(rels)) {
    hh <- utils::head(rels[!duplicated(rels$target), , drop = FALSE], max_neighbors)
    data.frame(id = hh$target,
               name = kg_node_display(g, match(hh$target, g$nodes$id)),
               label = hh$target_label,
               rel = hh$rel_type,
               dir = hh$direction,
               stringsAsFactors = FALSE)
  } else NULL

  tags$div(
    class = "kg-detail",
    tags$div(class = "kg-detail-head",
             tags$span(class = "kg-detail-name", nm),
             tags$span(class = "kg-match-label", nd$id)),
    tags$div(class = "kg-detail-meta",
             sprintf("共 %d 条关系", pr$rel_count),
             if (!is.null(by_type)) sprintf(" / %d 种关系类型", nrow(by_type))),
    tags$div(class = "kg-detail-sec", prop_rows),
    if (!is.null(by_type)) {
      tags$div(
        class = "kg-detail-sec",
        tags$div(class = "kg-detail-h", "关系构成"),
        tags$table(
          class = "kg-rel-table",
          tags$thead(tags$tr(tags$th("关系"), tags$th("出"), tags$th("入"))),
          tags$tbody(lapply(seq_len(nrow(by_type)), function(i) {
            tags$tr(tags$td(by_type$type[i]),
                    tags$td(by_type$out[i]),
                    tags$td(by_type$inn[i]))
          }))
        )
      )
    },
    if (!is.null(nb)) {
      tags$div(
        class = "kg-detail-sec",
        tags$div(class = "kg-detail-h", sprintf("邻居（前 %d 个，点击换中心）", nrow(nb))),
        lapply(seq_len(nrow(nb)), function(i) {
          arrow <- if (identical(nb$dir[i], "out")) "→" else "←"
          kg_focus_button(nb$id[i], nb$name[i],
                          paste0(arrow, " ", nb$rel[i], " · ", nb$label[i]))
        })
      )
    },
    tags$div(class = "kg-detail-sec",
             kg_focus_button(nd$id, "以该节点为中心重新展开",
                             class = "btn btn-sm btn-outline-success w-100"))
  )
}

# 交互图：viz.R 的 kg_vis_network() 之上补 hover 高亮、导航按钮与点击事件
KG_CLICK_JS <- paste0(
  "function(nodes){",
  "  if (!nodes.nodes.length) { Shiny.setInputValue('kgClick', null); return; }",
  "  Shiny.setInputValue('kgClick', nodes.nodes[0], {priority: 'event'});",
  "}"
)

kg_network_widget <- function(g, view, types = NULL, physics = TRUE, height = "640px") {
  w <- kg_vis_network(g, view$nodes, types = types, height = height, physics = physics)
  # 节点多时缩小字号，否则名字糊成一片
  if (length(view$nodes) > 60) w <- visNetwork::visNodes(w, font = list(size = 10))
  w |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = FALSE
    ) |>
    visNetwork::visInteraction(navigationButtons = TRUE, hover = TRUE) |>
    visNetwork::visEvents(click = KG_CLICK_JS)
}

# ---------------------------------------------------------------------------
# 4. UI
# ---------------------------------------------------------------------------
kg_page_ui <- function() {
  g <- tryCatch(kg_store_get(), error = function(e) NULL)
  if (is.null(g) || !kg_viz_ready()) {
    return(card(card_body(
      tags$p(class = "zh-empty",
             "图谱未就绪：需要 data/kg_store.rds 以及 igraph、visNetwork 两个包。")
    )))
  }

  n_nodes <- sum(!g$nodes$deleted)
  n_rels  <- sum(!g$rels$deleted)
  rel_types <- sort(unique(g$rels$type[!g$rels$deleted]))

  layout_sidebar(
    sidebar = sidebar(
      width = 330,
      open = "desktop",
      title = tags$span(class = "fw-semibold", "图谱探索"),
      textInput("kgQuery", "起点实体",
                placeholder = "药名 / 成分 / 方剂 / 病证，如 人参、槲皮素"),
      actionButton("kgRun", "查找", class = "btn-primary w-100"),
      uiOutput("kgMatches"),
      tags$hr(),
      selectInput("kgK", "扩展跳数",
                  choices = c("1 跳（直接邻居）" = 1, "2 跳" = 2), selected = 1),
      selectizeInput("kgRel", "关系类型（不选＝全部）", choices = rel_types,
                     multiple = TRUE, options = list(placeholder = "全部关系")),
      numericInput("kgMax", "最多显示节点", value = 150, min = 20, max = 600, step = 10),
      checkboxInput("kgPhysics", "力导向布局（节点多时关掉更流畅）", value = TRUE),
      tags$div(
        class = "kg-scale small text-muted",
        sprintf("%s 个节点 / %s 条关系", format(n_nodes, big.mark = ","),
                format(n_rels, big.mark = ",")),
        tags$br(),
        sprintf("%d 类实体 / %d 类关系", length(unique(g$nodes$label)), length(rel_types))
      )
    ),

    tags$div(class = "d-flex flex-wrap gap-2 align-items-center mb-2", uiOutput("kgStats")),

    navset_card_pill(
      id = "kgTabs",
      nav_panel(
        "关系网络",
        layout_columns(
          col_widths = c(8, 4),
          card_body(uiOutput("kgNetUI")),
          card_body(class = "kg-detail-scroll", uiOutput("kgDetail"))
        )
      ),
      nav_panel(
        "本体总览",
        card(
          full_screen = TRUE,   # 点右上角放大，图会按新尺寸重画，标签不容易挤在一起
          card_header("本体鸟瞰：节点＝实体类型（大小 ~ 数量），边＝关系类型（粗细 ~ 条数）"),
          card_body(
            layout_columns(
              col_widths = c(5, 4),
              selectInput("kgSchemaMin", "只画关系条数 ≥",
                          choices = c("全部" = 1, "10" = 10, "50" = 50, "150" = 150),
                          selected = 50),
              checkboxInput("kgSchemaLabels", "显示关系类型标签", value = TRUE)
            ),
            plotOutput("kgSchema", height = "620px"),
            tags$p(class = "small text-muted",
                   "本体是「以药为中心」的星形结构，实体类型多时中心标签必然重叠：",
                   "调高关系阈值、点右上角放大，或到「关系网络」页按实体逐层展开。")
          )
        )
      ),
    )
  )
}

# ---------------------------------------------------------------------------
# 5. Server
# ---------------------------------------------------------------------------
kg_page_server <- function(input, output, session, g = kg_store_get()) {

  matches <- reactiveVal(NULL)
  focus   <- reactiveVal(NULL)
  clicked <- reactiveVal(NULL)

  # ---- 起点解析 ----
  observeEvent(input$kgQuery, ignoreInit = TRUE, {
    q <- trimws(input$kgQuery %||% "")
    if (nchar(q) < 2L) {            # 单字过滤太松，等用户再打一个字
      matches(NULL)
      return()
    }
    matches(kg_seed_matches(g, q))
  })

  output$kgMatches <- renderUI({
    m <- matches()
    if (is.null(m) || !nrow(m)) {
      return(tags$div(class = "small text-muted mt-2",
                      "没有匹配的实体，换个写法试试（可直接用原书里的名称）"))
    }
    tags$div(
      class = "kg-matches mt-2",
      tags$div(class = "small text-muted mb-1",
               sprintf("匹配到 %d 个实体，点一个设为起点：", nrow(m))),
      lapply(seq_len(min(nrow(m), 8L)), function(i) {
        kg_focus_button(m$id[i], m$name[i], m$label[i])
      })
    )
  })

  observeEvent(input$kgRun, {
    q <- trimws(input$kgQuery %||% "")
    m <- kg_seed_matches(g, q)
    matches(m)
    if (!is.null(m) && nrow(m)) {
      focus(m$id[1])
      clicked(NULL)
    }
  })

  observeEvent(input$kgFocusPick, {
    focus(input$kgFocusPick)
    clicked(NULL)
  })

  # ---- 子图 ----
  selected_types <- reactive({
    v <- input$kgRel
    if (is.null(v) || !length(v)) NULL else as.character(v)
  })

  view <- reactive({
    id <- focus()
    if (is.null(id)) return(NULL)
    kg_build_view(g, id,
                  k = as.integer(input$kgK %||% 1L),
                  types = selected_types(),
                  max_nodes = as.integer(input$kgMax %||% 150L))
  })

  output$kgStats <- renderUI({
    v <- view()
    if (is.null(v)) {
      return(tags$div(class = "small text-muted",
                      "左侧输入实体名后点「查找」，再点匹配结果里的实体即可出图。"))
    }
    head_txt <- sprintf("%s（%s）", kg_node_display(g, v$center), g$nodes$label[v$center])
    tags$div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      kg_chip("中心：", head_txt),
      kg_chip(sprintf("子图 %d 节点 / %d 关系", length(v$nodes), length(v$edges))),
      kg_chip(sprintf("%d 类关系", v$n_rel_types)),
      if (v$truncated) {
        kg_chip(sprintf("第 %d 跳共 %s 个节点，已截断到 %d 个",
                        max(v$depth, 1L), format(v$found, big.mark = ","), length(v$nodes)))
      }
    )
  })

  # 还没选实体时给一张说明卡，避免空白画布
  output$kgNetUI <- renderUI({
    if (is.null(view())) {
      return(tags$div(
        class = "kg-placeholder",
        tags$p("在左侧输入一个实体（药名、成分、方剂、病证都行），点「查找」，",
               "再点匹配结果里的任意一项，这里就会画出以它为中心的关系网络。"),
        tags$p(class = "small text-muted",
               "画好之后：点节点看档案、双击空白处缩小、拖拽换中心（或点右侧邻居）。")
      ))
    }
    visNetwork::visNetworkOutput("kgNet", height = "640px")
  })

  output$kgNet <- visNetwork::renderVisNetwork({
    v <- view()
    if (is.null(v) || !length(v$nodes)) return(NULL)
    kg_network_widget(g, v, types = selected_types(),
                      physics = isTRUE(input$kgPhysics))
  })

  # visNetwork 回传的是节点行号，换回真实 id 再查档案（否则 kg_profile 找不到节点）
  observeEvent(input$kgClick, {
    clicked(kg_vis_node_id(g, input$kgClick))
  })

  output$kgDetail <- renderUI({
    id <- clicked() %||% focus()
    if (is.null(id)) {
      return(tags$p(class = "zh-empty", "点击图中的节点，这里显示它的属性与关系"))
    }
    kg_node_detail_ui(g, id)
  })

  # ---- 本体总览 ----
  output$kgSchema <- renderPlot({
    kg_plot_schema(
      g,
      min_edge_count = as.integer(input$kgSchemaMin %||% 1L),
      edge_label_cex = if (isTRUE(input$kgSchemaLabels)) 0.45 else 0
    )
  })

}
