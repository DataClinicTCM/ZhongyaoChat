# =============================================================================
# ui.R — 全站主题与「中药详情」排版
#
# 说明：
#   * 只做版式（分组 / 间距 / 字号 / 折叠），字段文本原样输出，不增删内容
#   * 原文中的换行、[小标题]、1. 2. ① 等序号会被识别成层级，避免糊成一段
# =============================================================================

# 主题：玉色主色 + 纸质底色，字体优先使用系统中文黑体
app_theme <- function() {
  bslib::bs_theme(
    version        = 5,
    primary        = "#2f6b4f",
    secondary      = "#7b867b",
    success        = "#2f6b4f",
    info           = "#3c7a63",
    warning        = "#b8860b",
    danger         = "#b5472f",
    bg             = "#f6f5f0",
    fg             = "#1e2a22",
    base_font      = c("Inter", "Segoe UI", "Noto Sans SC", "Source Han Sans SC",
                       "PingFang SC", "Microsoft YaHei", "system-ui", "sans-serif"),
    heading_font   = c("Inter", "Segoe UI", "Noto Sans SC", "Source Han Sans SC",
                       "PingFang SC", "Microsoft YaHei", "system-ui", "sans-serif"),
    `font-size-base`   = ".95rem",
    `border-radius`    = ".85rem",
    `card-border-radius` = "1rem"
  )
}

# ---- 详情页分区 -------------------------------------------------------------
# 顺序即原文栏目顺序；这里未列出的栏目会自动追加到「其他」分区，避免丢字段
herb_sections <- list(
  list(title = "名称与释名", cols = "药物名称"),
  list(title = "基原与栽培", cols = c("药物基原", "栽培采集")),
  list(title = "药材鉴别",   cols = "药材鉴别"),
  list(title = "化学成分",   cols = "化学成分"),
  list(title = "药理作用",   cols = "药理作用"),
  list(title = "加工炮制",   cols = "加工炮制"),
  list(title = "功效主治",   cols = "功效主治"),
  list(title = "宜忌配伍",   cols = "宜忌配伍"),
  list(title = "方剂制剂",   cols = "方剂制剂"),
  list(title = "各家学说",   cols = "各家学说")
)

# 篇幅短、查阅频率高的栏目放在页首速览
herb_quick_cols <- c("性味归经", "用法用量")

herb_blank <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))
}

# 把一段原文拆成「小标题 / 条目 / 段落」，保留全部文字
herb_text <- function(txt) {
  txt <- as.character(txt)
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(NULL)

  blocks <- lapply(lines, function(ln) {
    hit <- regmatches(ln, regexec("^\\[([^]]{1,12})\\]\\s*(.*)$", ln))[[1]]
    if (length(hit) == 3) {
      return(htmltools::tagList(
        htmltools::tags$div(class = "herb-sublabel", hit[2]),
        if (nzchar(hit[3])) htmltools::tags$p(class = "herb-p", hit[3])
      ))
    }
    numbered <- grepl("^(\\d+[.、．)）]|[①②③④⑤⑥⑦⑧⑨⑩])", ln)
    htmltools::tags$p(class = if (numbered) "herb-p herb-item" else "herb-p", ln)
  })

  htmltools::tags$div(class = "herb-text", blocks)
}

# 药品头部：名称 + 类型
herb_hero <- function(row) {
  htmltools::tags$div(
    class = "herb-hero",
    htmltools::tags$h2(class = "herb-name", row[["中药名称"]]),
    if (!herb_blank(row[["中药类型"]])) {
      htmltools::tags$span(class = "herb-type", row[["中药类型"]])
    }
  )
}

# 页首速览：性味归经 / 用法用量
herb_quick_facts <- function(row) {
  items <- lapply(herb_quick_cols, function(k) {
    if (!k %in% names(row) || herb_blank(row[[k]])) return(NULL)
    htmltools::tags$div(
      class = "quick-fact",
      htmltools::tags$div(class = "qf-k", k),
      herb_text(row[[k]])
    )
  })
  items <- Filter(Negate(is.null), items)
  if (!length(items)) return(NULL)
  htmltools::tags$div(class = "quick-facts", items)
}

# 主渲染：头部 + 速览 + 折叠分区
herb_detail_ui <- function(row) {
  if (is.null(row) || !nrow(row)) {
    return(htmltools::tags$p(class = "zh-empty", "暂无详细信息"))
  }
  row <- row[1, , drop = FALSE]

  acc_cols <- setdiff(names(row), c("药物ID", "中药名称", "中药类型", herb_quick_cols))
  panels <- list()

  add_panel <- function(title, cols) {
    cols <- intersect(cols, acc_cols)
    if (!length(cols)) return(NULL)
    bodies <- lapply(cols, function(k) {
      if (herb_blank(row[[k]])) return(NULL)
      htmltools::tagList(
        # 一个分区里合并多栏时，用栏名做小标题，便于区分
        if (length(cols) > 1) htmltools::tags$div(class = "herb-collabel", k),
        herb_text(row[[k]])
      )
    })
    bodies <- Filter(Negate(is.null), bodies)
    if (!length(bodies)) return(NULL)
    list(title = title, body = htmltools::tagList(bodies))
  }

  used <- character(0)
  for (sec in herb_sections) {
    p <- add_panel(sec$title, sec$cols)
    used <- c(used, intersect(sec$cols, acc_cols))
    if (!is.null(p)) panels[[length(panels) + 1]] <- p
  }
  # 兜底：未分区的栏目
  rest <- setdiff(acc_cols, used)
  if (length(rest)) {
    p <- add_panel("其他", rest)
    if (!is.null(p)) panels[[length(panels) + 1]] <- p
  }

  if (!length(panels)) {
    return(htmltools::tagList(
      herb_hero(row), herb_quick_facts(row),
      htmltools::tags$p(class = "zh-empty", "暂无详细信息")
    ))
  }

  titles <- vapply(panels, `[[`, character(1), "title")
  open   <- if ("功效主治" %in% titles) "功效主治" else titles[1]

  acc <- do.call(
    bslib::accordion,
    c(
      lapply(panels, function(p) bslib::accordion_panel(p$title, p$body)),
      list(multiple = TRUE, open = open, class = "herb-acc")
    )
  )

  htmltools::tagList(
    herb_hero(row),
    herb_quick_facts(row),
    acc
  )
}
