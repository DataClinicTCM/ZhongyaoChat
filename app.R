library(shinyjs)
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(fmsb)
library(purrr)
library(stringr)
library(bslib)
library(ggplot2)
library(leaflet)
library(sf)
library(ggiraph)
library(shinychat) 
library(ellmer)     
library(htmltools)

# ---- GraphRAG（纯 R：内置 zhyhkg 图引擎 + data/kg_store.rds）----
source("R/kg_boot.R")        # 引擎源码（R/kg/）+ 图库 + 实体词典
source("R/map_bivariate.R")  # 省份双变量地图（地形阴影 + 3×3 配色）
source("R/rag.R")            # 证据 / 引用 / 上下文 / 大模型
source("R/rag_retrieve.R")   # 实体链接 + 多步查询计划 + 证据融合
if (file.exists("R/rag_vectors.R")) source("R/rag_vectors.R")
source("R/rag_shiny.R")
source("R/ui.R")            # 主题与「中药详情」排版
source("R/kg/viz.R")        # 图谱可视化（igraph / visNetwork）
source("R/kg_shiny.R")      # 「知识图谱」页

# 启动即载入图库并预热（每进程一次，约 3–5 秒；之后每次检索 0.1–1 秒）
invisible(try(kg_store_get(), silent = TRUE))
invisible(try(kg_warmup(), silent = TRUE))

# ------------------------------------------------------------------
# 0. 网页更新信息（每次发布时修改此处）
# ------------------------------------------------------------------
# 更新日志：每次发布在最前面新增一条，旧记录原样保留往下累积
APP_UPDATE_HISTORY <- list(
  list(
    date  = "2026-11-24",
    notes = c(
      "Chat 页新增「回答方式」：默认「图谱检索 + 平台 Key」，可切换为「仅图谱检索（不调用大模型）」，只返回图谱与原文证据",
      "Chat 状态栏显示语义向量召回是否启用（EMBED_API_KEY / embeddings.rds），部署时便于核对",
      "修复一些细节问题"
    )
  ),
  list(
    date  = "2026-11-15",
    notes = c(
      "「知识图谱」页修好节点点击：点节点后右侧正常显示属性、关系构成与邻居（此前提示「节点不存在」）",
      "去掉「度数与枢纽」页：关系网络、本体总览之外不再单独列度数分布与枢纽排行",
      "Chat 页新增「导出对话」：把当前对话按「我 / 助手」导出为文本文件，回答里的 [E1] 标号仍对应检索链路里的证据"
    )
  ),
  list(
    date  = "2026-11-15",
    notes = c(
      "新增「知识图谱」页：输入药名/成分/方剂/病证，按跳数与关系类型展开关系网络，点节点看档案",
      "「知识图谱」页含本体鸟瞰（实体类型 × 关系类型的聚合视图）与全图度数分布、Top 10 枢纽节点",
      "「度数与枢纽」页可按实体类型筛选，看某一类实体内部的度数分布与枢纽排行",
      "更新浏览页面"
    )
  ),
  list(
    date = "2026-11-14",
    notes = c(
      "图谱数据按《中华药海》原文重新抽取：13 个章节逐篇核对，主要节点类型的覆盖从 5–7 成提升到 9 成以上",
      "界面简化：去掉中药类型/名称选择、API Key 输入和「载入知识图谱」按钮，打开页面直接提问",
      "新增「检索链路」折叠面板：看到问题被解析成什么条件、生成了什么 Cypher、每步耗时、证据出自哪一行",
      "清单类问题（哪些药、共 N 味）现在给出完整名单，不再只列前 20/40 个",
      "「检索链路」面板新增「下载完整名单（CSV）」：一键导出全部符合条件的药材（含性味归经/功效主治/用法）",
      "地图省份面板里的药名可点击，直接跳到「浏览」页查看该药详情",
      "「搜索」页改为模糊搜索：输入即列出最相近的药名，支持别名异名（「石英」→白石英）与全拼/首字母（renshen、rs→人参）"
    )
  ),
  list(
    date = "2026-11-13",
    notes = c(
      "GraphRAG 底层换成《中华药海》完整知识图谱：54,085 节点 / 292,025 关系（34 类实体、43 类关系），带中文全文索引",
      "新增「多步查询」：药→方剂→方中其他药→功效、功效→药→成分、成分×病证交集等，答案会列出检索步骤",
      "新增「综合查询」：任意条件组合筛选（功效×病证×成分×分类×归经×性味×毒性×中成药），以及全书统计（功效/方剂/成分/分类排行）",
      "新增两药对比（共同/独有功效、主治、成分）与全书原文检索（15,317 段，cjk 分词）",
      "图查询改用真正的 Cypher 引擎（支持变长路径、模式推导、OPTIONAL MATCH、聚合），不再局限于单跳模板",
      "其它页面（浏览、数据挖掘、搜索、关于）与交互方式保持不变"
    )
  ),
  list(
    date = "2026-11-09",
    notes = c(
      "大语言模型升级为知识图谱问答：可对整部《中华药海》提问，不再局限于单味药",
      "回答附带原文出处（文件·章节·行号），可逐条核对",
      "支持多条件组合提问，如「既能清热解毒又能活血化瘀的药」",
      "支持使用自己的 API Key；未配置 Key 时以「检索式回答」呈现，仍带出处",
      "新增 15,317 段原文的语义检索（BGE-M3），支持近义提问"
    )
  ),
  list(
    date  = "2026-11-04",
    notes = c(
      "正文全面校订：修正 OCR 错字、多余空格及错误符号，标点统一为中文全角",
      "优化UI显示"
    )
  )
)

APP_LATEST       <- APP_UPDATE_HISTORY[[1]]
APP_UPDATE_DATE  <- as.Date(APP_LATEST$date)
APP_UPDATE_NOTES <- APP_LATEST$notes
APP_UPDATE_LABEL <- paste0("更新于 ", format(APP_UPDATE_DATE, "%Y-%m-%d"))

# 整份日志渲染为弹窗内容：最新一条在前并带“最新”标记，超长可滚动
render_update_history <- function(history) {
  tags$div(
    style = "max-height: 60vh; overflow-y: auto;",
    lapply(seq_along(history), function(i) {
      entry <- history[[i]]
      tags$div(
        style = if (i > 1) "margin-top: .9rem; padding-top: .9rem; border-top: 1px solid rgba(0,0,0,.12);",
        tags$div(
          class = "fw-semibold mb-1",
          entry$date,
          if (i == 1) {
            tags$span(class = "badge text-bg-primary ms-1",
                      style = "font-size: .7rem;", "最新")
          }
        ),
        tags$ul(
          style = "padding-left: 1.1rem; margin-bottom: 0;",
          lapply(entry$notes, tags$li)
        )
      )
    })
  )
}

# ------------------------------------------------------------------
# 1. 读取主数据
# ------------------------------------------------------------------
raw <- readRDS(file = "data/ZHYH.rds") |>
  mutate(across(where(is.character), trimws)) |>
  filter(!is.na(`中药名称`), `中药名称` != "")

# 类型配色：20 个高区分度颜色，按类型名固定，换数据时同一类型颜色不变
type_pal <- c(
  "#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231",
  "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE",
  "#008080", "#E6BEFF", "#9A6324", "#FFFAC8", "#800000",
  "#AAFFC3", "#808000", "#FFD8B1", "#000075", "#808080"
)
type_levels <- raw |> count(`中药类型`, sort = TRUE) |> pull(`中药类型`)
type_colors <- setNames(rep_len(type_pal, length(type_levels)), type_levels)
herb_names  <- unique(raw$`中药名称`)   # 供 herb_link() 判断名称能否跳转

# 2. 中文关键词字典
qi_dict   <- c("寒", "热", "温", "凉", "平")
wei_dict  <- c("酸", "苦", "甘", "辛", "咸", "涩", "淡")
jing_dict <- c("心", "肝", "脾", "肺", "肾", "胃", "大肠",
               "小肠", "膀胱", "胆", "心包", "三焦")

# 3. 小工具
read_txt <- function(file){
  if (!file.exists(file)) return("（暂无内容）")
  paste(trimws(readLines(file, encoding = "UTF-8", warn = FALSE)), collapse = "\n")
}

# ---- 检索索引：正名 + 别名异名/处方用名 + 拼音 ------------------------------
# 拼音用 ICU 转写（stringi 随 stringr 一并加载），多音字按中药习惯读音校正
PINYIN_CHAR_FIX <- c("参" = "shen", "术" = "zhu", "桔" = "jie")
PINYIN_WORD_FIX <- list("阿胶" = c("e", "jiao"), "阿魏" = c("a", "wei"),
                        "薄荷" = c("bo", "he"),     "槟榔" = c("bing", "lang"),
                        "荸荠" = c("bi", "qi"),     "黄柏" = c("huang", "bo"))

# 一味药的拼音：full 为连写全拼，ini 为逐字首字母
herb_pinyin <- function(x) {
  syl <- if (x %in% names(PINYIN_WORD_FIX)) {
    PINYIN_WORD_FIX[[x]]
  } else {
    ch <- strsplit(x, "")[[1]]
    s  <- ifelse(ch %in% names(PINYIN_CHAR_FIX), PINYIN_CHAR_FIX[ch],
                 stringi::stri_trans_general(ch, "Han-Latin; Latin-ASCII; Lower"))
    gsub("[^a-z]", "", s)
  }
  syl <- syl[nzchar(syl)]
  c(full = paste0(syl, collapse = ""), ini = paste0(substr(syl, 1, 1), collapse = ""))
}

# 「药物名称」里 [标签] 段落的内容
herb_section <- function(txt, tag) {
  pat <- paste0("[", tag, "]")
  pos <- regexpr(pat, txt, fixed = TRUE)
  if (pos < 0) return("")
  rest <- substring(txt, pos + nchar(pat))
  nxt  <- regexpr("[", rest, fixed = TRUE)
  if (nxt > 0) rest <- substring(rest, 1, nxt - 1)
  rest
}

# 别名拆词：去掉书名与括号注释，按中文标点断开，只留纯汉字、长度合理的词
herb_alias_tokens <- function(txt) {
  txt <- gsub("（[^）]*）|《[^》]*》|\\([^)]*\\)", "", txt)
  tk  <- trimws(unlist(strsplit(txt, "[、，,；;。\\s]+")))
  tk  <- setdiff(tk, c("的", "了", "其", "之", "等", "又", "即", "名"))
  tk  <- tk[nzchar(tk) & nchar(tk) <= 12 & grepl("^[\\p{Han}]+$", tk, perl = TRUE)]
  unique(tk)
}

# 检索索引：一行一个可匹配项（正名 kind = name，别名 kind = alias）
herb_search_index <- local({
  drug <- raw$`药物名称`[match(herb_names, raw$`中药名称`)]
  ali  <- Map(function(n, x) {
    tk <- herb_alias_tokens(paste(herb_section(x, "别名异名"), herb_section(x, "处方用名")))
    tk <- setdiff(tk, n)
    # 别名若与另一味药的正名相同（书中常以它药代称），不参与别名检索，免得张冠李戴
    tk[!tk %in% herb_names]
  }, herb_names, drug)
  py <- vapply(herb_names, herb_pinyin, character(2))
  ali_df <- data.frame(herb = rep(herb_names, lengths(ali)),
                       key = unlist(ali, use.names = FALSE), kind = "alias",
                       py = NA_character_, ini = NA_character_,
                       stringsAsFactors = FALSE)
  # 一个别名对应多味药时（民间同名异物），无法唯一定位，不参与别名检索
  ali_df <- ali_df[!duplicated(ali_df$key) & !duplicated(ali_df$key, fromLast = TRUE), ]
  rbind(
    data.frame(herb = herb_names, key = herb_names, kind = "name",
               py = unname(py["full", ]), ini = unname(py["ini", ]),
               stringsAsFactors = FALSE),
    ali_df
  )
})

# 模糊检索：正名/别名（汉字输入）、全拼/首字母（拉丁输入），按相关度返回前 n 味
# 返回 herb（正名）、kind（name/alias/pinyin）、hit（命中的别名或全拼）、score
fuzzy_herb_hits <- function(query, n = 6) {
  q <- if (length(query) == 0) "" else gsub("\\s", "", trimws(query[1]))
  empty <- data.frame(herb = character(0), kind = character(0),
                      hit = character(0), score = numeric(0),
                      stringsAsFactors = FALSE)
  if (!nzchar(q)) return(empty)

  idx <- herb_search_index

  if (grepl("[A-Za-z]", q)) {
    ql   <- tolower(q)
    keep <- idx$kind == "name"
    full <- idx$py[keep]
    ini  <- idx$ini[keep]
    score <- dplyr::case_when(
      full == ql                    ~ 100,
      ini  == ql                    ~ 96,
      startsWith(full, ql)          ~ 92,
      startsWith(ini, ql)           ~ 88,
      nchar(q) >= 2 & grepl(ql, full, fixed = TRUE) ~ 80,
      TRUE                          ~ NA_real_
    )
    hits <- data.frame(herb = idx$herb[keep], kind = "pinyin", hit = full,
                       score = score, stringsAsFactors = FALSE)
    hits <- hits[!is.na(hits$score), ]
  } else {
    nm <- idx$kind == "name"
    al <- idx$kind == "alias"
    k  <- idx$key

    # 正名：完全一致 > 前缀 > 包含 > 近似（共用的字 + 编辑距离）
    score_nm <- ifelse(k[nm] == q, 100,
                ifelse(startsWith(k[nm], q), 92,
                ifelse(grepl(q, k[nm], fixed = TRUE), 84, NA_real_)))
    cand <- which(is.na(score_nm))
    if (length(cand)) {
      q_chr <- strsplit(q, "")[[1]]
      share <- vapply(k[nm][cand], function(x) mean(q_chr %in% strsplit(x, "")[[1]]), numeric(1))
      sim   <- 1 - as.numeric(utils::adist(q, k[nm][cand])) / pmax(nchar(q), nchar(k[nm][cand]))
      ok    <- share >= 0.5 & sim >= 1 / 3
      score_nm[cand[ok]] <- 60 + 18 * sim[ok]
    }

    # 别名：完全一致 > 前缀 > 包含（包含至少两个字，免得上百个别名一起涌出来）
    score_al <- ifelse(k[al] == q, 98,
                ifelse(nchar(q) >= 2 & startsWith(k[al], q), 88,
                ifelse(nchar(q) >= 2 & grepl(q, k[al], fixed = TRUE), 76, NA_real_)))

    hits <- rbind(
      data.frame(herb = idx$herb[nm], kind = "name",  hit = k[nm], score = score_nm,
                 stringsAsFactors = FALSE),
      data.frame(herb = idx$herb[al], kind = "alias", hit = k[al], score = score_al,
                 stringsAsFactors = FALSE)
    )
    hits <- hits[!is.na(hits$score), ]
  }

  hits <- hits[order(-hits$score, nchar(hits$herb)), ]
  hits <- hits[!duplicated(hits$herb), ]
  utils::head(hits, n)
}

# 药材名 → 可点击链接：点击写入 input$herb_click，由 server 跳到「浏览」页
# clickable = FALSE 用于书中用另一名称收录的品种（如「橘红」记作「化橘红」）
herb_link <- function(name, sep = "", clickable = TRUE, type = NULL) {
  label <- paste0(name, sep)          # 分隔符并入同一文本节点，避免多出空格
  if (!clickable) {
    return(tags$span(label, class = "herb-plain", title = "本书未收录该名称的独立条目"))
  }
  # 带上类别：同名但分属不同类型的条目，才能定位到具体那一行
  payload <- if (is.null(type)) {
    sprintf("{name: '%s'}", name)
  } else {
    sprintf("{name: '%s', type: '%s'}", name, type)
  }
  tags$a(label, href = "#", class = "herb-pick",
         onclick = sprintf(
           "Shiny.setInputValue('herb_click', %s, {priority: 'event'}); return false;",
           payload))
}

# 浏览页导航：所有类别下的全部药名，点药名打开详情，详情页可返回这里
browse_nav_ui <- function(df) {
  tagList(
    lapply(unique(df$`中药类型`), function(tp) {
      nm <- unique(df$`中药名称`[df$`中药类型` == tp])
      tags$section(
        class = "zh-nav-group",
        tags$div(class = "zh-nav-head",
                 tags$span(class = "zh-nav-type", tp),
                 tags$span(class = "zh-nav-count", paste0(length(nm), " 味"))),
        tags$div(class = "zh-nav-items",
                 lapply(nm, function(n) herb_link(n, type = tp)))
      )
    })
  )
}

# ------------------------------------------------------------------
# 4. 中药地图数据准备
# ------------------------------------------------------------------
province_herbs_raw <- readRDS(file = "data/province_herbs.rds")

province_herbs_clean <- province_herbs_raw |>
  mutate(herbs = strsplit(herbs_str, "、")) |>
  unnest(herbs) |>
  group_by(province) |>
  summarise(
    herbs = list(unique(herbs)),
    count = n()
  )

geo_path <- "www/china.json"
if (!file.exists(geo_path)) {
  dir.create("www", showWarnings = FALSE)
  download.file(
    "https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json",
    destfile = geo_path
  )
}
china_map <- st_read(geo_path, quiet = TRUE) |>
  mutate(province = gsub("特别行政区|维吾尔自治区|壮族自治区|回族自治区|自治区|省|市", "", name))

# 省份统计（收录数 / 面积 / 密度 / 毒性数）与地形底图
province_stats <- readRDS("data/province_stats.rds")
hill_img <- if (file.exists("www/china_hillshade.png")) kg_read_hillshade("www/china_hillshade.png") else NULL
hill_ext <- if (file.exists("data/hillshade_meta.rds")) readRDS("data/hillshade_meta.rds") else NULL

map_data <- china_map |>
  left_join(province_herbs_clean, by = c("name" = "province"))

# ------------------------------------------------------------------
# 5. 剂量图专用函数（ggiraph 版）
# ------------------------------------------------------------------
# 从“用法用量”中提取内服剂量区间；单位限定为“克”，
# 枚、只、个等非克单位不进入坐标轴，避免单位失真
parse_dose_gram <- function(x) {
  if (is.na(x)) return(c(NA_real_, NA_real_))
  m <- str_match(x, "(\\d+\\.?\\d*)\\s*[~～\\-－—至]\\s*(\\d+\\.?\\d*)\\s*克")
  c(as.numeric(m[2]), as.numeric(m[3]))
}

prep_dose <- function(sub_df) {
  sub_df |>
    select(中药名称, 用法用量) |>
    mutate(
      range = map(用法用量, parse_dose_gram),
      low   = map_dbl(range, 1),
      high  = map_dbl(range, 2),
      mid   = (low + high) / 2
    ) |>
    filter(str_detect(用法用量, "内服"), !is.na(low), !is.na(high)) |>
    arrange(mid) |>
    mutate(name = factor(中药名称, levels = unique(中药名称)))
}

plot_dose_gir <- function(dose_df) {
  if (is.null(dose_df) || nrow(dose_df) == 0) {
    return(girafe(
      ggobj = ggplot() +
        annotate("text", x = 0, y = 0, size = 4,
                 label = "该类型暂无可解析的内服剂量") +
        theme_void(),
      height_svg = 2
    ))
  }
  
  gg <- ggplot(dose_df,
               aes(x = low, xend = high, y = name, yend = name,
                   tooltip = sprintf("%s<br>最小：%.1f 克<br>最大：%.1f 克",
                                     中药名称, low, high))) +
    geom_segment_interactive(linewidth = .7, colour = "#D3D3D3") +
    geom_point_interactive(aes(x = low),  colour = "#6B8E23", size = 2) +
    geom_point_interactive(aes(x = high), colour = "#2F4F4F", size = 2) +
    scale_x_continuous(limits = c(0, NA), breaks = scales::pretty_breaks(),
                       name = "内服剂量（克）") +
    labs(y = "中药名称") +
    theme_bw() +
    theme(axis.text.y = element_text(size = 8, margin = margin(r = 10)))
  
  girafe(ggobj = gg,
         height_svg = nrow(dose_df) * 0.24,
         options = list(
           opts_sizing(rescale = TRUE, width = 0.5),
           opts_tooltip(css = "background-color:#fff7bc;border:1px solid #ccc;padding:4px;font-size:12px;")
         ))
}

# ------------------------------------------------------------------
# 6. UI
# ------------------------------------------------------------------

## 1. 统一主题 -------------------------------------------------------------
# 配色、字体、圆角在 R/ui.R::app_theme() 里定义；版式细节在 www/app.css
theme <- app_theme()

## 2. 最外层 UI -------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(
    class = "zh-brand",
    tags$img(src = "ZhongyaoChat_logo.png", alt = "ZhongyaoChat"),
    tags$span(class = "zh-brand-name", "ZhongyaoChat")
  ),
  theme  = theme,
  # 依赖放进 header：作为 page_navbar 的直接子元素会被当成面板内容并告警
  header = tagList(
    useShinyjs(),
    tags$link(rel = "stylesheet", href = "app.css?v=20261115c"),
    # Chat 页「导出对话」（读取已渲染的消息，前端直接下载文本）
    tags$script(src = "chat_export.js?v=20261115c"),
    # 在搜索框里按回车等同于点「查询」
    tags$script(HTML(
      "$(document).on('keydown', '#searchName', function(e) {
         if (e.key === 'Enter') { e.preventDefault(); $('#searchBtn').click(); }
       });"
    ))
  ),
  id     = "mainTabset",

  ## 2.1 浏览 --------------------------------------------------------------
  nav_panel(title = "浏览", value = "browse",
            uiOutput("browsePage")
  ),

  ## 2.1b 知识图谱（图谱可视化） -------------------------------------------
  nav_panel(title = "知识图谱", value = "kg",
            kg_page_ui()
  ),

  ## 2.2 数据挖掘（下拉菜单） ---------------------------------------------
  nav_menu("数据挖掘",
           nav_panel("四气五味归经",
                     layout_sidebar(
                       sidebar = sidebar(
                         width = 290,
                         selectInput("radarType", "中药类型", unique(raw$`中药类型`)),
                         actionButton("drawRadar", "可视化", class = "btn-primary w-100")
                       ),
                       card(
                         card_header("雷达图分布"),
                         card_body(
                           layout_column_wrap(
                             width = 1/3,
                             plotOutput("radarQi",   height = "320px"),
                             plotOutput("radarWei",  height = "320px"),
                             plotOutput("radarJing", height = "320px")
                           )
                         )
                       )
                     )
           ),
           nav_panel("中药剂量",
                     layout_sidebar(
                       sidebar = sidebar(
                         width = 290,
                         selectInput("doseType", "中药类型", unique(raw$`中药类型`)),
                         actionButton("drawDose", "可视化", class = "btn-primary w-100")
                       ),
                       card(
                         card_header("剂量区间图"),
                         card_body(uiOutput("dosePlotUI"))
                       )
                     )
           ),
           nav_panel("中药地图",
                     card(
                       card_header("中国中药资源分布：收录数量 × 资源密度"),
                       card_body(
                         layout_sidebar(
                           sidebar = sidebar(width = 360, uiOutput("herb_list")),
                           girafeOutput("map", height = "640px")
                         )
                       ),
                       card_footer(class = "text-center",
                                   paste("本页面内容来自《中华药海（精华本）》知识图谱；",
                                         "点击省份查看该省收录的中药与资源信息。"))
                     )
           )
  ),

  ## 2.3 其余 Tab 保持不变 --------------------------------------------------
  nav_panel("搜索",
            layout_sidebar(
              sidebar = sidebar(
                width = 290,
                # 输入即出候选；回车或点「查询」按同名检索，找不到时退到最相近的一味
                textInput("searchName", "中药名称", placeholder = "输入药名或拼音，如 参、黄芪、renshen"),
                uiOutput("searchSuggest"),
                actionButton("searchBtn", "查询", class = "btn-primary w-100")
              ),
              card(
                card_header("搜索结果"),
                card_body(class = "zh-detail-body", uiOutput("searchRes"))
              )
            )
  ),

  nav_panel("Chat",
            layout_columns(
              col_widths = c(7, 5),
              card(
                full_screen = TRUE,
                card_header(
                  class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
                  "与知识图谱对话（GraphRAG）",
                  tags$button(
                    type = "button",
                    class = "btn btn-sm btn-outline-success",
                    onclick = "zhDownloadChat();",
                    title = "把当前对话按「我 / 助手」导出为文本文件",
                    "导出对话"
                  )
                ),
                card_body(class = "zh-chat-body",
                          div(id = "chat_wrapper",
                              chat_ui("chat", height = "100%")))
              ),
              card(
                full_screen = TRUE,
                card_header("回答方式与检索链路"),
                card_body(rag_chat_ui())
              )
            )
  ),

  nav_panel(
    title = "关于",
    value = "about",
    div(
      class = "about-full",
      card(
        card_header("《中华药海（精华本）》简介"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            class = "align-items-center",
            tags$img(src = "book_cover.jpg", class = "book-cover"),
            p(class = "about-intro-text", read_txt("www/intro_book.txt"))
          )
        )
      ),

      layout_column_wrap(
        width = 1/2,
        card(
          card_header("各类型中药数量"),
          card_body(plotOutput("typeBar", height = "460px"))
        ),
        card(
          card_header("各类型中药占比"),
          card_body(plotOutput("typePie", height = "460px"))
        )
      ),

      card(
        card_header("关于本站"),
        card_body(
          # 三段文字按等宽列铺满卡片宽度，窄屏自动堆叠
          div(class = "about-site-text",
              lapply(strsplit(read_txt("www/intro_website.txt"), "\n\n", fixed = TRUE)[[1]],
                     function(par) p(class = "about-site-p", par)))
        )
      ),

      div(class = "about-footer",
          p("Powered by DataClinicTCM"),
          tags$img(src = "logo.jpg", alt = "DataClinicTCM")
      )
    )
  ),

  ## 2.4 更新提示（导航栏右上角） ----------------------------------------
  nav_spacer(),
  nav_item(
    bslib::popover(
      tags$span(APP_UPDATE_LABEL,
                class = "nav-link",
                style = "font-size: .86rem;"),
      title = "更新日志",
      width = "400px",
      render_update_history(APP_UPDATE_HISTORY)
    )
  )
)

# ------------------------------------------------------------------
# 7. Server
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # -------------------------------------------------
  # 0. 聊天输入框默认可用（图谱在启动时已加载，无需"载入"按钮）
  # -------------------------------------------------
  observe({
    shinyjs::enable("chat_user_input")
  })
  
  # -------------------------------------------------
  # -------------------------------------------------
  # 1. 浏览页：导航 ←→ 详情
  # -------------------------------------------------
  # NULL = 显示导航页（全部类别 × 全部药名）；有值 = 显示该味药详情
  detail <- reactiveVal(NULL)

  output$browsePage <- renderUI({
    d <- detail()
    if (is.null(d) || nrow(d) == 0) {
      return(
        card(
          card_header(
            div(class = "zh-nav-title",
                "中药导航",
                span(class = "zh-nav-meta",
                     sprintf("%d 类 · %d 味 · 点击药名查看详情",
                             dplyr::n_distinct(raw$`中药类型`), nrow(raw))))
          ),
          card_body(class = "zh-nav-body", browse_nav_ui(raw))
        )
      )
    }
    card(
      full_screen = TRUE,
      card_header(
        div(class = "zh-detail-head",
            actionButton("browseBack", "← 返回导航", class = "zh-back-btn"),
            span("中药详细信息"))
      ),
      card_body(class = "zh-detail-body", herb_detail_ui(d))
    )
  })

  # 导航页或地图省份面板里点药材名：切到「浏览」页并直接显示该药
  # （点击事件由药材链接上的 onclick 写入 input$herb_click）
  observeEvent(input$herb_click, {
    nm <- input$herb_click$name
    tp <- input$herb_click$type
    hit <- raw |> filter(`中药名称` == nm)
    if (!is.null(tp) && length(tp) == 1 && nzchar(tp)) {
      hit <- hit |> filter(`中药类型` == tp)
    }
    if (!nrow(hit)) return()
    bslib::nav_select(id = "mainTabset", selected = "browse", session = session)
    detail(hit[1, , drop = FALSE])      # 同名多类型时取第一条
  })

  # 详情页：返回导航
  observeEvent(input$browseBack, detail(NULL))
  
  # -------------------------------------------------
  # 2. 搜索页：输入即出候选药名，点选后展示详情
  # -------------------------------------------------
  # 输入框当前值（未初始化时为 NULL）与刚选中的药名
  search_q <- reactive({
    v <- input$searchName
    if (is.null(v) || is.na(v)) "" else trimws(v)
  })
  picked <- reactiveVal(NULL)

  # 输入过程中的候选，按相关度最多 6 个
  searchCands <- reactive(fuzzy_herb_hits(search_q(), n = 6))

  # 结果：未检索时给出提示，其余为具体药材行
  searchResult <- reactiveVal(
    list(msg = "输入药名开始检索；输入过程中下方会列出相近的药名。")
  )

  show_herb <- function(nm, note = NULL) {
    hits <- raw |> filter(`中药名称` == nm)
    if (nrow(hits) == 0) {
      searchResult(list(msg = paste0("未找到「", nm, "」，请检查名称是否与《中华药海（精华本）》一致。")))
    } else {
      searchResult(list(hits = hits, name = nm, note = note))
    }
  }

  # 输入改动后允许候选重新弹出（刚选中的名字除外）
  observeEvent(search_q(), {
    if (!identical(search_q(), picked())) picked(NULL)
  }, ignoreInit = TRUE)

  output$searchSuggest <- renderUI({
    q <- search_q()
    if (!nzchar(q) || identical(q, picked())) return(NULL)
    cands <- searchCands()
    cands <- searchCands()
    if (nrow(cands) == 0) {
      return(tags$div(class = "zh-suggest",
                      tags$div(class = "zh-suggest-empty", "没有相近的药名")))
    }
    tags$div(
      class = "zh-suggest",
      tags$div(class = "zh-suggest-h", "相近的药名"),
      lapply(seq_len(nrow(cands)), function(i) {
        nm   <- cands$herb[i]
        hint <- switch(cands$kind[i],
                       alias  = paste0("别名 · ", cands$hit[i]),
                       pinyin = paste0("拼音 · ", cands$hit[i]),
                       paste(unique(raw$`中药类型`[raw$`中药名称` == nm]), collapse = " / "))
        tags$a(
          href = "#", class = "zh-suggest-item",
          onclick = sprintf(
            "Shiny.setInputValue('search_pick', {name: '%s'}, {priority: 'event'}); return false;",
            nm),
          tags$span(class = "zh-suggest-name", nm),
          tags$span(class = "zh-suggest-type", hint)
        )
      })
    )
  })

  # 点候选：填入输入框并展示该药
  observeEvent(input$search_pick, {
    nm <- input$search_pick$name
    picked(nm)
    updateTextInput(session, "searchName", value = nm)
    show_herb(nm)
  })

  # 点「查询」或回车：先找同名，找不到就退到名称最接近的一味并注明
  observeEvent(input$searchBtn, {
    q <- search_q()
    if (!nzchar(q)) {
      picked(NULL)
      return(searchResult(list(msg = "请输入中药名称")))
    }
    if (q %in% herb_names) return(show_herb(q))
    near <- fuzzy_herb_hits(q, n = 1)
    if (nrow(near) == 0) {
      return(searchResult(list(msg = paste0(
        "未找到「", q, "」，请检查名称是否与《中华药海（精华本）》一致。"))))
    }
    note <- switch(near$kind[1],
                   alias  = sprintf("「%s」是「%s」的别名。", q, near$herb[1]),
                   pinyin = sprintf("按拼音「%s」找到「%s」。", q, near$herb[1]),
                   sprintf("图谱中没有「%s」，以下是名称最接近的「%s」。", q, near$herb[1]))
    show_herb(near$herb[1], note = note)
  })

  output$searchRes <- renderUI({
    res <- searchResult()
    if (!is.null(res$msg)) {
      return(p(class = "zh-empty", res$msg))
    }
    hits <- res$hits
    note <- if (is.null(res$note)) NULL else p(class = "zh-search-note", res$note)
    body <- if (nrow(hits) == 1) {
      herb_detail_ui(hits)
    } else {
      # 同名但分属不同类型时分开展示，避免字段混在一起
      # 每味药的名称与类型在详情头部已标明，这里只用分隔线区分
      tagList(lapply(seq_len(nrow(hits)), function(i) {
        tags$div(class = "zh-hit", herb_detail_ui(hits[i, ]))
      }))
    }
    tagList(note, body)
  })
  
  # -------------------------------------------------
  # 3. 雷达图
  # -------------------------------------------------
  make_radar_fixed <- function(text_vec, dict) {
    txt <- stringr::str_c(text_vec, collapse = " ")
    cnt <- purrr::map_int(dict, ~ stringr::str_count(txt, .x)) |>
      setNames(dict)
    # 「心包」中含一个「心」，扣掉被重复计入的部分
    if ("心包" %in% names(cnt)) cnt[["心"]] <- cnt[["心"]] - cnt[["心包"]]
    max_v <- max(cnt, 1)
    rbind(Max = max_v, Min = 0, Values = cnt) |> as.data.frame()
  }
  
  radarReactive <- eventReactive(input$drawRadar, {
    req(input$radarType)
    sub <- raw |> filter(`中药类型` == input$radarType)
    list(
      qi   = make_radar_fixed(sub$`性味归经`, qi_dict),
      wei  = make_radar_fixed(sub$`性味归经`, wei_dict),
      jing = make_radar_fixed(sub$`性味归经`, jing_dict)
    )
  })
  
  output$radarQi   <- renderPlot({
    fmsb::radarchart(radarReactive()$qi,
                     cglty = 1, cglcol = "gray", pcol = 2, plwd = 2,
                     pfcol = rgb(1, 0, 0, .25), title = "四气分布")
  })
  output$radarWei  <- renderPlot({
    fmsb::radarchart(radarReactive()$wei,
                     cglty = 1, cglcol = "gray", pcol = 3, plwd = 2,
                     pfcol = rgb(0, 1, 0, .25), title = "五味分布")
  })
  output$radarJing <- renderPlot({
    fmsb::radarchart(radarReactive()$jing,
                     cglty = 1, cglcol = "gray", pcol = 4, plwd = 2,
                     pfcol = rgb(0, 0, 1, .25), title = "归经分布")
  })
  
  # -------------------------------------------------
  # 4. 剂量图
  # -------------------------------------------------
  doseData <- eventReactive(input$drawDose, {
    req(input$doseType)
    raw |> filter(`中药类型` == input$doseType) |> prep_dose()
  })

  # 输出高度随条目数变化，避免上百味药的图被压进 400px 容器
  output$dosePlotUI <- renderUI({
    d <- doseData()
    n <- if (is.null(d)) 0 else nrow(d)
    girafeOutput("dosePlot", height = paste0(max(320, min(2400, 80 + 23 * n)), "px"))
  })

  output$dosePlot <- renderGirafe({
    req(doseData())
    plot_dose_gir(doseData())
  })
  
  # -------------------------------------------------
  # 5. 地图（双变量：收录数量 × 资源密度，ggiraph 可点击）
  # -------------------------------------------------
  output$map <- renderGirafe({
    p <- build_province_map(province_stats, china_map, hill_img, hill_ext)
    girafe(
      ggobj = p, width_svg = 11, height_svg = 7.2,
      options = list(
        opts_selection(type = "single", only_shiny = TRUE, css = "stroke:#1b4332;stroke-width:1.3px;"),
        opts_hover(css = "stroke:#1b4332;stroke-width:1.2px;cursor:pointer;"),
        opts_tooltip(opacity = 0.95, css = "background:#ffffff;border:1px solid #d0d0d0;border-radius:4px;padding:6px;font-size:12px;")
      )
    )
  })

  output$herb_list <- renderUI({
    province <- input$map_selected
    if (is.null(province) || !length(province)) {
      return(tagList(
        tags$h6("点击地图上的省份", style = "color:#2c6e49;"),
        tags$p("查看该省收录的中药、资源密度与毒性药材数；点击药名跳转到「浏览」页。",
               style = "color:#777; font-size:.9rem;"),
        tags$div(style = "font-size:.85rem;color:#555;margin-top:.6rem;",
          tags$div("全国收录：", nrow(province_stats), " 个省级行政区"),
          tags$div("省级最多：", province_stats$province[1], "（", province_stats$n_herb[1], " 味）"),
          tags$div("密度最高：", province_stats$province[which.max(province_stats$density)], "（",
                   sprintf("%.1f", max(province_stats$density)), " 味/万km²）")
        ),
        tags$div(style = "margin-top:.9rem;font-size:.78rem;color:#6b7785;line-height:1.6;",
          "背景：第四次全国中药资源普查（2011–2022）确认全国中药资源 18,817 种（药用植物 15,321 种）。",
          tags$a(href = "https://www.gov.cn/yaowen/liebiao/202402/content_6933882.htm", target = "_blank", "来源"),
          # tags$br(),
          # "地形底图：Esri World Hillshade（Esri, USGS, NOAA）；省界：DataV.GeoAtlas。"
        )
      ))
    }
    province <- province[1]
    row <- province_stats[province_stats$province == province, ]
    herbs_df <- province_herbs_clean |> filter(province == !!province)
    herbs <- if (nrow(herbs_df) == 0) character(0) else herbs_df$herbs[[1]]

    stat_ui <- if (nrow(row)) {
      rank_n <- rank(-province_stats$n_herb, ties.method = "min")[province_stats$province == province]
      tagList(
        tags$h5(province, style = "margin-bottom:.4rem;"),
        tags$div(class = "small", style = "line-height:1.9;",
          tags$div("收录中药：", tags$b(row$n_herb), " 味（全国第 ", rank_n, "）"),
          tags$div("资源密度：", tags$b(sprintf("%.1f", row$density)), " 味/万km²（全国第 ",
                   rank(-province_stats$density, ties.method = "min")[province_stats$province == province], "）"),
          tags$div("省域面积：", tags$b(sprintf("%.1f", row$area_wan)), " 万 km²"),
          tags$div("标注有毒：", tags$b(row$n_toxic), " 味")
        ),
        # 道地药材（按产区整理，不按拼音序）
        if (isTRUE(row$daodi_n > 0)) {
          daodi_vec <- strsplit(row$daodi, "、")[[1]]
          tagList(
            tags$div(class = "small mt-2", "道地药材：", tags$b(row$daodi_n), " 味"),
            tags$div(class = "daodi-list small", style = "line-height:1.8;color:#2c3e50;",
                     lapply(seq_along(daodi_vec), function(i) {
                       herb_link(daodi_vec[i],
                                 sep = if (i < length(daodi_vec)) "、" else "",
                                 clickable = daodi_vec[i] %in% herb_names)
                     })),
            if (nzchar(row$daodi_missing)) {
              tags$div(class = "small text-muted", style = "margin-top:.25rem;",
                       "另（本书未收录）：", row$daodi_missing)
            }
          )
        } else {
          tags$div(class = "small text-muted mt-2", "该省不在主要道地产区之列")
        },
        tags$div(class = "small text-muted", style = "margin-top:.2rem;",
                 "道地药材按产区整理：川药/关药/北药/怀药/浙药/广药/云药/贵药/淮药/江南药/秦药/藏药/蒙药/维药"),
        tags$hr(style = "margin:.5rem 0;")
      )
    } else NULL

    if (length(herbs) == 0) {
      return(tagList(stat_ui,
                     tags$p("该省暂无收录数据。", style = "color:#888; font-style:italic;")))
    }
    n_per_col <- 16
    cols <- split(herbs, ceiling(seq_along(herbs) / n_per_col))
    tagList(
      stat_ui,
      tags$div(class = "small text-muted", paste0("全部收录（", length(herbs), " 味，点击药名查看详情）")),
      fluidRow(
        lapply(cols, function(col_herbs)
          column(
            width = 4,
            tags$ul(
              lapply(col_herbs, function(h)
                tags$li(herb_link(h), style = "margin:1px 0;font-size:.85rem;")),
              style = "list-style-type:none; padding-left:0; line-height:1.7;"
            )
          )
        )
      )
    )
  })
  
  # -------------------------------------------------
  # 6. 工具函数
  # -------------------------------------------------
  # 详情渲染统一走 R/ui.R::herb_detail_ui()
  
  # -------------------------------------------------
  # 7. 关于页：类型分布图（随数据自动更新）
  # -------------------------------------------------
  output$typeBar <- renderPlot({
    raw |>
      count(`中药类型`) |>
      ggplot(aes(x = n, y = reorder(`中药类型`, n))) +
      geom_col(fill = "#7FA88A") +
      labs(x = "数量（味）", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(plot.margin = margin(6, 6, 8, 6))
  })

  output$typePie <- renderPlot({
    d <- raw |>
      count(`中药类型`) |>
      mutate(中药类型 = factor(`中药类型`, levels = type_levels),
             prop = n / sum(n),
             lab  = ifelse(prop >= 0.05, paste0(round(prop * 100), "%"), "")) |>
      arrange(`中药类型`)
    ggplot(d, aes(x = "", y = n, fill = `中药类型`)) +
      geom_col(width = 1, colour = "white", linewidth = 0.4) +
      coord_polar(theta = "y") +
      geom_text(aes(label = lab),
                position = position_stack(vjust = .5), size = 3.5) +
      scale_fill_manual(values = type_colors, breaks = type_levels, name = NULL) +
      theme_void(base_size = 12) +
      theme(legend.text = element_text(size = 8),
            legend.key.size = unit(0.7, "lines"),
            legend.spacing.y = unit(0.05, "cm"),
            plot.margin = margin(6, 6, 6, 6))
  })

  # -------------------------------------------------
  # 8. GraphRAG（图谱检索 + 可选大模型）
  #    原“把选中药物文本塞进 system prompt”的逻辑，改为：
  #    先检索知识图谱（图 + 全文 + 向量），再把证据交给模型。
  #    回答方式：默认「图谱检索 + 平台 Key」；可切换为「仅图谱检索」（不调用大模型）。
  # -------------------------------------------------
  rag <- rag_chat_server(input, output, session)

  # -------------------------------------------------
  # 9. 知识图谱页（子图探索 / 本体总览 / 度数分布）
  # -------------------------------------------------
  kg_page_server(input, output, session)
}
# ------------------------------------------------------------------
shinyApp(ui, server)
