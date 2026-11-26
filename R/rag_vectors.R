# =============================================================================
# rag_vectors.R — 可选的语义向量召回（BGE-M3，int8 量化）
#
# embeddings.rds 是 15,317 段原文块的向量（id 与图库 TextUnit.id 一一对应，
# 形如 U:地榆#001），因此可以直接回表取原文与出处。
# 没有 embeddings.rds 或没有 EMBED_API_KEY 时静默跳过，不影响其它召回。
# =============================================================================
.rag_vec <- new.env(parent = emptyenv())

rag_vec_index <- function(path = Sys.getenv("VECTOR_FILE", "embeddings.rds")) {
  if (is.null(.rag_vec$idx)) {
    if (!file.exists(path)) {
      .rag_vec$idx <- FALSE
    } else {
      .rag_vec$idx <- readRDS(path)
    }
  }
  if (identical(.rag_vec$idx, FALSE)) NULL else .rag_vec$idx
}

# 语义召回是否可用：索引文件与查询向量 Key 缺一不可（状态栏 / 部署核对用）
rag_vec_status <- function(cfg = rag_config()) {
  idx <- tryCatch(rag_vec_index(), error = function(e) NULL)
  has_index <- !is.null(idx)
  has_key <- nzchar(cfg$embed_key)
  list(
    enabled = has_index && has_key,
    has_index = has_index,
    has_key = has_key,
    n = if (has_index) length(idx$ids) else 0L,
    model = cfg$embed_model
  )
}

# 返回 top-k 的 chunk id 与分数（L2 归一化后的点积 ≈ 余弦）
rag_vec_search <- function(question, cfg = rag_config(), k = 8) {
  idx <- rag_vec_index()
  if (is.null(idx)) return(NULL)
  if (!nzchar(cfg$embed_key)) return(NULL)
  v <- rag_embed_one(question, cfg)
  if (is.null(v) || length(v) != idx$dim) return(NULL)
  nrm <- sqrt(sum(v^2))
  if (!is.numeric(nrm) || nrm == 0) return(NULL)
  q <- (v / nrm) * 127
  scores <- as.numeric(idx$Q %*% q) / pmax(idx$norms * sqrt(sum(q^2)), 1e-9)
  ord <- order(scores, decreasing = TRUE)
  ord <- utils::head(ord, k)
  data.frame(id = idx$ids[ord], score = scores[ord], stringsAsFactors = FALSE)
}

# 转成 evidence：文本与出处直接取自图库的 TextUnit 节点
rag_vec_evidence <- function(question, cfg = rag_config(), k = 8) {
  hit <- rag_vec_search(question, cfg, k)
  if (is.null(hit) || !nrow(hit)) return(list())
  g <- kg_store_get()
  pos <- match(hit$id, g$nodes$id)
  keep <- which(!is.na(pos))
  if (!length(keep)) return(list())
  lapply(keep, function(i) {
    j <- pos[i]
    rag_ev("text", g$nodes$title_path[j] %||% g$nodes$id[j],
           substr(g$nodes$text[j] %||% "", 1, 600),
           hit$score[i],
           list(chunk_id = g$nodes$id[j], doc = g$nodes$doc_id[j],
                section = g$nodes$section[j], line = g$nodes$src_line[j],
                source = "text:vector"))
  })
}

# 调用 embedding 接口（硅基流动 BGE-M3，OpenAI 兼容）
rag_embed_one <- function(text, cfg = rag_config()) {
  if (!nzchar(cfg$embed_key)) return(NULL)
  out <- tryCatch({
    req <- httr2::request(paste0(cfg$embed_url, "/embeddings"))
    req <- httr2::req_auth_bearer_token(req, cfg$embed_key)
    req <- httr2::req_body_json(
      req,
      list(model = jsonlite::unbox(cfg$embed_model),
           input = jsonlite::unbox(text)),
      auto_unbox = FALSE
    )
    req <- httr2::req_timeout(req, 30)
    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    as.numeric(unlist(body$data[[1]]$embedding))
  }, error = function(e) NULL)
  out
}
