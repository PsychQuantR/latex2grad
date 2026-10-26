## llm.R
## Optional entry hooks backed by Claude (design D8). They are the only
## non-deterministic components and are used only at the entry; whatever they
## decide is recorded in the report's assumptions by latex2grad().
##
## R has no official Anthropic SDK, so the Messages API is called over raw
## HTTP with httr2 (Suggests). The transport is injectable for tests.

.anthropic_url <- "https://api.anthropic.com/v1/messages"
.anthropic_version <- "2026-10-21"
.fallback_beta <- "server-side-fallback-2026-07-01"

.disambiguation_system <- paste(
  "You rewrite a LaTeX math formula so that a strict, deterministic grammar can parse it",
  "without guessing. Keep the mathematical meaning the author most plausibly intended.",
  "Make every product explicit with \\cdot, brace every function argument as \\sin\\left(...\\right),",
  "write divisions as \\frac{...}{...}, write multi-letter variable names as \\mathit{name},",
  "and use \\operatorname{qt}(u, \\nu) for a Student-t quantile.",
  "Do not simplify, expand, or differentiate the formula.",
  "Return the rewritten LaTeX and one short sentence for each interpretive decision you made."
)

.ocr_prompt <- paste(
  "Transcribe the mathematical formula in this image into LaTeX math (no $ delimiters).",
  "Transcribe exactly what is written; do not simplify or correct it.",
  "List, one sentence each, any symbol you were unsure how to read."
)

.json_schema <- function(field, list_field) {
  props <- list()
  props[[field]] <- list(type = "string")
  props[[list_field]] <- list(type = "array", items = list(type = "string"))
  list(type = "object", properties = props, required = c(field, list_field),
       additionalProperties = FALSE)
}

.httr2_transport <- function(url, headers, body) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("the Claude hooks need the httr2 package: install.packages(\"httr2\")", call. = FALSE)
  }
  req <- httr2::request(url)
  req <- do.call(httr2::req_headers, c(list(req), headers))
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, 600)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  attr(out, "status") <- httr2::resp_status(resp)
  out
}

.auth_headers <- function(api_key) {
  if (nzchar(api_key)) return(list(`x-api-key` = api_key, beta = character()))
  token <- Sys.getenv("ANTHROPIC_AUTH_TOKEN")
  if (nzchar(token)) return(list(Authorization = paste("Bearer", token), beta = "oauth-2025-04-20"))
  stop("no Anthropic credentials: set ANTHROPIC_API_KEY (or ANTHROPIC_AUTH_TOKEN)", call. = FALSE)
}

# One structured-output Messages API call; returns the parsed JSON object.
.claude_json <- function(content, system, schema, model, api_key, effort, max_tokens, transport) {
  auth <- .auth_headers(api_key)
  headers <- c(auth[setdiff(names(auth), "beta")],
               list(`anthropic-version` = .anthropic_version,
                    `anthropic-beta` = paste(c(.fallback_beta, auth$beta), collapse = ","),
                    `content-type` = "application/json"))
  body <- list(
    model = model,
    max_tokens = max_tokens,
    system = system,
    messages = list(list(role = "user", content = content)),
    output_config = list(effort = effort, format = list(type = "json_schema", schema = schema)),
    fallbacks = "default"
  )
  resp <- transport(.anthropic_url, headers, body)
  status <- attr(resp, "status")
  if (!is.null(resp$error) || (!is.null(status) && status >= 400)) {
    msg <- if (!is.null(resp$error$message)) resp$error$message else paste("HTTP", status)
    stop("Claude API error: ", msg, call. = FALSE)
  }
  if (identical(resp$stop_reason, "refusal")) {
    stop("Claude declined the request (stop_reason = refusal)", call. = FALSE)
  }
  if (identical(resp$stop_reason, "max_tokens")) {
    stop("Claude's answer was cut off (stop_reason = max_tokens)", call. = FALSE)
  }
  texts <- Filter(function(b) identical(b$type, "text"), resp$content)
  if (!length(texts)) stop("Claude returned no text block", call. = FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("the Claude hooks need the jsonlite package", call. = FALSE)
  }
  out <- jsonlite::fromJSON(texts[[1L]]$text, simplifyVector = TRUE)
  attr(out, "model") <- resp$model
  out
}

#' Claude-backed disambiguation hook for the entry
#'
#' Returns a function suitable for `latex2grad(disambiguate = )`. When the
#' deterministic grammar rejects a formula (or, with `strict = TRUE`, meets an
#' ambiguity), the hook sends the LaTeX and the grammar's message to Claude,
#' which returns an explicit rewrite plus one sentence per interpretive
#' decision. latex2grad parses the rewrite deterministically and records every
#' decision as an assumption tagged with the model id.
#'
#' Requests use structured JSON output and the server-side refusal fallback
#' (`fallbacks = "default"`); a refusal or truncated answer is an error.
#'
#' @param model Claude model id.
#' @param api_key API key; defaults to `ANTHROPIC_API_KEY` (falls back to an
#'   `ANTHROPIC_AUTH_TOKEN` OAuth token).
#' @param effort Effort level (`"low"` to `"max"`).
#' @param max_tokens Output cap.
#' @param transport Function `(url, headers, body)` returning the parsed JSON
#'   response; defaults to httr2. Replace it for testing or proxies.
#' @return A function `(latex, problem)` returning
#'   `list(latex = <rewritten>, decisions = <character>)`, with attribute
#'   `model`.
#' @export
llm_claude <- function(model = "claude-opus-5-5", api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                       effort = "high", max_tokens = 16000L, transport = .httr2_transport) {
  force(model); force(api_key); force(effort); force(max_tokens); force(transport)
  hook <- function(latex, problem) {
    content <- paste0("LaTeX formula:\n", latex, "\n\nThe deterministic grammar reported:\n", problem)
    out <- .claude_json(content, .disambiguation_system, .json_schema("latex", "decisions"),
                        model, api_key, effort, max_tokens, transport)
    list(latex = as.character(out$latex), decisions = as.character(unlist(out$decisions)))
  }
  structure(hook, model = model)
}

#' Claude-backed OCR hook for the entry
#'
#' Returns a function suitable for `latex2grad(image = , ocr = )` that sends
#' a PNG, JPEG, GIF, or WebP image of a formula to Claude and returns the
#' transcription. latex2grad records the transcription and the model in the
#' report's assumptions; uncertain symbols the model lists are recorded too.
#'
#' @inheritParams llm_claude
#' @return A function `(path)` returning `list(latex = , notes = )`, with
#'   attribute `model`.
#' @export
ocr_claude <- function(model = "claude-opus-5-5", api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                       effort = "high", max_tokens = 16000L, transport = .httr2_transport) {
  force(model); force(api_key); force(effort); force(max_tokens); force(transport)
  hook <- function(path) {
    if (!file.exists(path)) stop("image not found: ", path, call. = FALSE)
    ext <- tolower(tools::file_ext(path))
    media <- switch(ext, png = "image/png", jpg = , jpeg = "image/jpeg", gif = "image/gif",
                    webp = "image/webp", stop("unsupported image type: .", ext, call. = FALSE))
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("the Claude hooks need the jsonlite package", call. = FALSE)
    data <- jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size))
    data <- gsub("[\r\n]", "", data)
    content <- list(
      list(type = "image", source = list(type = "base64", media_type = media, data = data)),
      list(type = "text", text = .ocr_prompt)
    )
    out <- .claude_json(content, "You transcribe images of mathematical formulas into LaTeX.",
                        .json_schema("latex", "notes"), model, api_key, effort, max_tokens, transport)
    list(latex = as.character(out$latex), notes = as.character(unlist(out$notes)))
  }
  structure(hook, model = model)
}
