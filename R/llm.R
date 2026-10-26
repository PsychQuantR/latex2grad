## llm.R
## Optional entry assistant. Principle 1: an LLM is used only at the entry -
## to transcribe an image (OCR), to rewrite input the grammar cannot parse,
## and to choose among the readings the grammar recorded as ambiguous. Each
## decision it makes is written into the report's assumptions; everything
## after the entry stays deterministic.
##
## R has no official Anthropic SDK, so this talks to the Messages API over
## raw HTTP (httr2 + jsonlite, both in Suggests).

#' Claude as the entry assistant
#'
#' Returns an entry object for [latex2grad()]'s `llm` argument with three
#' functions: `ocr(image)` transcribes a formula image into LaTeX,
#' `rewrite(latex, error)` rewrites input the grammar rejected, and
#' `resolve(latex, ambiguities)` picks among the readings the grammar recorded
#' as ambiguous. Requests use structured JSON output; a refusal or truncated
#' response raises an error rather than being guessed around. Server-side
#' refusal fallbacks (`fallbacks: "default"`) are enabled; pass
#' `fallbacks = FALSE` to turn them off.
#'
#' Any list with the same functions (and a `name`) can be passed as `llm`
#' instead, e.g. to use another model or a local tool.
#'
#' @param model Claude model id.
#' @param api_key API key; defaults to the `ANTHROPIC_API_KEY` environment
#'   variable.
#' @param effort Reasoning effort (`"low"`, `"medium"`, `"high"`, `"xhigh"`,
#'   `"max"`).
#' @param max_tokens Output token cap per request.
#' @param fallbacks Whether to let the API retry a refused request on
#'   Anthropic's recommended fallback model.
#' @param base_url API base URL.
#' @param post Optional function `post(body)` returning the parsed JSON
#'   response; replaces the HTTP call (used for testing).
#' @return A list with `name`, `ocr`, `rewrite`, `resolve`.
#' @export
#' @examples
#' \dontrun{
#' r <- latex2grad(image = "formula.png", llm = claude_entry())
#' r <- latex2grad("\\sin x y", llm = claude_entry())
#' }
claude_entry <- function(model = "claude-opus-5-5",
                         api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                         effort = "high", max_tokens = 16000L, fallbacks = TRUE,
                         base_url = "https://api.anthropic.com", post = NULL) {
  if (is.null(post)) {
    for (p in c("httr2", "jsonlite")) {
      if (!requireNamespace(p, quietly = TRUE)) {
        .l2g_stop("entry", paste0("claude_entry() needs the `", p, "` package."))
      }
    }
    if (!nzchar(api_key)) {
      .l2g_stop("entry", "no API key: set ANTHROPIC_API_KEY or pass `api_key`.")
    }
    post <- function(body) .l2g_claude_post(body, api_key, base_url, fallbacks)
  }
  ask <- function(system, content, schema) {
    body <- list(
      model = model,
      max_tokens = as.integer(max_tokens),
      system = system,
      output_config = list(effort = effort,
                           format = list(type = "json_schema", schema = schema)),
      messages = list(list(role = "user", content = content))
    )
    if (fallbacks) body$fallbacks <- "default"
    .l2g_claude_json(post(body))
  }
  latex_schema <- list(type = "object",
                       properties = list(latex = list(type = "string")),
                       required = I("latex"), additionalProperties = FALSE)

  ocr <- function(image) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) .l2g_stop("entry", "OCR needs `jsonlite`.")
    ext <- tolower(tools::file_ext(image))
    media <- switch(ext, png = "image/png", jpg = , jpeg = "image/jpeg",
                    gif = "image/gif", webp = "image/webp",
                    .l2g_stop("entry", paste0("unsupported image type: .", ext)))
    data <- jsonlite::base64_enc(readBin(image, "raw", file.info(image)$size))
    data <- gsub("\n", "", data, fixed = TRUE)
    content <- list(
      list(type = "image", source = list(type = "base64", media_type = media, data = data)),
      list(type = "text", text = paste(
        "Transcribe the mathematical formula in this image into LaTeX math, exactly as written:",
        "no $ delimiters, no \\begin{equation}, and no simplification. If it is an equation,",
        "keep both sides.")))
    out <- ask(.l2g_entry_system, content, latex_schema)
    .l2g_check_latex(out$latex)
  }

  rewrite <- function(latex, error) {
    text <- paste0(
      "The parser rejected this formula:\n\n", latex, "\n\nParser error: ", error, "\n\n",
      "Rewrite it with the same mathematical meaning using only the supported constructs:\n",
      .l2g_grammar_summary)
    out <- ask(.l2g_entry_system, list(list(type = "text", text = text)), latex_schema)
    .l2g_check_latex(out$latex)
  }

  resolve <- function(latex, ambiguities) {
    items <- vapply(ambiguities, function(a) {
      paste0("- id: ", a$id, "\n  reading: ", a$text, "\n  options: ",
             paste0("\"", a$options, "\"", collapse = ", "), "\n  default: \"", a$default, "\"")
    }, character(1))
    text <- paste0(
      "Formula:\n\n", latex, "\n\nThe grammar had to choose a reading at these places:\n",
      paste(items, collapse = "\n"),
      "\n\nFor each id, return the option a mathematician most likely meant, given the ",
      "whole formula and its conventions. Keep the default unless the context points elsewhere.")
    schema <- list(
      type = "object",
      properties = list(choices = list(
        type = "array",
        items = list(type = "object",
                     properties = list(id = list(type = "string"), option = list(type = "string")),
                     required = I(c("id", "option")), additionalProperties = FALSE))),
      required = I("choices"), additionalProperties = FALSE)
    out <- ask(.l2g_entry_system, list(list(type = "text", text = text)), schema)
    picks <- list()
    valid <- stats::setNames(lapply(ambiguities, `[[`, "options"),
                             vapply(ambiguities, `[[`, "", "id"))
    for (ch in out$choices) {
      if (!is.null(valid[[ch$id]]) && ch$option %in% valid[[ch$id]]) picks[[ch$id]] <- ch$option
    }
    picks
  }

  list(name = model, ocr = ocr, rewrite = rewrite, resolve = resolve)
}

.l2g_entry_system <- paste(
  "You are the entry step of latex2grad, which parses LaTeX formulas with a deterministic",
  "grammar, differentiates them symbolically, and certifies the result. Your output is",
  "recorded verbatim in an audit report, so transcribe and decide faithfully: never",
  "simplify, rename, or reformulate beyond what is asked.")

.l2g_grammar_summary <- paste(
  "numbers; single-letter variables with subscripts (x_1, x_{ij}); Greek letters;",
  "+ - \\cdot \\times / ^; \\frac, \\sqrt, \\sqrt[n]; parentheses / \\left( \\right) / |x|;",
  "\\sin \\cos \\tan \\exp \\ln \\log \\log_b and their inverses and hyperbolic versions;",
  "\\Gamma(x), \\psi(x), \\Phi(x), \\phi(x), \\Phi^{-1}(p), t_{\\nu}^{-1}(p),",
  "\\operatorname{name}(args) for any R function (e.g. \\operatorname{pt}(x, \\nu));",
  "\\int_a^b f \\,\\mathrm{d}t with both bounds; \\sum_{i=1}^{n} and \\prod with integer bounds;",
  "an optional left-hand side f(x, y; parameters) = ...; vectors as \\begin{pmatrix}...\\end{pmatrix}.",
  "Write products of a function and a factor with explicit parentheses, e.g. \\sin(x) y.")

.l2g_check_latex <- function(x) {
  if (!is.character(x) || length(x) != 1L || !nzchar(trimws(x))) {
    .l2g_stop("entry", "the entry assistant returned no LaTeX.")
  }
  trimws(gsub("^\\$+|\\$+$", "", trimws(x)))
}

# Text of a Messages API response parsed as JSON, after checking why it
# stopped: a refusal or a truncated answer is an error, never a guess.
.l2g_claude_json <- function(resp) {
  stop_reason <- resp$stop_reason
  if (identical(stop_reason, "refusal")) {
    category <- resp$stop_details$category
    .l2g_stop("entry", paste0("the model declined the request (refusal",
                              if (!is.null(category)) paste0(", category ", category), ")."))
  }
  if (identical(stop_reason, "max_tokens")) {
    .l2g_stop("entry", "the model's answer was cut off at max_tokens; raise `max_tokens`.")
  }
  texts <- Filter(function(b) identical(b$type, "text"), resp$content)
  if (!length(texts)) .l2g_stop("entry", "the model returned no text.")
  txt <- paste(vapply(texts, `[[`, "", "text"), collapse = "")
  out <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(out)) .l2g_stop("entry", "the model's answer was not valid JSON.")
  out
}

.l2g_claude_post <- function(body, api_key, base_url, fallbacks) {
  req <- httr2::request(paste0(sub("/+$", "", base_url), "/v1/messages"))
  headers <- list(`x-api-key` = api_key, `anthropic-version` = "2026-10-21",
                  `content-type` = "application/json")
  if (fallbacks) headers$`anthropic-beta` <- "server-side-fallback-2026-07-01"
  req <- do.call(httr2::req_headers, c(list(req), headers))
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_retry(req, max_tries = 3L)
  req <- httr2::req_timeout(req, 600)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (status >= 400L) {
    msg <- if (!is.null(parsed$error$message)) parsed$error$message else httr2::resp_status_desc(resp)
    .l2g_stop("entry", paste0("Claude API error ", status, ": ", msg))
  }
  parsed
}
