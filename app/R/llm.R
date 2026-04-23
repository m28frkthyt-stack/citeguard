confidence_badge <- function(conf) {
  conf <- tolower(trimws(conf))
  switch(conf,
    high   = "<span style='background:#5cb85c;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>high</span>",
    medium = "<span style='background:#f0ad4e;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>medium</span>",
    low    = "<span style='background:#aaa;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>low</span>",
    paste0("<span style='background:#aaa;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>",
           htmltools::htmlEscape(conf), "</span>")
  )
}

format_llm_verdict_html <- function(verdict) {
  if (is.na(verdict) || !nzchar(trimws(verdict))) return("")

  v <- trimws(verdict)

  if (grepl("^no(\\.?|\\s+known\\s+miscitation\\s+found\\.?)$", v, ignore.case = TRUE))
    return("<span style='color:#5cb85c;font-weight:bold'>No known miscitation found</span>")
  if (grepl("^uncertain\\.?$", v, ignore.case = TRUE))
    return("<span style='color:#f0ad4e;font-weight:bold'>Uncertain</span>")

  lines  <- trimws(unlist(strsplit(v, "\n")))
  lines  <- lines[nzchar(lines)]
  pat    <- "^(M\\d+|Other)\\s*\\((high|medium|low)\\):\\s*(.+)$"
  parsed <- lapply(lines, function(line) {
    m <- regmatches(line, regexec(pat, line, ignore.case = TRUE))[[1]]
    if (length(m) == 4) list(code = m[2], conf = m[3], reason = m[4])
    else NULL
  })
  parsed <- Filter(Negate(is.null), parsed)

  if (length(parsed) > 0) {
    parts <- vapply(parsed, function(p) {
      code <- toupper(p$code)
      row  <- mistake_table[mistake_table$mistake_code == code, ]
      code_badge <- paste0(
        "<span style='background:#d9534f;color:#fff;border-radius:3px;",
        "padding:1px 5px;font-size:0.85em;font-weight:bold;margin-right:4px'>",
        htmltools::htmlEscape(code), "</span>"
      )
      title_span <- if (nrow(row) > 0)
        paste0("<span style='color:#333;font-weight:bold'>", htmltools::htmlEscape(row$mistake_title), "</span>")
      else ""
      paste0(
        code_badge, confidence_badge(p$conf), title_span,
        "<br><span style='color:#555;font-size:0.9em'>", htmltools::htmlEscape(p$reason), "</span>"
      )
    }, character(1))
    paste(parts, collapse = "<br><br>")
  } else {
    paste0("<span style='color:#555'><em>", htmltools::htmlEscape(v), "</em></span>")
  }
}

# Uses metacheck::llm() for all Groq API calls.
# Requires GROQ_API_KEY env var or explicit api_key argument.
run_llm_check <- function(df, model = "llama-3.1-8b-instant", api_key = Sys.getenv("GROQ_API_KEY"), status_fn = NULL, cancel_fn = NULL) {
  flagged_idx <- which(
    !is.na(df$db_known_miscited_paper) &
    df$db_known_miscited_paper == "Yes" &
    !is.na(df$expanded_text) &
    nzchar(trimws(df$expanded_text))
  )

  df$llm_verdict <- NA_character_
  if (length(flagged_idx) == 0) return(df)

  metacheck::llm_use(TRUE)
  metacheck::llm_max_calls(length(flagged_idx) * 3L + 20L)

  clean_field <- function(x) {
    v <- trimws(x %||% "")
    if (length(v) == 0 || is.na(v) || !nzchar(v) || v == "NA") "" else v
  }

  has_db_info <- vapply(flagged_idx, function(i) {
    row <- df[i, , drop = FALSE]
    nzchar(clean_field(row$db_mistake_codes)) ||
    nzchar(clean_field(row$db_why_incorrect)) ||
    nzchar(clean_field(row$db_quoted_text))
  }, logical(1))

  no_info_idx <- flagged_idx[!has_db_info]
  df$llm_verdict[no_info_idx] <- "No known miscitation found"

  flagged_idx <- flagged_idx[has_db_info]
  if (length(flagged_idx) == 0) return(df)

  meta <- lapply(flagged_idx, function(i) {
    row <- df[i, , drop = FALSE]
    list(
      why      = clean_field(row$db_why_incorrect),
      db_quote = clean_field(row$db_quoted_text),
      db_codes = clean_field(row$db_mistake_codes),
      db_titles= clean_field(row$db_mistake_titles),
      citation = trimws(row$expanded_text)
    )
  })

  system_prompt <- paste0(
    "A paper has been reported as commonly miscited in a specific way. ",
    "Your job: decide whether a new citation repeats that exact same wrong claim — not whether it covers the same general topic.\n\n",
    "Steps:\n",
    "1. EXTRACT THE WRONG VARIABLE: From the WRONG CLAIM and HOW TO DETECT sections, identify the specific variable, ",
    "measure, or construct that is wrongly attributed (e.g. 'pitch', 'dominance', 'eye contact'). Write it down.\n",
    "2. CHECK PRESENCE: Does the new citation explicitly name or unambiguously refer to that same specific variable? ",
    "If the variable is absent from the new citation — even if the topic is related — stop here and do NOT flag.\n",
    "3. CHECK THE CLAIM: If the variable IS present, does the new citation make the same wrong claim about it ",
    "as described in WRONG CLAIM? Only flag if both the variable AND the wrong claim match.\n\n",
    "Critical rules:\n",
    "- A citation about related but different variables is NOT a match.\n",
    "- A citation that merely mentions the same paper in a broad list is NOT a match.\n",
    "- When in doubt, do NOT flag.\n\n",
    "Output if flagging — one line per error:\n",
    "  CODE (confidence): quote the exact phrase from the new citation that names the wrong variable and wrong claim\n",
    "  confidence = high (variable and claim both explicit) / medium (variable present, claim implied) / low (uncertain)\n\n",
    "Output if not flagging:\n",
    "  No known miscitation found\n\n",
    "Output nothing else."
  )

  expand_query <- paste0(
    "A paper has been reported as commonly miscited in a specific way. ",
    "Expand the description below into a precise account for use in automated detection.\n\n",
    "Write exactly three sections:\n\n",
    "ACTUAL FINDING: What the cited paper actually found.\n\n",
    "WRONG CLAIM: The specific wrong claim researchers make. Name the exact variable(s) involved. ",
    "List only close synonyms for that variable.\n\n",
    "HOW TO DETECT:\n",
    "- TRIGGER (flag if present): list the specific words or phrases a new citation must contain to be flagged.\n",
    "- DO NOT TRIGGER (do not flag even if present): list related variables or claims that look similar but are NOT the same mistake.\n\n",
    "Output only these three sections. No preamble."
  )

  call_llm_once <- function(text, query, max_tokens) {
    res <- tryCatch(
      metacheck::llm(
        text      = text,
        query     = query,
        model     = model,
        maxTokens = max_tokens,
        temperature = 0.1,
        API_KEY   = api_key
      ),
      error = function(e) {
        if (conditionMessage(e) == "__CANCELLED__") stop("__CANCELLED__")
        stop(conditionMessage(e))
      }
    )
    answer <- res$answer[1]
    if (isTRUE(res$error[1])) stop(res$error_msg[1])
    if (is.na(answer) || !nzchar(trimws(answer))) stop("Empty response from model")
    trimws(answer)
  }

  expansion_cache <- list()
  get_expansion <- function(key, raw_text) {
    if (!is.null(expansion_cache[[key]])) return(expansion_cache[[key]])
    if (!is.null(status_fn)) status_fn("Expanding mistake description...")
    expanded <- tryCatch(call_llm_once(raw_text, expand_query, 600L),
                         error = function(e) raw_text)
    expansion_cache[[key]] <<- expanded
    expanded
  }

  texts <- vapply(seq_along(meta), function(j) {
    m        <- meta[[j]]
    raw_desc <- paste0(
      if (nzchar(m$db_quote)) paste0("Prototypical wrong claim: ", m$db_quote, "\n") else "",
      if (nzchar(m$why))      paste0("Why it is wrong: ", m$why, "\n") else ""
    )
    cache_key <- paste0(m$db_codes, "|", substr(raw_desc, 1, 120))
    expanded  <- get_expansion(cache_key, raw_desc)

    known_mistake <- paste0(
      "KNOWN MISCITATION\n",
      if (nzchar(m$db_codes)) paste0("Error code: ", m$db_codes,
        if (nzchar(m$db_titles)) paste0(" (", m$db_titles, ")") else "", "\n") else "",
      expanded
    )
    paste0(known_mistake, "\nNEW CITATION TO CHECK\n", m$citation)
  }, character(1))

  raw_answers    <- character(length(texts))
  cancelled_early <- FALSE

  for (ci in seq_along(texts)) {
    result_i <- tryCatch(
      call_llm_once(texts[[ci]], system_prompt, 300L),
      error = function(e) {
        if (conditionMessage(e) == "__CANCELLED__") {
          cancelled_early <<- TRUE
          NA_character_
        } else {
          paste0("ERROR: ", conditionMessage(e))
        }
      }
    )
    raw_answers[[ci]] <- if (is.na(result_i)) "" else result_i

    if (!is.null(status_fn)) {
      status_fn(sprintf("LLM checked %d/%d row(s)...", ci, length(texts)))
    }

    if (cancelled_early) break
    if (!is.null(cancel_fn) && isTRUE(cancel_fn())) {
      cancelled_early <- TRUE
      break
    }
  }

  completed  <- if (cancelled_early) ci - 1L else length(raw_answers)
  debug_info <- paste0("model=", model, " | n=", completed, "/", length(raw_answers),
                       " | raw: ", paste(raw_answers[seq_len(completed)], collapse = " | "))

  done_idx <- flagged_idx[seq_len(completed)]
  df$llm_verdict[done_idx] <- trimws(raw_answers[seq_len(completed)])

  list(
    df          = df,
    raw_answers = raw_answers[seq_len(completed)],
    flagged_idx = done_idx,
    debug_info  = debug_info,
    texts       = texts,
    cancelled   = cancelled_early
  )
}
