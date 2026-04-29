server <- function(input, output, session) {
  init_db_if_missing()

  diagnostics    <- reactiveVal("No diagnostics run yet.")
  process_status <- reactiveVal("No processing run yet.")
  llm_status     <- reactiveVal("")
  llm_cancel     <- reactiveVal(FALSE)
  raw_data       <- reactiveVal(tibble::tibble())
  live_log       <- reactiveVal(character())
  known_tbl_open <- reactiveVal(FALSE)

  stats <- reactiveValues(uploaded = 0, pdfs = 0, processed = 0, success = 0, fail = 0)

  reports_db_rv <- reactiveVal(read_reports_db())

  observe({
    if (isTRUE(known_tbl_open())) {
      shinyjs::show("known_tbl_body")
      updateActionLink(session, "toggle_known_tbl", label = "Hide")
    } else {
      shinyjs::hide("known_tbl_body")
      updateActionLink(session, "toggle_known_tbl", label = "Show")
    }
  })

  observeEvent(input$toggle_known_tbl, {
    known_tbl_open(!isTRUE(known_tbl_open()))
  })

  known_miscited_df <- reactive({
    summarise_known_miscitations(reports_db_rv())
  })

  output$known_miscited_table <- DT::renderDT({
    DT::datatable(
      known_miscited_df(),
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 8, scrollX = TRUE, dom = "tip")
    )
  })

  output$mistake_table_out <- renderTable({
    mistake_table
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  append_log <- function(...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste(..., collapse = ""))
    live_log(c(live_log(), msg))
    message(msg)
  }

  reset_log_and_stats <- function() {
    live_log(character())
    stats$uploaded  <- 0
    stats$pdfs      <- 0
    stats$processed <- 0
    stats$success   <- 0
    stats$fail      <- 0
  }

  observeEvent(input$test_grobid, {
    grobid_url <- trimws(input$grobid_url)
    res <- check_grobid_alive(grobid_url)
    diagnostics(paste(
      "GROBID TEST", "-----------",
      paste("URL:", paste0(sub("/+$", "", grobid_url), "/api/isalive")),
      paste("OK:", res$ok),
      paste("Status:", res$status %||% NA),
      "", "Response body:",
      substr(res$body %||% "", 1, 1000),
      sep = "\n"
    ))
  })

  observeEvent(input$test_pdf2grobid, {
    req(input$files)
    grobid_url <- trimws(input$grobid_url)
    alive      <- check_grobid_alive(grobid_url)

    if (!isTRUE(alive$ok) || is.na(alive$status) || alive$status < 200 || alive$status >= 300) {
      diagnostics(paste(
        "PDF2GROBID TEST", "----------------",
        paste("GROBID URL:", grobid_url),
        "Result: FAILED",
        "Reason: GROBID is not reachable or not healthy.",
        paste("HTTP status:", alive$status %||% NA),
        "", "Response body:", substr(alive$body %||% "", 1, 1000),
        sep = "\n"
      ))
      return()
    }

    collected <- collect_uploaded_pdfs(input$files)
    pdfs      <- collected$pdfs
    req(nrow(pdfs) > 0)

    msg <- tryCatch({
      xml_path <- safe_pdf2grobid(pdfs$pdf_path[1], grobid_url)
      paste(c(
        "PDF2GROBID TEST", "----------------",
        paste("File:", pdfs$pdf_name[1]),
        paste("GROBID URL:", grobid_url),
        "Result: SUCCESS",
        paste("XML path:", xml_path),
        "", "Upload scan messages:", collected$messages
      ), collapse = "\n")
    }, error = function(e) {
      paste(c(
        "PDF2GROBID TEST", "----------------",
        paste("File:", pdfs$pdf_name[1]),
        paste("GROBID URL:", grobid_url),
        "Result: FAILED", "",
        conditionMessage(e),
        "", "Upload scan messages:", collected$messages
      ), collapse = "\n")
    })
    diagnostics(msg)
  })

  observeEvent(input$process, {
    req(input$files)

    reset_log_and_stats()
    raw_data(tibble::tibble())
    process_status("Checking GROBID connection...")

    # Reload database so annotation uses the latest reports
    reports_db_rv(read_reports_db())

    grobid_url <- trimws(input$grobid_url)
    alive      <- check_grobid_alive(grobid_url)

    if (!isTRUE(alive$ok) || is.na(alive$status) || alive$status < 200 || alive$status >= 300) {
      msg <- paste(
        "Processing stopped.",
        paste("GROBID URL:", grobid_url),
        "Reason: GROBID is not reachable or not healthy.",
        paste("HTTP status:", alive$status %||% NA),
        "", "Response body:", substr(alive$body %||% "", 1, 1000),
        sep = "\n"
      )
      process_status(msg)
      append_log(msg)
      return()
    }

    process_status("Processing started...")
    append_log("Starting processing run")
    stats$uploaded <- nrow(input$files)

    collected       <- collect_uploaded_pdfs(input$files)
    pdfs            <- collected$pdfs
    status_messages <- c(
      paste0("Using GROBID URL: ", grobid_url),
      paste0("Uploaded items: ", nrow(input$files)),
      collected$messages
    )

    for (m in collected$messages) append_log(m)
    stats$pdfs <- nrow(pdfs)

    if (nrow(pdfs) == 0) {
      append_log("No PDFs found after scanning uploads")
      process_status(paste(c(status_messages, "No PDFs could be found or extracted from the uploaded files."), collapse = "\n"))
      return(NULL)
    }

    append_log("Found ", nrow(pdfs), " PDF(s) to process")
    status_messages <- c(status_messages, paste0("PDFs found: ", nrow(pdfs)))

    all_results <- vector("list", nrow(pdfs))

    shiny::withProgress(message = "Processing PDFs", value = 0, {
      for (i in seq_len(nrow(pdfs))) {
        pdf_path      <- pdfs$pdf_path[i]
        pdf_name      <- pdfs$pdf_name[i]
        source_upload <- pdfs$source_upload[i]

        detail_msg <- paste0("Processing ", i, "/", nrow(pdfs), ": ", pdf_name)
        append_log(detail_msg)
        shiny::incProgress(1 / nrow(pdfs), detail = detail_msg)
        status_messages <- c(status_messages, paste0("Processing: ", pdf_name, " (from ", source_upload, ")"))

        res <- tryCatch({
          out <- extract_citation_contexts(
            pdf_path   = pdf_path,
            pdf_name   = pdf_name,
            grobid_url = grobid_url
          ) |> dplyr::mutate(source_upload = source_upload, .before = file)

          append_log("Rows extracted for ", pdf_name, ": ", nrow(out))
          append_log("Non-empty citations: ", sum(!is.na(out$citation) & nzchar(out$citation)))
          append_log("Non-empty sentences: ", sum(!is.na(out$sentence) & nzchar(out$sentence)))
          append_log("Non-empty ref titles: ", sum(!is.na(out$ref_title) & nzchar(out$ref_title)))

          stats$success <- stats$success + 1
          append_log("Success: ", pdf_name, " | rows extracted: ", nrow(out))
          out
        }, error = function(e) {
          stats$fail <- stats$fail + 1
          append_log("FAILED: ", pdf_name, " | ", conditionMessage(e))
          status_messages <<- c(status_messages, paste0("Failed: ", pdf_name, " -> ", conditionMessage(e)))

          tibble::tibble(
            source_upload = source_upload, file = pdf_name,
            citation = NA_character_, sentence = NA_character_, expanded_text = NA_character_,
            ref_title = NA_character_, ref_authors = NA_character_,
            ref_author_last_names = NA_character_, ref_author_key = NA_character_,
            ref_lead_author = NA_character_, ref_title_key = NA_character_,
            ref_year = NA_character_, ref_doi = NA_character_,
            citation_lead_author_guess = NA_character_, citation_year_guess = NA_character_,
            section = NA_character_, div = NA_integer_, p = NA_integer_, s = NA_integer_,
            note = paste("ERROR:", conditionMessage(e))
          )
        })

        all_results[[i]] <- res
        stats$processed  <- i
      }
    })

    combined <- dplyr::bind_rows(all_results)
    combined <- annotate_with_db_matches(combined, known_miscited_df())

    raw_data(combined)
    append_log("Processing finished")
    process_status(paste(status_messages, collapse = "\n"))
  })

  filtered_data <- reactive({
    dat <- raw_data()
    if (nrow(dat) == 0) return(dat)

    if (nzchar(trimws(input$citation_filter))) {
      dat <- dat |>
        dplyr::filter(stringr::str_detect(
          dplyr::coalesce(citation, ""),
          stringr::regex(trimws(input$citation_filter), ignore_case = TRUE)
        ))
    }

    if (nzchar(trimws(input$author_filter))) {
      dat <- dat |>
        dplyr::filter(
          stringr::str_detect(dplyr::coalesce(ref_authors, ""),            stringr::regex(trimws(input$author_filter), ignore_case = TRUE)) |
          stringr::str_detect(dplyr::coalesce(ref_author_last_names, ""), stringr::regex(trimws(input$author_filter), ignore_case = TRUE))
        )
    }

    if (isTRUE(input$only_known_miscited)) {
      dat <- dat |> dplyr::filter(db_known_miscited_paper == "Yes")
    }

    dat
  })

  output$stat_uploaded     <- renderText(stats$uploaded)
  output$stat_pdfs         <- renderText(stats$pdfs)
  output$stat_processed    <- renderText(paste0(stats$processed, "/", stats$pdfs))
  output$stat_success_fail <- renderText(paste0(stats$success, " / ", stats$fail))
  output$live_log          <- renderText(paste(live_log(), collapse = "\n"))
  output$diagnostic_text   <- renderText(diagnostics())
  output$status_text       <- renderText(process_status())

  output$results_table <- DT::renderDT({
    dat <- filtered_data()

    if ("target_sentence" %in% names(dat) && "expanded_text" %in% names(dat)) {
      dat$expanded_text <- mapply(function(et, ts) {
        et     <- trimws(dplyr::coalesce(as.character(et), ""))
        ts     <- trimws(dplyr::coalesce(as.character(ts), ""))
        et_html <- htmltools::htmlEscape(et)
        ts_html <- trimws(htmltools::htmlEscape(ts))

        if (!nzchar(ts_html)) return(et_html)

        et_marked <- if (nzchar(et_html) && grepl(ts_html, et_html, fixed = TRUE)) {
          sub(ts_html, paste0("<mark>", ts_html, "</mark>"), et_html, fixed = TRUE)
        } else et_html

        marked_ts <- paste0("<mark>", ts_html, "</mark>")

        if (!nzchar(et_html) || et_html == ts_html) {
          marked_ts
        } else {
          paste0(
            marked_ts,
            "<details><summary style='color:#888;font-size:0.82em;cursor:pointer;padding-top:3px'>&#9660;&nbsp;context</summary>",
            "<div style='margin-top:4px;color:#444'>", et_marked, "</div>",
            "</details>"
          )
        }
      }, dat$expanded_text, dat$target_sentence, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    }

    if ("db_known_miscited_paper" %in% names(dat)) {
      is_flagged <- !is.na(dat$db_known_miscited_paper) & dat$db_known_miscited_paper == "Yes"
      if ("db_match_reason" %in% names(dat))
        dat$db_match_reason[!is_flagged] <- NA_character_
    }

    default_visible <- c(
      "file", "citation", "expanded_text",
      if ("llm_verdict" %in% names(dat) && any(!is.na(dat$llm_verdict))) "llm_verdict" else NULL
    )
    hide_targets <- which(!(names(dat) %in% default_visible)) - 1

    if ("llm_verdict" %in% names(dat)) {
      has_rids     <- "db_report_ids"    %in% names(dat)
      has_db_codes <- "db_mistake_codes" %in% names(dat)
      dat$llm_verdict <- vapply(seq_len(nrow(dat)), function(i) {
        v     <- format_llm_verdict_html(dat$llm_verdict[i])
        if (!nzchar(v)) return(v)
        raw_v <- trimws(dat$llm_verdict[i] %||% "")
        if (grepl("^no(\\.?|\\s+known\\s+miscitation\\s+found\\.?)$", raw_v, ignore.case = TRUE))
          return(v)

        db_codes <- if (has_db_codes) trimws(dat$db_mistake_codes[i] %||% "") else ""
        if (is.na(db_codes)) db_codes <- ""
        reported_line <- if (nzchar(db_codes) && db_codes != "NA") {
          codes      <- trimws(unlist(strsplit(db_codes, ";")))
          code_spans <- paste(vapply(codes, function(cd) {
            paste0("<span style='background:#888;color:#fff;border-radius:3px;",
                   "padding:1px 4px;font-size:0.78em;font-weight:bold;margin-right:2px'>",
                   htmltools::htmlEscape(cd), "</span>")
          }, character(1)), collapse = "")
          paste0("<span style='color:#aaa;font-size:0.8em'>Reported as: ", code_spans, "</span>")
        } else ""

        rids      <- if (has_rids) dat$db_report_ids[i] else NA_character_
        case_line <- if (!is.na(rids) && nzchar(trimws(rids)) && trimws(rids) != "NA") {
          ids      <- trimws(unlist(strsplit(rids, ";")))
          id_spans <- paste(vapply(ids, function(id) {
            paste0("<code style='font-size:0.78em;color:#aaa'>", htmltools::htmlEscape(trimws(id)), "</code>")
          }, character(1)), collapse = " ")
          paste0("<span style='color:#aaa;font-size:0.8em'>Case: ", id_spans, "</span>")
        } else ""

        footer <- paste(c(reported_line, case_line)[nzchar(c(reported_line, case_line))], collapse = "<br>")
        if (nzchar(footer)) paste0(v, "<br>", footer) else v
      }, character(1))
    }

    html_cols   <- intersect(c("expanded_text", "llm_verdict"), names(dat))
    escape_cols <- setdiff(seq_along(names(dat)), which(names(dat) %in% html_cols))

    col_defs <- list(
      list(targets = which(names(dat) == "file")          - 1, width = "12%"),
      list(targets = which(names(dat) == "citation")      - 1, width = "14%"),
      list(targets = which(names(dat) == "expanded_text") - 1, width = "60%", className = "wide-text"),
      list(targets = hide_targets, visible = FALSE)
    )
    if ("llm_verdict" %in% names(dat)) {
      col_defs <- c(col_defs, list(
        list(targets = which(names(dat) == "llm_verdict") - 1, width = "14%", className = "wide-text")
      ))
    }

    DT::datatable(
      dat,
      escape     = escape_cols,
      extensions = c("Buttons"),
      filter     = "top",
      rownames   = FALSE,
      class      = "cell-border stripe compact",
      options    = list(
        pageLength  = 10,
        scrollX     = TRUE,
        autoWidth   = FALSE,
        dom         = "Bfrtip",
        buttons     = list(list(extend = "colvis", text = "Show / hide columns")),
        columnDefs  = col_defs
      )
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("citation_contexts_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )

  observeEvent(input$run_llm, {
    llm_status("")
    dat <- raw_data()

    if (is.null(dat) || nrow(dat) == 0) {
      llm_status("No data loaded. Process PDFs first.")
      return()
    }

    api_key <- trimws(input$groq_api_key)
    if (!nzchar(api_key)) api_key <- Sys.getenv("GROQ_API_KEY")
    if (!nzchar(api_key)) {
      llm_status("No Groq API key provided. Enter your key above or set GROQ_API_KEY in .Renviron.")
      return()
    }

    flagged_count <- sum(!is.na(dat$db_known_miscited_paper) & dat$db_known_miscited_paper == "Yes", na.rm = TRUE)
    if (flagged_count == 0) {
      llm_status("No rows matched the miscitation database. Nothing to check.")
      return()
    }

    selected_model <- input$llm_model
    llm_status(paste0("Running LLM check on ", flagged_count, " row(s)... please wait."))
    llm_cancel(FALSE)
    shinyjs::disable("run_llm")
    shinyjs::show("stop_llm")

    session$onFlushed(function() {
      llm_out <- tryCatch(
        run_llm_check(dat, model = selected_model, api_key = api_key,
                      status_fn = llm_status,
                      cancel_fn = function() isolate(llm_cancel())),
        error = function(e) {
          llm_status(paste("LLM error:", conditionMessage(e)))
          NULL
        }
      )

      if (!is.null(llm_out)) {
        result       <- llm_out$df
        raw_data(result)
        n_done       <- length(llm_out$flagged_idx)
        stopped_early <- isTRUE(llm_out$cancelled)
        verdict_lines <- if (n_done > 0) {
          paste(vapply(seq_along(llm_out$flagged_idx), function(j) {
            i         <- llm_out$flagged_idx[j]
            cit       <- trimws(result$citation[i] %||% paste("Row", i))
            cit_short <- if (nchar(cit) > 60) paste0(substr(cit, 1, 60), "...") else cit
            raw       <- llm_out$raw_answers[j]
            normalized <- result$llm_verdict[i]
            paste0("  • ", cit_short,
                   "\n    Raw: ", if (is.na(raw) || !nzchar(raw)) "(empty)" else raw,
                   "\n    Verdict: ", if (is.na(normalized) || !nzchar(normalized)) "(empty)" else normalized)
          }, character(1)), collapse = "\n")
        } else ""
        prefix <- if (stopped_early) paste0("Stopped early. LLM checked ", n_done, " row(s) (partial results kept).\n")
                  else paste0("Done. LLM checked ", n_done, " row(s).\n")
        llm_status(paste0(
          prefix, verdict_lines, "\n\n",
          "DEBUG: ", llm_out$debug_info, "\n\n",
          "Text sent to LLM:\n", paste(llm_out$texts, collapse = "\n---\n")
        ))
      }

      shinyjs::enable("run_llm")
      shinyjs::hide("stop_llm")
    }, once = TRUE)
  })

  observeEvent(input$stop_llm, {
    llm_cancel(TRUE)
    llm_status("Stopping after current request completes...")
  })

  output$llm_status <- renderText(llm_status())
}
