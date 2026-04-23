spinner_tag <- tags$span(
  style = paste0(
    "display:inline-block; width:10px; height:10px; border:2px solid #aaa;",
    "border-top-color:#555; border-radius:50%;",
    "animation:spin 0.7s linear infinite; margin-right:6px;",
    "vertical-align:middle"
  )
)

server <- function(input, output, session) {
  init_db_if_missing()

  report_status <- reactiveVal("")
  erase_status  <- reactiveVal("")
  crossref_meta <- reactiveVal(NULL)
  orcid_meta    <- reactiveVal(NULL)
  orcid_search_results <- reactiveVal(list())

  output$mistake_table_out <- renderTable(mistake_table,
    striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  # ORCID lookup — uses metacheck::check_orcid() for validation,
  # metacheck::get_orcid() for name search, metacheck::orcid_person() for details
  observeEvent(input$lookup_orcid, {
    query <- trimws(input$reporter_orcid)
    if (!nzchar(query)) {
      orcid_meta(list(ok = FALSE, error = "Please enter a name or ORCID iD."))
      return()
    }

    cleaned <- metacheck::check_orcid(query)
    is_orcid <- !isFALSE(cleaned) && nzchar(cleaned)

    if (is_orcid && cleaned != query)
      updateTextInput(session, "reporter_orcid", value = cleaned)

    orcid_meta(list(ok = FALSE, loading = TRUE))
    shinyjs::disable("lookup_orcid")
    updateActionButton(session, "lookup_orcid", label = "Searching…")

    session$onFlushed(function() {
      meta <- if (is_orcid) {
        tryCatch({
          person <- metacheck::orcid_person(cleaned)
          if (!is.null(person) && nrow(person) > 0 && !"error" %in% names(person)) {
            name <- trimws(paste(
              person$given[1]  %||% "",
              person$family[1] %||% ""
            ))
            list(ok = TRUE, mode = "verify",
                 name  = if (nzchar(name)) name else "(name not public)",
                 orcid = cleaned)
          } else {
            list(ok = FALSE, error = "ORCID not found or details not public.")
          }
        }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
      } else {
        # Name search: split on last space into given/family
        parts  <- trimws(unlist(strsplit(query, "\\s+")))
        family <- parts[length(parts)]
        given  <- if (length(parts) > 1) paste(parts[-length(parts)], collapse = " ") else "*"

        tryCatch({
          orcids <- metacheck::get_orcid(family = family, given = given)
          if (length(orcids) == 0 || (length(orcids) == 1 && !nzchar(orcids[1]))) {
            list(ok = TRUE, mode = "search", results = list())
          } else {
            persons <- metacheck::orcid_person(orcids)
            results <- lapply(seq_len(nrow(persons)), function(i) {
              row  <- persons[i, ]
              name <- trimws(paste(row$given %||% "", row$family %||% ""))
              if (!nzchar(name)) name <- "(name not public)"
              list(orcid = row$orcid, name = name, institution = "")
            })
            orcid_search_results(results)
            list(ok = TRUE, mode = "search", results = results)
          }
        }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
      }

      orcid_meta(meta)
      shinyjs::enable("lookup_orcid")
      updateActionButton(session, "lookup_orcid", label = "Search")
    }, once = TRUE)
  })

  observeEvent(input$orcid_select, {
    results <- orcid_search_results()
    idx     <- as.integer(input$orcid_select)
    req(idx >= 1, idx <= length(results))
    chosen <- results[[idx]]
    updateTextInput(session, "reporter_orcid", value = chosen$orcid)
    orcid_meta(list(ok = TRUE, mode = "verify", name = chosen$name, orcid = chosen$orcid))
  })

  output$orcid_meta_box <- renderUI({
    m <- orcid_meta()
    if (is.null(m)) return(NULL)

    if (isTRUE(m$loading))
      return(div(style = "color:#888; font-size:0.9em; margin-bottom:8px", spinner_tag, "Searching…"))

    if (!isTRUE(m$ok))
      return(div(class = "small-note", style = "color:#c00; margin-bottom:8px", m$error))

    if (m$mode == "search") {
      results <- orcid_search_results()
      if (length(results) == 0)
        return(div(class = "small-note", style = "margin-bottom:8px", "No results found."))

      rows <- lapply(seq_along(results), function(i) {
        r <- results[[i]]
        tags$div(
          style   = "padding:5px 0; border-bottom:1px solid #eee; cursor:pointer",
          onclick = sprintf("Shiny.setInputValue('orcid_select', %d, {priority: 'event'})", i),
          tags$b(r$name),
          tags$span(class = "small-note", style = "margin-left:6px", r$orcid)
        )
      })
      return(div(
        style = "background:#f5f5f5; border-radius:4px; padding:6px 10px; margin-bottom:8px; font-size:0.92em",
        tags$p(class = "small-note", style = "margin:0 0 4px", "Click a result to select:"),
        tagList(rows)
      ))
    }

    div(
      style = "background:#f5f5f5; border-radius:4px; padding:8px 10px; margin-bottom:8px; font-size:0.92em",
      tags$b(m$name), br(),
      tags$span(class = "small-note", m$orcid)
    )
  })

  # DOI lookup — uses metacheck::doi_clean() and metacheck::crossref_doi() via fetch_doi_meta()
  observeEvent(input$lookup_doi, {
    doi_raw <- trimws(input$own_work_doi)
    if (!nzchar(doi_raw)) {
      crossref_meta(list(ok = FALSE, error = "Please enter a DOI first."))
      return()
    }
    doi_clean <- metacheck::doi_clean(doi_raw)
    if (doi_clean != doi_raw) updateTextInput(session, "own_work_doi", value = doi_clean)

    crossref_meta(list(ok = FALSE, loading = TRUE))
    shinyjs::disable("lookup_doi")
    updateActionButton(session, "lookup_doi", label = "Looking up…")

    session$onFlushed(function() {
      meta <- fetch_doi_meta(doi_clean)
      crossref_meta(meta)
      shinyjs::enable("lookup_doi")
      updateActionButton(session, "lookup_doi", label = "Look up")
    }, once = TRUE)
  })

  output$doi_meta_box <- renderUI({
    m <- crossref_meta()
    if (is.null(m)) return(NULL)
    if (isTRUE(m$loading)) {
      return(div(
        style = "color:#888; font-size:0.9em; margin-bottom:8px",
        spinner_tag, "Looking up…",
        tags$style("@keyframes spin { to { transform: rotate(360deg); } }")
      ))
    }
    if (!isTRUE(m$ok))
      return(div(class = "small-note", style = "color:#c00; margin-bottom:8px", m$error))

    div(
      style = "background:#f5f5f5; border-radius:4px; padding:8px 10px; margin-bottom:8px; font-size:0.92em",
      tags$b(m$title), br(),
      tags$span(class = "small-note", m$authors),
      if (nzchar(m$year)) tags$span(class = "small-note", paste0(" (", m$year, ")")) else NULL
    )
  })

  observeEvent(input$submit_report, {
    errs <- c()

    own_doi <- metacheck::doi_clean(input$own_work_doi %||% "")
    if (!nzchar(own_doi)) errs <- c(errs, "DOI of the cited work is required.")
    if (required_msg(input$why_incorrect)) errs <- c(errs, "An explanation is required.")

    if (length(errs) > 0) {
      report_status(paste("Please fix:\n- ", paste(errs, collapse = "\n- ")))
      return()
    }

    meta    <- crossref_meta()
    meta_ok <- isTRUE(meta$ok)

    fetched_authors_str <- if (meta_ok) meta$authors %||% "" else ""
    fetched_year        <- if (meta_ok) meta$year    %||% "" else ""
    fetched_title       <- if (meta_ok) meta$title   %||% "" else ""

    submitted_last_names <- extract_last_names_from_ref_authors(fetched_authors_str)
    author_key           <- make_author_key(submitted_last_names)
    lead_author          <- if (length(submitted_last_names) > 0) submitted_last_names[1] else NA_character_
    title_key            <- make_title_key(fetched_title)

    selected_codes  <- input$mistake_codes %||% character(0)
    selected_titles <- get_mistake_titles(selected_codes)

    rid <- make_report_id(
      orcid      = trimws(input$reporter_orcid %||% ""),
      own_doi    = own_doi,
      citing_doi = trimws(input$citing_work_doi %||% "")
    )

    row <- data.frame(
      report_id                  = rid,
      submitted_at               = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      reporter_role              = as.character(input$reporter_role %||% ""),
      reporter_orcid             = as.character(input$reporter_orcid %||% ""),
      own_work_doi               = own_doi,
      citing_work_doi            = as.character(input$citing_work_doi %||% ""),
      target_author_last_names   = paste(submitted_last_names, collapse = "; "),
      target_lead_author         = lead_author %||% NA_character_,
      target_author_key          = author_key  %||% NA_character_,
      target_publication_year    = fetched_year,
      target_title_clue          = substr(fetched_title, 1, 80),
      target_title_key           = title_key   %||% NA_character_,
      mistake_codes              = paste(selected_codes,  collapse = "; "),
      mistake_titles             = paste(selected_titles, collapse = "; "),
      quoted_or_paraphrased_text = as.character(input$quoted_or_paraphrased_text %||% ""),
      why_incorrect              = as.character(input$why_incorrect %||% ""),
      stringsAsFactors           = FALSE
    )

    tryCatch({
      append_row_safely(row)

      report_status(paste0(
        "Submitted ✓\n",
        "Report ID: ", rid, "\n",
        "Saved to: ", DB_PATH, "\n",
        if (meta_ok) paste0("Cited work: ", fetched_title, " (", fetched_year, ")\n") else "",
        if (length(selected_codes) > 0) paste0("Miscitation type(s): ", paste(selected_codes, collapse = ", "), "\n") else ""
      ))

      updateTextInput(session,     "own_work_doi",                value = "")
      updateTextInput(session,     "citing_work_doi",             value = "")
      updateTextInput(session,     "reporter_orcid",              value = "")
      updateSelectizeInput(session,"mistake_codes",               selected = character(0))
      updateTextAreaInput(session, "quoted_or_paraphrased_text",  value = "")
      updateTextAreaInput(session, "why_incorrect",               value = "")
      crossref_meta(NULL)
      orcid_meta(NULL)
      orcid_search_results(list())

    }, error = function(e) {
      report_status(paste("Error:", conditionMessage(e)))
    })
  })

  output$report_status <- renderText(report_status())

  observeEvent(input$erase_db, {
    erase_status("")

    if (!isTRUE(input$confirm_erase_db)) {
      erase_status("Tick the confirmation box before erasing the database.")
      return()
    }

    tryCatch({
      backup_path <- erase_database_safely()
      erase_status(paste0("Database erased.\nBackup saved to: ", backup_path))
      report_status("")
      updateCheckboxInput(session, "confirm_erase_db", value = FALSE)
    }, error = function(e) {
      erase_status(paste("Error:", conditionMessage(e)))
    })
  })

  output$erase_status <- renderText(erase_status())
}
