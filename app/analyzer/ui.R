ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper .dt-buttons { margin-bottom: 10px; }
      table.dataTable tbody td { vertical-align: top; }
      .wide-text { white-space: normal !important; word-break: break-word; }
      .small-note { color:#666; font-size:0.95em; }
      .section-card { border:1px solid #ddd; border-radius:8px; padding:10px 14px; margin-bottom:12px; }
      .section-header { display:flex; justify-content:space-between; align-items:center; cursor:pointer; }
      .collapsed-body { display:none; margin-top:12px; }
    "))
  ),

  titlePanel("Citation analyzer"),

  fluidRow(
    column(
      width = 3,
      wellPanel(
        fileInput("files", "Upload PDF(s) or ZIP file(s)", multiple = TRUE, accept = c(".pdf", ".zip")),
        textInput("grobid_url", "GROBID URL", value = "http://localhost:8070"),

        hr(),

        textInput("citation_filter", "Filter citation string", placeholder = "e.g. Smith, [12], 2020"),
        textInput("author_filter", "Filter cited author", placeholder = "e.g. Kahneman"),
        checkboxInput("only_known_miscited", "Only show citations to papers in the database", value = FALSE),

        hr(),

        actionButton("test_grobid",     "Test GROBID"),
        actionButton("test_pdf2grobid", "Test first PDF"),
        br(), br(),
        actionButton("process", "Process files", class = "btn-primary"),
        br(), br(),
        downloadButton("download_csv", "Download CSV"),

        hr(),

        strong("LLM miscitation check"),
        helpText(class = "small-note",
                 "Checks flagged rows (matched in miscitation database) using an LLM via Groq."),
        passwordInput("groq_api_key", "Groq API key",
                      placeholder = "gsk_... (or set GROQ_API_KEY in .Renviron)"),
        selectInput("llm_model", "Model",
          choices = c(
            "llama-3.1-8b-instant (fast, cheap)"   = "llama-3.1-8b-instant",
            "llama3-8b-8192 (stable)"               = "llama3-8b-8192",
            "llama-3.3-70b-versatile (more capable)"= "llama-3.3-70b-versatile",
            "llama-4-scout-17b (balanced)"          = "meta-llama/llama-4-scout-17b-16e-instruct"
          ),
          selected = "llama-3.1-8b-instant"
        ),
        actionButton("run_llm",  "Run LLM check", class = "btn-warning"),
        shinyjs::hidden(actionButton("stop_llm", "Stop & keep results", class = "btn-danger")),
        br(), br(),
        verbatimTextOutput("llm_status")
      )
    ),

    column(
      width = 9,
      fluidRow(
        column(3, wellPanel(strong("Uploaded"),       textOutput("stat_uploaded"))),
        column(3, wellPanel(strong("PDFs found"),     textOutput("stat_pdfs"))),
        column(3, wellPanel(strong("Processed"),      textOutput("stat_processed"))),
        column(3, wellPanel(strong("Success / Fail"), textOutput("stat_success_fail")))
      ),

      div(
        class = "section-card",
        div(
          class = "section-header",
          tags$div(
            tags$strong("Known miscited entries in database"),
            tags$div(class = "small-note", "Collapsed by default.")
          ),
          actionLink("toggle_known_tbl", label = "Show")
        ),
        div(id = "known_tbl_body", class = "collapsed-body", DT::DTOutput("known_miscited_table"))
      ),

      tabsetPanel(
        tabPanel("Results",     DT::DTOutput("results_table")),
        tabPanel("Live log",    verbatimTextOutput("live_log")),
        tabPanel("Diagnostics", verbatimTextOutput("diagnostic_text")),
        tabPanel("Status",      verbatimTextOutput("status_text"))
      )
    )
  )
)
