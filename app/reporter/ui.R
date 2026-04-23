ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .small-note { color:#666; font-size:0.95em; }
      .danger-box { border:1px solid #d9534f; padding:12px; border-radius:8px; background:#fff5f5; }
    "))
  ),

  titlePanel("Report a miscitation"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "reporter_role",
        "You are submitting as",
        choices  = c("Author of the cited work", "Co-author", "Reader", "Other"),
        selected = "Author of the cited work"
      ),

      tags$label("Your ORCID (optional)"),
      fluidRow(
        column(8, textInput("reporter_orcid", label = NULL,
                            placeholder = "ORCID iD or search by name")),
        column(4, actionButton("lookup_orcid", "Search", class = "btn-sm btn-default",
                               style = "margin-top:0"))
      ),
      helpText(class = "small-note",
               "Enter your ORCID iD to verify, or type your name to search."),
      uiOutput("orcid_meta_box"),
      helpText(
        class = "small-note",
        "Providing your ORCID increases the credibility of your report, ",
        "especially if you are an author of the cited work. ",
        "Anonymous reports are accepted but will be flagged for extra scrutiny."
      ),

      hr(),

      tags$label("DOI of the cited work *"),
      fluidRow(
        column(8, textInput("own_work_doi", label = NULL, placeholder = "10.xxxx/xxxxx")),
        column(4, actionButton("lookup_doi", "Look up", class = "btn-sm btn-default", style = "margin-top:0"))
      ),
      uiOutput("doi_meta_box"),

      tags$label("DOI of the citing paper (optional)"),
      helpText(class = "small-note",
               "Leave blank to submit a general warning about how your work is commonly miscited."),
      textInput("citing_work_doi", label = NULL, placeholder = "10.xxxx/xxxxx"),

      hr(),

      textAreaInput(
        "quoted_or_paraphrased_text",
        "Miscited text (optional)",
        placeholder = "Paste the exact sentence(s) from the citing paper, or a paraphrase of the claim.",
        rows = 4
      ),

      textAreaInput(
        "why_incorrect",
        "Explanation *",
        placeholder = "Explain what is wrong and what the original work actually shows.",
        rows = 5
      ),

      selectizeInput(
        "mistake_codes",
        "Miscitation type(s) (optional)",
        choices  = mistake_choices,
        multiple = TRUE,
        options  = list(
          placeholder = "Select one or more miscitation types",
          plugins     = list("remove_button")
        )
      ),

      actionButton("submit_report", "Submit report", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("report_status"),

      hr(),

      div(
        class = "danger-box",
        strong("Danger zone"),
        br(), br(),
        checkboxInput("confirm_erase_db", "I understand this will erase the miscitation database", value = FALSE),
        actionButton("erase_db", "Erase database", class = "btn-danger"),
        br(), br(),
        verbatimTextOutput("erase_status")
      )
    ),

    mainPanel(
      h4("Miscitation categories"),
      tableOutput("mistake_table_out")
    )
  )
)
