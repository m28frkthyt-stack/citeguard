DB_DIR  <- Sys.getenv("CITEGUARD_DATA_DIR",
                      unset = normalizePath("../data", mustWork = FALSE))
DB_PATH   <- file.path(DB_DIR, "miscitation_reports.csv")
LOCK_PATH <- paste0(DB_PATH, ".lock")

dir.create(DB_DIR, showWarnings = FALSE, recursive = TRUE)

GH_OWNER   <- "mink-veltman"
GH_REPO    <- "citeguard-data"
GH_FILE    <- "miscitation_reports.csv"
GH_BRANCH  <- "master"
GH_API_URL <- paste0("https://api.github.com/repos/", GH_OWNER, "/", GH_REPO, "/contents/", GH_FILE)
GH_RAW_URL <- paste0("https://raw.githubusercontent.com/", GH_OWNER, "/", GH_REPO, "/", GH_BRANCH, "/", GH_FILE)

db_cols <- c(
  "report_id",
  "submitted_at",
  "reporter_role",
  "reporter_orcid",
  "own_work_doi",
  "citing_work_doi",
  "target_author_last_names",
  "target_lead_author",
  "target_author_key",
  "target_publication_year",
  "target_title_clue",
  "target_title_key",
  "mistake_codes",
  "mistake_titles",
  "quoted_or_paraphrased_text",
  "why_incorrect"
)

empty_db_frame <- function() {
  as.data.frame(
    setNames(replicate(length(db_cols), character(0), simplify = FALSE), db_cols),
    stringsAsFactors = FALSE
  )
}

required_msg <- function(x) {
  if (is.null(x)) return(TRUE)
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(TRUE)
  !any(nzchar(trimws(x)))
}

make_report_id <- function(orcid, own_doi, citing_doi) {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  h <- digest::digest(paste(orcid, own_doi, citing_doi, runif(1), sep = "|"), algo = "sha1")
  paste0(stamp, "_", substr(h, 1, 10))
}

get_mistake_titles <- function(codes) {
  if (is.null(codes) || length(codes) == 0) return(character(0))
  mistake_table$mistake_title[match(codes, mistake_table$mistake_code)]
}

init_db_if_missing <- function() {
  if (!file.exists(DB_PATH)) {
    write.csv(empty_db_frame(), DB_PATH, row.names = FALSE, na = "")
  }
}

read_reports_db <- function() {
  init_db_if_missing()

  out <- tryCatch(
    read.csv(DB_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE),
    error = function(e) empty_db_frame()
  )

  out <- as.data.frame(out, stringsAsFactors = FALSE)

  for (cc in db_cols) {
    if (!cc %in% names(out)) out[[cc]] <- NA_character_
  }

  out <- out[, db_cols, drop = FALSE]
  for (cc in db_cols) out[[cc]] <- as.character(out[[cc]])
  out
}

acquire_lock <- function(timeout_sec = 10) {
  start <- Sys.time()
  while (file.exists(LOCK_PATH)) {
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout_sec) {
      return(FALSE)
    }
    Sys.sleep(0.1)
  }
  file.create(LOCK_PATH)
}

release_lock <- function() {
  if (file.exists(LOCK_PATH)) unlink(LOCK_PATH)
}

merge_semicolon_field <- function(existing_val, new_val) {
  existing_parts <- trimws(unlist(strsplit(as.character(existing_val), ";\\s*")))
  new_parts      <- trimws(unlist(strsplit(as.character(new_val),      ";\\s*")))
  all_parts <- unique(c(
    existing_parts[nzchar(existing_parts) & existing_parts != "NA"],
    new_parts[nzchar(new_parts) & new_parts != "NA"]
  ))
  paste(all_parts, collapse = "; ")
}

merge_freetext_field <- function(existing_val, new_val) {
  e <- trimws(as.character(existing_val))
  n <- trimws(as.character(new_val))
  e[e == "NA"] <- ""
  n[n == "NA"] <- ""
  if (!nzchar(n) || n == e) return(e)
  if (!nzchar(e)) return(n)
  paste(e, n, sep = " || ")
}

append_row_safely <- function(row_df) {
  init_db_if_missing()

  if (!acquire_lock()) stop("Database is busy. Please try again.")
  on.exit(release_lock(), add = TRUE)

  existing <- read_reports_db()
  row_df <- as.data.frame(row_df, stringsAsFactors = FALSE)

  for (cc in db_cols) {
    if (!cc %in% names(row_df)) row_df[[cc]] <- NA_character_
  }

  row_df <- row_df[, db_cols, drop = FALSE]
  for (cc in db_cols) row_df[[cc]] <- as.character(row_df[[cc]])

  new_own    <- trimws(row_df$own_work_doi[1])
  new_citing <- trimws(row_df$citing_work_doi[1])

  match_idx <- which(
    trimws(existing$own_work_doi)    == new_own &
    trimws(existing$citing_work_doi) == new_citing &
    nzchar(new_own) & nzchar(new_citing)
  )

  if (length(match_idx) > 0) {
    idx <- match_idx[1]
    existing$mistake_codes[idx]              <- merge_semicolon_field(existing$mistake_codes[idx],              row_df$mistake_codes[1])
    existing$mistake_titles[idx]             <- merge_semicolon_field(existing$mistake_titles[idx],             row_df$mistake_titles[1])
    existing$why_incorrect[idx]              <- merge_freetext_field(existing$why_incorrect[idx],               row_df$why_incorrect[1])
    existing$quoted_or_paraphrased_text[idx] <- merge_freetext_field(existing$quoted_or_paraphrased_text[idx],  row_df$quoted_or_paraphrased_text[1])
    existing$submitted_at[idx]               <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    out <- existing
  } else {
    out <- dplyr::bind_rows(existing, row_df)
  }

  out <- out[, db_cols, drop = FALSE]
  write.csv(out, DB_PATH, row.names = FALSE, na = "")
  invisible(out)
}

sync_db_from_github <- function() {
  tryCatch({
    df <- read.csv(GH_RAW_URL, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
    write.csv(df, DB_PATH, row.names = FALSE, na = "")
  }, error = function(e) {
    message("GitHub DB read failed (using local copy): ", conditionMessage(e))
  })
}

push_db_to_github <- function(df, commit_msg) {
  pat <- Sys.getenv("GITHUB_PAT")
  if (!nzchar(pat)) return("GitHub sync skipped: GITHUB_PAT not set")

  tryCatch({
    sha_resp <- request(GH_API_URL) |>
      req_headers(Authorization = paste("token", pat),
                  Accept        = "application/vnd.github.v3+json") |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    sha <- if (resp_status(sha_resp) == 200) resp_body_json(sha_resp)$sha else NULL

    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    write.csv(df, tmp, row.names = FALSE, na = "")
    encoded <- base64encode(readBin(tmp, "raw", file.info(tmp)$size))

    body <- list(message = commit_msg, content = encoded, branch = GH_BRANCH)
    if (!is.null(sha)) body$sha <- sha

    put_resp <- request(GH_API_URL) |>
      req_headers(Authorization = paste("token", pat),
                  Accept        = "application/vnd.github.v3+json") |>
      req_body_json(body) |>
      req_method("PUT") |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()

    if (resp_status(put_resp) %in% c(200L, 201L)) {
      "GitHub synced"
    } else {
      paste0("GitHub sync failed (HTTP ", resp_status(put_resp), "): ",
             resp_body_string(put_resp))
    }
  }, error = function(e) {
    paste0("GitHub sync error: ", conditionMessage(e))
  })
}

erase_database_safely <- function() {
  init_db_if_missing()

  if (!acquire_lock()) stop("Database is busy. Please try again.")
  on.exit(release_lock(), add = TRUE)

  backup_path <- file.path(
    DB_DIR,
    paste0("miscitation_reports_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )

  if (file.exists(DB_PATH)) {
    file.copy(DB_PATH, backup_path, overwrite = TRUE)
  }

  empty <- empty_db_frame()
  write.csv(empty, DB_PATH, row.names = FALSE, na = "")
  gh_status <- push_db_to_github(empty, "erase: database cleared")

  list(backup_path = backup_path, gh_status = gh_status)
}

summarise_known_miscitations <- function(db) {
  if (is.null(db) || nrow(db) == 0) {
    return(tibble::tibble(
      target_author_key = character(),
      target_lead_author = character(),
      target_publication_year = character(),
      target_title_key = character(),
      target_author_last_names = character(),
      report_count = integer(),
      mistake_codes = character(),
      mistake_titles = character()
    ))
  }

  db2 <- db |>
    dplyr::rowwise() |>
    dplyr::mutate(
      target_author_key = make_author_key(extract_last_names_from_ref_authors(target_author_last_names)),
      target_lead_author = {
        tmp <- extract_last_names_from_ref_authors(target_author_last_names)
        if (length(tmp) > 0) normalize_last_name(tmp[1]) else NA_character_
      },
      target_publication_year = as.character(target_publication_year),
      target_title_key = make_title_key(target_title_clue)
    ) |>
    dplyr::ungroup()

  db2 |>
    dplyr::filter(
      !is.na(target_author_key) |
        !is.na(target_lead_author) |
        !is.na(target_title_key)
    ) |>
    dplyr::group_by(
      target_author_key,
      target_lead_author,
      target_publication_year,
      target_title_key
    ) |>
    dplyr::summarise(
      target_author_last_names = dplyr::first(target_author_last_names),
      report_count = dplyr::n(),
      report_ids = paste(unique(na.omit(report_id)), collapse = "; "),
      mistake_codes = paste(unique(na.omit(unlist(strsplit(paste(mistake_codes, collapse = "; "), ";\\s*")))), collapse = "; "),
      mistake_titles = paste(unique(na.omit(unlist(strsplit(paste(mistake_titles, collapse = "; "), ";\\s*")))), collapse = "; "),
      why_incorrect = {
        parts <- trimws(unlist(strsplit(paste(why_incorrect, collapse = " || "), "\\s*\\|\\|\\s*")))
        parts <- unique(parts[nzchar(parts) & parts != "NA"])
        paste(parts, collapse = " || ")
      },
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(report_count), target_author_last_names)
}
