`%||%` <- function(x, y) if (is.null(x)) y else x

normalize_text_ascii <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- enc2utf8(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x
}

normalize_last_name <- function(x) {
  if (is.null(x)) return(character(0))
  x <- normalize_text_ascii(x)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z'-]", "", x, perl = TRUE)
  x[nchar(x) == 0] <- NA_character_
  x
}

make_author_key <- function(last_names, max_n = 5) {
  if (is.null(last_names) || length(last_names) == 0) return(NA_character_)
  last_names <- normalize_last_name(last_names)
  last_names <- last_names[!is.na(last_names)]
  last_names <- unique(last_names)
  if (length(last_names) == 0) return(NA_character_)
  paste(last_names[seq_len(min(length(last_names), max_n))], collapse = "|")
}

normalize_title_tokens <- function(x, max_tokens = 6) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- normalize_text_ascii(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  if (!nzchar(x)) return(character(0))

  toks <- unlist(strsplit(x, "\\s+"))
  toks <- toks[nchar(toks) >= 4]
  toks <- toks[!toks %in% c(
    "with", "from", "that", "this", "these", "those", "their", "there",
    "about", "among", "into", "using", "effects", "effect", "study",
    "analysis", "research", "evidence", "paper"
  )]
  toks <- unique(toks)

  if (length(toks) == 0) return(character(0))
  toks[seq_len(min(length(toks), max_tokens))]
}

make_title_key <- function(x, max_tokens = 6) {
  toks <- normalize_title_tokens(x, max_tokens = max_tokens)
  if (length(toks) == 0) return(NA_character_)
  paste(toks, collapse = "|")
}

extract_year_guess_one <- function(x) {
  if (length(x) != 1 || is.null(x) || is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  m <- stringr::str_extract(as.character(x), "\\b(19|20)\\d{2}[a-z]?\\b")
  ifelse(is.na(m), NA_character_, m)
}

extract_year_guess_vec <- function(x) {
  purrr::map_chr(as.character(x), extract_year_guess_one)
}

extract_lead_author_from_citation_one <- function(citation_text) {
  if (length(citation_text) != 1 || is.null(citation_text) || is.na(citation_text) || !nzchar(trimws(citation_text))) {
    return(NA_character_)
  }

  txt <- normalize_text_ascii(citation_text)
  txt <- gsub("\\bet al\\.?\\b", "", txt, ignore.case = TRUE)
  txt <- trimws(txt)

  tokens <- stringr::str_extract_all(txt, "\\b[A-Za-z][A-Za-z'\\-]+\\b")[[1]]
  if (length(tokens) == 0) return(NA_character_)

  bad <- c("and", "the", "for", "see", "also")
  tokens <- tokens[!(tolower(tokens) %in% bad)]
  if (length(tokens) == 0) return(NA_character_)

  out <- normalize_last_name(tokens[1])
  if (length(out) == 0) NA_character_ else out
}

extract_lead_author_from_citation_vec <- function(x) {
  purrr::map_chr(as.character(x), extract_lead_author_from_citation_one)
}

extract_last_names_from_ref_authors <- function(ref_authors, max_n = 5) {
  if (is.null(ref_authors) || is.na(ref_authors) || !nzchar(trimws(ref_authors))) {
    return(character(0))
  }

  authors <- unlist(strsplit(ref_authors, ";"))
  authors <- trimws(authors)
  authors <- authors[nzchar(authors)]

  out <- character(0)
  for (a in authors) {
    a2 <- normalize_text_ascii(a)
    a2 <- gsub(",", " ", a2)
    a2 <- stringr::str_squish(a2)
    toks <- unlist(strsplit(a2, "\\s+"))
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) next
    last <- toks[length(toks)]
    last <- normalize_last_name(last)
    if (length(last) > 0 && !is.na(last)) out <- c(out, last)
  }

  out <- unique(out)
  if (length(out) == 0) return(character(0))
  out[seq_len(min(length(out), max_n))]
}

count_title_overlap <- function(a, b) {
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(0L)
  ta <- unique(unlist(strsplit(a, "\\|")))
  tb <- unique(unlist(strsplit(b, "\\|")))
  length(intersect(ta, tb))
}

author_name_distance_ok <- function(a, b) {
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(FALSE)

  a <- normalize_last_name(a)
  b <- normalize_last_name(b)

  if (length(a) == 0 || length(b) == 0 || is.na(a) || is.na(b)) return(FALSE)
  if (a == b) return(TRUE)

  d <- utils::adist(a, b)[1]
  if (max(nchar(a), nchar(b)) >= 8) d <= 2 else d <= 1
}

fuzzy_author_overlap <- function(ref_names, target_names, max_compare = 3) {
  ref_names <- normalize_last_name(ref_names)
  target_names <- normalize_last_name(target_names)

  ref_names <- unique(ref_names[!is.na(ref_names)])
  target_names <- unique(target_names[!is.na(target_names)])

  if (length(ref_names) == 0 || length(target_names) == 0) return(0L)

  ref_names <- ref_names[seq_len(min(length(ref_names), max_compare))]
  target_names <- target_names[seq_len(min(length(target_names), max_compare))]

  matched <- 0L
  used_target <- rep(FALSE, length(target_names))

  for (r in ref_names) {
    for (j in seq_along(target_names)) {
      if (!used_target[j] && author_name_distance_ok(r, target_names[j])) {
        matched <- matched + 1L
        used_target[j] <- TRUE
        break
      }
    }
  }

  matched
}

year_close_score <- function(ref_year, target_year) {
  if (is.na(ref_year) || is.na(target_year) || !nzchar(ref_year) || !nzchar(target_year)) return(0L)

  ref_num <- suppressWarnings(as.integer(stringr::str_extract(ref_year, "(19|20)\\d{2}")))
  tar_num <- suppressWarnings(as.integer(stringr::str_extract(target_year, "(19|20)\\d{2}")))

  if (is.na(ref_num) || is.na(tar_num)) return(0L)
  if (ref_num == tar_num) return(2L)
  if (abs(ref_num - tar_num) == 1) return(1L)
  0L
}
