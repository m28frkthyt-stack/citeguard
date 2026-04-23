mistake_table <- data.frame(
  mistake_code = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10"),
  mistake_title = c(
    "Direct contradiction of finding",
    "Non-existent finding",
    "Non-significant finding",
    "Overgeneralization of population",
    "Misrepresentation of experimental conditions",
    "Causal relation distorted or confused",
    "Independent variable distortion",
    "Dependent variable distortion",
    "Measure-to-construct inflation",
    "Non-contextualized citation"
  ),
  mistake_description = c(
    "The findings of the cited paper are directly contradicted in the citation.",
    "Neither the cited finding nor the measures exist in the cited paper.",
    "A non-significant finding is cited.",
    "The cited finding is overgeneralized to a larger population without justification.",
    "The experimental conditions are misrepresented.",
    "The causal claim is reversed or overstated.",
    "The cited independent variable is distorted.",
    "The cited dependent variable is distorted.",
    "A measure is inflated to a broader construct without justification.",
    "Citation relevance is not apparent."
  ),
  stringsAsFactors = FALSE
)

mistake_choices <- stats::setNames(
  mistake_table$mistake_code,
  paste0(mistake_table$mistake_code, " — ", mistake_table$mistake_title)
)

VALID_MISTAKE_CODES <- mistake_table$mistake_code

TAXONOMY_BLOCK <- paste(
  paste0(mistake_table$mistake_code, " – ", mistake_table$mistake_title),
  collapse = "\n"
)
