install.packages("styler")
install.packages("lintr")

library(dplyr)
library(styler)

# Transformer: replace T/F with TRUE/FALSE
replace_TF <- function(pd) {
  pd$token[pd$text == "T"] <- "TRUE"
  pd$text[pd$text == "T"]  <- "TRUE"
  pd$token[pd$text == "F"] <- "FALSE"
  pd$text[pd$text == "F"]  <- "FALSE"
  pd
}

# Custom style: tidyverse + T/F replacement + line length 80
custom_style <- function(...) {
  tidyverse_style(...) %>%
    add_transformer(replace_TF, transformer_name = "replace_TF") %>%
    # limit lines to 80 chars with sensible indentation
    set_line_breaks(line_length = 80)
}

# Helper: configure line breaking
set_line_breaks <- function(style, line_length = 80) {
  style$line_break <- list(
    line_length = line_length,
    # These defaults ensure readability:
    force_braces_in_function_call = TRUE,
    force_named_argument_braces   = TRUE
  )
  style
}
