library(wordVectors)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)


#all of this is to prep my csv, making txt files by set of years
df <- read.csv("NHgazette1756-1783.csv")
df <- df[, c("sequence", "date", "ocr_eng")]

df<- df %>%
    mutate(year = substr(date, 1, 4))

df <- df %>%
    mutate(
        year = as.integer(substr(date, 1, 4)),
        period = case_when(
            year >= 1756 & year <= 1764 ~ "1756-1764",
            year >= 1765 & year <= 1776 ~ "1765-1776",
            year >= 1777 & year <= 1783 ~ "1777-1783",
            TRUE ~ NA_character_
        )
    )

# group by period instead of year
period_corpus <- df %>%
    group_by(period) %>%
    summarise(text = paste(ocr_eng, collapse = " "), .groups = "drop")

output_dir <- "period_corpus"
dir.create(output_dir, showWarnings = FALSE)

for (i in 1:nrow(period_corpus)) {
    file_path <- file.path(output_dir, paste0(period_corpus$period[i], ".txt"))
    writeLines(period_corpus$text[i], file_path)
}


prep_word2vec(
    origin = "period_corpus/",
    destination = "period_corpus/corpus_clean.txt",
    lowercase = TRUE,
    bundle_ngrams = 1
)

model <- train_word2vec(
  train_file = "period_corpus/corpus_clean.txt",
  output_file = "period_corpus/vectorsv2.bin",
  vectors = 100,
  threads = 6,
  window = 7,
  iter = 10,
  negative_samples = 5
)

periods <- c("1756-1764", "1765-1776", "1777-1783")
input_dir <- "period_corpus"
output_dir <- "period_models"
dir.create(output_dir, showWarnings = FALSE)

for (p in periods) {
    input_file <- file.path(input_dir, paste0(p, ".txt"))
    clean_file <- file.path(output_dir, paste0(p, ".txt"))

    prep_word2vec(
        origin = input_file,
        destination = clean_file,
        lowercase = TRUE,
        bundle_ngrams = 1
    )

    model <- train_word2vec(
        train_file = clean_file,
        output_file = file.path(output_dir, paste0(p, "_vectors.bin")),
        vectors = 100,
        threads = 6,
        window = 7,
        iter = 10,
        negative_samples = 5
    )
}










