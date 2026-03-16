library(tidyverse)
library(tidytext)
library(wordcloud2)
library(gganimate)
library(scales)
library(stringr)
library(stringdist)

# =========================================================
# 1. LOAD + BASIC PREP
# =========================================================

df_raw <- read.csv("NHgazette1756-1783.csv")

df_pages <- df_raw %>%
  select(sequence, date, ocr_eng) %>%
  mutate(
    year = substr(date, 1, 4),
    month = substr(date, 1, 6)
  )

# Pages per year
pages.by.year <- df_pages %>%
  count(year, name = "pages")

# =========================================================
# 2. PAGE-LEVEL WORD MENTIONS
# =========================================================

df_mentions <- df_pages %>%
  mutate(
    libertymention = str_detect(ocr_eng, regex("liberty", ignore_case = TRUE)),
    tyrannymention = str_detect(ocr_eng, regex("tyranny", ignore_case = TRUE))
  )

# =========================================================
# 3. TEXT NORMALIZATION
# =========================================================

df_text <- df_pages %>%
  mutate(
    ocr_eng = str_replace_all(
      ocr_eng,
      regex("\\bstanding\\s+armies?\\b", ignore_case = TRUE),
      "standing army"
    )
  )

# =========================================================
# 4. TOKENS + BIGRAMS
# =========================================================

word.tokens <- df_text %>%
  unnest_tokens(word, ocr_eng)

total.words <- nrow(word.tokens)

# Optional cleaned tokens object
clean.tokens <- word.tokens %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$"))

bigrams <- df_text %>%
  unnest_tokens(bigram, ocr_eng, token = "ngrams", n = 2)

# =========================================================
# 5. YEARLY + MONTHLY TOKEN TABLES
# =========================================================

token.yearly <- word.tokens %>%
  mutate(year = substr(date, 1, 4))

token.monthly <- word.tokens %>%
  mutate(month = substr(date, 1, 6))

bigrams.yearly <- bigrams %>%
  mutate(year = substr(date, 1, 4))

# Total words per year
fullcount.yearly <- token.yearly %>%
  count(year, name = "total_words")

# =========================================================
# 6. TYRANNY COUNTS BY MONTH / YEAR
# =========================================================

ty.per.month <- token.monthly %>%
  filter(word == "tyranny") %>%
  count(month, name = "n")

ty.per.year <- token.yearly %>%
  filter(word == "tyranny") %>%
  count(year, name = "n")

lib.per.year <- token.yearly %>%
  filter(word == "liberty") %>%
  count(year, name = "n")
view(lib.per.year)
# =========================================================
# 7. QUICK PLOT: TYRANNY IN FIRST PERIOD
# =========================================================

ty.fil <- ty.per.year %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1756 & year <= 1764)

# x11()
ggplot(ty.fil, aes(x = year, y = n)) +
  geom_segment(aes(xend = year, yend = 0), color = "grey70") +
  geom_point(size = 3, color = "darkred") +
  theme_minimal() +
  labs(
    title = "Mentions of 'Tyranny' by Year (1756-1764)",
    y = "Number of Mentions",
    x = "Year"
  )

# =========================================================
# 8. TYRANNY MONTHLY LINE GRAPH
# =========================================================

ty.per.month.plot <- ty.per.month %>%
  mutate(month = as.Date(paste0(month, "01"), format = "%Y%m%d"))

# x11()
ggplot(ty.per.month.plot, aes(x = month, y = n)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Mentions of 'tyranny' per month",
    x = "Month",
    y = "Count of 'tyranny'"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y"
  )


# =========================================================
# 9. TYRANNY ANIMATIONS GRAPHS
# =========================================================





#monthly
all.months <- seq(
  min(ty.per.month.plot$month),
  max(ty.per.month.plot$month),
  by = "month"
)

all.months <- data.frame(month = all.months)

full.ty <- all.months %>%
  left_join(ty.per.month.plot, by = "month") %>%
  mutate(n = replace_na(n, 0))

b <- ggplot(full.ty, aes(x = month, y = n, group = 1)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(size = 2) +
  transition_reveal(month) +
  labs(title = "Mentions of 'tyranny': {format(frame_along, '%Y-%m')}")

x11()
animate(
  b,
  nframes = 100,
  fps = 10,
  renderer = gifski_renderer("nhgazettevisualizations/tyranny_animation.gif")
)

#yearly
# convert year to numeric for animation
ty.year.anim <- ty.per.year %>%
  mutate(year = as.numeric(year))

anim_plot <- ggplot(ty.year.anim, aes(x = year, y = n, group = 1)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(size = 2) +
  transition_reveal(year) +
  labs(
    title = "Mentions of 'Tyranny' in the New Hampshire Gazette",
    subtitle = "Year: {frame_along}",
    x = "Year",
    y = "Count of 'Tyranny'"
  ) +
  theme_minimal()

animate(
  anim_plot,
  nframes = 100,
  fps = 10,
  renderer = gifski_renderer("nhgazettevisualizations/tyranny_year_animation.gif")
)

# =========================================================
# 10. TOP WORDS BY YEAR + WORD CLOUD
# =========================================================

count.yearly <- token.yearly %>%
  count(year, word, sort = TRUE) %>%
  group_by(year) %>%
  slice_max(n, n = 10) %>%
  ungroup()

count.overall <- word.tokens %>%
  count(word, sort = TRUE)

wordcloud2(
  count.overall,
  size = 0.5,
  color = "random-light",
  backgroundColor = "black"
)

# =========================================================
# 11. NORMALIZED YEARLY FREQUENCIES: TYRANNY + LIBERTY
# =========================================================

ty.freq <- ty.per.year %>%
  left_join(fullcount.yearly, by = "year") %>%
  mutate(
    frequencypermillion = n / total_words * 1000000,
    word = "tyranny"
  )

lib.freq <- lib.per.year %>%
  left_join(fullcount.yearly, by = "year") %>%
  mutate(
    frequencypermillion = n / total_words * 1000000,
    word = "liberty"
  )

all.freq <- bind_rows(ty.freq, lib.freq) %>%
  mutate(year_date = as.Date(paste0(year, "0101"), format = "%Y%m%d"))

# add missing 1759 for tyranny if you want that gap shown explicitly
ty.freq.plot <- ty.freq %>%
  mutate(year_date = as.Date(paste0(year, "0101"), format = "%Y%m%d")) %>%
  bind_rows(
    data.frame(
      year = "1759",
      n = 0,
      total_words = NA,
      frequencypermillion = 0,
      word = "tyranny",
      year_date = as.Date("1759-01-01")
    )
  ) %>%
  arrange(year_date)

# x11()
ggplot(all.freq, aes(x = year_date, y = frequencypermillion, color = word)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Frequency of Tyranny and Liberty in New Hampshire Gazette 1756–1783",
    x = "Year",
    y = "Frequency per Million Words",
    color = "Word"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y"
  )

# =========================================================
# 12. GROUPED WORD FREQUENCIES
# =========================================================

groupedwords <- token.yearly %>%
  filter(word %in% c("liberty", "freedom", "slavery", "tyranny", "king", "parliament")) %>%
  count(year, word, name = "count")

groupedfreq <- groupedwords %>%
  left_join(fullcount.yearly, by = "year") %>%
  mutate(
    frequencypermillion = count / total_words * 1000000,
    year = as.numeric(year)
  )

ggplot(groupedfreq, aes(x = year, y = frequencypermillion, color = word)) +
  geom_line() +
  labs(
    title = "Frequency of Words in New Hampshire Gazette 1756-1783",
    x = "Year",
    y = "Frequency Per Million Words"
  ) +
  scale_x_continuous(breaks = seq(1756, 1783, by = 5))

# =========================================================
# 13. CATO
# =========================================================

cato.yearly <- token.yearly %>%
  filter(word == "cato") %>%
  count(year, name = "n") %>%
  left_join(fullcount.yearly, by = "year") %>%
  mutate(
    frequencypermillion = n / total_words * 1000000,
    year = as.integer(year)
  )

# x11()
ggplot(cato.yearly, aes(x = year, y = frequencypermillion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    title = "Frequency of 'Cato' in the New-Hampshire Gazette, 1756–1783",
    subtitle = "Occurrences per million words (normalized by total yearly word count)",
    x = "Year",
    y = "Per million words",
    caption = "Source: New-Hampshire Gazette (Chronicling America)."
  ) +
  scale_x_continuous(
    breaks = seq(1756, 1783, by = 3),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    minor_breaks = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4)
  )

ggsave("nhgazettevisualizations/cato_frequency_nhgazette.png", width = 8, height = 5, dpi = 300)

# =========================================================
# 14. POPERY
# =========================================================

popery.yearly <- token.yearly %>%
  filter(word == "popery") %>%
  count(year, name = "n") %>%
  left_join(fullcount.yearly, by = "year") %>%
  mutate(
    frequencypermillion = n / total_words * 1000000,
    year = as.integer(year)
  )

# x11()
ggplot(popery.yearly, aes(x = year, y = frequencypermillion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    title = "Frequency of 'popery' in the New-Hampshire Gazette, 1756–1783",
    subtitle = "Occurrences per million words (normalized by total yearly word count)",
    x = "Year",
    y = "Per million words",
    caption = "Source: New-Hampshire Gazette (Chronicling America)."
  ) +
  scale_x_continuous(
    breaks = seq(1756, 1783, by = 3),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    minor_breaks = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4)
  )

ggsave("nhgazettevisualizations/popery_frequency_nhgazette.png", width = 8, height = 5, dpi = 300)

# =========================================================
# 15. STANDING ARMY / STANDING ARMIES
# =========================================================

standingarmies.yearly <- bigrams.yearly %>%
  filter(bigram == "standing army") %>%
  count(year, name = "n") %>%
  left_join(fullcount.yearly, by = "year") %>%
  mutate(
    frequencypermillion = n / total_words * 1000000,
    year = as.integer(year)
  )

# x11()
ggplot(standingarmies.yearly, aes(x = year, y = frequencypermillion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    title = "Frequency of 'Standing Army/Standing Armies' in the New-Hampshire Gazette, 1756–1783",
    subtitle = "Occurrences per million words (normalized by total yearly word count)",
    x = "Year",
    y = "Per million words",
    caption = "Source: New-Hampshire Gazette (Chronicling America)."
  ) +
  scale_x_continuous(
    breaks = seq(1756, 1783, by = 3),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    minor_breaks = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4)
  )

ggsave("nhgazettevisualizations/standingarmiesfreq_nhgazette.png", width = 8, height = 5, dpi = 300)

# =========================================================
# 16. LIBERTY / TYRANNY RATIO
# =========================================================

tyranny.counts <- token.yearly %>%
  filter(word == "tyranny") %>%
  count(year, name = "Tyranny Count")

liberty.counts <- token.yearly %>%
  filter(word == "liberty") %>%
  count(year, name = "Liberty Count")

combinedcount <- liberty.counts %>%
  left_join(tyranny.counts, by = "year") %>%
  mutate(`Ratio of Liberty to Tyranny` = `Liberty Count` / `Tyranny Count`) %>%
  filter(!year %in% c("1759", "1781")) %>%
  mutate(Year = as.numeric(year)) %>%
  select(Year, `Liberty Count`, `Tyranny Count`, `Ratio of Liberty to Tyranny`)

ggplot(combinedcount, aes(x = Year, y = `Ratio of Liberty to Tyranny`)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_smooth(method = "loess", se = FALSE, color = "#E74C3C", linetype = "dashed") +
  labs(
    title = "'Liberty to Tyranny' Ratio Over Time",
    subtitle = "New Hampshire Gazette, 1756–1783",
    x = "Year",
    y = "Liberty / Tyranny Ratio",
    caption = "Source: Chronicling America | Visualized by Antonio W"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 1)
  )









