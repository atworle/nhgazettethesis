library(tidyverse)
library(tidytext)
library(wordcloud2)
library(gganimate)
library(scales)


df<-read.csv("NHgazette1756-1783.csv")
df<-df[,c("sequence", "date", "ocr_eng")]
#just reducing size of df

#the code below counts rows by year, rows represent pages, so pages per year
df <- df %>% mutate(year = substr(date, 1, 4)) %>%
  group_by(year) %>% summarise(pages = n())
 

df.lib <- df %>%
  mutate(libertymention = str_detect(ocr_eng, regex("liberty", ignore_case = TRUE)))
df.ty <- df %>%
  mutate(tyrannymention = str_detect(ocr_eng, regex("tyranny", ignore_case = TRUE)))
  #creating a column that shows whether a page has tyranny, contains yes or no

df.month<- df %>%
  mutate(month= substr(date, 1, 6)) 
df.year<- df %>%
  mutate(year = substr(date, 1,4))


table_by_year <- table(df.year$year, df.year$tyrannymention)
table_by_year_2 <- table(df.year$year, df.year$libertymention)
chisq.test(table_by_year_2)
#tyranny results X-squared = 204.3, df = 27, p-value < 2.2e-16
#liberty results X-squared = 338.51, df = 27, p-value < 2.2e-16
#the chisquared test allows you to see how observed counts differ from expectations, so in essence whether or not the relationship has strong association
#i ran one for the word tyranny and the year, the test shows that frequency of tyranny varied significantly across years, showing clear temporal patterns






word.tokens <- df %>%
  unnest_tokens(word, ocr_eng)
#i want to count total words to generate frequencies
total.words<-nrow(word.tokens) #this should be total words

clean.tokens<- word.tokens %>%
  anti_join(stop_words)

clean.tokens <- clean.tokens %>%
  filter(!str_detect(word, "^[0-9]+$"))
#numbers are most used 'words' so getting rid of them

token.yearly<- word.tokens %>%
mutate(year = substr(date, 1, 4)) %>%
group_by(year)
#this generates a year column then groups the tokens by year

token.monthly<- word.tokens %>%
mutate(month = substr(date, 1, 6)) %>%
group_by(month)

ty.per.month<- token.monthly %>% 
  filter(word == "tyranny") %>%
count (word)

ty.per.year <-token.yearly %>%
  filter(word == "tyranny") %>%
  count(word)


#quick plot to only show tyranny in the first period
ty.fil<- ty.per.year %>%
filter(year >= 1756 & year <= 1764)

x11()
ggplot(ty.fil, aes(x = year, y = n)) +
  geom_segment(aes(xend = year, yend = 0), color = "grey70") +
  geom_point(size = 3, color = "darkred") +
  theme_minimal() +
  labs(
    title = "Mentions of 'Tyranny' by Year (1756-1764)",
    y = "Number of Mentions", x = "Year"
  )

ty.per.year$year <- as.Date(paste0(ty.per.year$year, "0101"), format = "%Y%m%d")
ty.per.year <- ty.per.year %>%
  bind_rows(
    data.frame(
      year = as.Date("1759-01-01"), # January 1st of 1759
      n = 0 # only include columns you have
    )
  ) %>%
  arrange(year)

lib.per.year <- token.yearly %>%
  filter(word == "liberty") %>%
  count(word)
View(lib.per.year)
lib.per.year$year <- as.Date(paste0(lib.per.year$year, "0101"), format = "%Y%m%d")






#making line graph for tyranny by month
ty.per.month$month <- as.Date(paste0(ty.per.month$month, "01"), format = "%Y%m%d")
x11()
ggplot(ty.per.month, aes(x = month, y = n)) +
  geom_line(color = "blue", size = 1) + # line connecting points
  labs(
    title = "Mentions of 'tyranny' per month",
    x = "Month",
    y = "Count of 'tyranny'"
  ) +
  theme_minimal() +
    scale_x_date(
        date_breaks = "3 years", # put a tick every 5 years
        date_labels = "%Y" # show only the year
      )

  






#this is attempted animation
all.months <- seq(min(ty.per.month$month), max(ty.per.month$month), by = "month")
all.months <- data.frame(month = all.months)

full.ty <- all.months %>%
  left_join(ty.per.month, by = "month") %>%
  mutate(n = replace_na(n, 0))

b <- ggplot(full.ty, aes(x = month, y = n)) +
  geom_point(size = 3) +
  transition_time(month) +
  labs(title = "Mentions of 'tyranny': {format(frame_time, '%Y-%m')}")
x11()
animate(b, nframes = 100, fps = 10, renderer = gifski_renderer("tyranny_animation.gif"))
#attempting to animate a line graph




count.yearly<-token.yearly %>%
  count(year, word, sort = TRUE) %>%
  group_by(year)%>%
  slice_max(n, n = 10)
#I have separated the most used words by year now

wordcloud2(count, size = 0.5, color = "random-light", backgroundColor = "black")


fullcount.yearly<- word.tokens %>%
mutate(year = substr(date, 1, 4)) %>%
count(year)
#this makes a df with the total number of words per year
fullcount.yearly$year <- as.Date(paste0(fullcount.yearly$year, "0101"), format = "%Y%m%d")

#here I have made a simple df showing the count of tyranny by year

#had to make the year numeric because substr makes it into a character
ty.freq <- ty.per.year %>%
left_join(fullcount.yearly, by = "year") %>%
mutate(frequencypermillion = n.x/n.y*1000000)

lib.freq <-lib.per.year %>%
left_join(fullcount.yearly,by = "year") %>%
mutate(frequencypermillion = n.x / n.y * 1000000)

#this joins ty word count with total word count df so I can make a frequency column
#multiplied by 1 million to get a standardized word frequency per million

ty.freq <- ty.freq %>% mutate(word = "tyranny")
lib.freq <- lib.freq %>% mutate(word = "liberty")

# Combine into one data frame
all.freq <- bind_rows(ty.freq, lib.freq)

# Plot both lines
x11()
ggplot(all.freq, aes(x = year, y = frequencypermillion, color = word)) +
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





groupedwords<- token.yearly %>%
filter(word %in% c("liberty", "freedom", "slavery", "tyranny", "king", "parliament")) %>%
count(word, name = "count")

groupedfreq <- groupedwords %>%
left_join(fullcount.yearly, by = "year") %>%
mutate(frequencypermillion = count/n*1000000)


groupedfreq<- groupedfreq%>%
mutate(year = as.numeric(year))


ggplot(groupedfreq, aes( x = year, y = frequencypermillion, color = word)) +
  geom_line() +
  labs(
    title = "Frequency of Words in New Hampshire Gazette 1756-1783",
    x = "Year",
    y = "Frequency Per Million Words"
  ) +
  scale_x_continuous(breaks = seq(1756, 1783, by = 5))






tyranny<- token.yearly %>%
filter(word ==  "tyranny") %>%
count(year)

liberty<- token.yearly %>%
filter(word == "liberty") %>%
count(year)

combinedcount <- liberty %>%
left_join(tyranny, by = "year")

combinedcount<- combinedcount %>%
mutate(ratio = n.x/n.y)

combinedcount <-combinedcount %>%
filter(!year %in% c("1759", "1781"))
combinedcount$year <- as.numeric(combinedcount$year)


colnames(combinedcount)<-c("Year", "Liberty Count", "Tyranny Count", "Ratio of Liberty to Tyranny")


ggplot(combinedcount, aes(x = Year, y = `Ratio of Liberty to Tyranny`)) +
  geom_line(color = "#2C3E50", size = 1.2) +
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

# Save the plot in high resolution









