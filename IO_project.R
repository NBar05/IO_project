library(zoo)
library(dplyr)
library(tidyr)
library(psych)
library(readxl)
library(ggplot2)
library(stargazer)

# it's a kind of magic, skip it
`%--%` <- function(x, y) {
  do.call(sprintf, c(list(x), y))
}

# задаём временной промежуток вытягиваемых данных
dates <- as.yearmon(2018 + 6/12 + seq(1, 21) / 12) # август 2018 -- апрель 2020

# читаем файлы и хоба в таблицу
naming <- "mediaholdings/mh-%s.xlsx" %--% list(c(1:21))

media_holdings <- data.frame()
for (i in 1:length(naming)) { 
  data <- read_xlsx(naming[i], skip = 3)
  data$Период <- dates[i]
  media_holdings <- rbind(media_holdings, data)
}

# читаем файлы и хоба в таблицу 2
naming <- "media_news/news-%s.csv" %--% list(c(1:21))

media_news <- data.frame()
for (i in 1:length(naming)) {
  data <- read.csv(naming[i], header = TRUE, na.strings = "", stringsAsFactors = FALSE)
  data$Период <- dates[i]
  media_news <- rbind(media_news, data)
}

media_news$Период <- as.yearmon(media_news$Период)
# не будем искать врагов там, где их нет
media_news_c <- media_news %>%
  filter(Тематики.ресурса !=
      "Авто и мото->Издания об автомобилях, Авто и мото, Новости и СМИ->Издания об автомобилях, Новости и СМИ") %>%
  filter(Тематики.ресурса !=
      "Наука, Техника и технологии, Наука->Научные журналы и публикации, Новости и СМИ->Научные журналы и публикации, Техника и технологии->Новости Hi-Tech, Новости и СМИ") %>%
  filter(Тематики.ресурса !=
      "Техника и технологии, Новости и СМИ->Деловые новости, Техника и технологии->Новости Hi-Tech, Бизнес, Новости и СМИ, Бизнес->Деловые новости")

# оставим только газеты и журналы
# media_only_news <- media_news %>% filter(Тематики.ресурса == "Новости и СМИ, Новости и СМИ->Газеты, журналы")


# 
media_holdings <- media_holdings %>% mutate(year = as.integer(format(as.Date(Период), "%Y")), 
                                            month = as.integer(format(as.Date(Период), "%m")))

names(media_holdings)[1:6] <- c("name", "cross_device_visitors", "users", "daily_audience", "avg_time", "date")

summary_holdings <- media_holdings %>% group_by(name, year) %>% summarise(yearly_audience = sum(cross_device_visitors),
                                                                          counts = n()) %>% filter(year == 2019)

summary_holdings$gov <- c(1, 0, 1, 1, 0, 0, 1, NA, 1, NA, 0, 0, 1, 1, 0, 0, 0, 0, 0, NA, 1)

summary_holdings_fin <- summary_holdings %>% arrange(gov, desc(yearly_audience)) %>% 
  filter(!(name %in% c()))

# stargazer(summary_holdings, type = "latex", summary = FALSE)

names(media_news_c) <- c("name", "url", "theme", "type", "holding", "metrics", "cross_device_visitors",
                         "users", "avg_time", "mobile_audience", "daily_audience", "date")

summary_media_news <- media_news_c %>% mutate(year = as.integer(format(as.Date(date), "%Y")), 
                                              month = as.integer(format(as.Date(date), "%m"))) %>%
                      group_by(year, name) %>% summarise(yearly_audience = sum(cross_device_visitors),
                                                            counts = n()) %>% filter(year == 2019) %>%
                      filter(counts == 12) %>% mutate(share = yearly_audience / sum(yearly_audience),
                                                      share_2 = share^2) %>% summarise(H = sum(share_2)*10000)

