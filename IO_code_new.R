library(zoo)
library(dplyr)
library(tidyr)
library(psych)
library(readxl)
library(writexl)
library(ggplot2)
library(stargazer)

# it's a kind of magic, skip it
`%--%` <- function(x, y) {
  do.call(sprintf, c(list(x), y))
}

# задаём временной промежуток вытягиваемых данных
dates <- as.yearmon(2019 + seq(0, 11) / 12) # январь 2019 -- декабрь 2019

# читаем файлы и хоба в таблицу
naming <- "full_media/news-%s.xlsx" %--% list(c(1:12))

media_news <- data.frame()
for (i in 1:length(naming)) { 
  data <- read_xlsx(naming[i], skip = 5, col_names = TRUE)
  data$Период <- dates[i]
  media_news <- rbind(media_news, data)
}

# переименовываем
names(media_news) <- c("name", "url", "theme", "type", "holding", "metrics", "cross_device_visitors",
                         "users", "avg_time", "mobile_audience", "daily_audience", "date")

# не будем искать врагов там, где их нет
media_news <- media_news %>%
  filter(theme != "Наука, Наука->Научные журналы и публикации, Новости и СМИ->Научные журналы и публикации, Новости и СМИ") %>%
  filter(theme != "Новости и СМИ, Новости и СМИ->Новости технологий") %>%
  filter(theme != "Авто и мото->Издания об автомобилях, Авто и мото, Новости и СМИ->Издания об автомобилях, Новости и СМИ") %>%
  filter(theme != "Техника и технологии, Новости и СМИ->Деловые новости, Техника и технологии->Новости Hi-Tech, Бизнес, Новости и СМИ, Бизнес->Деловые новости") %>%
  filter(theme != "Развлечения и отдых, Новости и СМИ, Развлечения и отдых->Афиша и продажа билетов") %>%
  filter(theme != "Наука, Техника и технологии, Наука->Научные журналы и публикации, Новости и СМИ->Научные журналы и публикации, Техника и технологии->Новости Hi-Tech, Новости и СМИ")

media_news <- data.frame(media_news)
media_news$holding <- ifelse(media_news$holding == "РИА", "МИА «Россия сегодня»", media_news$holding)

media_news %>% group_by(holding) %>% summarise(s = round(sum(cross_device_visitors) / 10^6)) %>%
  data.frame() %>% arrange(desc(s)) %>% select("Холдинг" = holding, "Годовая аудитория (в млн. человек)" = s) %>% 
  stargazer(summary = FALSE, type = "latex")

# череда преобразований
summary_media_news <- media_news %>% mutate(year = as.integer(format(as.Date(date), "%Y")),
                                            month = as.integer(format(as.Date(date), "%m"))) %>% group_by(name) %>% 
  summarise(yearly_audience = sum(cross_device_visitors), counts = n()) %>% filter(counts %in% 6:12) %>%
  mutate(year_aud = yearly_audience * 12 / counts, share = year_aud / sum(year_aud)) %>% 
  left_join(unique(media_news[, c("name", "holding")]), by = "name") %>% 
  select(name, year_aud, share, holding) %>% mutate(holding_or_site = ifelse(is.na(holding), name, holding))

# объединяем доли по холдигнам, если они есть
summary_holdings_and_sites <- summary_media_news %>% group_by(holding_or_site) %>% summarise(year_aud = sum(year_aud),
                                                                                             share = sum(share))
# топ 20
table_1 <- summary_holdings_and_sites %>% mutate(share = round(share*100, digits = 4)) %>% arrange(desc(share)) %>%
  select("Холдинг / Сайт" = holding_or_site, "Доля рынка" = share) %>% head(20)

stargazer(table_1, summary = FALSE, type = "latex")

# индекс Герфиндаля
H_1 <- summary_holdings_and_sites %>% mutate(share_2 = share^2) %>% summarise(H = sum(share_2)*10000)

# Этот кусочек нужен для подготовки таблицы с гос и проправительст. компаниями
#table <- summary_holdings_and_sites %>% arrange(desc(share))
#table %>% mutate(fed = NA, progov = NA) %>% select(holding_or_site, fed, progov) %>%
#  head(100) %>% write_xlsx(path = "dependence.xlsx")

# открываем созданную вручную таблицу
status <- read_xlsx("dependence.xlsx")

fed_or_gov <- summary_holdings_and_sites %>% left_join(status, by = "holding_or_site") %>%
  mutate(fed_or_ind = ifelse(fed == 1, "Государственные", holding_or_site),
         fed_or_ind = ifelse(is.na(fed_or_ind), holding_or_site, fed_or_ind),
         gov_or_ind = ifelse(progov == 1, "Проправительственные", holding_or_site),
         gov_or_ind = ifelse(is.na(gov_or_ind), holding_or_site, gov_or_ind))

# сгруппируем по государственным компаниям
table_2 <- fed_or_gov %>% group_by(fed_or_ind) %>% summarise(share = sum(share) * 100) %>% arrange(desc(share)) %>% head(20)

H_2 <- table_2 %>% mutate(share_2 = share^2) %>% summarise(H = sum(share_2))

table_2 %>% mutate(share = round(share, digits = 4)) %>% data.frame() %>%
  select("Источники информации" = fed_or_ind, "Доля рынка" = share) %>% stargazer(summary = FALSE, type = "latex")

# сгруппируем по проправительственным компаниям
table_3 <- fed_or_gov %>% group_by(gov_or_ind) %>% summarise(share = sum(share) * 100) %>% arrange(desc(share)) %>% head(20)

H_3 <- table_3 %>% mutate(share_2 = share^2) %>% summarise(H = sum(share_2))

table_3 %>% mutate(share = round(share, digits = 4)) %>% data.frame() %>%
  select("Источники информации" = gov_or_ind, "Доля рынка" = share) %>% stargazer(summary = FALSE, type = "latex")

