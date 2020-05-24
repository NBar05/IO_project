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


