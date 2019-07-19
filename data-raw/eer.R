## code to prepare `eer` dataset goes here

# Libraries
library(tidyverse)

# I use monthly effective exchange rate indices of 60 countries,
# which BIS publishes on https://www.bis.org/statistics/eer.htm.

httr::GET("https://www.bis.org/statistics/eer/broad.xlsx",
          httr::write_disk("data-raw/broad.xlsx"))

broad <- "data-raw/broad.xlsx"

res1 <- readxl::read_excel(broad, sheet = "Real", skip = 3)
reer <- readxl::read_excel(broad, sheet = "Real", skip = 4)
names(reer) <- names(res1)
names(reer)[1] <- "date"
reer$symbol <- "reer"

res2 <- readxl::read_excel(broad, sheet = "Nominal", skip = 3)
neer <- readxl::read_excel(broad, sheet = "Nominal", skip = 4)
names(neer) <- names(res2)
names(neer)[1] <- "date"
neer$symbol <- "neer"

eer <- bind_rows(reer, neer)

eer <- eer %>%
  gather(key = "area", value = "value", -date, -symbol) %>%
  spread(key = symbol, value = value) %>%
  mutate(deflator = neer / reer * 100) %>%
  gather(key = "symbol", value = "value", -date, -area) %>%
  spread(key = area, value = value) %>%
  arrange(symbol, date)

eer$date <- as.Date(eer$date)

usethis::use_data(eer)

