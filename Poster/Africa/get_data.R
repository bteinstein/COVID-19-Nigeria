library("fs")
library("wbstats")
library(tidyverse)

# source("Summary Board/utils.R", local = T)

# Function to download timeseries data on covid-19 from github page of John Hopkins CSSEGIS
downloadGithubData <- function() {
  base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  
  download.file(url = paste0(base_url,"confirmed_global.csv"), destfile = "./data/confirmed_global.csv")
  download.file(url = paste0(base_url,"deaths_global.csv"), destfile = "./data/deaths_global.csv")
  download.file(url = paste0(base_url,"recovered_global.csv"), destfile = "./data/recovered_global.csv")
  
}

# Download data from Johns Hopkins (https://github.com/CSSEGISandData/COVID-19) if the data is older than 12h
updateData <- function() {
  h = 12
  if (!dir_exists("data")) {
    dir.create('data')
    downloadGithubData()
  } else if ((!file.exists("data/world/confirmed_global.csv")) || 
             (as.double(Sys.time() - file_info("data/confirmed_global.csv")$change_time, units = "hours") > h)
             ||
             (!file.exists("data/world/deaths_global.csv")) || 
             (as.double(Sys.time() - file_info("data/deaths_global.csv")$change_time, units = "hours") > h)
             ||
             (!file.exists("data/world/recovered_global.csv")) || 
             (as.double(Sys.time() - file_info("data/recovered_global.csv")$change_time, units = "hours") > h)
  )
  {
    downloadGithubData()
  }
  message("Data up to date")
}


# Update with start of app
updateData()

# TODO: Still throws a warning but works for now
data_confirmed    <- readr::read_csv("data/world/confirmed_global.csv")
data_deceased     <- readr::read_csv("data/world/deaths_global.csv")
data_recovered    <- readr::read_csv("data/world/recovered_global.csv")



# Get evolution data by country
data_confirmed_lng <- data_confirmed %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_confirmed)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("confirmed" = sum(value, na.rm = T))

data_recovered_lng <- data_recovered %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_recovered)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("recovered" = sum(value, na.rm = T))

data_deceased_lng <- data_deceased %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_deceased)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("deceased" = sum(value, na.rm = T))


# Data Evolution
data_evolution <- data_confirmed_lng %>%
  full_join(data_recovered_lng) %>%
  full_join(data_deceased_lng) %>% 
  ungroup() %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  arrange(date) %>%
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  fill(confirmed, recovered, deceased) %>%
  replace_na(list(deceased = 0, confirmed = 0)) %>%
  mutate(
    recovered_est = lag(confirmed, 14, default = 0) - deceased,
    recovered_est = ifelse(recovered_est > 0, recovered_est, 0),
    recovered     = coalesce(recovered, recovered_est),
    active        = confirmed - recovered - deceased
  ) %>%
  select(-recovered_est) %>% 
  # filter(!(Lat == 0 & Long == 0)) %>%
  ungroup()

rm(data_confirmed, data_confirmed_lng, data_recovered, data_recovered_lng, data_deceased, data_deceased_lng)


## ---- Download population data ----
# Country info from world bank
# wbcntr <- wbstats::wbcountries() %>% select(country,iso2c, long, lat, income, region)
# 
# population <- wb(country = "countries_only", indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2020) %>%
#   select(country, value) %>%
#   rename(population = value)
# 
# 
# wbcntr <- wbcntr %>% left_join(population)
# readr::write_csv(x = wbcntr, path = "data/utils/wbcntr.csv")


wbcntr <- readr::read_csv("data/utils/wbcntr.csv")

data_evolution$`Country/Region`[data_evolution$`Country/Region` == "Taiwan*"] <- "Taiwan" 

# Quick Check on available country information
# quick function 
`%nin%` = Negate(`%in%`)
sum(unique(data_evolution$`Country/Region`) %nin% wbcntr$country) # 24 Names
# list the countries
unique(data_evolution$`Country/Region`)[which(unique(data_evolution$`Country/Region`) %nin% wbcntr$country)]


countryNamesWbcntr = c("Bahamas, The", "Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.", 
                       "Czech Republic", "Egypt, Arab Rep.", "Gambia, The", "Iran, Islamic Rep.", 
                       "Korea, Rep.", "Kyrgyz Republic", "Lao PDR", "Myanmar", 
                       "Russian Federation", "Slovak Republic", "St. Kitts and Nevis", "St. Lucia", 
                       "St. Vincent and the Grenadines", "Syrian Arab Republic", "Taiwan, China", 
                       "United States", "Venezuela, RB", "Yemen, Rep.")


countryNamesDat = c("Bahamas", "Brunei", "Congo (Kinshasa)", "Congo (Brazzaville)", 
                    "Czechia", "Egypt", "Gambia", "Iran", 
                    "Korea, South", "Kyrgyzstan", "Laos", "Burma", 
                    "Russia", "Slovakia", "Saint Kitts and Nevis", "Saint Lucia", 
                    "Saint Vincent and the Grenadines", "Syria", "Taiwan", 
                    "US", "Venezuela", "Yemen")

ind_country = which(wbcntr$country %in% countryNamesWbcntr)
wbcntr$country[ind_country] <- countryNamesDat[match(wbcntr$country[ind_country],countryNamesWbcntr)]

# Errata Fix - No wiki data
# https://www.nature.com/articles/d41586-020-00885-w
# https://en.wikipedia.org/wiki/MS_Zaandam
# https://tools.wmflabs.org/geohack/geohack.php?pagename=Western_Sahara&params=25_N_13_W_type:country
noDataCountries <- tibble(
  country = c("Holy See", "Western Sahara", "MS Zaandam","Diamond Princess" ),
  iso2c = c('VA',"EH","Cruise ship","Cruise ship"),
  long = c(12.4534,-12.8858,NA,NA),
  lat = c(41.9029,24.2155,NA,NA),
  income = c(NA,NA,NA,NA),
  region = c(NA,NA,NA,NA),
  population = c(618,567402,1729,3711)
)

wbcntr <- bind_rows(wbcntr, noDataCountries)




data_covid <- data_evolution %>%
  left_join(wbcntr, by = c("Country/Region" = "country"))



rm(data_evolution,countryNamesWbcntr, countryNamesDat, noDataCountries, wbcntr, ind_country)
readr::write_csv(x = data_covid, path = "data/world/data_covid.csv")

rm(`%nin%`, downloadGithubData,updateData)
