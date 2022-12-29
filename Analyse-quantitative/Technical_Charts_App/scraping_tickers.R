############################## Web Scraping ##################################
library(tidyverse)
library(rvest)
# Lecture de la page web 

url_sp500 <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies" # USA
url_cac40 <- "https://en.wikipedia.org/wiki/CAC_40" # Paris
url_ftse100 <- "https://en.wikipedia.org/wiki/FTSE_100_Index" # Londres
url_nikkei <- "https://topforeignstocks.com/indices/the-components-of-the-nikkei-225-index/" # Tokyo
url_dax <- "https://en.wikipedia.org/wiki/DAX"

# DAX 

dax_html <- read_html(url_dax) 

tables_dax <- html_table(dax_html, fill = TRUE)

dax_raw <- tables_dax[[5]]

dax <- dax_raw %>%
  select(Ticker, Company)

dax_ticker_name <- dax %>%
  select(Company, Ticker) %>%
  mutate(NameOfStock = paste(Company, Ticker, sep=","))

write.csv(
  x = dax_ticker_name, 
  file = "D:/Web Apps and Dashboards/R Shiny/Portfolio Shiny Apps/Analyse quantitative/technical_Charts_App/dax_tickers_names.csv", 
  row.names = FALSE
)


# Nikkei

nikkei_html <- read_html(url_nikkei) 

tables_nikkei <- html_table(nikkei_html, fill = TRUE)

nikkei_raw <- tables_nikkei[[1]]

nikkei <- nikkei_raw 

nikkei_ticker_name <- nikkei %>%
  select(`Company Name`, Code) %>%
  mutate(`Company Name` = str_remove(`Company Name`, pattern = ","),
         pointT = ".T", 
         NameOfStock = paste(`Company Name`, Code, sep=",")) %>%
  mutate(NameOfStock = paste(NameOfStock, pointT, sep=""))

write.csv(
  x = nikkei_ticker_name, 
  file = "D:/Web Apps and Dashboards/R Shiny/Portfolio Shiny Apps/Analyse quantitative/technical_Charts_App/nikkei_tickers_names.csv", 
  row.names = FALSE
)

# S&P500

sp500_html <- read_html(url_sp500) 

tables <- html_table(sp500_html, fill = TRUE)

sp_500_raw <- tables[[1]]

sp_500 <- sp_500_raw %>%
  select(Symbol, Security)

sp500_ticker_name <- sp_500 %>%
  select(Security, Symbol) %>%
  mutate(NameOfStock = paste(Security, Symbol, sep=","))

write.csv(
  x = sp500_ticker_name, 
  file = "D:/Web Apps and Dashboards/R Shiny/Portfolio Shiny Apps/Analyse quantitative/technical_Charts_App/sp500_tickers_names.csv", 
  row.names = FALSE
)


# CAC40

cac40_html <- read_html(url_cac40) 

tables_cac40 <- html_table(cac40_html, fill = TRUE)

cac40_raw <- tables_cac40[[5]]

cac40 <- cac40_raw %>%
  select(Ticker, Company)

cac40_ticker_name <- cac40 %>%
  select(Company, Ticker) %>%
  mutate(NameOfStock = paste(Company, Ticker, sep=","))

write.csv(
  x = cac40_ticker_name, 
  file = "D:/Web Apps and Dashboards/R Shiny/Portfolio Shiny Apps/Analyse quantitative/technical_Charts_App/cac40_tickers_names.csv", 
  row.names = FALSE
)


# FTSE100

ftse100_html <- read_html(url_ftse100) 

tables_ftse100 <- html_table(ftse100_html, fill = TRUE)

ftse100_raw <- tables_ftse100[[5]]

ftse100 <- ftse100_raw # on prend les 3 colonnes

ftse100_ticker_name <- ftse100 %>%
  select(Company, EPIC) %>%
  mutate(NameOfStock = paste(Company, EPIC, sep=","))

write.csv(
  x = ftse100_ticker_name, 
  file = "D:/Web Apps and Dashboards/R Shiny/Portfolio Shiny Apps/Analyse quantitative/technical_Charts_App/ftse100_tickers_names.csv", 
  row.names = FALSE
)


##############################################################################