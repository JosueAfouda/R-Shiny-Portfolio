library(tidyquant)
library(tidyverse)
library(plotly)
library(scales)
library(rvest)
library(quantmod)
library(stringr)
library(psych)

# Web Scraping USA
url_sp500 <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
sp500_html <- read_html(url_sp500)
tables <- html_table(sp500_html, fill = TRUE)
sp500 <- tables[[1]]
sp500 <- sp500 %>%
  select(Security, Symbol, `GICS Sector`, `GICS Sub-Industry`) %>%
  rename(Company = Security,
         Sector = `GICS Sector`, 
         `Sub-Sector` = `GICS Sub-Industry`)
# Je classe les lignes de la dataframe selon 
  #l'ordre alphabétique des valeurs de la colonnee Symbol
sp500 <- sp500[order(sp500$Symbol), ]

# Web Scraping France
url_cac40 <- "https://en.wikipedia.org/wiki/CAC_40"
cac40_html <- read_html(url_cac40)
tables_cac <- html_table(cac40_html, fill = TRUE)
cac40 <- tables_cac[[4]]
cac40 <- cac40 %>%
  rename(`Sub-Sector` = `GICS Sub-Industry`, Symbol = Ticker)
# Je classe les lignes de la dataframe selon 
#l'ordre alphabétique des valeurs de la colonnee Symbol
cac40 <- cac40[order(cac40$Symbol), ]

# Web Scraping Allemagne
url_dax <- "https://en.wikipedia.org/wiki/DAX"
dax_html <- read_html(url_dax)
tables_dax <- html_table(dax_html, fill = TRUE)
dax <- tables_dax[[4]]
dax <- dax %>%
  select(Company, `Ticker symbol`, `Prime Standard Sector`) %>%
  rename(Symbol = `Ticker symbol`, Sector = `Prime Standard Sector`)
# Je classe les lignes de la dataframe selon 
#l'ordre alphabétique des valeurs de la colonnee Symbol
dax <- dax[order(dax$Symbol), ]


# Web Scraping Angleterre
url_ftse <- "https://en.wikipedia.org/wiki/FTSE_100_Index"
ftse_html <- read_html(url_ftse)
tables_ftse <- html_table(ftse_html, fill = TRUE)
ftse <- tables_ftse[[4]]
ftse <- ftse %>%
  mutate(EPIC = paste(EPIC, ".L", sep = "")) %>%
  rename(Symbol = EPIC, 
         Sector = `FTSE Industry Classification Benchmark sector[14]`)
# Je classe les lignes de la dataframe selon 
#l'ordre alphabétique des valeurs de la colonnee Symbol
ftse <- ftse[order(ftse$Symbol), ]

# Web Scraping Japon
url_nikkei <- "https://topforeignstocks.com/indices/the-components-of-the-nikkei-225-index/"
nikkei_html <- read_html(url_nikkei)
tables_nikkei <- html_table(nikkei_html, fill = TRUE)
nikkei <- tables_nikkei[[1]]
nikkei <- nikkei %>%
  select(`Company Name`, Code, Industry) %>%
  rename(Company = `Company Name`, Symbol = Code, Sector = Industry)
# Je classe les lignes de la dataframe selon 
#l'ordre alphabétique des valeurs de la colonnee Symbol
nikkei <- nikkei[order(nikkei$Symbol), ]

# Statistic'names vector
stats_names <- c("Mean", "Standard Deviation", "Skewness", "Kurtosis")



# Prix de cloture (exemple : cac40)
stocks <- new.env()
getSymbols(
  cac40$Symbol, 
  env = stocks,
  from = as.Date("2015-01-01"), 
  to = as.Date("2022-01-01")
)
cac40_daily_close_price <- do.call(merge, lapply(stocks, Cl))
cac40_daily_close_price <- cac40_daily_close_price[,order(colnames(cac40_daily_close_price))]

# Rendements journaliers de chaque société du cac40 sur une période donnée
cac40_stocks_returns <- Return.calculate(cac40_daily_close_price)[-1]

# Moyenne
cac40_returns_mean <- colMeans(cac40_stocks_returns)

cac40$Statistic <- cac40_returns_mean