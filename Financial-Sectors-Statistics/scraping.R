library(tidyverse)
library(rvest)

# Web Scraping France
url_cac40 <- "https://en.wikipedia.org/wiki/CAC_40"
cac40_html <- read_html(url_cac40)
tables_cac <- html_table(cac40_html, fill = TRUE)
cac40 <- tables_cac[[5]]
cac40 <- cac40 %>%
  rename(`Sub-Sector` = `GICS Sub-Industry`, Symbol = Ticker)
# Je classe les lignes de la dataframe selon 
#l'ordre alphabétique des valeurs de la colonnee Symbol
cac40 <- cac40[order(cac40$Symbol), ]
write.csv(cac40, "cac40.csv", row.names = FALSE)

# Web Scraping Allemagne
url_dax <- "https://en.wikipedia.org/wiki/DAX"
dax_html <- read_html(url_dax)
tables_dax <- html_table(dax_html, fill = TRUE)
dax <- tables_dax[[5]]
dax <- dax %>%
  select(Company, Ticker, `Prime Standard Sector`) %>%
  rename(Symbol = Ticker, Sector = `Prime Standard Sector`)
# Je classe les lignes de la dataframe selon 
#l'ordre alphabétique des valeurs de la colonnee Symbol
dax <- dax[order(dax$Symbol), ]
write.csv(dax, "dax.csv", row.names = FALSE)


# Web Scraping Angleterre
url_ftse <- "https://en.wikipedia.org/wiki/FTSE_100_Index"
ftse_html <- read_html(url_ftse)
tables_ftse <- html_table(ftse_html, fill = TRUE)
ftse <- tables_ftse[[5]]
ftse <- ftse %>%
  mutate(EPIC = paste(EPIC, ".L", sep = "")) %>%
  rename(Symbol = EPIC, 
         Sector = `FTSE Industry Classification Benchmark sector[15]`)
# Je classe les lignes de la dataframe selon 
#l'ordre alphabétique des valeurs de la colonnee Symbol
ftse <- ftse[order(ftse$Symbol), ]
write.csv(ftse, "ftse.csv", row.names = FALSE)