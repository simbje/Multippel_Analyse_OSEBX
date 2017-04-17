




## Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(tidyverse)
library(xlsx)

# Web Scraping: Get the List of S&P500 Stocks ----------------------------------
#Scarping

OSE<- read_html("https://en.wikipedia.org/wiki/List_of_companies_listed_on_the_Oslo_Stock_Exchange") %>%
  html_node(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table()
# Format Navn
names(OSE) <- OSE %>%
  names() %>%
  str_to_lower() %>%
  make.names()
names(OSE)

#### Stuff
OSE<-tbl_df(OSE)
OSE<-OSE %>%
  rename(gics.industry = gics.industry.5.) %>%
  select(company,ticker,gics.industry) %>%
  arrange(gics.industry)

OSE %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist() 

OSE %>%
  group_by(ticker) %>%
  summarize(count = n()) %>%
  filter(count > 1)
OSE <- OSE %>% 
  filter(ticker != "OSE: SCH", ticker !="OSE: SAS+NOK", ticker != "OSE: ATLA+NOK.OL",
        company != "Arcus", ticker != "OSE: SOLON", ticker != "OSE: KOG")#ticker != "OSE: NAS")

OSE$ticker <- gsub("OSE: ","OSE:", OSE$ticker)
OSE$ticker <- gsub("OSE:SAS+NOK","OSE:SAS", OSE$ticker)
OSE$ticker <- gsub("OSE:ATLA+NOK","OSE:ATLA", OSE$ticker)

#ticker <- OSE$ticker
OSE$ticker <- gsub("OSE:","",OSE$ticker)
OSE$ticker <- paste(OSE$ticker,".OL", sep = "")
ticker <- OSE$ticker
#last ned data 
#Aksjer<-tryCatch(getSymbols(Symbols = ticker, from = "2014-01-01"), error=function(e) NULL)



#for(i in 1:length(ticker))# {
  #Aksjer<-tryCatch(getSymbols(Symbols = ticker, from = "2014-01-01"), error=function(e) NULL) }



what_metrics <- yahooQF(c("Price/Sales", 
                          "P/E Ratio",
                          "Price/Book"))
#mult <-for(i in 1:length(ticker)){
  # tryCatch(getQuote(paste(ticker, sep =".", collapse=";"), what=what_metrics),error=function(e) NULL)
#}

mult                          
                          
                          
                          
mult <- getQuote(paste(ticker, sep =".", collapse=";"), what=what_metrics)

    View(mult)
     mult <- mult[-c(1)]
     mult <- tbl_df(mult)
    # mult <- mult %>%
       #mutate(ticker = rownames(mult)) %>%
       #m
       
     mult<-mutate(mult, ticker = rownames(mult))
     tickerM <- mult$ticker
     mult <- lapply(mult[c(1:3)],as.numeric)
     mult <- data.frame(mult,tickerM)
     #mult <- rename(mult,ticker = OSE.ticker)
  
    
OSE<-OSE %>%
    left_join(mult, by = "ticker")  
OSE <- arrange(OSE, gics.industry)

test<- cbind(OSE,mult)

MeanMultSector<- OSE %>%
  na.omit() %>%
  group_by(gics.industry) %>%
  summarise(meanPE = mean("P/E Ratio"), MeanPS = mean("Price/Sales"), MeanPB = mean("Price/Book"))


MeanMultSector
names(OSE)
    
underVAL <- sp_500 %>%
  na.omit() %>%
  group_by(gics.sub.industry) %>%
  select(company,gics.sub.industry,ticker.symbol,P.E.Ratio,Price.Book)%>%
  filter(P.E.Ratio < mean(P.E.Ratio), Price.Book < mean(Price.Book))                      
                          
#excel

excel <- data.frame(OSE)

write.xlsx(excel, file = "C:\\Users\\Simen\\Desktop\\M.xlsx", sheetName = "M")
                        
                        
                          
#Fancy shit funker ikke helt
get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
  # Rename
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    stock_prices <- stock_prices_xts
  }
  stock_prices
}

get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
  # Convert tibble to xts
  if (!is.xts(x)) {
    x <- xts(x[,-1], order.by = x$Date)
  }
  # Get stock prices
  log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
  # Rename
  names(log_returns_xts) <- "Log.Returns"
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    log_returns <- log_returns_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    log_returns <- log_returns_xts
  }
  log_returns
}


# Mapping the Functions --------------------------------------------------------
from <- "2014-01-01"
to   <- today()
OSE <- OSE %>%
  mutate(
    stock.prices = map(ticker,
                       function(.x) get_stock_prices(.x,
                                                     return_format = "tibble",
                                                     from = from,
                                                     to   = to)
    ),
    log.returns  = map(stock.prices,
                       function(.x) get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )


