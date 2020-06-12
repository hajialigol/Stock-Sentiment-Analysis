library(twitteR)
library(base64enc)
library(dplyr)
library(stringr)
library(quantmod)
library(textclean)
library(tm)
library(syuzhet)
library(wordcloud)


# Industry Twitter handles and tickers ------------------------------------
energyTwitterHandles <- c('@exxonmobil', "@BP_plc", "@Chevron", "@conocophillips",
                          "@Total", "@MarathonPetroCo", "@Kinder_Morgan", "@ValeroEnergy", "@Phillips66Co", "@WeAreOxy")
energyTickers <- c("XOM", "BP", "CVX", "COP", "TOT", "MPC", "KMI", "VLO", "PSX", "OXY")

materialsTwitterHandles <- c("@Ecolab", "@SherwinWilliams", "@Intelligrated", "@BallCorpHQ", "@DowDuPontCo",
                            "@DowNewsroom", "@LyondellBasell", "@airproducts", "@NewmontGoldcorp", "@IntlPaperCo")
materialTickers <- c("ECL", "SHW", "HON", "BLL", "DD", "DOW", "LYB", "APD", "NEM", "IN")

industrialTwitterHandles <- c("@Boeing", "@LockheedMartin", "@3M", "@XylemInc", "@generalelectric",
                              "@CaterpillarInc", "@UnionPacific", "@Fiserv", "@JetBlue", "@UTC")
industrialTickers <- c("BA", "LMT", "MMM", "XYL", "GE", "CAT", "UNP", "FISV", "JBLU", "UTX")

consumerdiscretionTwitterHandles <- c("@amazon", "@HomeDepot", "@Disney", "@comcast", "@McDonalds", "@Nike", "@Starbucks", 
                                      "@BookingHoldings",  "@tjmaxx", "@Lowes")
consumerDiscTickers <- c("AMZN", "HD", "DIS", "CMCSA", "MCD", "NKE", "SBUX", "BKNG", "TJX", "LOW")

consumerStaplesTwitterHandles <- c("@Walmart", "@ProcterGamble", "@CocaCola", "@pepsi", "@InsidePMI",
                                   "@AltriaNews", "@Walgreens", "@EsteeLauder", "@KCCorp", "@Sysco")
consumerStaplesTickers <- c("WMT", "PG", "KO", "PEP", "PM", "MO", "WBA", "EL", "KMB", "SYY")

healthcareTwitterHandles <- c("@JNJCares", "@UnitedHealthGrp", "@Merck", "@pfizer", "@AbbottNews", 
                              "@Medtronic", "@Amgen", "@abbvie", "@thermofisher", "@LillyPad")
healthcareTickers <- c("JNJ", "UNH", "MRK", "PFE", "ABT", "MDT", "AMGN", "ABBV", "TMO", "LLY")

financeTwitterHandles <- c("@BHHSRealEstate", "@jpmorgan", "@BankofAmerica", "@WellsFargo", "@Citi", 
                           "@HSBC", "@usbank", "@FISGlobal", "@AmericanExpress", "@MorganStanley")
financialTickers <- c("BRK", "JPM", "BAC", "WFC", "C", "HSBC", "USB", "FIS", "AXP", "MS")

infoTechTwitterHandles <- c("@Apple", "@Microsoft", "@Google", "@facebook", "@Visa", "@Mastercard",
                            "@intel", "@Cisco", "@Oracle", "@Adobe")
techTickers <- c("AAPL", "MSFT", "GOOG", "FB", "V", "MA", "INTC", "CSCO", "ORCL", "ADBE")

teleCommTwitterHandles <- c("@ATT", "@verizon", "@TMobile", "@sprint", "@CenturyLink", "@IridiumComm", "@CogentCo",
                            "@USCellular", "@Vonage", "@bandwidth")
telecommTickers <- c("T", "VZ", "TMUS", "S", "CTL", "IRDM", "CCOI", "UZB", "VG", "BAND")

utilitiesTwitterHandles <- c("@nexteraenergy", "@DominionEnergy", "@SouthernCompany", "@DukeEnergy", 
                             "@AEPnews", "@Exelon", '@SempraEnergy', "@xcelenergy", "@PSEGNews", "@ConEdison")
utilitiesTickers <- c("NEE", "D", "SO", "DUK", "AEP", "EXC", "SRE", "XEL", "PEG", "ED")

realEstateTwitterHandles <- c("@SimonPropertyGp", "@Prologis", "@Weyerhaeuser", 
                              "@AvalonBay", "@PublicStorage", "@Welltower",
                              "@RealtyIncome", "@Equinox", "@EquityRes", "@digitalrealty")
realEstateTickers <- c("SPG", "PLD", "WY", "AVB", "PSA", "WELL", "O", "EQIX", "EQR", "DLR")



# Get stock data ----------------------------------------------------------
getData <- function(tickers){
  stockEnv <- new.env()
  getSymbols(tickers, env = stockEnv)
  
  stockOpen <- eapply(stockEnv, Op)
  openDf <- do.call(merge, stockOpen)
  
  stockClose <- eapply(stockEnv, Cl)
  closeDf <- do.call(merge, stockClose)
  
  mergeDf <- cbind(openDf, closeDf)
  
  return(mergeDf)
}


# Load stock data into dataframes -----------------------------------------
energyStockDf <- getData(energyTickers)
materialsDf <- getData(materialTickers)
industrialDf <- getData(industrialTickers)
consumerDiscDf <- getData(consumerDiscTickers)
consumerStaplesDf <- getData(consumerStaplesTickers)
healthcareDf <- getData(healthcareTickers)
financialsDf <- getData(financialTickers)
techDf <- getData(techTickers)
telecommDf <- getData(telecommTickers)
utilitiesDf <- getData(utilitiesTickers)
realEstateDf <- getData(realEstateTickers)


#Function that returns the difference of current days open with the previous day's closing price.
getStockData <- function(companyTicker, dayInteger, companyDayDF, industryStockDf){
  open <- paste0(companyTicker, ".Open")
  close <- paste0(companyTicker, ".Close")
  if (dayInteger == 18){
    previousHigh <- as.character(15)
  }
  else{
    previousHigh <- as.character(dayInteger - 1)
  }
  day <- as.character(dayInteger)
  closeDate <- paste0("2019-11-", previousHigh)
  openDate <- paste0("2019-11-", day)
  closingValue <- as.numeric(industryStockDf[, close][closeDate])
  openingValue <- as.numeric(industryStockDf[, open][openDate])
  percentChange <- 100 * ((openingValue - closingValue) / closingValue)
  companyDayDF["AfterHourDif"] <- rep(percentChange, nrow(companyDayDF))
  return (companyDayDF)
}


# Returns the tweets for each company
findTweetsForCompanies <- function(handles, industryTickers, industryStockDf){
  handlesSector <- data.frame()
  i = 1
  for (handle in handles){
    companyDFMon <- data.frame()
    companyDFTues <- data.frame()
    companyDFWed <- data.frame()
    companyDFTh <- data.frame()
    copmanyDFFr <- data.frame()
    
    companyMon <- searchTwitter(handle, n = 200, since = '2019-11-17', until = '2019-11-18',
                                resultType = 'mixed', lang = "en", retryOnRateLimit = 1)
    if (length(companyMon) > 0){
    companyDFMon <- twListToDF(companyMon)
    companyDFMon["Company"] <- str_sub(handle, 2, -1)
    print(industryTickers[i])
    companyDFMon <- getStockData(industryTickers[i], 18, companyDFMon, industryStockDf)
    }
    
    companyTues <- searchTwitter(handle, n = 200, since = '2019-11-18', until = '2019-11-19',
                                 resultType = 'mixed', lang = "en", retryOnRateLimit = 1)
    if (length(companyTues) > 0){
    companyDFTues <- twListToDF(companyTues)
    companyDFTues["Company"] <- str_sub(handle, 2, -1)
    companyDFTues <- getStockData(industryTickers[i], 19, companyDFTues, industryStockDf)
    }
    
    companyWed <- searchTwitter(handle, n = 200, since = '2019-11-19', until = '2019-11-20',
                                resultType = "mixed", lang = "en", retryOnRateLimit = 1)
    if (length(companyWed) > 0){
    companyDFWed <- twListToDF(companyWed)
    companyDFWed["Company"] <- str_sub(handle, 2, -1)
    companyDFWed <- getStockData(industryTickers[i], 20, companyDFWed, industryStockDf)
    }
  
    companyTh <- searchTwitter(handle, n = 200, since = '2019-11-20', until = '2019-11-21',
                               resultType = 'mixed', lang = "en", retryOnRateLimit = 1)
    if (length(companyTh) > 0){
    companyDFTh <- twListToDF(companyTh)
    companyDFTh["Company"] <- str_sub(handle, 2, -1)
    companyDFTh <- getStockData(industryTickers[i], 21, companyDFTh, industryStockDf)
    }
    
    companyFr <- searchTwitter(handle, n = 200, since = '2019-11-21', until = '2019-11-22',
                               resultType = 'mixed', lang = "en", retryOnRateLimit = 1)
    if (length(companyFr) > 0){
    companyDFFr <- twListToDF(companyFr)
    companyDFFr["Company"] <- str_sub(handle, 2, -1)
    companyDFFr <- getStockData(industryTickers[i], 22, companyDFFr, industryStockDf)
    }
    handlesSector <- rbind(handlesSector, companyDFMon, companyDFTues, companyDFWed, companyDFTh, companyDFFr)
    i = i + 1
  }
  handlesSector <- handlesSector %>%
    filter(isRetweet == FALSE)
  return (handlesSector)
}


# Loads tweets for each company
energy <- findTweetsForCompanies(energyTwitterHandles, energyTickers, energyStockDf)
materials <- findTweetsForCompanies(materialsTwitterHandles, materialTickers, materialsDf)
industrial <- findTweetsForCompanies(industrialTwitterHandles, industrialTickers, industrialDf)
consumerDiscretion <- findTweetsForCompanies(consumerdiscretionTwitterHandles, consumerDiscTickers, consumerDiscDf)
consumerStaples <- findTweetsForCompanies(consumerStaplesTwitterHandles, consumerStaplesTickers, consumerStaplesDf)


# Natural Language Processing
cleanTweets <- function(industryTweetsDf){
  industryCorpus <- Corpus(VectorSource(industryTweetsDf$text))
  industryTweetsDf$text <- gsub("http[^[:space:]]*", "", industryTweetsDf$text)
  industryTweetsDf$text <- gsub("@[[:alnum:]]*", "", industryTweetsDf$text)
  industryTweetsDf$text <- gsub("  ", " ", industryTweetsDf$text)
  return (industryTweetsDf)
}


# Cleaning dataframes and getting sentiment -------------------------------
gr <- cleanTweets(energy)
s <- gsub("@[[:alnum:]]*", "", energy$text[2])
ind <- Corpus(VectorSource(energy$text))
d <- tm_map(ind, removeWords, stopwords())
industryDoc <- DocumentTermMatrix(d)
wordcloud(ind, min.freq = 15)
sentiment <- get_nrc_sentiment(ind$content)
barplot(colSums(sentiment), col = rainbow(10))
