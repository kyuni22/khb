# required libraries
require(quantmod)
require(blotter)
#Load required functions
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

#### 1. Data Load (from BBG)
require("Rbbg")
bbg.conn <- blpConnect() # Connect to Bloomberg
symbols <- c("ES1_Index") # list of symbols
end.date <- Sys.Date()
start.date <- end.date - 365*5 # Setting Days
bbg.field <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST")
field <- c("Open","High","Low","Close") # Field Setting
.symbolEnv <- new.env()

currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
  assign(symbol, getBDH(bbg.conn, gsub("_"," ",symbol), bbg.field, start.date, 
                        end.date, fields_names=field), envir=.symbolEnv)
}

#### 2. Signal
for(symbol in symbols) {
  tmp <- EMA(Cl(.symbolEnv[[symbol]]), n=20)
  signal <- ifelse(tmp>lag(tmp),1,-1)
  names(signal) <- "signal"
  .symbolEnv[[symbol]] <- cbind(.symbolEnv[[symbol]], signal)
}

#### 3. Blotter
# initalize blotter
initDate <- start.date
initEq <- 0
{
  # Try to clean up in case the demo was run previously
  try(rm(list=ls(envir=.blotter),envir=.blotter))
  
  #init envir
  .blotter <- new.env()
  .instrument <- new.env()
  Sys.setenv(environment=".blotter", TZ="UTC")
  Sys.setenv(environment=".instrument", TZ="UTC")
  Sys.setenv(TZ="UTC")
  
  # Set up a portfolio object and an account object
  portfolio = "p"
  initPortf(name=portfolio,c(symbols),initDate=initDate)
  account = "acct"
  initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
}

# Other Properties to Setup
verbose=TRUE # TRUE if you want to see transaction 
b_symbol <- symbols[1] # Set benchmark symbol
start_i <- 1600 #Set 760 if you want to see after Dec 2008

# Create trades
for( i in start_i:NROW(get(b_symbol)) ) { # Assumes all dates are the same
  CurrentDate=time(get(b_symbol))[i] # time setting with benchmark symbol
  equity = getEndEq(account, CurrentDate)
  
  for(symbol in symbols) {
    x = get(symbol, envir=.symbolEnv)
    ClosePrice = as.numeric(Cl(x[i,]))
    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    UnitSize = 1 # Contracts
    
    ### Path dependent signals

    ### End of Path dependent signals
    
    # Position Entry (assume fill at close, so account for slippage)
    if( Posn == 0 ) { 
      # Initiate Long position
      if( signal > 0 ) { 
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize, TxnFees=0, verbose=verbose)
      } 
      else if ( signal < 0 ) { 
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -UnitSize, TxnFees=0, verbose=verbose)
      }
    } else {   
      # Have position check exit
      if( signal == 0 ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn, TxnFees=0, verbose=verbose)
      }
      else if( (Posn > 0) && ( signal < 0 ) ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn - UnitSize, TxnFees=0, verbose=verbose)
      } 
      else if( (Posn < 0) && ( signal > 0 ) ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn + UnitSize, TxnFees=0, verbose=verbose)
      }
    } #else
    # Maintain Position
  } # End symbol loop
  # Now that we've updated all of our trades, its time to mark the book
  updatePortf(Portfolio = portfolio, Dates = CurrentDate)
  updateAcct(account, Dates = CurrentDate)
  updateEndEq(account, Dates = CurrentDate)
} # End dates loop

####################################################################################################
# Final values
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')

initPar <- par()

# Charting individual Position
if (require(quantmod)) {
  for(symbol in symbols){
    chart.Posn(Portfolio=portfolio,Symbol=symbol) #chart.Posn(Portfolio=portfolio,Symbol=symbol, Dates="2012::")
  }
}
chart.Posn(Portfolio=portfolio,Symbol=symbol, Dates="2013::")

# Bench Mark Return - Buy and Hold with Same initEq
## Lets change this latter
BM_return <- ROC(vix)#ROC(Cl(SPVXSP.Index))
BM_return[1] <- 0

## Write and read
write.zoo(PortfReturns(account),"./data/pf_return.csv",sep=",")
pf_return <-  as.xts(read.zoo("./data/pf_return.csv", header=TRUE, sep=",", tz="" ))

# Simulation Return
returns <- cbind(pf_return,BM_return)
names(returns) <- c("sim","BuyAndHold")

#####!!!! Need to be revised
returns.month <- cbind(ROC(Cl(to.monthly(Sim_return, indexAt='lastof')), type="discrete"),ROC(Cl(to.monthly(BM_return, indexAt='lastof')), type="discrete"))
names(returns.month) <- c("sim","BuyAndHold")

# Charting 
if(require(PerformanceAnalytics)){
  charts.PerformanceSummary(returns,main="Performance", geometric=FALSE, ylog=FALSE, wealth.index=TRUE, event.labels=TRUE)
}

# Writing Files for Report
filename <- "./reports/data/simsum.csv"
write.zoo(returns, file=filename, sep=",")
filename <- "./reports/data/simsum.month.csv"
write.zoo(returns.month, file=filename, sep=",")
filename <- "./reports/data/tradeData.csv"
write.zoo(cbind(.blotter$portfolio.Fut$symbols$KSFA020$posPL, signal[start_i:NROW(get(b_symbol))]$sig), file=filename, sep=",")

# Get End Value 
getEndEq(account,Sys.time())

write.table(perTradeStats(Portfolio=portfolio, Symbol=symbol), file="./data/perTradeStats.csv",sep=",", row.name=F)
chart.ME(Portfolio=portfolio, Symbol=symbol, scale=c("percent"))
write.table(t(tradeStats(Portfolios=portfolio, Symbols=symbol, use="trades")), file="./data/tradeStats.csv", sep=",")
