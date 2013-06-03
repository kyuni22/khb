# required libraries
require(quantmod)
require(blotter)
#Load required functions
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

# Set initial values
initDate="2006-08-25" 
initEq=10^6

vixdata <- as.xts(read.zoo("./data/vixdata2.csv", header=TRUE, sep=","))
index(vixdata) <- as.POSIXct(index(vixdata))

# Try to clean up in case the demo was run previously
try(rm(list=ls(envir=.blotter),envir=.blotter))

print("Initializing portfolio and account structure")

# List up all sectors
symbols <- c("SPVXSP.Index")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

#init envir
.blotter <- new.env()
.instrument <- new.env()
Sys.setenv(environment=".blotter", TZ="UTC")
Sys.setenv(environment=".instrument", TZ="UTC")
Sys.setenv(TZ="UTC")

#vix and ux1
SPVXSP.Index <- vixdata[,1]
names(SPVXSP.Index) <- "Close"
vix <- vixdata[,4]
ux1 <- vixdata[,2]
ux2 <- vixdata[,3]

#### Signal Generating
for(symbol in symbols) {
  #
  vix_sma <- SMA(vix, n=10)
  vix_ema <- EMA(vix, n=10)
  vix_ts <- ux1-ux2
}

# Set up a portfolio object and an account object
portfolio = "Fut" 
initPortf(name=portfolio,c(symbols),initDate=initDate)
account = "Fut"
initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
# Other Properties to Setup
verbose=TRUE # TRUE if you want to see transaction 
b_symbol <- "SPVXSP.Index" # Set benchmark symbol
start_i <- 20 #Set 760 if you want to see after Dec 2008

# Create trades
for( i in start_i:NROW(get(b_symbol)) ) { # Assumes all dates are the same
  CurrentDate=time(get(b_symbol))[i] # time setting with benchmark symbol
  equity = getEndEq(account, CurrentDate)
  
  for(symbol in symbols) {
    x = get(symbol)
    ClosePrice = as.numeric(Cl(x[i,]))
    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    UnitSize = as.numeric(trunc(initEq/ClosePrice)) # Contracts
    
    ### Path dependent signals
#    signal$dates[i-1] <- ifelse(signal$TS[i-1] > signal$longPer[i-1],signal$dates[i-2]+1,0)
#    signal$sig[i-1] <- ifelse( (signal$dates[i-1] > 4) && (signal$ShortMA[i-1] > signal$LongMA[i-1]), 1, 0)
    ### End of Path dependent signals
    signal <- ifelse((vix_ts[i-1] < 0) && (vix_sma[i-1] < vix_ema[i-1]), -1, ifelse((vix_ts[i-1] > 0) && (vix_sma[i-1] > vix_ema[i-1]), 1, 0))
    
    # Position Entry (assume fill at close, so account for slippage)
    if( Posn == 0 ) { 
      # Initiate Long position
      if( signal > 0 ) { 
        # Store trade with blotter
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
#        print(paste("Inital Long",signal$sig[i-1][1],UnitSize))
      } 
      else if ( signal < 0 ) { 
        # Store trade with blotter
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
#        print(paste("Inital Short",signal$sig[i-1][1],-UnitSize))       
      }
    } else {   
      # Have position check exit
      if( signal == 0 ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
      }
      else if( (Posn > 0) && ( signal < 0 ) ) {
        # Store trade with blotter
        txnQty <- -Posn - UnitSize
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = txnQty , TxnFees=0, verbose=verbose)
#        print(paste("short",signal$sig[i-1][1],txnQty))
      } 
      else if( (Posn < 0) && ( signal > 0 ) ) {
        # Store trade with blotter        
        txnQty <- -Posn + UnitSize
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = txnQty , TxnFees=0, verbose=verbose)
#        print(paste("long",signal$sig[i-1][1],txnQty))        
      }
    } #else
    # Maintain Position
#    print(paste("R U doing twice?",symbol))
  } # End symbol loop
  # Now that we've updated all of our trades, its time to mark the book
#  print(paste("R U doing twice?",i))
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