# required libraries
require(quantmod)
require(blotter)
#Load required functions
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

# Set initial values
initDate="2004-11-16" 
initEq=10^6

bt_data <- as.xts(read.zoo("./data/nbuy_data.csv", header=TRUE, sep=",", tz="" ))

# Try to clean up in case the demo was run previously
try(rm(list=ls(envir=.blotter),envir=.blotter))

print("Initializing portfolio and account structure")

# List up all sectors
symbols <- c("KOSPI2.Index")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

#init envir
.blotter <- new.env()
.instrument <- new.env()

#symbols and signal
KOSPI2.Index <- bt_data[,1]
names(KOSPI2.Index) <- "Close"
prg_nbuy <- bt_data[,2]
fi_nbuy <- bt_data[,3] + bt_data[,4]

#### Signal Generating
for(symbol in symbols) {
  #
  prg_sma <- SMA(prg_nbuy, n=15)
  prg_lma <- SMA(prg_nbuy, n=40)
  fi_sma <- SMA(fi_nbuy, n=15)
  fi_lma <- SMA(fi_nbuy, n=40)  
}

# Set up a portfolio object and an account object
portfolio = "Fut" 
initPortf(name=portfolio,c(symbols),initDate=initDate)
account = "Fut"
initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
# Other Properties to Setup
verbose=TRUE # TRUE if you want to see transaction 
b_symbol <- "KOSPI2.Index" # Set benchmark symbol
start_i <- 63 

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
    signal <- ifelse((prg_sma[i-1] < prg_lma[i-1]) && (fi_sma[i-1] < fi_lma[i-1]), -1, ifelse((prg_sma[i-1] > prg_lma[i-1]) && (fi_sma[i-1] > fi_lma[i-1]), 1, 0))
    
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

# Simulation Return
returns <- cbind(PortfReturns(account),BM_return)
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