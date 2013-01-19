# required libraries
require(quantmod)
require(blotter)
#Load required functions
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

# Try to clean up in case the demo was run previously
try(rm(list=ls(envir=.blotter),envir=.blotter))

# Set initial values
initDate="2006-01-02" #Sim.S03 DB Start from 2006-01-02
initEq=180

print("Initializing portfolio and account structure")

# List up all sectors
symbols <- c("KSFA020")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

#Load Data
DB_info <- "Driver={MySQL ODBC 5.1 Driver};DataBase=;Pwd=3450;UID=abcd;Server=localhost;OPTION=3"
attr_condition <- "Select tDate, Close, Open, High, Low from sim.s02 where sCode = "
getSymbols.DB(DB_info, attr_condition, symbols, from=initDate)

#init envir
.blotter <- new.env()
.instrument <- new.env()

#### Signal Generating
for(symbol in symbols) {

  # Get MACD, Signal, hist
  signal <- MACD(Cl(get(symbol)), 12, 26, 9, maType="EMA", percent=FALSE)
  signal$hist <- signal[,'macd'] - signal[,'signal']
  signal$sig <- ifelse(signal$hist>0,1,-1)
}

# Set up a portfolio object and an account object
portfolio = "Fut" 
initPortf(name=portfolio,c(symbols),initDate=initDate)
account = "Fut"
initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
# Change it TRUE if you want to see transaction detail.
verbose=TRUE
b_symbol = "KSFA020" # Set benchmark symbol

# Create trades
for( i in 60:NROW(x) ) { # Assumes all dates are the same
  CurrentDate=time(get(b_symbol))[i] # time setting with benchmark symbol
  equity = getEndEq(account, CurrentDate)
  
  for(symbol in symbols) {
    x = get(symbol)
    ClosePrice = as.numeric(Cl(x[i,]))
    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    UnitSize = 1 # Contracts
    
    # Position Entry (assume fill at close, so account for slippage)
    if( Posn == 0 ) { 
      # Initiate Long position
      if( signal$sig[i-1] > 0 ) { 
        # Store trade with blotter
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
        print(paste("Inital Long",signal$sig[i-1][1],UnitSize))
      } 
      else if ( signal$sig[i-1] < 0 ) { 
        # Store trade with blotter
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
        print(paste("Inital Short",signal$sig[i-1][1],-UnitSize))       
      }
    } else {   
      # Have position check exit
      if( (Posn > 0) && ( signal$sig[i-1] <= 0 ) ) {
        # Store trade with blotter
        txnQty <- -Posn# - UnitSize
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = txnQty , TxnFees=0, verbose=verbose)
        print(paste("short",signal$sig[i-1][1],txnQty))
      } 
      else if( (Posn < 0) && ( signal$sig[i-1] >= 0 ) ) {
        # Store trade with blotter        
        txnQty <- -Posn# + UnitSize
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = txnQty , TxnFees=0, verbose=verbose)
        print(paste("long",signal$sig[i-1][1],txnQty))        
      }
    } #else
    # Maintain Position
    print(paste("R U doing twice?",symbol))
  } # End symbol loop
  # Now that we've updated all of our trades, its time to mark the book
  print(paste("R U doing twice?",i))
  updatePortf(Portfolio = portfolio, Dates = CurrentDate)
  updateAcct(account, Dates = CurrentDate)
  updateEndEq(account, Dates = CurrentDate)
} # End dates loop

####################################################################################################
# Final values
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')

# Charting individual Position
if (require(quantmod)) {
  for(symbol in symbols){
    chart.Posn(Portfolio=portfolio,Symbol=symbol)
  }
}

chart.Posn(Portfolio=portfolio,Symbol=fut)
BM_return <- ROC(Cl(KSFA020), type="discrete")
Sim_return <- ROC(getAccount(account)$summary$End.Eq, type="discrete")
names(Sim_return)[1] <- "Sim"
returns <- cbind(Sim_return, BM_return)

# Charting 
if(require(PerformanceAnalytics)){
  charts.PerformanceSummary(returns,main="Performance")
}

# Get End Value 
getEndEq(account,Sys.time())