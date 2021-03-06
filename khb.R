# required libraries
require(quantmod)
require(blotter)
#Load required functions
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

# Set initial values
initDate="2006-01-02" #Sim.S03 DB Start from 2006-01-02
initEq=180

#Load symbol data First
DB_info <- "Driver={MySQL ODBC 5.1 Driver};DataBase=;Pwd=3450;UID=abcd;Server=localhost;OPTION=3"
attr_condition <- "Select tDate, Close, Open, High, Low from sim.s02 where sCode = "
getSymbols.DB(DB_info, attr_condition, symbols, from=initDate)

KSFA020 <- KSFA020[1:(NROW(KSFA020)-3)]

Nbuy_vol_data <- as.xts(read.zoo("./data/Nbuy_Vol_data.csv", header=TRUE, sep=",", tz="" ))
index(Nbuy_vol_data) <- as.POSIXct(index(Nbuy_vol_data))+9*60*60

# Try to clean up in case the demo was run previously
try(rm(list=ls(envir=.blotter),envir=.blotter))

print("Initializing portfolio and account structure")

# List up all sectors
symbols <- c("KSFA020")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

#init envir
.blotter <- new.env()
.instrument <- new.env()

#### Signal Generating
for(symbol in symbols) {
  
  # 
  signal <- SMA(Cl(get(symbol)), n=20)
  names(signal) <- "SMA1"
  signal$SMA2 <- SMA(Cl(get(symbol)), n=50)
  signal$MAO <- signal$SMA1 - signal$SMA2
  signal$signal <- EMA(signal$MAO, n=9)
  signal$hist <- signal$MAO - signal$signal
  signal$sig <- ifelse(signal$hist>0,1,-1)
  
  
}

# Set up a portfolio object and an account object
portfolio = "Fut" 
initPortf(name=portfolio,c(symbols),initDate=initDate)
account = "Fut"
initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
# Other Properties to Setup
verbose=TRUE # TRUE if you want to see transaction 
b_symbol <- "KSFA020" # Set benchmark symbol
start_i <- 180

# Create trades
for( i in start_i:NROW(get(b_symbol)) ) { # Assumes all dates are the same
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
#        print(paste("Inital Long",signal$sig[i-1][1],UnitSize))
      } 
      else if ( signal$sig[i-1] < 0 ) { 
        # Store trade with blotter
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
#        print(paste("Inital Short",signal$sig[i-1][1],-UnitSize))       
      }
    } else {   
      # Have position check exit
      if( signal$sig[i-1] == 0 ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
      }
      else if( (Posn > 0) && ( signal$sig[i-1] < 0 ) ) {
        # Store trade with blotter
        txnQty <- -Posn - UnitSize
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = txnQty , TxnFees=0, verbose=verbose)
#        print(paste("short",signal$sig[i-1][1],txnQty))
      } 
      else if( (Posn < 0) && ( signal$sig[i-1] > 0 ) ) {
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

# Charting individual Position
if (require(quantmod)) {
  for(symbol in symbols){
    chart.Posn(Portfolio=portfolio,Symbol=symbol)
  }
}

# Bench Mark Return - Buy and Hold with Same initEq
#### Warning.. maybe this should be changed to make FULL BuyAndHold returns
BM_return <- Cl(KSFA020[start_i:NROW(get(b_symbol))]) - lag(Cl(KSFA020[start_i:NROW(get(b_symbol))]))
BM_return[1] <- 0
BM_return <- BM_return/initEq

# Simulation Return
returns <- cbind(PortfReturns(account),BM_return)
names(returns) <- c("sim","BuyAndHold")
#####!!!! Need to be revised
returns.month <- cbind(ROC(Cl(to.monthly(Sim_return, indexAt='lastof')), type="discrete"),ROC(Cl(to.monthly(BM_return, indexAt='lastof')), type="discrete"))
names(returns.month) <- c("sim","BuyAndHold")

# Charting 
if(require(PerformanceAnalytics)){
  charts.PerformanceSummary(returns,main="Performance", geometric=FALSE)
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