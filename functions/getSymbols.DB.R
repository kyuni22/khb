####################################################################################################
## Retrieve Data From mysql Database 
## and convert data class in to xts
## factor -> factor to bactest
####################################################################################################
getSymbols.DB <- function(DB_info, attr_condition, symbols, from='2006-01-01', env = .GlobalEnv){
  if (require(RODBC)) {
    dbcon <- odbcDriverConnect(DB_info)
  }
  
  #load Data from DB
  for (symbol in symbols) {
      loadquery <- paste(attr_condition, "'", symbol, "' and tDate >= '",
                      from, "'", sep="")
      assign(symbol, sqlQuery(dbcon, paste(loadquery)), env)
      env[[symbol]] <- as.xts(env[[symbol]][,-1], order.by=as.POSIXct(env[[symbol]][,1])) # Makte data set as xts
      print(symbol)
  }
  
  #Close DB connection
  odbcClose(dbcon)
}