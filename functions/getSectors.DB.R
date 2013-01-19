####################################################################################################
## Retrieve Data From mysql Database 
## and convert data class in to xts
## factor -> factor to bactest
####################################################################################################
getSymbols.DB <- function(DB_info, symbols, factors=NULL, from='2006-01-01', env = .GlobalEnv){
  if (require(RODBC)) {
    dbcon <- odbcDriverConnect(DB_info)
  }
  
  #load Data from DB
  for (symbol in symbols) {
      loadquery <- paste("select s.tDate, s.Open, s.High, s.Low, S.Close
                      from sim.s01 s
                      where s.Close is not null and s.sCode = '", symbol, "' and s.tDate >= '",
                      from, "'", sep="")
      assign(symbol, sqlQuery(dbcon, paste(loadquery)), env)
      env[[symbol]] <- xts(env[[symbol]][,-1], env[[symbol]][,1]) # Makte data set as xts
      index(env[[symbol]]) <- as.POSIXct(index(env[[symbol]])) # Change index Class to POSIXt,POSIXct
  }
  
  #Close DB connection
  odbcClose(dbcon)
}