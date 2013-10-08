####################################################################################################
## Retrieve Data From Bloomberg
## and convert data class in to xts
####################################################################################################
getBDH <- function(conn, securities, fields, start_date, end_date = NULL, 
           override_fields = NULL, override_values = NULL, option_names = NULL, 
           option_values = NULL, always.display.tickers = FALSE, dates.as.row.names = (length(securities) == 
           1), include.non.trading.days = NULL, fields_names = NULL) {
  # get Tmp data
  tmp <- bdh(conn, securities, fields, start_date, end_date, 
             override_fields, override_values, option_names, 
             option_values, always.display.tickers, dates.as.row.names, include.non.trading.days)
  # Change into xts
  tmp <- as.xts(zoo(tmp[,-1], order.by = as.POSIXct(rownames(tmp))))
  
  # Change field name
  if(!is.null(fields_names)) {
    colnames(tmp) <- fields_names
  }
  return(tmp)
}