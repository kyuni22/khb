#set function for storing intermediate values
updatePair <- function(Portfolio, Symbol, TxnDate, PosUnitsQty)
{ # @author Peter Carl
  
  # DESCRIPTION:
  # Adds transactions-related data to the STRATEGY timeseries.
  
  # Inputs
  # TxnDate: transaction date as ISO 8106, e.g., '2008-09-01'
  # PosUnitsQty: total units (shares) of the transaction
  # StopPrice: price at which the transaction was done
  # TxnPrice: last trade price
  # TxnN: calculated N for last transaction
  # Outputs:
  # No output.  Modifies STRATEGY in local namespace.
  
  # FUNCTION
  # Store the transaction and calculations, returns the portfolio
  pname=Portfolio
  NewTxn = xts(PosUnitsQty, order.by=as.POSIXct(TxnDate))
  colnames(NewTxn) = c('Fut.Units')
  Portfolio<-getPortfolio(Portfolio)
  Portfolio[[Symbol]]$pair <- rbind(Portfolio[[Symbol]]$pair, NewTxn)
  assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}
