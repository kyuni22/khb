#set function for storing intermediate values
updateStrat <- function(Portfolio, Symbol, TxnDate, PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
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
  NewTxn = xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.POSIXct(TxnDate))
  colnames(NewTxn) = c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
  Portfolio<-getPortfolio(Portfolio)
  Portfolio[[Symbol]]$strat <- rbind(Portfolio[[Symbol]]$strat, NewTxn)
  assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}
