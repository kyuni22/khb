require(PerformanceAnalytics)
require(quantmod)
pfs <- as.xts(read.zoo("./data/Returns.csv", header=TRUE, sep=",", tz="" ))


pfs.month <- {}
for(i in 1:ncol(pfs)) {
  pfs.month <- cbind(pfs.month, apply.monthly(pfs[,i],sum))
}

charts.PerformanceSummary(pfs,geometric=FALSE,wealth.index=TRUE)

chart.CumReturns(pfs.month,geometric=FALSE,wealth.index=TRUE, main="Simple Returns", legend.loc='topleft')

chart.RelativePerformance(pfs.month[,2:10],pfs.month[,1], legend.loc='topleft')

chart.Boxplot(pfs.month)
chart.CaptureRatios(pfs.month[,2:10], pfs.month[,1,drop=FALSE])
chart.Correlation(pfs.month, histogram=TRUE, pch="+")
chart.RiskReturnScatter(pfs.month)

table.AnnualizedReturns(pfs.month)
table.Stats(pfs.month)
cor(pfs.month)


r_pfs.month <- pfs.month[,10]+pfs.month[,11]
r_pfs.month <- r_pfs.month/10
names(r_pfs.month) <- "All-BM"
pfs.month <- cbind(pfs.month, r_pfs.month)
chart.CumReturns(pfs.month[,c(1,17)],geometric=FALSE,wealth.index=TRUE, main="Simple Returns", legend.loc='topleft', colorset=rich8equal)