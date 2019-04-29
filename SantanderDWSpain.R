#Libraries to connect to Tableau and SQL Server.
library(Rserve)
Rserve()
library(RODBC)

#Calling Data Warehouse and Retriving ALL data from Fact Table.
sql_connetor <- odbcDriverConnect("driver=SQL Server;server=WIN-E4IFM4AFP2P; database=SantanderDW;trusted_connection=true")
initialtdata<- sqlQuery(sql_connetor,paste("select * from  dbo.FactTableSantander"))

#Organizing data into data frames, according to Index Benchmarks and Santander Stock's in Spain, respectively.
IBEX35 <- sqlQuery(sql_connetor, paste("select * from dbo.FactTableSantander where TickerID = 1"))
SANMC <- sqlQuery(sql_connetor, paste("select * from dbo.FactTableSantander where TickerID = 3"))

#Merging Index Benchmark data with Santander Madrid Stock data into one data frame, for statistical analysis.
SANMC_MERGE <- merge(IBEX35, SANMC, by = "Dates")

#Time Series analysis of both stocks on individual basis. Preliminary check for correlation.

tsIBEX35 <- ts(IBEX35$ClosingPrice, frequency = 652)
plot(tsIBEX35)
tsSANMADRID <- ts(SANMC$ClosingPrice, frequency = 652)
plot(tsSANMADRID)
tsIBEX35components <- decompose(tsIBEX35)
plot(tsIBEX35components)
tsSANMADRIDcomponents <- decompose(tsSANMADRID)
plot(tsSANMADRIDcomponents)

#Arranging samples of size 25 from the Index Benchmark and Santander Madrid Stock, to check for normality. 

xbarIBEX35 <- rep(NA, 1000)
for(i in 1:1000)
{x <- sample(IBEX35$ClosingPrice, size = 25)
xbarIBEX35 [i] <- mean(x)}
hist(xbarIBEX35, xlab = "IBEX35 - Spanish Benchmark", main = "Histogram IBEX35 - Spanish Benchmark Index")

xbarSANMC <- rep(NA, 1000)
for (i in 1:1000)
{x <- sample(SANMC$ClosingPrice, size = 25)
xbarSANMC[i] <- mean(x)}
hist(xbarSANMC, xlab = "SAN MC - Santander Madrid", main = "Histogram SAN.MC - Bank Santander Madrid")

#Setting up matrices to run the correlation between Spanish Index Benchmark & Bank Santander Madrid
# x variable refers to the Spanish Index Benchmark - IBEX35
# y variable refers to the Bank Santander in Madrid - SAN.MC
correlation = data.frame(
  x <- c(SANMC_MERGE$ClosingPrice.x),
  y <- c(SANMC_MERGE$ClosingPrice.y)
)

#Scatter Plot and Line of best fit for the linear regression model.
plot(y~x, data = correlation, xlab = "Historical Closing Price IBEX35", 
     ylab = "Historical Closing Price SAN.MC", main = "IBEX35 vs. SAN.MC - 01/01/2012 to 31/12/2016")
abline(lm(y~x), col="blue", lty = "dotted") 

#A linear model is built, based on the evidence of linear regression between the Index Benchmark and Banco Santander Stock.
linearmodel = lm(y~x, data = correlation)
summary(linearmodel)