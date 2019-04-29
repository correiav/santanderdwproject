#Libraries to connect to SQL Server.
library(RODBC)

#Calling Data Warehouse and Retriving ALL data from Fact Table.
sql_connetor <- odbcDriverConnect("driver=SQL Server;server=WIN-E4IFM4AFP2P; database=SantanderDW;trusted_connection=true")
initialtdata<- sqlQuery(sql_connetor,paste("select * from  dbo.FactTableSantander"))

#Organizing data into data frames, according to Index Benchmark and Santander Stock's in USA, respectively.
SAN <- sqlQuery(sql_connetor, paste("select * from dbo.FactTableSantander where TickerID = 2"))
SP500 <- sqlQuery(sql_connetor, paste("select * from dbo.FactTableSantander where TickerID = 4"))

#Merging Index Benchmark data with Santander Madrid Stock data into one data frame, for statistical analysis.
SAN_MERGE <- merge(SP500, SAN, by = "Dates")

#Time Series analysis of both stocks on individual basis. Preliminary check for correlation.

tsSP500 <- ts(SP500$ClosingPrice, frequency = 576)
plot(tsSP500)
tsSAN <- ts(SAN$ClosingPrice, frequency = 576)
plot(tsSAN)
tsSP500components <- decompose(tsSP500)
plot(tsSP500components)
tsSANcomponents <- decompose(tsSAN)
plot(tsSANcomponents)

#Arranging samples of size 25 from the Index Benchmark and Santander Madrid Stock, to check for normality. 

xbarSP500 <- rep(NA, 1000)
for(i in 1:1000)
{x <- sample(SP500$ClosingPrice, size = 25)
xbarSP500 [i] <- mean(x)}
hist(xbarSP500, xlab = "SP500 - US Benchmark", main = "Histogram SP500 - US Benchmark Index")

xbarSAN <- rep(NA, 1000)
for (i in 1:1000)
{x <- sample(SAN$ClosingPrice, size = 25)
xbarSAN[i] <- mean(x)}
hist(xbarSAN, xlab = "SAN - Santander US", main = "Histogram SAN - Bank Santander US")

#Setting up matrices to run the correlation between US Index Benchmark & Bank Santander US
# x variable refers to the US Index Benchmark - SP500
# y variable refers to the Bank Santander in US - SAN
correlation = data.frame(
  x <- c(SAN_MERGE$ClosingPrice.x),
  y <- c(SAN_MERGE$ClosingPrice.y)
)

#Scatter Plot and Line of best fit for the linear regression model.
plot(y~x, data = correlation, xlab = "Historical Closing Price SP500", 
     ylab = "Historical Closing Price SAN", main = "SP500 vs. SAN - 01/01/2012 to 02/08/2016")
abline(lm(y~x), col="blue", lty = "dotted") 

#A linear model is built, based on the evidence of linear regression between the Index Benchmark and Banco Santander Stock.
linearmodel = lm(y~x, data = correlation)
summary(linearmodel)