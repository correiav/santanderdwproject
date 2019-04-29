#Libraries to connect to Tableau and SQL Server.

#install.packages('Rserve')
#install.packages('RODBC')
library(Rserve)
Rserve()
library(RODBC)

#Calling Data Warehouse and Retriving ALL data from Fact Table.

channel <- odbcDriverConnect("driver=SQL Server;server=WIN-E4IFM4AFP2P; database=SantanderDW;trusted_connection=true")
initdata<- sqlQuery(channel,paste("select * from  dbo.FactTableSantander"))

#Organizing data into vectors, according to Index Benchmarks and Santander Stock's in Spain & US, respectively.

IBEX35 <- sqlQuery(channel, paste("select*from dbo.FactTableSantander where TickerID = 1"))
SANMC <- sqlQuery(channel, paste("select*from dbo.FactTableSantander where TickerID = 3"))
SP500 <- sqlQuery(channel, paste("select*from dbo.FactTableSantander where TickerID = 4"))
SAN <- sqlQuery(channel, paste("select*from dbo.FactTableSantander where TickerID = 2"))

#Organizing Sentiment Score, available only to Santander US (SAN) & Santander Spain(SANMC).

SENT_US <- sqlQuery(channel, paste("select Score from dbo.FactTableSantander where TickerID = 2"))
SENT_EU <- sqlQuery(channel, paste("select Score from dbo.FactTableSantander where TickerID = 3"))

#Merging the datasets to facilitate the regression analysis, to perform CAPM. 

SAN_SPAIN_MERGE <- merge(IBEX35, SANMC, by = "DatesID")
SAN_USA_MERGE <- merge(SP500, SAN, by = "DatesID")


# Making sure R knows the Date column is Date

SAN_SPAIN_MERGE$Dates.x <- as.Date(SAN_SPAIN_MERGE$Dates.x)

# Usually index contains more values
# we only subset the part of index data that contains stock information in it
SAN_SPAIN <- SAN_SPAIN_MERGE[4:length(SAN_SPAIN_MERGE$Dates.x), ]

# Making sure dates are matching and then we grab the weekly close prices of both index and the stock
range <- SAN_SPAIN_MERGE$Dates.x == SAN_SPAIN_MERGE$Dates.x
indexPrices <- SAN_SPAIN_MERGE$ClosingPrice.x[range]
stockPrices <- SAN_SPAIN_MERGE$ClosingPrice.y[range]

# Calculating the weekly return, e.g (x2-x1)/x1
indexReturns <- ( indexPrices[1:(length(indexPrices) - 1)] - indexPrices[2:length(indexPrices)] ) / indexPrices[2:length(indexPrices)]
stockReturns <- ( stockPrices[1:(length(stockPrices) - 1)] - stockPrices[2:length(stockPrices)] ) / stockPrices[2:length(stockPrices)]

# using R's lm function, we run a  regression analysis 
# we're using stockReturns as our y value
# and using indexReturns as our x value
# y ~ x is our formula
fit <- lm(stockReturns~indexReturns)
result <- summary(fit)
result

# summary gives us a lot of useful information, but we're mostly in beta value !!
beta <- result$coefficients[2,1]
print(beta)