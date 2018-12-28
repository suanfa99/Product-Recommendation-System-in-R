########################################

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)

#Load dataset
df_data <- read.csv('data.csv', na.strings = '')
glimpse(df_data)

source('DataQualityReport.R')
DataQualityReport(df_data)

### Data Cleaning
##Delete all negative Quantity and Price. Delete all records with missing customer ID
df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

df_data <- df_data %>%
  drop_na()

#Convert character to factors
df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

#Create a variable called Total dollar = Quantity * UnitPrice
df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)
glimpse(df_data)
glimpse(df_RFM)

########### RFM Analysis ########################
#Further process the data set in by the following steps:

#  1. Recency: Find the most recent date for each customer and calculate the days since last order
#  2. Frequency: Find the number of order per customer
#  3. Monetary: Find the total amount spent per order

df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monetary= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)
kable(head(df_RFM))

#Recency histogram
hist(df_RFM$recency, col = 'turquoise', main = "Histogram of Recency metric", xlab="Recency")

#Frequency histogram
hist(df_RFM$frequency, breaks = 200, xlim = c(0,30),col='grey',main= "Histogram of Frequency", xlab="Frequency of Purchases")

#Monetary histogram
hist(df_RFM$monetary, breaks = 2000, xlim =c(0, 2000), col='blue', xlab="Monetary", main ="Histogram of Monetary metric")

#Because the data is realy skewed, we use log scale to normalize
df_RFM$monetary <- log(df_RFM$monetary)
hist(df_RFM$monetary, col='cyan', xlab="Monetary", main ="Histogram of Monetary metric" )

### Clustering
df_RFM2 <- df_RFM
row.names(df_RFM2) <- df_RFM2$CustomerID
df_RFM2$CustomerID <- NULL

df_RFM2 <- scale(df_RFM2)
summary(df_RFM2)

d <- dist(df_RFM2)
c <- hclust(d, method = 'ward.D')

plot(c)

members <- cutree(c,k = 5)

members[1:5]
table(members)

final_df = cbind(df_RFM, members)
head(final_df)
rownames(final_df) = NULL

#merging cluster with data file to assign cluster to each customer
final = merge(df_data, final_df, by = 'CustomerID')

#Loading libraries for market basket analysis
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

write.csv(final,'final.csv')
retail <- final

# ## Data preprocessing
# #read excel into R dataframe
# #complete.cases(data) will return a logical vector indicating which rows have no missing values. Then use the vector to get only rows that are complete using retail[,].
# retail <- retail[complete.cases(retail), ]
# #mutate function is from dplyr package. It is used to edit or add new columns to dataframe. Here Description column is being converted to factor column. as.factor converts column to factor column. %>% is an operator with which you may pipe values to another function or expression
# retail %>% mutate(Description = as.factor(Description))
# retail %>% mutate(Country = as.factor(Country))
# #Converts character data to date. Store InvoiceDate as date in new variable
# retail$Date <- as.Date(retail$InvoiceDate)
# #Extract time from InvoiceDate and store in another variable
# TransTime<- format(as.POSIXct(retail$InvoiceDate,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
# #Convert and edit InvoiceNo into numeric
# InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
# #Bind new columns TransTime and InvoiceNo into dataframe retail
# cbind(retail,TransTime)
# #get a glimpse of your data
# glimpse(retail)

library(plyr)

transactionData <- ddply(retail,c("InvoiceNo","InvoiceDate"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
transactionData$InvoiceNo <- NULL
transactionData$InvoiceDate <- NULL
#Rename column to items
colnames(transactionData) <- c("items")

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

#Setting performance parameters for the model
# minimum confidence = 0.8
#minimum support = 0.01
association.rules <- apriori(tr, parameter = list(supp=0.01, conf=0.8,maxlen=4))

#storing rules output in dataframe
library(reshape)
rules <- as(association.rules, "data.frame")

library(tidyr)
rules <- rules %>% separate(rules, c('input','output'), sep = "=>")
rules$input <- gsub("\\{", "", rules$input)
rules$input <- gsub("\\}", "", rules$input)
rules$output <- gsub("\\{", "", rules$output)
rules$output <- gsub("\\}", "", rules$output)

############## Cluster wise recommendation system #################

library(plyr)
retail_1 <- retail[retail$members == 1,]
retail_2 <- retail[retail$members == 2,]
retail_3 <- retail[retail$members == 3,]
retail_4 <- retail[retail$members == 4,]
retail_5 <- retail[retail$members == 5,]

#Creating a function to run analysis on clsuter specific data
getRules <- function(df){
  transData <- ddply(df,c("InvoiceNo","InvoiceDate"),
                     function(df1)paste(df1$Description,
                                        collapse = ","))
  
  transData$InvoiceNo <- NULL
  transData$InvoiceDate <- NULL
  
  colnames(transData) <- c("items")
  
  write.csv(transData,"temp.csv", quote = FALSE, row.names = TRUE)
  
  tr_temp <- read.transactions('temp.csv', format = 'basket', sep=',')
  
  assoc.rules <- apriori(tr_temp, parameter = list(supp=0.01, conf=0.8,maxlen=4))
  library(reshape)
  ruleForDf<- as(assoc.rules, "data.frame")
  
  library(tidyr)
  ruleForDf <- ruleForDf %>% separate(rules, c('input','output'), sep = "=>")
  ruleForDf$input <- gsub("\\{", "", ruleForDf$input)
  ruleForDf$input <- gsub("\\}", "", ruleForDf$input)
  ruleForDf$output <- gsub("\\{", "", ruleForDf$output)
  ruleForDf$output <- gsub("\\}", "", ruleForDf$output)
  return(ruleForDf)
}

rules_1 <- getRules(retail_1)
rules_2 <- getRules(retail_2)
rules_3 <- getRules(retail_3)
rules_4 <- getRules(retail_4)
rules_5 <- getRules(retail_5)

#Writing the recommendations to a csv file for creating functions on Shiny app
write.csv(rules_1, 'rules_1.csv')
write.csv(rules_2, 'rules_2.csv')
write.csv(rules_3, 'rules_3.csv')
write.csv(rules_4, 'rules_4.csv')
write.csv(rules_5, 'rules_5.csv')

write.csv(rules, 'rules.csv')

