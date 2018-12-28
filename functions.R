# Reading the files created by Market basket analysis and RFM
final = read.csv('final.csv')
rules = read.csv('rules.csv')
rules_1 = read.csv('rules_1.csv')
rules_2 = read.csv('rules_2.csv')
rules_3 = read.csv('rules_3.csv')
rules_4 = read.csv('rules_4.csv')
rules_5 = read.csv('rules_5.csv')
seg = read.csv('segmentation.csv')

#creating functions to render output on Shiny app
#first tab
MBA <- function(input){
  x <- data.frame()
  x <- (rules[rules$input == input, c('output','confidence')])
  names(x) <- c('Recommended Product(s)','Affinity to Purchase')
  rownames(x) <- NULL
  return (x)
}

library(sqldf)
sqldf(paste0('select members from final where',' CustomerID = ','12346'))

#second tab - Cluster name
getClust <- function(custId){
  return (final$members[final$CustomerID == custId])
}

#second tab - Description of cluster
getClustAndDesc <- function(custId){
  clustID = getClust(custId)
  return (seg$Cluster_Name[seg$Cluster== clustID])
}


getClustFromDesc <- function(custDesc){
  return( seg$Cluster[seg$Cluster_Name == custDesc])
}

#tab 3 - recommendations
getTopReco <- function(custDesc){
  clust <- getClustFromDesc(custDesc)
  recom = sqldf(paste0('select sum(Quantity), Description
                     from final
                     where members = ',clust,
                     ' group by members, Description',
                     ' order by sum(Quantity) desc',
                     ' limit 5'))
  return (recom)
}

#tab 2 - recommendations
getRecoForCustandProd <- function(custId=0, clustDesc = '', prodDesc){
  if(custId == 0){
    clust <- getClustFromDesc(clustDesc)
  } else{
    clust <- getClust(custId)
  }
  
  
  dfForThisFn = data.frame()
  if(clust == 1){
    dfForThisFn = rules_1
  } else if (clust == 2) {
    dfForThisFn = rules_2
  } else if (clust == 3) {
    dfForThisFn = rules_3
  } else if (clust == 4) {
    dfForThisFn = rules_4
  } else if (clust == 5) {
    dfForThisFn = rules_5
  } else {
    dfForThisFn = rules
  }
  x <- data.frame()
  x <- (dfForThisFn[dfForThisFn$input == prodDesc, c('output','confidence')])
  names(x) <- c('Recommended Product(s)','Affinity to Purchase')
  rownames(x) <- NULL
  return (x)
  
}

#tab 2 - past purchase of customer
getPastPurcahses <- function(custId){
  bro = sqldf(paste0('select Description
                     from final',
                     ' order by InvoiceDate desc',
                     ' limit 5'))
  return (bro)
}