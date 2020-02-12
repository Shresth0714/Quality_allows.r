library(readxl)

weeklyVisits <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet=2, skip=4, col_names = T)
financials <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet=3, skip=4, col_names = T)

install.packages("plotly")
library(plotly)
packageVersion('plotly')



################################################################################
## 1
plot_ly(weeklyVisits, x = ~weeklyVisits$`Week (2008-2009)`, y = ~weeklyVisits$Visits, type = 'bar', name = 'Visits') %>%
  layout(title = 'Visits vs weeks', xaxis = list(title = 'Week'), yaxis = list(title = 'Visits'), barmode = 'group')


plot_ly(financials, x = ~financials$`Week (2008-2009)`, y = ~financials$Revenue, type = 'bar', name = 'Visits') %>%
  layout(title= 'Revenue vs weeks', xaxis = list(title = 'Week'), yaxis = list(title = 'Revenue'), barmode = 'group')

plot_ly(financials, x = ~financials$`Week (2008-2009)`, y = ~financials$Profit, type = 'bar', name = 'Visits') %>%
  layout(title= 'Profit vs weeks', xaxis = list(title = 'Week'), yaxis = list(title = 'Profit'), barmode = 'group')


plot_ly(financials, x = ~financials$`Week (2008-2009)`, y = ~financials$`Lbs. Sold`, type = 'bar', name = 'Visits') %>%
  layout(title= 'Lbs.sold vs week', xaxis = list(title = 'Week'), yaxis = list(title = 'Lbs.sold'), barmode = 'group')




################################################################################
## 2
process <- function(data){
  return (c(mean(data), median(data), sd(data), min(data), max(data)))
}

processVisits <- process(weeklyVisits$Visits)
processUniqueVisits <- process(weeklyVisits$`Unique Visits`)
processRevenue <- process(financials$Revenue)
processProfit <- process(financials$Profit)
processLbs.Sold <- process(financials$`Lbs. Sold`)
finalProcess <- cbind(processVisits, processUniqueVisits, processRevenue, processProfit, processLbs.Sold)
colnames(finalProcess) <- c('Mean', 'Median', 'Std.dev', 'Minimum', 'Maximum')
rownames(finalProcess) <- c('Visits', 'Unique Visits', 'Revenue', 'Profit', 'Lbs.Sold')
finalProcess



################################################################################
## 3
type <- c( rep('Initial', 14), rep('Pre-prom', 21), rep('Prom', 17), rep('Post-prom', 14))
weeklyVisits$type <- type
financials$type <- type
meanVisits <- tapply(weeklyVisits$Visits, weeklyVisits$type, mean)
meanUniqueVisits <- tapply(weeklyVisits$`Unique Visits`, weeklyVisits$type, mean)
meanRevenues <- tapply(financials$Revenue, financials$type, mean)
meanProfits <- tapply(financials$Profit, financials$type, mean)
meanLbs.Solds <- tapply(financials$`Lbs. Sold`, financials$type, mean)
finalMeans <- cbind(meanVisits, meanUniqueVisits, meanRevenues, meanProfits, meanLbs.Solds)
finalMeans



################################################################################
## 4
plot_ly(financials, x = financials$`Lbs. Sold`, y = financials$Revenue, type = 'scatter',  name = 'Revenue') %>%
  layout(title = "Revenue vs Pounds sold", xaxis = list(title = "Pounds Sold"),
         yaxis=list(title='Revenue'))
corr_1 <- cor(financials$Revenue, financials$`Lbs. Sold`)
corr_1




################################################################################
## 5
plot_ly(x = weeklyVisits$Visits, y = financials$Revenue, type = 'scatter', name = 'Visits') %>%
  layout(title = "Revenue vs Visits", xaxis = list(title = "Visits"),
         yaxis=list(title='Revenue'))
corr_2 <- cor(financials$Revenue, weeklyVisits$Visits)
corr_2



################################################################################
## 6
lbsSold <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet=4, skip=4, col_names = T)

# a.
processSold <- process(lbsSold$`Lbs. Sold`)
processSold <- rbind(c('Mean', 'Median', 'Std.dev', 'Minimum', 'Maximum'), processSold)
processSold


# b.
hist(lbsSold$`Lbs. Sold`, main='Distribution of Pounds sold', xlab = 'Pounds sold', breaks=20,
     xlim=c(0, 50000))

# c.

# d.
actualCount <- function(data, number){
  means <- mean(data)
  std <- sd(data)
  threshold <- means + number * std
  counts <- sum(data <= threshold)
  return (counts)
}

upper_1 <- actualCount(lbsSold$`Lbs. Sold`, 1)
lower_1 <- actualCount(lbsSold$`Lbs. Sold`, -1)
upper_1 - lower_1

upper_2 <- actualCount(lbsSold$`Lbs. Sold`, 2)
lower_2 <- actualCount(lbsSold$`Lbs. Sold`, -2)
upper_2 - lower_2

upper_3 <- actualCount(lbsSold$`Lbs. Sold`, 3)
lower_3 <- actualCount(lbsSold$`Lbs. Sold`, -3)
upper_3 - lower_3

# e.
actualCount(lbsSold$`Lbs. Sold`, 1) - actualCount(lbsSold$`Lbs. Sold`, 0)

actualCount(lbsSold$`Lbs. Sold`, 0) - actualCount(lbsSold$`Lbs. Sold`, -1)

actualCount(lbsSold$`Lbs. Sold`, 2) - actualCount(lbsSold$`Lbs. Sold`, 1)

actualCount(lbsSold$`Lbs. Sold`, -1) - actualCount(lbsSold$`Lbs. Sold`, -2)

actualCount(lbsSold$`Lbs. Sold`, 3) - actualCount(lbsSold$`Lbs. Sold`, 2)

actualCount(lbsSold$`Lbs. Sold`, -2) - actualCount(lbsSold$`Lbs. Sold`, -3)

# f.
install.packages('fBasics')
library(fBasics)
skewness <- skewness(lbsSold$`Lbs. Sold`)
kurtosis <- kurtosis(lbsSold$`Lbs. Sold`)
