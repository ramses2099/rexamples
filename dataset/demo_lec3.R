# demo the data summarization

# init env:
if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())
options(scipen=9)

setwd("C:/Users/yunqi/OneDrive - Conestoga College/Course_teach/DataScience8435_24F/datasets")

# read dataset
test_data <- read.table('Reading_Test.txt', sep=',', header=TRUE)

head(test_data)

colnames(test_data)[1] <- "City"

str(test_data)

test_data$City <- as.factor(test_data$City)
head(test_data)
str(test_data)

# summarization of data:
mean(test_data$Pop)
sd(test_data$Pop)
median(test_data$Pop)
quantile(test_data$Pop, 0.75)

# Cross Tabulation
table(test_data$City)

ctab_city_size <- table(test_data$City, test_data$Size)

round(prop.table(ctab_city_size, 1), 2)

# aggregate table
aggtable_city_pop <- aggregate(test_data$Pop,
                               by = list(test_data$City),
                               FUN = mean)
aggtable_city_pop

colnames(aggtable_city_pop) <- c("City", "AvgPop")

aggtable_city_pop

# change colums
test_data_c <- test_data
colnames(test_data_c) <- paste(colnames(test_data_c), "study", sep="_")

# visualization
# bar plot
city_count <- table(test_data$City)
barplot(city_count)

city_count_order <- city_count[order(city_count,decreasing = TRUE)]
barplot(city_count_order)

#histogram:
hist(test_data$Pop,
     main = "Histogram of Population",
     xlab = "Range of pop",
     col = "green", density = 30, angle = 45,
     breaks = seq(min(test_data$Pop), max(test_data$Pop), length.out=5))

#boxplot
boxplot(test_data$Houses,
        horizontal = TRUE,
        main = "box plot of num. of houses",
        xlab = "house")

#scatter plot
plot(test_data$Pop, test_data$Houses,
     main = "houses vs pop",
     xlab = "pop",
     ylab = "houses",
     col = "red",
     pch = 5)

# pie chart
aggtable_city_sumpop <- aggregate(test_data$Pop,
                               by = list(test_data$City),
                               FUN = sum)
colnames(aggtable_city_sumpop) <- c("City", "TotalPop")

aggtable_city_sumpop
pie(aggtable_city_sumpop$TotalPop, aggtable_city_sumpop$City,
    main = "Population of Cities")

# Pareto Chart
# install and Load qcc package
# install.packages('qcc')
# library(qcc)

defect <- c(27, 789, 9, 65, 12, 109, 30, 15, 45, 621)

names(defect) <- c("Too noisy", "Overpriced", "Food not fresh",
                   "Food is tasteless", "Unfriendly staff",
                   "Wait time", "Not clean", "Food is too salty",
                   "No atmosphere", "Small portions")

pareto.chart(defect, 
             #xlab = "Categories", # x-axis label
             ylab="Frequency", # label y left
             col=heat.colors(length(defect)),
             
             # ranges of the percentages at the right
             cumperc = seq(0, 100, by = 20),
             ylab2 = "Cumulative Percentage",
             
             main = "Complaints of customers")



