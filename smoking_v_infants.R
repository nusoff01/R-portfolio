#Written by Nick Usoff with help from Brent Cohn

#RETRIEVE DATA

`2014.CHR.analytic.data` <- read.csv("~/Documents/R/2014 CHR analytic data.csv", header=FALSE)
cleanData <- read.csv("~/Downloads/2014 CHR analytic data.csv", header=TRUE, skip=1)

#STATISTICAL SUMMARY

asv_mean <- mean(cleanData$Adult.smoking.Value, na.rm = TRUE)
imv_mean <- mean(cleanData$Infant.mortality.Value, na.rm = TRUE)
asv_median <- median(cleanData$Adult.smoking.Value, na.rm = TRUE)
imv_median <- median(cleanData$Infant.mortality.Value, na.rm = TRUE)


temp <- table(as.vector(cleanData$Adult.smoking.Value))
asv_mode <- names(temp)[temp == max(temp)]
temp <- table(as.vector(cleanData$Infant.mortality.Value))
imv_mode <- names(temp)[temp == max(temp)]
asv_IQR = IQR(cleanData$Adult.smoking.Value, na.rm = TRUE)
imv_IQR = IQR(cleanData$Infant.mortality.Value, na.rm = TRUE)

print(paste0("mean Adult smoking rate", asv_mean))
print(paste0("mean Infant mortality rate: ", imv_mean))

print(paste0("median Adult smoking rate", asv_median))
print(paste0("median Infant mortality rate: ", imv_median))

print("mode Adult smoking rate")
print(asv_mode)
print("mode Infant mortality rate")
print(imv_mode)

print(paste0("IQR Adult smoking rate", asv_IQR))
print(paste0("IQR Infant mortality rate", imv_IQR))

#MAX AND MIN VALUES

max_imv <- which.max( cleanData$Infant.mortality.Value )
print(paste0("County with the maximum infant mortality rate: ", cleanData$County[max_imv]))
max_asv <- which.max( cleanData$Adult.smoking.Value )
print(paste0("County with the maximum adult smoking rate: ", cleanData$County[max_asv]))

min_imv <- which.min( cleanData$Infant.mortality.Value )
print(paste0("County with the minimum infant mortality rate: ", cleanData$County[min_imv]))
min_asv <- which.min( cleanData$Adult.smoking.Value )
print(paste0("County with the minimum adult smoking rate: ", cleanData$County[min_asv]))

#STATE SUMMARIES

by(cleanData$Adult.smoking.Value,cleanData$State, summary)
by(cleanData$Infant.mortality.Value,cleanData$State, summary)

#PLOTS

plot(cleanData$Infant.mortality.Value~cleanData$Adult.smoking.Value, main="Infant mortality by smoking rate",xlab="Adult smoking rate", ylab="Infant mortality rate (per 1000 births)")

regline <- lm(cleanData$Infant.mortality.Value~cleanData$Adult.smoking.Value) 
abline(regline)

boxplot(cleanData$Infant.mortality.Value, range=0, main="Boxplot of Infant Mortality Rates by County", ylab="rates in number per 1000 births")
hist(cleanData$Infant.mortality.Value, main="Histogram of Infant Mortality Rates by County", xlab="rates in number per 1000 births", ylab="number with this value")

