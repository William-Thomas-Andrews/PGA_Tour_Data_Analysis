




##### Data Cleaning ############################################################


options(scipen=999)

pga = read.csv('./Math_150_data/pgaTourData.csv')
pga = data.frame(pga)
pga[is.na(pga)] <- 0
pga = pga[pga$Avg.Distance != 0, ]
pga = pga[pga$gir != 0, ]
pga = pga[pga$Average.Putts != 0, ]
pga = pga[pga$Average.Score != 0, ]
pga = pga[pga$Average.SG.Putts != 0, ]
pga = pga[pga$Average.SG.Total != 0, ]
pga = pga[pga$SG.OTT != 0, ]
pga = pga[pga$SG.APR != 0, ]
pga = pga[pga$SG.ARG != 0, ]
pga = pga[pga$Money != 0, ]

library(readr)
pga['Money'] = as.numeric(parse_number(pga$Money))
pga['Money']

################################################################################







##### Traditional Golf Stats ###################################################

## Fairway percentage and Money
plot(pga$Fairway.Percentage, pga$Money, col = 'black', main ='Fairway Percentage (%) on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Fairway Percentage (%)', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$Fairway.Percentage)
abline(fit, col = 'red')
cor(pga$Fairway.Percentage, pga$Money, use = "complete.obs")
text(60, 11000000, 'r = 0.01547088', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)


## Average distance and Money
plot(pga$Avg.Distance, pga$Money, col = 'black', main ='Average Driving Distance (yards) on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Average Driving Distance (yards)', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$Avg.Distance)
abline(fit, col = 'red')
cor(pga$Avg.Distance, pga$Money, use = "complete.obs")
text(277, 10000000, 'r = 0.3306003', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)


## Greens in regulation and Money
plot(pga$gir, pga$Money, col = 'black', main ='Number of Greens in Regulation on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Number of Greens in Regulation', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$gir)
abline(fit, col = 'red')
cor(pga$gir, pga$Money, use = "complete.obs")
text(59, 10000000, 'r = 0.316449', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)


## Average putts and Money - axes reversed
plot(pga$Average.Putts, pga$Money, col = 'black', main ='Average Number of Putts on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Average Number of Putts', pch = 16, cex = 0.6, yaxt="n", xlim = rev(range(pga$Average.Putts)))
fit = lm(pga$Money ~ pga$Average.Putts)
abline(fit, col = 'red')
cor(pga$Average.Putts, pga$Money, use = "complete.obs")
text(30, 10000000, 'r = 0.2564512', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)

################################################################################







##### Strokes Gained ###########################################################

## Average SG Putts and Money
plot(pga$Average.SG.Putts, pga$Money, col = 'black', main ='Average Strokes Gained Putts on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Average Strokes Gained Putts', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$Average.SG.Putts)
abline(fit, col = 'red')
cor(pga$Average.SG.Putts, pga$Money, use = "complete.obs")
text(-1, 10000000, 'r = 0.2772874', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)

## SG Off the Tee and Money
plot(pga$SG.OTT, pga$Money, col = 'black', main ='Strokes Gained Off the Tee on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Strokes Gained Off the Tee', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$SG.OTT)
abline(fit, col = 'red')
cor(pga$SG.OTT, pga$Money, use = "complete.obs")
text(-1, 10000000, 'r = 0.4424441', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)

## SG Approach and Money
plot(pga$SG.APR, pga$Money, col = 'black', main ='Strokes Gained Approach on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Strokes Gained Approach', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$SG.APR)
abline(fit, col = 'red')
cor(pga$SG.APR, pga$Money, use = "complete.obs")
text(-1, 10000000, 'r = 0.5102146', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)

## SG Around the green and Money
plot(pga$SG.ARG, pga$Money, col = 'black', main ='Strokes Gained Around the Green on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Strokes Gained Around the Green', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$SG.ARG)
abline(fit, col = 'red')
cor(pga$SG.ARG, pga$Money, use = "complete.obs")
text(-0.5, 10000000, 'r = 0.278182', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)

## Average SG total and Money
plot(pga$Average.SG.Total, pga$Money, col = 'black', main ='Average Total Strokes Gained on Dollars Earned (USD)',cex.main=0.86, ylab = 'Dollars Earned (USD)', xlab = 'Average Total Strokes Gained', pch = 16, cex = 0.6, yaxt="n")
fit = lm(pga$Money ~ pga$Average.SG.Total)
abline(fit, col = 'red')
cor(pga$Average.SG.Total, pga$Money, use = "complete.obs")
text(-1, 10000000, 'r = 0.7467889', font = 2, col = 'red')
axis(2,cex.axis=0.5, las = 2)

################################################################################







##### Correlation Frame ########################################################

library(dplyr)
pga_no_names = pga[2:length(pga)]
pga_no_names_subsetted = pga_no_names[c(4, 5, 6, 8, 10, 11:17)]
df2 <- mutate_all(pga_no_names_subsetted, function(x) as.numeric(as.character(x)))
data = as.matrix(df2)
mydata.cor = cor(data[complete.cases(data), ])
# mydata.cor
correlation_frame = data.frame(mydata.cor)
# correlation_frame

################################################################################







##### Tiger Woods Stats ########################################################

woods <- subset(pga, pga$Player.Name == 'Tiger Woods', select = c("Average.SG.Putts","SG.OTT","SG.APR", "SG.ARG"))
woods_cleaned <- subset(woods, woods$Average.SG.Putts != is.na, select = c("Average.SG.Putts","SG.OTT","SG.APR", "SG.ARG"))
woods_cleaned <- na.omit(woods)
woods_cleaned
barplot(c(mean(woods_cleaned$Average.SG.Putts), mean(woods_cleaned$SG.OTT), mean(woods_cleaned$SG.APR), mean(woods_cleaned$SG.ARG)), xlab = "Part of Game",
        ylab = "Strokes Gained", main ="Tiger Woods Strokes Gained Stats", names.arg = c('AVG.SG.PT', 'SG.OTT', 'SG.APR', 'SG.ARG'), las = 1
)
hist(pga$SG.APR)
z_score <- (1.2-mean(pga$SG.APR))/sd(pga$SG.APR)
z_score
boxplot(pga$SG.APR)

## Tiger Wood's approach shots strokes gained are 2.98 standard deviations above the mean

################################################################################





