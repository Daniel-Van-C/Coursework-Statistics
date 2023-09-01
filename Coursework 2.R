# Libraries.
library(readr)
library(rstudioapi)
library(e1071)
library(stringr)

library(car)
library(MASS)
library(leaps)
library(lmtest)

################################################################################

# Loads the data in.
setwd("D:/Google Drive/KCL/Year 4/MS61/CW/B")
data = read_csv('mobile.csv')
# Deletes empty rows.
data = data[rowSums(is.na(data)) != ncol(data),]
# Changes spaces in column names to underscores so that the data can be
# processed further.
names(data) = str_replace_all(names(data), c(" " = "_"))

################################################################################

# Statistical summaries about the data.
summary_stats <- function(category) {
  summary1 = summary(category)
  range1 = range(category)[2] - range(category)[1]
  data.frame(averages=c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', 'Variance',
                        'Standard Deviation', 'Range', 'Interquartile Range',
                        'Skewness', 'Kurtosis'),
             summaries=c(summary1['Min.'], summary1['1st Qu.'],
                         summary1['Median'], summary1['Mean'],
                         summary1['3rd Qu.'], summary1['Max.'],
                         var(category), sd(category), range1, IQR(category),
                         skewness(category), kurtosis(category))
  )
}
# List of each of the variables.
var_list = list(data$Price_AUS, data$weight_gr, data$resoloution, data$ppi,
                data$cpu_core, data$cpu_freq, data$internal_mem, data$ram,
                data$RearCam, data$Front_Cam, data$battery, data$thickness)
# For each variable in the list, print its statistical summary.
for (i in 1:length(var_list)) {
  print(names(data)[i])
  print(summary_stats(var_list[[i]]))
}

################################################################################

# Calculates a table to joint marginal distributions for each variable compared
# to the price variable.
calc_distribution <- function(category2, breaks2, labels2) {
  breaks = list(500, 1500, 2000, 2500, 3000, 4500)
  labels = c('Extremely Low', 'Low', 'Moderate', 'High', 'Extremely High')
  summary_data = data.frame(category1=cut(data$Price_AUS, breaks=breaks,
                                          labels=labels),
                            category2=cut(category2, breaks=breaks2,
                                          labels=labels2))
  
  addmargins(table(summary_data$category1, summary_data$category2))
}

calc_distribution(data$weight_gr, list(65, 110, 125, 140, 165, 755),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

calc_distribution(data$resoloution, list(1, 4.5, 5, 5.5, 6, 12.5),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

calc_distribution(data$ppi, list(0, 200, 300, 400, 500, 850),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

calc_distribution(data$cpu_core, list(-1, 0.5, 1.5, 2.5, 4.5, 8.5),
                  c('0', '1', '2', '4', '8'))

calc_distribution(data$cpu_freq, list(-1, 0.6, 1.2, 1.8, 2.4, 3),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

calc_distribution(data$internal_mem, list(-1, 0.001, 0.005, 0.129, 0.257, 4.1,
                                          8.1, 16, 32.1, 64.1, 128.1),
                  c('0', '0.004', '0.128', '0.256', '4', '8', '16', '32', '64', '128'))

calc_distribution(data$ram, list(-1, 0.001, 0.005, 0.009, 0.033, 0.129, 0.257,
                                 0.513, 1.1, 1.6, 2.1, 3.1, 4.1, 6.1),
                  c('0', '0.004', '0.008', '0.032', '0.128', '0.256',
                    '0.512', '1', '1.5', '2', '3', '4', '6'))

calc_distribution(data$RearCam, list(-1, 3, 6, 9, 12, 25),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

calc_distribution(data$Front_Cam, list(-1, 3, 6, 9, 12, 25),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

calc_distribution(data$battery, list(799, 1800, 2800, 3800, 4800, 10000),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

calc_distribution(data$thickness, list(5, 7, 9, 11, 13, 20),
                  c('Extremely Low', 'Low', 'Moderate', 'High',
                    'Extremely High'))

################################################################################

# Generates a histogram for each variables data, based on an input bin size.
generate_histogram <- function(category, xlim, ylim, breaks, xlab, ylab, main) {
  hist(category, xlim=xlim, ylim=ylim, breaks=breaks, xlab=xlab, ylab=ylab,
       main=main, xaxt="n", col=rgb(1,0,0,0.6))
  axis(1, at=breaks)
}

# Calls the above function for continuous variables. Also generates bar charts
# for the discreet variables.
{
par(mfrow=c(2,3))

generate_histogram(data$Price_AUS, c(500, 4500), c(0, 40),
                   seq(500, 4500, by=500),
                   'Price (AUS)',
                   'Number of Phones',
                   'Figure 1a: Price of\nMobile Phones')

generate_histogram(data$weight_gr, c(65, 755), c(0, 50),
                   seq(65, 755, by=15),
                   'Weight (grams)',
                   'Number of Phones',
                   'Figure 1b: Weight of Mobile Phones')

generate_histogram(data$resoloution, c(1, 12.5), c(0, 60),
                   seq(1, 12.5, by=0.5),
                   'Resolution (Number of Pixels)',
                   'Number of Phones',
                   'Figure 1c: Resolution of\nMobile Phones')

generate_histogram(data$ppi, c(100, 900), c(0, 60),
                   seq(100, 900, by=100),
                   'PPI',
                   'Number of Phones',
                   'Figure 1d: PPI of Mobile Phones')

barplot(table(data$cpu_core), xlab='Number of CPU Cores',
        ylab='Number of Phones',
        main='Figure 1e: Number of CPU Cores\nof Mobile Phones',
        col=rgb(1,0,0,0.6))

generate_histogram(data$cpu_freq, c(0, 3), c(0, 60),
                   seq(0, 3, by=0.6),
                   'CPU Speed (GHz)',
                   'Number of Phones',
                   'Figure 1f: CPU Speed of\nMobile Phones')
}


{
par(mfrow=c(2,3))

barplot(table(data$internal_mem), xlab='Internal Memory (GB)',
        ylab='Number of Phones',
        main='Figure 1g: Internal Memory of\nMobile Phones',
        col=rgb(1,0,0,0.6), cex.names=0.8)

barplot(table(data$ram), xlab='RAM (GB)',
        ylab='Number of Phones',
        main='Figure 1h: RAM of Mobile Phones',
        col=rgb(1,0,0,0.6), cex.names=0.8)

generate_histogram(data$RearCam, c(0, 24), c(0, 30),
                   seq(0, 24, by=3),
                   'Number of Pixels on Rear Camera',
                   'Number of Phones',
                   'Figure 1i: Number of Pixels on\nRear Camera of Mobile Phones')

generate_histogram(data$Front_Cam, c(0, 24), c(0, 70),
                   seq(0, 24, by=3),
                   'Number of Pixels on Front Camera',
                   'Number of Phones',
                   'Figure 1j: Number of Pixels on\nFront Camera of Mobile Phones')

generate_histogram(data$battery, c(800, 9800), c(0, 70),
                   seq(800, 9800, by=800),
                   'Battery Capacity (mAh)',
                   'Number of Phones',
                   'Figure 1k: Battery Capacity of\nMobile Phones')

generate_histogram(data$thickness, c(5, 19), c(0, 90),
                   seq(5, 19, by=2),
                   'Phone Thickness (mm)',
                   'Number of Phones',
                   'Figure 1l: Thickness of\nMobile Phones')
}

################################################################################

# Generates box plots for each variables data.
{
par(mfrow=c(2,3))

boxplot(data$Price_AUS,
        main='Figure 2a: Boxplot of Prices of\nMobile Phones',
        xlab='Price of Mobile Phones (AUS)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$weight_gr,
        main='Figure 2b: Boxplot of Weights of\nMobile Phones',
        xlab='Weight of Mobile Phones (grams)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$resoloution,
        main='Figure 2c: Boxplot of Resolution of\nMobile Phones',
        xlab='Resolution of Mobile Phones\n(Number of Pixels)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$ppi,
        main='Figure 2d: Boxplot of PPI of\nMobile Phones',
        xlab='PPI of Mobile Phones',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$cpu_core,
        main='Figure 2e: Boxplot of CPU Cores of\nMobile Phones',
        xlab='Number of CPU Cores of Mobile Phones',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$cpu_freq,
        main='Figure 2f: Boxplot of CPU Speeds of\nMobile Phones',
        xlab='CPU Speed of Mobile Phones (GHz)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)
}

{
par(mfrow=c(2,3))

boxplot(data$internal_mem,
        main='Figure 2g: Boxplot of Internal Memory of\nMobile Phones',
        xlab='Internal Memory of Mobile Phones (GB)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$ram,
        main='Figure 2h: Boxplot of RAM of\nMobile Phones',
        xlab='RAM of Mobile Phones (GB)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$RearCam,
        main='Figure 2i: Boxplot of Rear Camera\nResolution of Mobile Phones',
        xlab='Rear Camera Resolution of\nMobile Phones (Number of Pixels)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$Front_Cam,
        main='Figure 2j: Boxplot of Front Camera\nResolution ofMobile Phones',
        xlab='Front Camera Resolution of\nMobile Phones (Number of Pixels)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$battery,
        main='Figure 2k: Boxplot of Battery of\nMobile Phones',
        xlab='Battery of Mobile Phones (mAh)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(data$thickness,
        main='Figure 2l: Boxplot of Thickness of\nMobile Phones',
        xlab='Thickness of Mobile Phones (mm)',
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)
}

################################################################################

# Pie charts for the discreet variables.
par(mfrow=c(1,1))

{
pie_labels = paste0(table(data$internal_mem), " = ",
                    round(100 * table(data$internal_mem)/sum(table(data$internal_mem)),
                          2), "%")
pie(table(data$internal_mem), labels=pie_labels, cex=0.8,
    main="Figure 6a: Internal Memory (GB) of Mobile Phones")
}

{
pie_labels = paste0(table(data$ram), " = ",
                    round(100 * table(data$ram)/sum(table(data$ram)),
                          2), "%")
pie(table(data$ram), labels=pie_labels, cex=0.8,
    main="Figure 6b: RAM (GB) of Mobile Phones")
}

{
pie_labels = paste0(table(data$cpu_core), " = ",
                    round(100 * table(data$cpu_core)/sum(table(data$cpu_core)),
                          2), "%")
pie(table(data$cpu_core), labels=pie_labels, cex=0.8,
    main="Figure 6c: Number of CPU Cores of\nMobile Phones")
}

################################################################################

# Generates a matrix of scatter plots between each of the variables. 
pairs(Price_AUS~., data=data, col='blue', pch=20, cex=0.5)

# Generates the correlation coefficients table between each variable.
cor_cof = cor(data)
cor_cof
write.csv(cor_cof, file = "cor_cof.csv")

################################################################################

# Generates a linear model with the price as the dependent variable (and all
# other variables as the predictor variables)
mobile_lm = lm(Price_AUS~., data=data)

# Generates the variance inflation factor for each variable in the linear model.
vif(mobile_lm)

# Runs the stepwise regression method (a variable selection method) using the
# constructed linear model. 
lm_null = lm(Price_AUS~1, data=data)
step(lm_null, scope = list(upper=mobile_lm), data=boston, direction="both")

################################################################################

# Prints and saves data about the linear model.
mobile_lm_subset = lm(Price_AUS~weight_gr+ppi+cpu_core+cpu_freq+internal_mem+ram+battery+thickness, data=data)
lm_summary = summary(mobile_lm_subset)
lm_summary
write.csv(lm_summary$coefficients, file = "lm_coefficients.csv")

################################################################################

# Plots the residuals against the estimated prices of mobile phones.
residuals_mobile = mobile_lm_subset$residuals
fits_mobile = mobile_lm_subset$fitted.values

{
plot(fits_mobile, residuals_mobile, xlab='Estimated Price of Mobile Phone (AUS)',
     ylab='Residuals', pch=20, col='blue',
     main='Figure 4: Plot of Residuals vs\nEstimated Prices of Mobile Phones.')
abline(h=0)
}

################################################################################

# Plots a qq-plot of the residuals.
{
points(qqnorm(residuals_mobile, pch=1, frame=FALSE,
              main='Figure 5: Normal Q-Q Plot\nof Residuals'))
qqline(residuals_mobile, col="steelblue", lwd=2)
}

################################################################################