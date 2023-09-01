library(readr)
library(rstudioapi)
library(e1071)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read_csv('crime.csv')

############################################################################
# Code Sample 1 ############################################################
############################################################################

calc_distribution <- function(category, breaks, labels) {
  data$Category = cut(category,
                      breaks=breaks,
                      labels=labels)
  
  table(factor(data$Category))
}

calc_distribution(data$CrimeRate, seq(20, 200, by=30),
                  c('Extremely Low', 'Low', 'Moderately  Low',
                    'Moderatly High', 'High', 'Extremely High'))
calc_distribution(data$CrimeRate10, seq(20, 200, by=30),
                  c('Extremely Low', 'Low', 'Moderately Low',
                    'Moderately High', 'High', 'Extremely High'))

cols = c('Extremely Low', 'Low', 'Moderate', 'High', 'Extremely High')

calc_distribution(data$ExpenditureYear0, seq(30, 180, by=30), cols)
calc_distribution(data$ExpenditureYear10, seq(30, 180, by=30), cols)

calc_distribution(data$Wage, seq(250, 750, by=100), cols)
calc_distribution(data$Wage10, seq(250, 750, by=100), cols)



############################################################################
# Code Sample 2 ############################################################
############################################################################

generate_histogram <- function(category, xlim, ylim, breaks, xlab, ylab, main,
                                category10, legend_position) {
  hist(category, xlim=xlim, ylim=ylim, breaks=breaks, xlab=xlab, ylab=ylab,
       main=main, col=rgb(0,0,1,0.7), xaxt="n")
  hist(category10, breaks=breaks, add=TRUE, col=rgb(1,0.3,0,0.6))
  axis(1, at=breaks)
  legend(legend_position, c('Original Data', '10 Years Later'),
         fill=c(rgb(0,0,1,0.7), rgb(1,0.3,0,0.6)), pt.cex=1, cex=0.7)
}

generate_histogram(data$CrimeRate, c(20, 200), c(0, 20), seq(20, 200, by=30),
                    'Number of Offences per Million Population',
                    'Frequency of States',
                    'Figure 1a: Crime Rate Across 47 US States',
                    data$CrimeRate10, 'topright')

generate_histogram(data$ExpenditureYear0, c(30, 180), c(0, 25), seq(30, 180, by=30),
                    'Per capita expenditure on police ($)',
                    'Frequency of States',
                    'Figure 1b: Expenditure on Police\nAcross 47 US States',
                    data$ExpenditureYear10, 'topright')

generate_histogram(data$Wage, c(250, 800), c(0, 20), seq(250, 750, by=100),
                    'Median Weekly Wage ($)',
                    'Frequency of States',
                    'Figure 1c: Median Weekly Wage\nAcross 47 US States',
                    data$Wage10, 'topleft')

######

points(qqnorm(data$CrimeRate, pch=1, frame=FALSE))
qqline(data$CrimeRate, col="steelblue", lwd=2)
points(qqnorm(data$CrimeRate10, pch=1, frame=FALSE))



############################################################################
# Code Sample 3 ############################################################
############################################################################

summary_stats <- function(category) {
  summary1 = summary(category)
  range1 = range(category)[2] - range(category)[1]
  data.frame(averages=c('Q1', 'Median', 'Mean', 'Q3', 'Variance',
                          'Standard Deviation', 'Range', 'Interquartile Range',
                          'Skewness', 'Kurtosis'),
             summaries=c(summary1['1st Qu.'], summary1['Median'],
                           summary1['Mean'], summary1['3rd Qu.'],
                           var(category), sd(category), range1, IQR(category),
                           skewness(category), kurtosis(category))
             )
  }

summary_stats(data$CrimeRate)
summary_stats(data$CrimeRate10)

summary_stats(data$ExpenditureYear0)
summary_stats(data$ExpenditureYear10)

summary_stats(data$Wage)
summary_stats(data$Wage10)



############################################################################
# Code Sample 4 ############################################################
############################################################################

boxplot(data$CrimeRate, data$CrimeRate10,
        main='Figure 2a: Boxplot of Original Crime Rate\nCompared to 10 Years Later',
        xlab='Number of Offences per Million Population',
        ylab='Time Frame',
        names=c("Original", "10 Years"),
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)



############################################################################
# Code Sample 5 ############################################################
############################################################################

scatterplot <- function(category, main, ylab, category10) {
  plot(data$CrimeRate, category, main=main,
       xlab='Number of Offences per Million Population', ylab=ylab,
       col=rgb(1,0,0,0.8))
  lines(lowess(data$CrimeRate, category), col=rgb(1,0,0,0.8), lwd=2)
  points(data$CrimeRate10, category10, col=rgb(0,0,1,0.8))
  lines(lowess(data$CrimeRate10, category10), col=rgb(0,0,1,0.8), lwd=2)
  legend('topright', c('Original Data', '10 Years Later'),
         fill=c(rgb(1,0,0,0.8), rgb(0,0,1,0.8)), pt.cex=1, cex=0.5)
}

scatterplot(data$Youth,
            'Figure 3a: Crime Rate by Number of Youths\nacross 47 US States',
            'Males Aged 18-24 per 1000', data$Youth10)

scatterplot(data$Education,
            'Figure 3b: Crime Rate by Education Level\nacross 47 US States',
            'Average Years of Schooling', data$Education10)

scatterplot(data$ExpenditureYear0,
            'Figure 3c: Crime Rate by Expendature on Police\nacross 47 US States',
            'Per capita expenditure on police ($)', data$ExpenditureYear10)

scatterplot(data$LabourForce,
            'Figure 3d: Crime Rate by Employed Young Males\nacross 47 US States',
            'Employed Males Aged 18-24 per 1000', data$LabourForce10)

scatterplot(data$Males,
            'Figure 3e: Crime Rate by Number of Males\nacross 47 US States',
            'Males per 1000 females', data$Males10)

scatterplot(data$StateSize,
            'Figure 3f: Crime Rate by State Size\nacross 47 US States',
            'State Size (square miles in hundread thousands)', data$StateSize10)

scatterplot(data$YouthUnemployment,
            'Figure 3g: Crime Rate by Employed Young Males\nacross 47 US States',
            'Employed Males Aged 18-24 per 1000', data$YouthUnemploy10)

scatterplot(data$MatureUnemployment,
            'Figure 3h: Crime Rate by Unemployed Mature Males\nacross 47 US States',
            'Unemployed Males Aged 35-39 per 1000', data$MatureUnemploy10)

scatterplot(data$Wage,
            'Figure 3i: Crime Rate by Wage\nacross 47 US States',
            'Median Weekly Wage ($)', data$Wage10)

scatterplot(data$BelowWage,
            'Figure 3j: Crime Rate by Families in Poverty\nacross 47 US States',
            'Number of Families Below Half Wage per 1000', data$BelowWage10)



############################################################################
# Code Sample 6 ############################################################
############################################################################

x = seq(3, 9, by=.01)
A = dnorm(x, mean=5.4, sd=0.4)
B = dnorm(x, mean=6.4, sd=0.5)

{
plot(x, A, col=rgb(1,0,0,0.7),
     type="l", lwd=3,
     yaxt='n', xlab='Pine needle length (cm)',
     ylab='',
     main='Figure 4: Normal Distribution of\nSpecies A & B Pine Needle Lengths')
lines(x, B, col=rgb(0,0,1,0.7),
      type="l", lwd=3)
legend('topleft', c('Species A', 'Species B'),
       fill=c(rgb(1,0,0,0.7), rgb(0,0,1,0.7)),
       pt.cex=1, cex=0.6)
}

pt(1.96, 99)
dt(1.96, 99)


############################################################################
# Code Sample 7 ############################################################
############################################################################

x = seq(-100, 150, by=.01)
O = dnorm(x, mean=37, sd=21)

{
  plot(x, O, col=rgb(1,0,0,0.7),
       type="l", lwd=3,
       yaxt='n', xlab='Demand for tickets (in thousands)',
       ylab='',
       main='Figure 5: Normal Distribution of\nthe Demand for Tickets',
       xaxt="n")
  axis(1, at=seq(-100, 150, by=25))
}



############################################################################
# Code Sample 8 ############################################################
############################################################################

x = seq(-100, 150, by=.01)
O = dnorm(x, mean=37, sd=21)
limit = 40

{
  plot(x, O, col=rgb(1,0,0,0.7),
       type="l", lwd=3,
       yaxt='n', xlab='Demand for tickets (in thousands)',
       ylab='',
       main='Figure 6: Normal Distribution of\nthe Demand for Tickets',
       xaxt="n")
  axis(1, at=seq(-100, 150, by=25))
  polygon(c(x[x>=limit], max(x), limit),
          c(O[x>=limit], 0, 0),
          col=rgb(1,0,0,0.7),
          border=2)
}



############################################################################
# Code Sample 9 ############################################################
############################################################################

x = seq(-100, 150, by=.01)
O2 = dnorm(x, mean=38, sd=21)
limit = 40

{
  plot(x, O, col=rgb(1,0,0,0.7),
       type="l", lwd=3,
       yaxt='n', xlab='Demand for tickets (in thousands)',
       ylab='',
       main='Figure 7: Normal Distribution of\nthe Demand for Tickets',
       xaxt="n")
  axis(1, at=seq(-100, 150, by=25))
  polygon(c(x[x<=limit ], limit),
          c(O2[x<=limit ], 0),
          col=rgb(1,0,0,0.7),
          border=2)
}



############################################################################
# Code Sample 10 ###########################################################
############################################################################

x = seq(-100, 150, by=.01)
O2 = dnorm(x, mean=38, sd=21)
l = min(which(x >= 42.5))
h = max(which(x < 43.5))

{
  plot(x, O, col=rgb(1,0,0,0.7),
       type="l", lwd=3,
       yaxt='n', xlab='Demand for tickets (in thousands)',
       ylab='',
       main='Figure 8: Normal Distribution of\nthe Demand for Tickets',
       xaxt="n")
  axis(1, at=seq(-100, 150, by=25))
  polygon(c(x[c(l, l:h, h)]),
          c(0, O2[l:h], 0),
          col=rgb(1,0,0,0.7),
          border=2)
}



############################################################################
# Code Sample 11 ###########################################################
############################################################################

generate_density_plot <- function(category, category10, legend_position, main) {
  plot(density(category), main=main, col=rgb(1,0,0,0.6), lwd=2)
  lines(density(category10), col=rgb(0,0,1,0.7), lwd=2)
  legend(legend_position, c('Original Data', '10 Years Later'),
         fill=c(rgb(1,0,0,0.6), rgb(0,0,1,0.7)), pt.cex=1, cex=0.7)
}

generate_density_plot(data$CrimeRate, data$CrimeRate10, 'topright',
                      'Figure 9a: Density Plot of Crime Rate')
generate_density_plot(data$ExpenditureYear0, data$ExpenditureYear10, 'topright',
                      'Figure 9b: Density Plot of\nExpenditure on Police')
generate_density_plot(data$Wage, data$Wage10, 'topleft',
                      'Figure 9c: Density Plot of\nMedian Weekly Wage')



############################################################################
# Code Sample 12 ###########################################################
############################################################################

northern = data[data$Southern == 0,]
southern = data[data$Southern == 1,]

boxplot(northern$CrimeRate, southern$CrimeRate,
        main='Figure 10a: Boxplot of Original Crime Rate\nbetween Northern and Southern states',
        xlab='Number of Offences per Million Population',
        ylab='Time Frame',
        names=c("Northern", "Southern"),
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)

boxplot(northern$CrimeRate10, southern$CrimeRate10,
        main='Figure 10b: Boxplot of Crime Rate 10 years later\nbetween Northern and Southern states',
        xlab='Number of Offences per Million Population',
        ylab='Time Frame',
        names=c("Northern", "Southern"),
        col=rgb(1,0,0,0.6),
        border=rgb(0,0.2,0.89,0.6),
        horizontal=TRUE)


