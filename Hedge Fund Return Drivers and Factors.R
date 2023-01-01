library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tseries)
library(tidyr)
library(xts)
library(tidyverse)
library(PerformanceAnalytics)
library(TSstudio)
library(corrplot)
library(dygraphs)
library(psych)
library(runner)
library(reactable)
library(lubridate)
library(GGally)
library(formattable)
library(MASS)
library(shiny)
require("pairsD3")

data <- read_xlsx('C:\\Users\\TMF Jagon Chou\\Desktop\\all_factor_ds.xlsx')
fundlst <- read.csv('C:\\Users\\TMF Jagon Chou\\Desktop\\fundlist.csv')
fundret <- read.csv('C:\\Users\\TMF Jagon Chou\\Desktop\\fundret.csv')

# Long Short Equities
## Data Wrangling
fund <- subset(fundlst, fundlst$Main.Investment.Strategy == 'Long Short Equities')
fundname <- fund$Fund.Name
fund.ret <- subset(fundret, fundret$Fund.Name %in% fundname)  # only 3 funds have returns
col_name <- colnames(fund.ret)
date <- col_name[c(-1, -2, -3)]
co.ret = t(fund.ret[,4:ncol(fund.ret)] / 100)
co.ret.xts = xts(co.ret, as.yearmon(date,'%b.%y'))
colnames(co.ret.xts) <- c('Redwood','Renaissance','SEG')
co.ret.xts <- co.ret.xts[index(co.ret.xts) >= as.yearmon('Jan 1998'),]


index.dt <- dplyr::select(data, contains(c("Date", "Equity")))
index.xts = xts(index.dt, as.yearmon(index.dt$Date,'%b.%y'))
colnames(index.xts) <- c('Date', 'Value', 'Momentum', 'Carry', 'Defensive', 'Multi-Style', 'Market')
index.xts <- index.xts[index.xts$Date >= "1998-01-30"]
index.xts <- index.xts[, 2:7]

storage.mode(index.xts) <- "numeric"

## EDA
### 1. How's the monthly performance of the three selected Long Short Strategy companies as well as different indices?
# Comparison among companies
dygraph(co.ret.xts, main = "Company Comparison") %>%
  dyAxis("y", label = "%") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
# Comparison among indices
dygraph(index.xts, main = "Index Comparison") %>%
  dyAxis("y", label = "%") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))

# Volatility for companies
vol.co.ret <- rollapply(co.ret.xts, width = 2, FUN = sd, na.pad = TRUE)
plot(vol.co.ret, legend.loc = 'topleft')
# Volatility for indices
vol.index.ret <- rollapply(index.xts, width = 2, FUN = sd, na.pad = TRUE)
plot(vol.index.ret, legend.loc = 'topleft')

# Cumulative performance for both companies and indices
chart.CumReturns(co.ret.xts,legend.loc = 'topleft', main='Cumulative Return for companies')
chart.CumReturns(index.xts,legend.loc = 'topleft', main='Cumulative Return for indices')

# Sharpe Ratio
overall_sharpe_ratio.co <- round(SharpeRatio(co.ret.xts, Rf = 0), 4)
overall_sharpe_ratio.co

overall_sharpe_ratio.index <- round(SharpeRatio(index.xts, Rf = 0), 4)
overall_sharpe_ratio.index

chart.RollingPerformance(co.ret.xts,
                         FUN = 'mean', width = 12, colorset = rich8equal,
                         lwd = 2, legend.loc = "topleft",
                         main = "Rolling 12-Month Mean Return for Companies")
chart.RollingPerformance(index.xts,
                         FUN = 'SharpeRatio.annualized', width = 12,
                         colorset = rich8equal, lwd = 2, legend.loc = "topleft",
                         main = "Rolling 12-Month Sharpe Ratio for Indices")

Return.annualized(co.ret.xts)
StdDev.annualized(co.ret.xts)
SharpeRatio.annualized(co.ret.xts)

Return.annualized(index.xts)
StdDev.annualized(index.xts)
SharpeRatio.annualized(index.xts)

# MaxDrawdown
d <- make_date(year = format(index(co.ret.xts), format = '%Y'), month = format(index(co.ret.xts), format = '%m'), day = 1)
Redwood <- co.ret.xts[,1]
Renaissance <- co.ret.xts[,2]
SEG <- co.ret.xts[,3]

index(Redwood) = d
index(Renaissance) = d
index(SEG) = d

table.Drawdowns(Redwood)
table.Drawdowns(Renaissance)
table.Drawdowns(SEG)

maxDrawdown(co.ret.xts)
chart.Drawdown(co.ret.xts, legend.loc = 'bottomleft', main='MaxDrawdown for Companies')


d <- make_date(year = format(index(index.xts), format = '%Y'), month = format(index(index.xts), format = '%m'), day = 1)
Value <- index.xts[,1]
Momentum <- index.xts[,2]
Carry <- index.xts[,3]
Defensive <- index.xts[,4]
Multi.Style <- index.xts[,5]
Market <- index.xts[,6]

index(Value) = d
index(Momentum) = d
index(Carry) = d
index(Defensive) = d
index(Multi.Style) = d
index(Market) = d

table.Drawdowns(Value)
table.Drawdowns(Momentum)
table.Drawdowns(Carry)
table.Drawdowns(Defensive)
table.Drawdowns(Multi.Style)
table.Drawdowns(Market)

maxDrawdown(index.xts)
chart.Drawdown(index.xts, legend.loc = 'bottomleft', main='MaxDrawdown for Indices')

#-------------------------------------------------------------------------------
#### I. Redwood's performance is quite unstable (extreme values)

#### I. SEG has outperformed Redwood throughout its existing time
#### II. Since Renaissance started its business, it operates quite well and beats Redwood from 2007-2008 financial crisis.
####     Outperformed 2 or 3 times as Redwood
#### III. Although both Renaissance and Redwood slumped from 2020 due to the COVID pandemic, Redwood bounced back quickly
####      while Renaissance continued dropping until 2021
#### IV. Recently, Renaissance performs well while the return of Redwood continue declining.

#### I. SEG is the best performer among the three Long-Short strategy companies, with sharpe ratio arount 1

#-------------------------------------------------------------------------------



## Regression
### Redwood
plot(Redwood, main = "Historical Performance", cex=0.4, xlab = "Time", ylab = "Return", legend.loc = 'topright')
Redwood_reg <- Redwood[2:length(Redwood),]
index.reg.xts <- index.xts
index(index.reg.xts) <- as.yearmon(index(Redwood_reg))
Redwood_reg <- cbind(Redwood_reg, index.reg.xts)

#### Pairwise Evaluation
ggpairs(Redwood_reg)
round(cor(Redwood_reg),3)


#### Regression Model
lm_redwood_v1 = lm(Redwood ~ ., data = Redwood_reg)
summary(lm_redwood_v1)
anova(lm_redwood_v1)
coefficients(lm_redwood_v1) # model coefficients

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm_redwood_v1)

#### Backward Selection
lm_redwood_v1.step <- stepAIC(lm_redwood_v1, direction = "both")
summary(lm_redwood_v1.step)
anova(lm_redwood_v1.step)


### Renaissance
par(mfrow=c(1,1))
plot(Renaissance, main = "Historical Performance", cex=0.4, xlab = "Time", ylab = "Return", legend.loc = 'topright')
Renaissance_reg <- Renaissance[2:length(Renaissance),]
Renaissance_reg <- cbind(Renaissance, index.reg.xts)
Renaissance_reg <- na.omit(Renaissance_reg)

#### Pairwise Evaluation
ggpairs(Renaissance_reg)
round(cor(Renaissance_reg),3)

#### Regression Model
lm_renaissance_v1 = lm(Renaissance ~ ., data = Renaissance_reg)
summary(lm_renaissance_v1)
anova(lm_renaissance_v1)
coefficients(lm_renaissance_v1) # model coefficients

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm_renaissance_v1)

#### Backward Selection
lm_renaissance_v1.step <- stepAIC(lm_renaissance_v1, direction = "both")
summary(lm_renaissance_v1.step)
anova(lm_renaissance_v1.step)


### SEG
par(mfrow=c(1,1))
plot(SEG, main = "Historical Performance", cex=0.4, xlab = "Time", ylab = "Return", legend.loc = 'topright')
SEG_reg <- SEG[2:length(SEG),]
SEG_reg <- cbind(SEG, index.reg.xts)
SEG_reg <- na.omit(SEG_reg)

#### Pairwise Evaluation
ggpairs(SEG_reg)
round(cor(SEG_reg),3)

#### Regression Model
lm_seg_v1 = lm(SEG ~ ., data = SEG_reg)
summary(lm_seg_v1)
anova(lm_seg_v1)
coefficients(lm_seg_v1) # model coefficients

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm_seg_v1)

#### Backward Selection
lm_seg_v1.step <- stepAIC(lm_seg_v1, direction = "both")
summary(lm_seg_v1.step)
anova(lm_seg_v1.step)






lr
backward selection
pca()

compare








































Redwood_diff <- diff(Redwood, differences = 1)
Redwood_diff <- na.omit(Redwood_diff)
Redwood_diff$Res <- ifelse(Redwood_diff>0, 1, 0)
Redwood_diff <- head(Redwood_diff, -1)









































