#----------------------------------------------------------------------------- #
#     Asset Pricing and Portoflio Theory - Assignement
#     NOVA IMS 
#     Group roject: Carlos Cardoso    | 20211220    Carlota Reis   | 20211208   
#                   Guilherme Miranda | 20210420    Mariana Garcia | 20210838 
#----------------------------------------------------------------------------- #

rm(list=ls(all=TRUE))
graphics.off()
close.screen(all = TRUE)
erase.screen()
windows.options(record=TRUE)


ipath <- 'C:/Users/lotar/OneDrive - NOVAIMS/2nd trimester/Asset Pricing & Portfolio Theory'
setwd(ipath)

library(pacman)
library(quantmod)
if (!require("pacman")) {install.packages("pacman")}
library(fBasics)
library(evir)
library(dplyr)
library(rcompanion)


p_load(xts)                   # to manipulate time series of stock data
p_load(quantmod)              # to download stock data
p_load(PerformanceAnalytics)  # to compute performance measures
p_load(portfolioBacktest)     # portfolio Backtesting
p_load(CVXR)                  # Convex Optimization in R
p_load(DT)
p_load(riskParityPortfolio)   # RPP



# ------------------------------------------------------------------------------
# 1.Select A Diversified portfolio of 11 S&P500 - 2008-08-01 to 2021-11-30
# ------------------------------------------------------------------------------

# Loading stock data from three different sectors:
#   1.sector technology: ADBE - ADOBE
#   2.sector healthcare: AMGN - AMGEN
#   3.sector Financials: JPM  - JPMorgan Chase & Co
#   4.sector C. Discret: BBY  - Best Buy
#   5.sector Comm. Serv: DIS  - Walt Disney Co
#   6.sector Industrial: BA   - Boeing Company
#   7.sector C. Staples: PG   - Procter & Gamble
#   8.sector Energy    : CVX  - Chevron
#   9.sector Utilities : AES  - AES Corp
#  10.sector Real State: BXP  - Boston Properties 
#  11.sector Materials : BLL  - Ball Corp

# Download data from Yahoo Finance
stock_namelist <- c("ADBE", "AMGN", "JPM",  "BBY", "DIS", "BA",  "PG", "CVX", "AES", "BXP", "BLL")
prices <- xts()
for (i in 1:length(stock_namelist)) {
  tmp <- Ad(getSymbols(stock_namelist[i], from = "2008-08-01", to = "2021-12-01", auto.assign = FALSE))
  tmp <- na.approx(tmp, na.rm = FALSE)  # interpolate NAs
  prices <- cbind(prices, tmp)
}
colnames(prices) <- stock_namelist
tclass(prices) <- "Date"

str(prices)

head(prices,3)

tail(prices)

# ------------------------------------------------------------------------------
# 2.Compute daily and weekly linear and log-return (Adjusted Closing Price)
# ------------------------------------------------------------------------------

#DAILY RETURNS

# compute log-returns and linear returns
# A. log-returns
X_log <- diff(log(prices))[-1]

# B. linear returns
X_lin <- CalculateReturns(prices)[-1]

#WEEKLY RETURNS

# A. log-returns

wlog <- data.frame()
for (i in 1:length(stock_namelist)) {
  W_log = periodReturn(prices[,i], period='weekly', subset = NULL, type = "log", leading=TRUE)
  
  wlog <- cbind(W_log,wlog)
  
}

colnames(wlog) <- stock_namelist


# B. linear returns

wlin <- data.frame()
for (i in 1:length(stock_namelist)) {
  W_lin = periodReturn(prices[,i], period='weekly', subset = NULL , type = "arithmetic", leading=TRUE)
  wlin <- cbind(W_lin,wlin)
  
  
}

colnames(wlin) <- stock_namelist


# ------------------------------------------------------------------------------
# 3.Empirically investigate the stylized facts of financial market returns different 
#   data frequencies
# ------------------------------------------------------------------------------



#Skewness and Kurtosis

skewness(X_log)

kurtosis(X_log)

#Histogram of daily log returns

for (i in 1:length(stock_namelist)) {
  plotNormalHistogram(X_log[,i], prob = FALSE, main= c("Daily Log Returns of",colnames(X_log[,i])), xlab = 'Return')
}


#Histogram of daily linear returns

for (i in 1:length(stock_namelist)) {
  plotNormalHistogram(X_lin[,i], prob = FALSE, main= c("Daily Linear Returns of",colnames(X_lin[,i])), xlab = 'Return')
}


#Histogram of weekly log returns

for (i in 1:length(stock_namelist)) {
  plotNormalHistogram(wlog[,i], prob = FALSE, main= c("Weekly Log Returns of",colnames(X_log[,i])), xlab = 'Return')
}


#Histogram of weekly linear returns

for (i in 1:length(stock_namelist)) {
  plotNormalHistogram(wlin[,i], prob = FALSE, main= c("Weekly Linear Returns of",colnames(X_lin[,i])), xlab = 'Return')
}



# Stylised Facts (log returns)

for (i in 1:length(stock_namelist)) {
  boxPlot(X_log[,i], main = c("Box plot of Returns of ", colnames(X_log[,i])),
          col = "blue", cex = 0.5, pch = 19, title = FALSE)
  acf(X_log[,i], main = c("ACF of Returns", colnames(X_log[,i])), lag.max = 20, ylab = "",
      xlab = "", col = "blue", ci.col = "red")
  pacf(X_log[,i], main = c("PACF of Returns", colnames(X_log[,i])), lag.max = 20, ylab = "",
       xlab = "", col = "blue", ci.col = "red")
  
}


for (i in 1:length(stock_namelist)) {

X_log_series = timeSeries(X_log[,i])


X_log_abs = abs(X_log_series)
log_returns100 <- tail(sort(abs(series(X_log_series))), 100)[1]
idx <- which(series(X_log_abs) > log_returns100, arr.ind = TRUE)
log_returnsabs100 <- timeSeries(rep(0, length(X_log_series)),
                           charvec = time(X_log_series))
log_returnsabs100[idx, 1] <- X_log_abs[idx]
acf(X_log_abs, main = c("ACF of Absolute Returns", colnames(X_log[,i])), lag.max = 20,
    ylab = "", xlab = "", col = "blue", ci.col = "red")
pacf(X_log_abs, main = c( "PACF of Absolute Returns", colnames(X_log[,i])), lag.max = 20,
     ylab = "", xlab = "", col = "blue", ci.col = "red")
qqnormPlot(X_log[,i], main = c("QQ-Plot of Daily Returns", colnames(X_log[,i])), title = FALSE,
           col = "blue", cex = 0.5, pch = 19)
plot(log_returnsabs100, type = "h", main = c("Volatility Clustering", colnames(X_log[,i])),
     ylab = "", xlab = "", col = "blue")

}

#Weekly

for (i in 1:length(stock_namelist)) {
  
  wlog_series = timeSeries(wlog[,i])
  
  
  wlog_abs = abs(wlog_series)
  log_returns100 <- tail(sort(abs(series(wlog_series))), 100)[1]
  idx <- which(series(wlog_abs) > log_returns100, arr.ind = TRUE)
  log_returnsabs100 <- timeSeries(rep(0, length(wlog_series)),
                                  charvec = time(wlog_series))
  qqnormPlot(wlog[,i], main = c("QQ-Plot of Weekly Returns", colnames(wlog[,i])), title = FALSE,
             col = "blue", cex = 0.5, pch = 19)
}
  


# ------------------------------------------------------------------------------
# 4. Training data and test data
# ------------------------------------------------------------------------------

N <- ncol(X_log)  # number of stocks
T <- nrow(X_log)  # number of days

# First look at the prices of the stocks:
plot(prices/rep(prices[1, ], each = nrow(prices)), col = rainbow10equal, 
     legend.loc = "topleft", main = "Normalized prices")

# Divide the data into a training set and test set

T_trn <-  round(0.67*T)   # 2/3 of data
X_log_trn <- X_log[1:T_trn, ] #Training log returns
X_log_tst <- X_log[(T_trn+1):T, ] #Test log returns
X_lin_trn <- X_lin[1:T_trn, ] #Training linear returns
X_lin_tst <- X_lin[(T_trn+1):T, ] #Test linear returns

# A. Performance of the following Heuristic Portfolios

# Estimation of the expected return and covariance matrix (Log_returns)

mu_log <- colMeans(X_log_trn)
Sigma_log <- cov(X_log_trn)
Sigma_log_test <- cov(X_log_tst)

# ------------------------------------------------------------------------------
# Buy & Hold (B&H)
# ------------------------------------------------------------------------------

# Estimate mu and sigma from the in-sample simple returns:
mu <- colMeans(X_lin_trn)
Sigma <- cov(X_lin_trn)

# Buy & Hold simply means that we allocate the whole budget to one stock and we 
# stick to it. Since we have N=11 stocks in our universe, we can define N=11 different 
# B&H portfolios, which we will store as column vectors.

# a B&H portfolio is trivially the zero vector with a one on the stock held
w_BnH <- diag(N)
rownames(w_BnH) <- colnames(X_lin)
colnames(w_BnH) <- paste0("B&H - ", colnames(X_lin))
w_BnH


# compute the performance of those N=11 portfolios in the training data with the package PerformanceAnalytics
# compute returns of all B&H portfolios
ret_BnH <- xts(X_lin %*% w_BnH, index(X_lin))
ret_BnH_trn <- ret_BnH[1:T_trn, ]
ret_BnH_tst <- ret_BnH[-c(1:T_trn), ]
head(ret_BnH)


# Performance measures

# Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe Ratio
#Train Set
t(table.AnnualizedReturns(ret_BnH_trn, scale = 252, Rf = 0.0))

#Test Set
t(table.AnnualizedReturns(ret_BnH_tst, scale = 252, Rf = 0.0))


# To compute the wealth or cumulative P&L, we have two options: one assumes the 
# same quantity is repeateadly invested, whereas the other assumes reinvesting (compounding):

# Compute cumulative wealth

# A. same quantity is repeateadly invested
wealth_arith_BnH_trn <- 1 + cumsum(ret_BnH_trn)  # initial budget of 1$

# B. Assume reinvesting (compounding):
wealth_geom_BnH_trn <- cumprod(1 + ret_BnH_trn)  # initial budget of 1$

tail(wealth_geom_BnH_trn,2)


#PLOT CUMULATIVE RETURNS 

#Select the chosen stocks ( 6 = BOEING; 11 = BALL CORP) 
chart.CumReturns(ret_BnH_trn[, 6], 
                 main = paste(stock_namelist[6], "Buy & Hold performance (not compounded)", sep=": "),
                 geometric = FALSE, col='blue', wealth.index = TRUE)

chart.CumReturns(ret_BnH_trn[, 11], 
                 main = paste(stock_namelist[11], "Buy & Hold performance (compounded)", sep=": "), 
                 geometric = TRUE, col='magenta', wealth.index = TRUE)


#Plot all stocks BnH return
chart.CumReturns(ret_BnH, main = "Buy & Hold performance", 
                 wealth.index = TRUE, legend.loc = "topleft", colorset = rich10equal)

#Training Data 
chart.CumReturns(ret_BnH_trn, main = "Buy & Hold performance", 
                 wealth.index = TRUE, legend.loc = "topleft", colorset = rich10equal)


# Combined wealth index, period performance, and drawdown chart

# Note: Drawdown: Any time the cumulative returns dips below the maximum cumulative returns, 
#  it's a drawdown. Drawdowns are measured as a percentage of that maximum cumulative return
# , in effect, measured from peak equity.

charts.PerformanceSummary(ret_BnH_trn, main = "Buy & Hold performance", 
                          wealth.index = TRUE, colorset = rich10equal)

chart.Boxplot(ret_BnH_trn)

# Annualized risk and return
chart.RiskReturnScatter(ret_BnH_trn, symbolset = 21, bg = "red")


# ------------------------------------------------------------------------------
# EWP - Equally weighted Portfolio (11 assets)
# ------------------------------------------------------------------------------

w_EWP <- rep(1/N, N)
names(w_EWP) <- colnames(X_lin)
w_EWP


# ------------------------------------------------------------------------------
# QuintP - Quintile Portfolio
# ------------------------------------------------------------------------------
# Steps: 1) rank the N stocks
#        2) divide them into five parts
#        3) long the top part (and possibly short the bottom part)

# create portfolios

# Sort stocks according to:

#Mean
i1 <- sort(mu, decreasing = TRUE, index.return = TRUE)$ix

#Variance
i2 <- sort(mu/diag(Sigma), decreasing = TRUE, index.return = TRUE)$ix

#Standard Deviation
i3 <- sort(mu/sqrt(diag(Sigma)), decreasing = TRUE, index.return = TRUE)$ix

sort_stocks <- cbind(i1, i2, i3)
rownames(sort_stocks) <- colnames(X_lin)


#Pick top 2 assets and divide money equally among the top 2 and then select
#Meaning its equally weighted only among the top 2 assets not among all assets

w_QuintP_1 <- w_QuintP_2 <- w_QuintP_3 <- rep(0, N)
w_QuintP_1[i1[1:round(N/5)]] <- 1/round(N/5) #Mu rank
w_QuintP_2[i2[1:round(N/5)]] <- 1/round(N/5) #Variance rank
w_QuintP_3[i3[1:round(N/5)]] <- 1/round(N/5) #Stdev rank
w_QuintP <- cbind("QuintP (mu)"        = w_QuintP_1, 
                  "QuintP (mu/sigma2)" = w_QuintP_2, 
                  "QuintP (mu/sigma)"  = w_QuintP_3)
rownames(w_QuintP) <- colnames(X_lin)
w_QuintP


# ------------------------------------------------------------------------------
# GMRP - Global Maximum Portfolio
# ------------------------------------------------------------------------------
# Global maximum return portfolio (GMRP) 
# GMRP chooses the stock with the highest return during the in-sample period

i_max <- which.max(mu)
w_GMRP <- rep(0, N)
w_GMRP[i_max] <- 1
names(w_GMRP) <- colnames(X_lin)
w_GMRP

# ------------------------------------------------------------------------------
# COMPARISON OF ALL HEURISTIC MODELS
# ------------------------------------------------------------------------------

# put together all portfolios

w_heuristic <- cbind("EWP" = w_EWP, w_QuintP, "GMRP" = w_GMRP)
round(w_heuristic, digits = 2)

barplot(t(w_heuristic), col = rainbow8equal[1:5], legend = colnames(w_heuristic), beside = TRUE,
        main = "Portfolio allocation of heuristic portfolios", xlab = "stocks", ylab = "dollars")


# Then we can compare the performance (in-sample vs out-of-sample):

# compute returns of all portfolios
ret_heuristic <- xts(X_lin %*% w_heuristic, index(X_lin))
ret_heuristic$`QuintP (mu/sigma2)` <- NULL  # remove since it coincides with "QuintP (mu/sigma)"
ret_heuristic_trn <- ret_heuristic[1:T_trn, ]        # training set
ret_heuristic_tst <- ret_heuristic[-c(1:T_trn), ]    # test set

# Performance metrics
t(table.AnnualizedReturns(ret_heuristic_trn))

t(table.AnnualizedReturns(ret_heuristic_tst))

#Plot
# Let's plot the wealth evolution (cumulative PnL) over time:
{ chart.CumReturns(ret_heuristic, main = "Cumulative return of heuristic portfolios", 
                   wealth.index = TRUE, legend.loc = "topleft", colorset = rich8equal)
  addEventLines(xts("training", index(X_lin[T_trn])), srt=90, pos=2, lwd = 2, col = "darkblue") }

charts.PerformanceSummary(ret_heuristic, main = "Performance of heuristic portfolios", 
                          wealth.index = TRUE, colorset = rich8equal)

# Finally, we can plot the risk-return scatter plot (training):
chart.RiskReturnScatter(ret_heuristic_trn, symbolset = 21, bg = "red",
                        main = "Annualized Return and Risk (in-sample)")

# ------------------------------------------------------------------------------
# 4. B), C), D) 
# ------------------------------------------------------------------------------

set.seed(5)
stock_namelist2 <- c("ADBE", "AMGN", "JPM",  "BBY", "DIS", "BA",  "PG", "CVX", "AES", "BXP", "BLL")

Stocks <- stockDataDownload(stock_symbols = stock_namelist2,
                           from = "2008-12-01", to = "2018-12-01")

my_dataset_list <- financialDataResample(Stocks, 
                                         N_sample = 11,     # Desired number of financial instruments in each resample
                                         T_sample = 252*2,  # Desired length of each resample (consecutive samples with a random initial time)
                                         num_datasets = 10) # Number of resampled datasets

# Markowitz MVP

Markowitz_portfolio_fun <- function(dataset, lambda=0.5, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  mu    <- colMeans(X)  # compute mean vector
  Sigma <- cov(X)       # compute the SCM
  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - lambda*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}

# GMVP (don't allow short-selling)

GMVP_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- abs(w)/sum(abs(w))
  return(w)
}


# MSRP 
MSRP <- function(mu, Sigma) {
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  result <- CVXR::solve(prob)
  w <- as.vector(result$getValue(w_)/sum(result$getValue(w_)))
  names(w) <- colnames(Sigma)
  return(w)
}

w_MSRP <- MSRP(mu,Sigma)
round(w_MSRP,2)

#not very diversified portfolio

barplot(w_MSRP, col = rainbow8equal[1:8], main = "Maximum Sharpe ratio portfolio (MSRP)", 
        xlab = "stocks", ylab = "weights", beside = TRUE, legend = colnames(w_MSRP))

######################
# 1. Quintile portfolio

quintile_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  N <- ncol(X)
  # design quintile portfolio
  ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
  w <- rep(0, N)
  w[ranking[1:round(N/5)]] <- 1/round(N/5)
  return(w)
}


portfolios <- list("Quintile"  = quintile_portfolio_fun,
                   "GMVP"      = GMVP_portfolio_fun,
                   "Markowitz" = Markowitz_portfolio_fun)



bt <- portfolioBacktest(portfolios, my_dataset_list,
                        benchmarks = "uniform",
                        rebalance_every = 21,
                        optimize_every = 21,
                        lookback = 252,
                        shortselling = F)

names(bt)

#Portfolios Weights (for all dates)
bt$GMVP$`dataset 1`$w_bop
bt$Quintile$`dataset 1`$w_bop
bt$Markowitz$`dataset 1`$w_bop
bt$uniform$`dataset 1`$w_bop


# The sum of the weights should be equal to 1 (invest all of the money)
sum(bt$Markowitz$`dataset 1`$w_bop[1,])
sum(bt$Quintile$`dataset 1`$w_bop[1,])
sum(bt$GMVP$`dataset 1`$w_bop[1,])
sum(bt$uniform$`dataset 1`$w_bop[1,])

# Select several performance measures for each portfolio (dataset 1):

# Performance Criteria
bt$Markowitz$`dataset 1`$performance
bt$Quintile$`dataset 1`$performance
bt$GMVP$`dataset 1`$performance
bt$uniform$`dataset 1`$performance

# Print selected performance criteria (sharpe ratio, max drawdown,...) of each portfolio

backtestSelector(bt, portfolio_name = 'Quintile',
                 measures = c('Sharpe ratio', 'max drawdown'))

backtestSelector(bt, portfolio_name = 'GMVP',
                 measures = c('Sharpe ratio', 'max drawdown'))

backtestSelector(bt, portfolio_name = 'Markowitz',
                 measures = c('Sharpe ratio', 'max drawdown'))

backtestSelector(bt, portfolio_name = 'uniform',
                 measures = c('Sharpe ratio', 'max drawdown'))


# Tables of several performance measures of the portfolios (classified by performance criteria):

# Portfolio's performance in tables 
ppt <- backtestTable(bt, measures = c('Sharpe ratio', 'max drawdown'))

# Summary of performance measures:
res_sum <- backtestSummary(bt, summary_fun = median)
names(res_sum)

res_sum$performance_summary

apply(ppt$`Sharpe ratio`,2,median)



# ------------------------------------------------------------------------------
# Plotting your results
# ------------------------------------------------------------------------------

# Performance table:

#Rank portfolios considering a certain criteria (in this case is Sharpe Ratio but you can change)

summaryTable(res_sum, type = "DT", order_col = "Sharpe ratio", order_dir = "desc")


# Barplot (summaryTable() information in a visual way):
summaryBarPlot(res_sum, measures = c("Sharpe ratio", "max drawdown"))


# BoxPlot:
backtestBoxPlot(bt, measure = "Sharpe ratio", type = c("ggplot2"))

# Cumulative return or wealth plot of a single backtest:

#Chart of the cumulative returns on wealth for a single backtest
backtestChartCumReturn(bt, c("Quintile", "GMVP","uniform", "Markowitz"), dataset_num = 4)

backtestChartDrawdown(bt, c("Quintile", "GMVP","uniform", "Markowitz"), type = c("ggplot2"))



# Weights chart
backtestChartStackedBar(bt, "GMVP", legend = TRUE)


# ------------------------------------------------------------------------------
# E.Empirically investigate the performance of the following Risk-Based Portfolios 
#   
# ------------------------------------------------------------------------------
# RBP Portfolio definitions
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# A. Inverse volatility portfolio (IVP)
# ------------------------------------------------------------------------------

#Training Dataset 

#Only argument is the VCV matrix 

IVP <- function(Sigma) {
  sigma <- sqrt(diag(Sigma)) #Extract the stdev of the 11 assets
  w <- 1/sigma
  w <- w/sum(w) #Sum of the weights should be 100%
  return(w)
}

# this function can now be used as
w_IVP <- IVP(Sigma)

barplot(w_IVP, col = rainbow8equal[1:8], main = "Inverse volatility portfolio (IVP)", 
        xlab = "stocks", ylab = "weights", beside = TRUE, legend = colnames(w_IVP))


# ------------------------------------------------------------------------------
# Risk Parity Portfolio (RPP)
# ------------------------------------------------------------------------------

#The only input is the VCV matrix 
rpp <- riskParityPortfolio(Sigma)
names(rpp)

# portfolio weights

rpp$w #how much money we're going to allocate to each asset

round(rpp$w,2)
round(w_IVP,2) #IVP portfolio just to see the previous method

# Relative risk contribution (RRC)
rpp$relative_risk_contribution #1 out of 11 

# plot (% of total amount we're going to allocate - dollar value)
w_all <- NULL
w_all <- cbind(w_all,rpp$w)
barplot(t(w_all), col = rainbow8equal[1:8],
        main = "Risk Parity Portfolio (RPP)", xlab = "stocks", ylab = "dollars", beside = TRUE, 
        legend = colnames(w_all))


# compute risk contributions (relative risk contribution)
risk_all <- w_all
risk_all <- rpp$w * (Sigma %*% rpp$w)
RRC_all <- sweep(risk_all, MARGIN = 2, STATS = colSums(risk_all), FUN = "/")  # normalize each column

# plot
barplot(t(RRC_all), col = rainbow8equal[1:8],
        main = "Relative risk contribution", xlab = "stocks", ylab = "risk", beside = TRUE, 
        legend = colnames(RRC_all))



# ------------------------------------------------------------------------------
# B. Most diversified portfolio (MDP)
# ------------------------------------------------------------------------------

# B.1 Maximum Sharpe ratio portfolio (MSRP)
# Note: rewritten in convex form as the dual problem: Minimize varP(w1) s.t. E(Rp)=1, w1=w1/sum(w1)

MSRP <- function(mu, Sigma) {
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  result <- CVXR::solve(prob)
  w <- as.vector(result$getValue(w_)/sum(result$getValue(w_)))
  names(w) <- colnames(Sigma)
  return(w)
}

w_MSRP <- MSRP(mu,Sigma)
round(w_MSRP,2)

#not very diversified portfolio

barplot(w_MSRP, col = rainbow8equal[1:8], main = "Maximum Sharpe ratio portfolio (MSRP)", 
        xlab = "stocks", ylab = "weights", beside = TRUE, legend = colnames(w_MSRP))


# MDP weights
w_MDP <- MSRP(mu = sqrt(diag(Sigma)), Sigma)
round(w_MDP,2)

barplot(w_MDP, col = rainbow8equal[1:8], main = "Most diversified portfolio (MDP)", 
        xlab = "stocks", ylab = "weights", beside = TRUE, legend = colnames(w_MDP))

# ------------------------------------------------------------------------------
# C. Maximum decorrelation portfolio (MDCP)
# ------------------------------------------------------------------------------

# create function for MDCP based on GMVP

# create function for GMVP (MDCP is a variation of GMVP)

GMVP <- function(Sigma) {
  w <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w, Sigma)), 
                  constraints = list(w >= 0, sum(w) == 1)) #Short-selling constraint and sum of weights =1
  result <- CVXR::solve(prob)
  w <- as.vector(result$getValue(w))
  names(w) <- colnames(Sigma)
  return(w)
}

# this function can now be used as
w_GMVP <- GMVP(Sigma)
round(w_GMVP,2)

barplot(w_GMVP, col = rainbow8equal[1:8], main = "Global minimum variance portfolio (GMVP)", 
        xlab = "stocks", ylab = "weights", beside = TRUE, legend = colnames(w_GMVP))


# MDCP portfolio
MDCP <- function(Sigma) {
  C <- diag(1/sqrt(diag(Sigma))) %*% Sigma %*% diag(1/sqrt(diag(Sigma)))
  colnames(C) <- colnames(Sigma)
  return(GMVP(Sigma = C))
}

# this function can now be used as
w_MDCP <- MDCP(Sigma)
round(w_MDCP,2)

barplot(w_MDCP, col = rainbow8equal[1:8], main = "Maximum decorrelation portfolio (MDCP)", 
        xlab = "stocks", ylab = "weights", beside = TRUE, legend = colnames(w_MDCP))

# ------------------------------------------------------------------------------
# F. COMPARE THE PERFORMANCE OF THE HEURISTIC PORTFOLIOS W/ RISK-BASED PORTFOLIOS 
#    AND W/ MVP, GMVP, MSRP
# ------------------------------------------------------------------------------

w_heuristic
w_Markowitz <- cbind( "GMVP"   = w_GMVP, "IVP"    = w_IVP, "MSRP"   = w_MSRP, "MDP"    = w_MDP, 
                     "MDCP"   = w_MDCP)


w_comparison <- cbind(w_heuristic, w_Markowitz)


#Computing returns

ret_all <- xts(X_lin %*% w_comparison, index(X_lin))

ret_all_trn <- ret_all[1:T_trn, ]
ret_all_tst <- ret_all[-c(1:T_trn), ]

#Performance Training Data

t(table.AnnualizedReturns(ret_all_trn)) %>% as.data.frame() %>% arrange(-`Annualized Sharpe (Rf=0%)`)

#Performance Test Data

t(table.AnnualizedReturns(ret_all_tst)) %>% as.data.frame() %>% arrange(-`Annualized Sharpe (Rf=0%)`)



#Plot
# Let's plot the wealth evolution (cumulative PnL) over time:
{ chart.CumReturns(ret_all_trn, main = "Cumulative return of all portfolios", 
                   wealth.index = TRUE, legend.loc = "topleft", colorset = rich8equal)
  addEventLines(xts("training", index(X_lin[T_trn])), srt=90, pos=2, lwd = 2, col = "darkblue") }

charts.PerformanceSummary(ret_all_trn, main = "Performance of all portfolios", 
                          wealth.index = TRUE, colorset = rich8equal)

# Finally, we can plot the risk-return scatter plot (training set):
chart.RiskReturnScatter(ret_all_trn, symbolset = 21, bg = "red",
                        main = "Annualized Return and Risk (in-sample)")

# Finally, we can plot the risk-return scatter plot (testset):
chart.RiskReturnScatter(ret_all_tst, symbolset = 21, bg = "red",
                        main = "Annualized Return and Risk (in-sample)")


charts.PerformanceSummary(ret_all_tst, main = "Performance of all portfolios", 
                          wealth.index = TRUE, colorset = rich8equal)


