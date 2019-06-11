# # # # # # # # # # # # # # # # #
# Libraries Needed              #
#                               #
# 1. readxl: To use read_excel  #
# 2. tidyverse: To use pipe %>% #
# 3. dplyr: To use bind_rows    #
# # # # # # # # # # # # # # # # #

library(readxl)
library(tidyverse)
library(dplyr)


              # # # # # # # # # # # # # # # #
              # STEP 1: Import needed data  #
              # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data                                                    #
#                                                         #
# Prices                                                  #
# 1. 5 years daily levels of PSEi and prices of 10 stocks #
# 2. 5 years daily 91-day Philippine Treasury bill rates  #
# 3. Arranged in ascending order                          #
#                                                         #
# Dividends                                               #
# 1. Dividend amount, NOT dividend yield                  #
# 2. Same years covered in data                           #
# 3. Arranged in ascending order                          #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Get training set from Excel
data <- read_excel("data.xlsx")
dividends <- read_excel("dividends.xlsx")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data for Validation                                     #
#                                                         #
# Prices                                                  #
# 1. 1 year daily levels of PSEi and prices of 10 stocks  #
# 2. 1 year daily 91-day Philippine Treasury bill rates   #
# 3. Arranged in ascending order                          #
#                                                         #
# Dividends                                               #
# 1. Dividend amount, NOT dividend yield                  #
# 2. Same years covered in data                           #
# 3. Arranged in ascending order                          #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# get validation set from Excel
data_validation <- read_excel("data_validation.xlsx")
dividends_validation <- read_excel("dividends_validation.xlsx")


              # # # # # # # # # # # # #
              # STEP 2: Explore data  #
              # # # # # # # # # # # # #

# Graph of the PSE index
ggplot(data = data, aes(x = date, y = psei)) + geom_line(size = 0.5)

# Graph of tel
ggplot(data = data, aes(x = date, y = tel)) + geom_line(size = 0.5)

# Graph of mer
ggplot(data = data, aes(x = date, y = mer)) + geom_line(size = 0.5)

# Graphs of data of other stocks
df <- data %>% select(date, bdo, bpi, jfc, secb, smc, urc) %>% gather(key = "stocks", value = "price", -date)

ggplot(df, aes(x = date, y = price)) + geom_line(aes(color = stocks), size = 1) + theme_minimal()

# Graphs of data stocks with low price
df <- data %>% select(date, ali, dmc) %>% gather(key = "stocks", value = "price", -date)

ggplot(df, aes(x = date, y = price)) + geom_line(aes(color = stocks), size = 1) + theme_minimal()


              # # # # # # # # # # # # # # # # # # # # #
              # STEP 3: Convert daily data to monthly #
              # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # #
# Monthly Data  #
# # # # # # # # #

# Data: Get year, month, day for each date
data$year <- as.numeric(format(data$date, "%Y"))
data$month <- as.numeric(format(data$date, "%m"))
data$day <- as.numeric(format(data$date, "%d"))

# Data: Get combination of year, month
data$ym <- paste0(data$year, data$month)

# Monthly: Get only month-end dates
data_monthly <- aggregate(day ~ year + month, data, max) %>% arrange(year)
data_monthly$date <- paste0(data_monthly$year, data_monthly$month, data_monthly$day)

# Monthly: Get combination of year, month
data_monthly$ym <- paste0(data_monthly$year, data_monthly$month)

# Dividends: Get year, month, day for each date
dividends$year <- as.numeric(format(dividends$date, "%Y"))
dividends$month <- as.numeric(format(dividends$date, "%m"))

# Dividends: Get combination of year, month
dividends$ym <- paste0(dividends$year, dividends$month)

# Dividends: Replace NA with 0
dividends[is.na(dividends)] <- 0

# Dividends: Collect all dividends for each year-month combination
dividends <- dividends %>% select(-date, -year, -month)
dividends <- dividends %>% group_by(ym) %>% summarise_all(funs(sum))

# Data: Reformat date column
data$date <- paste0(data$year, data$month, data$day)
data <- data %>% select(-day)

# Monthly: Get data for month-end dates only
data_monthly <- merge(data, data_monthly[, c("date", "ym")])

# Monthly: Add dividends to monthly data
data_monthly <- bind_rows(data_monthly, dividends) %>% select(-date) %>% group_by(ym) %>% summarise_all(funs(sum(., na.rm = TRUE)))

# Monthly: Arrange chronologically
data_monthly <- data_monthly[order(data_monthly$year, data_monthly$month), ]
data_monthly <- data_monthly %>% select(-year, -month)


# # # # # # # # # # # # # # # #
# Monthly Data for Validation #
# # # # # # # # # # # # # # # #

# Data: Get year, month, day for each date
data_validation$year <- as.numeric(format(data_validation$date, "%Y"))
data_validation$month <- as.numeric(format(data_validation$date, "%m"))
data_validation$day <- as.numeric(format(data_validation$date, "%d"))

# Data: Get combination of year, month
data_validation$ym <- paste0(data_validation$year, data_validation$month)

# Monthly: Get only month-end dates
data_validation_monthly <- aggregate(day ~ year + month, data_validation, max) %>% arrange(year)
data_validation_monthly$date <- paste0(data_validation_monthly$year, data_validation_monthly$month, data_validation_monthly$day)

# Monthly: Get combination of year, month
data_validation_monthly$ym <- paste0(data_validation_monthly$year, data_validation_monthly$month)

# Dividends: Get year, month, day for each date
dividends_validation$year <- as.numeric(format(dividends_validation$date, "%Y"))
dividends_validation$month <- as.numeric(format(dividends_validation$date, "%m"))

# Dividends: Get combination of year, month
dividends_validation$ym <- paste0(dividends_validation$year, dividends_validation$month)

# Dividends: Replace NA with 0
dividends_validation[is.na(dividends_validation)] <- 0

# Dividends: Collect all dividends for each year-month combination
dividends_validation <- dividends_validation %>% select(-date, -year, -month)
dividends_validation <- dividends_validation %>% group_by(ym) %>% summarise_all(funs(sum))

# Data: Reformat date column
data_validation$date <- paste0(data_validation$year, data_validation$month, data_validation$day)
data_validation <- data_validation %>% select(-day)

# Monthly: Get data for month-end dates only
data_validation_monthly <- merge(data_validation, data_validation_monthly[, c("date", "ym")])

# Monthly: Add dividends to monthly data
data_validation_monthly <- bind_rows(data_validation_monthly, dividends_validation) %>% select(-date) %>% group_by(ym) %>% summarise_all(funs(sum(., na.rm = TRUE)))

# Monthly: Arrange chronologically
data_validation_monthly <- data_validation_monthly[order(data_validation_monthly$year, data_validation_monthly$month), ]
data_validation_monthly <- data_validation_monthly %>% select(-year, -month)


              # # # # # # # # # # # # # # # # # # #
              # STEP 4: Compute monthly returns   #
              # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monthly Returns for Data                                                  #
#                                                                           #
# 1. 91-day Treasury bill rates: Annualized rates; don't do anything first  #
# 2. PSEi, stocks: Percentage change from month-end i to month-end i + 1    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# PSEi monthly returns
r_m <- ((as.data.frame(data_monthly$psei) - mutate_all(as.data.frame(data_monthly$psei), lag))/mutate_all(as.data.frame(data_monthly$psei), lag))[-1, ]*100

# Stocks monthly returns
r <- ((as.data.frame(data_monthly[-1:-3]) - mutate_all(as.data.frame(data_monthly[-1:-3]), lag))/mutate_all(as.data.frame(data_monthly[-1:-3]), lag))[-1, ]*100


# # # # # # # # # # # # # # # # # #
# Monthly Returns for Validation  #
# # # # # # # # # # # # # # # # # #

# PSEi monthly returns
r_validation_m <- ((as.data.frame(data_validation_monthly$psei) - mutate_all(as.data.frame(data_validation_monthly$psei), lag))/mutate_all(as.data.frame(data_validation_monthly$psei), lag))[-1, ]*100

# Stocks monthly returns
r_validation <- ((as.data.frame(data_validation_monthly[-1:-3]) - mutate_all(as.data.frame(data_validation_monthly[-1:-3]), lag))/mutate_all(as.data.frame(data_validation_monthly[-1:-3]), lag))[-1, ]*100


              # # # # # # # # # #
              # STEP 5: Model 1 #
              # # # # # # # # # #

# Suppose an investor has PHP100,000 to invest

# Equal investment of PHP10,000 on each of the 10 stocks in the list
portfolio1_stocks <- (100000/10)/data_validation[1, 4:13]
portfolio1_start <- portfolio1_stocks*data_validation[1, 4:13]

# Portfolio 1 risk
portfolio1_w <- rep(1/10, 10)
portfolio1_risk <- sqrt(t(portfolio1_w)%*%cov(r_validation)%*%portfolio1_w)

# Track performance of Portfolio 1
portfolio1_progress <- portfolio1_start
for (i in 1:nrow(r_validation)){
  portfolio1_progress[i+1, ] <- portfolio1_progress[i, ]*(1+r_validation[i, ]/100)
}

# Portfolio 1 return
portfolio1_end <- portfolio1_progress[nrow(r_validation)+1, ]
portfolio1_return <- (sum(portfolio1_end)/sum(portfolio1_start) - 1)*100

# PSEi return
psei_return <- (prod(1+r_validation_m/100) - 1)*100

# Place results in a table
results <- data_frame(Portfolio = "Portfolio 1", Model = "Equal Weights", Risk = as.numeric(portfolio1_risk), Return = portfolio1_return, PSEi = psei_return)


# # # # # # # # # # # # # #
# Explore validation data #
# # # # # # # # # # # # # #

# Reload the original validation set
data_validation <- read_excel("data_validation.xlsx")

# Graph of the PSE index
ggplot(data = data_validation, aes(x = date, y = psei)) + geom_line(size = 0.5)

# Graph of tel
ggplot(data = data_validation, aes(x = date, y = tel)) + geom_line(size = 0.5)

# Graph of mer
ggplot(data = data_validation, aes(x = date, y = mer)) + geom_line(size = 0.5)

# Graphs of data of other stocks
df <- data_validation %>% select(date, bdo, bpi, jfc, secb, smc, urc) %>% gather(key = "stocks", value = "price", -date)

ggplot(df, aes(x = date, y = price)) + geom_line(aes(color = stocks), size = 1) + theme_minimal()

# Graphs of data stocks with low price
df <- data_validation %>% select(date, ali, dmc) %>% gather(key = "stocks", value = "price", -date)

ggplot(df, aes(x = date, y = price)) + geom_line(aes(color = stocks), size = 1) + theme_minimal()


              # # # # # # # # # # # # # # # # # # # # #
              # STEP 6: Model 2 - Single-Index Model  #
              # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Variables Used                                                #
#                                                               #
# 1. R_f: Average risk-free rate (monthly, %)                   #
# 2. R: Average return on stocks (monthly, %)                   #
# 3. sigma_m: Risk/Volatility of market index returns (monthly) #
# 4. R_m: Average return on market index (monthly, %)           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 0.8: Front-loading of 20% tax
R_f <- 0.8*mean(data_monthly$tbill)/3

# 1.2: 20% tax
R <- 1.2*colMeans(r)

# Market risk and return
sigma_m <- sd(r_m)
R_m <- mean(r_m)


# # # # # # # # # # # # # # # # # # # # # # # # # #
# Linear Regression                               #
#                                                 #
# 1. Regress (stock returns) on (PSEi returns)    #
# 2. beta: Coefficient of stock returns           #
# 3. sigma_mi2: Absolute unsystematic risk        #
# 4. R2: Percentage systematic risk of total risk #
# # # # # # # # # # # # # # # # # # # # # # # # # #

# List of stocks
dep_var <- colnames(data[ , -1:-3])
dep_var <- dep_var[-11:-13]

# Tabulate PSEi and stock returns for regression
data_lin_reg <- mutate(psei = r_m, r)

# Perform linear regression: PSEi vs stock
lin_reg <- expand.grid(ind_var = "psei", dep_var = dep_var) %>% mutate(reg = paste(dep_var, "~", "psei")) %>% group_by(reg) %>% mutate(beta = lm(reg, data_lin_reg)$coefficients[2], sigma_mi2 = anova(lm(reg, data_lin_reg))["Residuals", "Sum Sq"]/sum(anova(lm(reg, data_lin_reg))$Df), R2 = summary(lm(reg, data_lin_reg))$r.squared) %>% ungroup()

# Determine variable values from linear regression results
beta <- lin_reg$beta
sigma_mi2 <- lin_reg$sigma_mi2
R2 <- lin_reg$R2


# # # # # # # # #
# Treynor Ratio #
# # # # # # # # #

T <- (R - R_f)/beta


# # # # # # #
# Cut-off C #
# # # # # # #

# Prepare for computation of initial cut-off variable c
sum1 <- beta*(R - R_f)/sigma_mi2
sum2 <- (beta^2)/sigma_mi2

# Organize computatations; Rank stocks according to Treynor ratio
summary <- data.frame(stock = dep_var, R, beta, sigma_mi2, T, sum1, sum2) %>% arrange(desc(T))
c <- NULL
for(i in 1:nrow(summary)){
  c[i] <- ((sigma_m^2)*sum(summary$sum1[1:i]))/(1 + (sigma_m^2)*sum(summary$sum2[1:i]))
}
summary <- summary %>% mutate(c)

# Compare Treynor ratio with c: stocks with T > c will be included in portfolio
c_count <- sum(summary$T > summary$c)

# Last c is the final cut-off variable C
C <- c[c_count]


# # # # # #
# Weights #
# # # # # #

# Remove stocks not to be included in the portfolio
index <- order(-data.frame(dep_var, T)$T)[1:c_count]
summary <- summary[1:c_count, -6:-7]

# Compute Z for recommended stocks
Z <- (summary$beta/(summary$sigma_mi2))*(summary$T - C)

# Compute weights
w <- Z/sum(Z)
summary <- summary %>% mutate("weight (%)" = w*100)

# Show recommended portfolio
ideal_portfolio <- data_frame(Stock = summary$stock, "Weight (%)" = summary$`weight (%)`)


# # # # # # # # # # # # # # # # # # # #
# Portfolio Expected Characteristics  #
# # # # # # # # # # # # # # # # # # # #

# Portfolio risk
beta_p <- sum(w*beta[index])
sigma_p <- sqrt((beta_p^2)*(sigma_m^2) + sum((w^2)*(sigma_mi2[index])))

# Portfolio return
R_p <- sum(w*R[index])

# Tabulate ideal results from single-index model
ideal_results <- data_frame(Model = "Single-Index", Risk = sigma_p, Return = R_p)


              # # # # # # # # # # # # # # # #
              # STEP 7: Model 2 Validation  #
              # # # # # # # # # # # # # # # #

# Invest PHP100,000 proportionally according to weights recommended by Model 2
portfolio2_amount <- 100000*(summary$`weight (%)`/100)
portfolio2_stocks <- portfolio2_amount/data_validation[1, index+3]
portfolio2_start <- portfolio2_stocks*data_validation[1, index+3]

# Portfolio 2 risk
portfolio2_w <- summary$`weight (%)`/100
portfolio2_risk <- sqrt(t(portfolio2_w)%*%cov(r_validation[ , index])%*%portfolio2_w)

# Track performance of Portfolio 2
portfolio2_progress <- portfolio2_start
for (i in 1:nrow(r_validation)){
  portfolio2_progress[i+1, ] <- portfolio2_progress[i, ]*(1+r_validation[i, index]/100)
}

# Portfolio 2 return
portfolio2_end <- portfolio2_progress[nrow(r_validation)+1, ]
portfolio2_return <- (sum(portfolio2_end)/sum(portfolio2_start) - 1)*100

# Place results in a table
results <- bind_rows(results, data_frame(Portfolio = "Portfolio 2", Model = "Single-Index", Risk = as.numeric(portfolio2_risk), Return = portfolio2_return, PSEi = psei_return))


              # # # # # # # # # #
              # STEP 8: Model 3 #
              # # # # # # # # # #

# Equal investment in the stocks recommended by Model 2
portfolio3_stocks <- (100000/length(index))/data_validation[1, index+3]
portfolio3_start <- portfolio3_stocks*data_validation[1, index+3]

# Portfolio 3 risk
portfolio3_w <- rep(1/length(index), length(index))
portfolio3_risk <- sqrt(t(portfolio3_w)%*%cov(r_validation[ , index])%*%portfolio3_w)

# Track performance of Portfolio 3
portfolio3_progress <- portfolio3_start
for (i in 1:nrow(r_validation)){
  portfolio3_progress[i+1, ] <- portfolio3_progress[i, ]*(1+r_validation[i, index]/100)
}

# Portfolio 3 return
portfolio3_end <- portfolio3_progress[nrow(r_validation)+1, ]
portfolio3_return <- (sum(portfolio3_end)/sum(portfolio3_start) - 1)*100

# Place results in a table
results <- bind_rows(results, data_frame(Portfolio = "Portfolio 3", Model = "Single-Index + Equal Weights", Risk = as.numeric(portfolio3_risk), Return = portfolio3_return, PSEi = psei_return))


              # # # # # # # # # # # # # #
              # STEP 9: Compare results #
              # # # # # # # # # # # # # #

# Print results comparing Models 1-3
results