------------------------------------
##### Install Packages #####
------------------------------------
  
#install.packages("sampleSelection")
#install.packages("stargazer")
#install.packages("dplyr")
#install.packages("systemfit")
#install.packages(c("tidymodels","themis","knitr","ranger","vip","skimr","corrplot","ggridges"))
#install.packages("doParallel")
#install.packages('tidyverse')
#install.packages("CLVTools")

library(sampleSelection)
library(stargazer)
library(dplyr)
library(ggplot2)
library(systemfit)

library(tidyverse)
library(tidymodels)
library(themis)
library(knitr)
library(ranger)
library(doParallel)
library("vip")
library("skimr")
library("corrplot")
library("ggridges")
library(CLVTools)

------------------------------------
  ##### Import Data #####
------------------------------------
df_users <- read.csv("~/Data/df_userlevel.csv")
df_ts_users <- read.csv("~/Data/df_ts.csv")

------------------------------------
  ##### Final Processing Steps #####
------------------------------------

# Remove negative values 
df_ts_users$week_spent <- ifelse(df_ts_users$week_spent<0,0,df_ts_users$week_spent)
df_ts_users$cumul_spent <- ifelse(df_ts_users$cumul_spent<0,0,df_ts_users$cumul_spent)

------------------------------------
  ##### Summary statistics #####
------------------------------------
stargazer(df_ts_users,
          summary.stat = c("n","mean","p25","p75","sd","min","max"),
          type='text')

------------------------------------
  ##### Visualize a customer #####
------------------------------------
# Select one customer 
individual_customer_data <- subset(df_ts_users, userId == "8384e584009d0f09dfa51d97e2c3f57b")

# Create a new plotting window with three bar charts in a column
par(mfrow = c(3, 1))

# First Graph: Clicks (Bar Chart)
barplot(individual_customer_data$click_week, names.arg = individual_customer_data$weeks_customer,
        xlab = "Weeks as Customer", ylab = "Clicks", col = "black", main = "Clicks")

# Second Graph: Transactions (Bar Chart)
barplot(individual_customer_data$week_trans, names.arg = individual_customer_data$weeks_customer,
        xlab = "Weeks as Customer", ylab = "Transactions", col = "black", main = "Transactions",ylim=c(0,1))

# Third Graph: Mailings (Bar Chart)
barplot(individual_customer_data$week_mail, names.arg = individual_customer_data$weeks_customer,
        xlab = "Weeks as Customer", ylab = "Mailings", col = "black", main = "Mailings")

# Create histogram for number of mails 
hist(df_users$n_mails)

------------------------------------
  ##### Testing number of lags #####
------------------------------------
  
# Number of lags: 9 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) 
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) + lag(week_mail,8) + lag(week_mail,9) 
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) + lag(week_mail,8) + lag(week_mail,9) 
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) + lag(week_mail,8) + lag(week_mail,9) 

system_9_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_9_u <- systemfit(system_9_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 8 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) + lag(week_mail,8)
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) + lag(week_mail,8)
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) + lag(week_mail,8)

system_8_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_8_u <- systemfit(system_8_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 7
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) 
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7)
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) + lag(week_mail,7) 

system_7_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_7_u <- systemfit(system_7_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 6 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) 
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) 
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) + lag(week_mail,6) 

system_6_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_6_u <- systemfit(system_6_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 5
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) 
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) 
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) + lag(week_mail,5) 

system_5_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_5_u <- systemfit(system_5_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 4 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) 
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) 
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) + lag(week_mail,4) 

system_4_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_4_u <- systemfit(system_4_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 3 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) 
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3)
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3) 
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) + lag(week_mail,3)

system_3_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_3_u <- systemfit(system_3_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 2 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) 
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2)
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(week_mail,2) 

system_2_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_2_u <- systemfit(system_2_lags_u, method = "SUR", data = df_ts_users)

# Number of lags: 1 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) 
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) 
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1)

system_1_lags_u <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_1_u <- systemfit(system_1_lags_u, method = "SUR", data = df_ts_users)

------------------------------------
  ##### Performance Evaluation #####
------------------------------------

# Collect the metrics of all models
#AIC_values_u <- AIC(sur_1_u,sur_2_u,sur_3_u,sur_4_u,sur_5_u,sur_6_u,sur_7_u,sur_8_u,sur_9_u)
BIC_values_u <- BIC(sur_1_u,sur_2_u,sur_3_u,sur_4_u,sur_5_u,sur_6_u,sur_7_u,sur_8_u,sur_9_u)

#AIC_values_u$AIC_gain <- c(NA, diff(AIC_values_u$AIC))
BIC_values_u$BIC_gain <- c(NA, diff(BIC_values_u$BIC))

#AIC_values_u <- AIC_values_u %>% mutate(lags = 1:nrow(AIC_values_u))
BIC_values_u <- BIC_values_u %>% mutate(lags = 1:nrow(BIC_values_u))


BIC_values_u$lags <- as.integer(BIC_values_u$lags)

# Visualize the metrics 
BIC_plot_u <- ggplot(data = BIC_values_u[BIC_values_u$lags %in% 1:9, ], aes(x = lags, y = BIC)) +
  geom_line(size=0.7) +
  geom_point(size=1.6)+
  xlab("Number of Lags") +
  ylab("BIC")+
  scale_x_continuous(breaks = 1:9)
  

BIC_plot_u
ggsave("BIC_plot_u.jpg", plot = BIC_plot_u, dpi = 300, width = 8, height = 4, units = "in")


-----------------------------------------
  ##### Model Evaluation #####
-----------------------------------------
# Select a model 
summary(sur_3_u)

-----------------------------------------
  ##### Improved model: evaluation  #####
-----------------------------------------
df_ts_users$cumulative_lag_mail_squared <- I(lag(df_ts_users$cumul_mail, 1))^2
df_ts_users$lag_mail_squared <- I(lag(df_ts_users$week_mail))^2

  
# Replace transactions with the cumulative number of transactions
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) 
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(df_ts_users$cumul_mail, 1)
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(df_ts_users$cumul_mail, 1)
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag(df_ts_users$cumul_mail, 1)

system_v2_1 <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_v2_1 <- systemfit(system_v2_1, method = "SUR", data = df_ts_users)

# Evaluate the results
summary(sur_v2_1)
# Evaluate the BIC
BIC(sur_v2_1)

# Include quadratic term 
eq1 <- week_mail ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) 
eq2 <- click_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag_mail_squared+ lag(df_ts_users$cumul_mail, 1) + cumulative_lag_mail_squared
eq3 <- week_trans ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag_mail_squared + lag(df_ts_users$cumul_mail, 1) + cumulative_lag_mail_squared
eq4 <- unsubscribe_week ~ lag(click_week,1) + lag(week_trans,1) + lag(week_mail,1) + lag_mail_squared + lag(df_ts_users$cumul_mail, 1) + cumulative_lag_mail_squared

system_v2_2 <- list(eq1 = eq1, eq2 = eq2, eq3 = eq3,eq4=eq4)
sur_v2_2 <- systemfit(system_v2_2, method = "SUR", data = df_ts_users)

# Evaluate the results
summary(sur_v2_2)
# Evaluate the BIC
BIC(sur_v2_2)

-------------------------------------------------------------
  ##### Correlation check #####
-------------------------------------------------------------
# Make columns 
df_ts_users$mail_t_1 <- lag(df_ts_users$week_mail, 1)
df_ts_users$mail_t_2 <- lag(df_ts_users$week_mail, 2)
df_ts_users$mail_t_3 <- lag(df_ts_users$week_mail, 3)
df_ts_users$mail_total_lag <- lag(df_ts_users$cumul_mail, 1)
df_ts_users$click_t_1 <- lag(df_ts_users$click_week, 1)
df_ts_users$trans_t_1 <- lag(df_ts_users$week_trans, 1)

#Check correlations# 
df_ts_users_2 <- subset(df_ts_users, select = c(mail_t_1, mail_t_2,mail_t_3,mail_total_lag,click_t_1,trans_t_1))
cor(df_ts_users_2,use='pairwise.complete.obs')

library(corrplot)

df_ts_users_2 <- subset(df_ts_users, select = c(mail_t_1, mail_t_2, mail_t_3, mail_total_lag, click_t_1, trans_t_1))
cor_matrix <- cor(df_ts_users_2, use = 'pairwise.complete.obs')

corrplot(cor_matrix, method = 'color', type = 'upper', tl.col = 'black', addCoef.col = 'black')
