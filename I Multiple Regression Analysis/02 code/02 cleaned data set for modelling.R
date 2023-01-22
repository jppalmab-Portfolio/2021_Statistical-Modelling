#Checking for Influential Data Points
cooks.distance(bestmodel)
par(mfrow=c(1,1))
influencePlot(model = bestmodel, scale = 3, main = "Influence Plot")

# StudRes     Hat  CookD
# 89    -4.50 0.00290 0.0114
# 331    2.59 0.12631 0.1925
# 454    2.56 0.07873 0.1107
# 678   -3.52 0.00635 0.0156

df2 = df
df2 = df2[-c(89,331,454,678),]

bestmodel2 = lm(log_Credit_Debt~log_Household_Income+Other_Debt+log_Debt_Income_ratio+Previous_Defaulted, data = df2)
summary(bestmodel)
summary(bestmodel2)

#Standard diagnostic plots
par(mfrow=c(2,2))
plot(bestmodel2)

#Details of the Fitted Model
fitted(bestmodel2)
residuals(bestmodel2)

#Checking for Independence of Errors
durbinWatsonTest(bestmodel)
durbinWatsonTest(bestmodel2)

#Checking for Multicollinearity
vif(bestmodel)
vif(bestmodel2)

#Checking for Influential Data Points
cooks.distance(bestmodel2)
par(mfrow=c(1,1))
influencePlot(model = bestmodel2, scale = 3, main = "Influence Plot")

#The check_model function in the performance function provides atttractive diagnostic plots
library(performance)
library(see)
library(patchwork)
check_model(bestmodel)
check_model(bestmodel2)
check_model(lm5)

library(car)
ncvTest(bestmodel)
bestmodel

plot(bestmodel)


#bestmodel3 = lm(log_Credit_Debt~log_Household_Income+log(Other_Debt)+log_Debt_Income_ratio+Previous_Defaulted, data = df2)
summary(bestmodel)
summary(bestmodel2)
#summary(bestmodel3)


##########

df$check = predict(bestmodel2, df)
plot(df$check)

cor(df$log_Credit_Debt, df$check)
plot(df$log_Credit_Debt, df$check)


