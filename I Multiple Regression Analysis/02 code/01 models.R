#Upload librarys
library(pacman)
p_load(ggplot2, dplyr,tidyverse, tidyr, dplyr, BiocManager, corplot, RColorBrewer,stargazer, leaps, texreg)

#Read the data
data = read.csv("01 data/Credit_v2.csv")

data = rename(data,c("Age" = "age", 
                     "Education" = "ed", 
                     "Years_Current_Employ" = "employ", 
                     "Years_Current_Address" = "address",
                     "Household_Income" = "income",
                     "Debt_Income_ratio" = "debtinc",
                     "Credit_Debt" = "creddebt",
                     "Other_Debt" = "othdebt",
                     "Previous_Defaulted" = "default"))

head(data)
names(data)

#Best (All) Subsets
BestCredDebt<-regsubsets(Credit_Debt~Age+Education+Years_Current_Employ+Years_Current_Address+Household_Income+Other_Debt+Debt_Income_ratio+Previous_Defaulted,data=data, nbest = 2)
subsets(BestCredDebt, statistic="adjr2")
par(mfrow=c(1,1))
plot(BestCredDebt, scale="adjr2")
#subsets(BestCredDebt, statistic="bic")

#Respecting the linearity
#Best (All) Subsets
BestCredDebt<-regsubsets(log(Credit_Debt)~Age+factor(Education)+Years_Current_Employ+Years_Current_Address+Household_Income+Other_Debt+Debt_Income_ratio+as.factor(Previous_Defaulted),data=data, nbest = 2)
subsets(BestCredDebt, statistic="adjr2")
par(mfrow=c(1,1))
plot(BestCredDebt, scale="adjr2")
#subsets(BestCredDebt, statistic="bic")


# Manual checking

lm0 = lm(Credit_Debt~Age+as.factor(Education)+Years_Current_Employ+Years_Current_Address+Household_Income+Other_Debt+Debt_Income_ratio+as.factor(Previous_Defaulted),data=data)
summary(lm0)

lm1 = lm(log(Credit_Debt)~Age+as.factor(Education)+Years_Current_Employ+Years_Current_Address+Household_Income+Other_Debt+Debt_Income_ratio+as.factor(Previous_Defaulted),data=data)
summary(lm1)

lm2 = lm(log(Credit_Debt)~Years_Current_Employ+Household_Income+Other_Debt+Debt_Income_ratio+as.factor(Previous_Defaulted),data=data)
summary(lm2)

lm3 = lm(log(Credit_Debt)~Years_Current_Employ+log(Household_Income)+Other_Debt+Debt_Income_ratio+as.factor(Previous_Defaulted),data=data)
summary(lm3)

lm4 = lm(log(Credit_Debt)~Years_Current_Employ+log(Household_Income)+Other_Debt+log(Debt_Income_ratio)+as.factor(Previous_Defaulted),data=data)
summary(lm4)

###########################
lm5 = lm(log(Credit_Debt)~log(Household_Income)+log(Other_Debt)+log(Debt_Income_ratio)+as.factor(Previous_Defaulted),data=data)
summary(lm5)

lm6 = lm(log(Credit_Debt)~log(Household_Income)+Other_Debt+log(Debt_Income_ratio),data=data)
summary(lm6)

texreg(list(lm1, lm2, lm3, lm4, lm4_1),
       caption.above = T, caption = "Manual Backforward modeling",
       label = "regtab1", 
       float.pos = "h")



# #POlyexamples
# lm5 = lm(log(Credit_Debt)~Years_Current_Employ+Household_Income+I(Household_Income^2)+Other_Debt+I(Other_Debt^2)+log(Debt_Income_ratio)+as.factor(Previous_Defaulted),data=data)
# summary(lm5)
# 
# str(data)
# 
# lm6 = lm(log(Credit_Debt)~Years_Current_Employ+Household_Income+I(Household_Income^2)+Other_Debt+log(Debt_Income_ratio)+as.factor(Previous_Defaulted),data=data)
# summary(lm6)


texreg(list(lm5, lm6),
       caption.above = T, caption = "Modeling using Polynomial form",
       label = "regtab2", 
       float.pos = "h")

#with education as a factor
# lm7 = lm(log(Credit_Debt)~as.factor(Education)+Years_Current_Employ+Household_Income+I(Household_Income^2)+Other_Debt+I(Other_Debt^2)+log(Debt_Income_ratio)+as.factor(Previous_Defaulted),data=data)
# summary(lm7)
# lm8 = lm(log(Credit_Debt)~Years_Current_Employ+Household_Income+I(Household_Income^2)+Other_Debt+I(Other_Debt^2)+log(Debt_Income_ratio)+as.factor(Previous_Defaulted),data=data)
# summary(lm8) #without

cor(data$Credit_Debt, data$Debt_Income_ratio)
m = data %>% select(Credit_Debt, Other_Debt, Household_Income, Debt_Income_ratio)
cor(m)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#lm.beta function provides standardised coefficients
library(lm.beta)
lm.beta(lm5)

#DIAGNOSTICS
lm4_1 = lm(log(Credit_Debt)~log(Household_Income)+Other_Debt+log(Debt_Income_ratio)+as.factor(Previous_Defaulted),data=data)
summary(lm4_1)

df = as.data.frame(cbind(log_Credit_Debt, log_Household_Income, Other_Debt, log_Debt_Income_ratio, Previous_Defaulted))
df$log_Credit_Debt = log(data$Credit_Debt)
df$log_Household_Income = log(data$Household_Income)
df$Other_Debt = data$Other_Debt
df$log_Debt_Income_ratio = log(data$Debt_Income_ratio)
df$Previous_Defaulted = as.factor(data$Previous_Defaulted)

bestmodel = lm(log_Credit_Debt~log_Household_Income+Other_Debt+log_Debt_Income_ratio+Previous_Defaulted, data = df)
summary(lm4_1)
summary(bestmodel)

#Standard diagnostic plots
par(mfrow=c(2,2))
plot(bestmodel)

#Details of the Fitted Model
fitted(bestmodel)
residuals(bestmodel)

#Checking for Independence of Errors
durbinWatsonTest(lm4_1)
durbinWatsonTest(bestmodel)

#Checking for Multicollinearity
vif(bestmodel)

#Checking for Influential Data Points
cooks.distance(bestmodel)
par(mfrow=c(1,1))
influencePlot(model = bestmodel, scale = 3, main = "Influence Plot")

#The check_model function in the performance function provides atttractive diagnostic plots
library(performance)
library(see)
library(patchwork)

check_model(bestmodel)



