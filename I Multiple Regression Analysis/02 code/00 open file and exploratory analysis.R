rm(list = ls())

#Upload librarys
library(pacman)
p_load(ggplot2, dplyr,tidyverse, tidyr, dplyr, BiocManager, corplot, corrplot, RColorBrewer,stargazer)

#Read the data
data = read.csv("01 data/Credit_v2.csv")

head(data)

# we define our DV as creddebt
# and the other is the set of IV that we are evaluating

#check NA values
data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

#Clean de data set
names(data)

data = rename(data,c("Age" = "age", 
                     "Education" = "ed", 
                     "Years_Current_Employ" = "employ", 
                     "Years_Current_Address" = "address",
                     "Household_Income" = "income",
                     "Debt_Income_ratio" = "debtinc",
                     "Credit_Debt" = "creddebt",
                     "Other_Debt" = "othdebt",
                     "Previous_Defaulted" = "default"))

names(data)


# general viewing of the dataset
str(data)
summary(data)
summary(data[,-9])

?stargazer
stargazer(data,
          title = "Statistical Descriptive",
          digits = 2,
          label = "tab1")

#HISTOGRAM
data_IV = select(data, -Credit_Debt)

data_IV %>% gather() %>% head()
ggplot(gather(data_IV), aes(value)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# Household Income
hist(data$Household_Income)
hist(log(data$Household_Income))

#employ
hist(data$Years_Current_Employ)
hist(data$Education)

####### Check scale in debts and income
summary(data$Household_Income) # in thousands
summary(data$Debt_Income_ratio) #*100
summary(data$Other_Debt) # *100
summary(data$Credit_Debt) # *100

# It is neccesary to re scalar all the debt and income to an exactly the same measure,
# it will make easier the interpretation

summary(data$Household_Income) # in thousands
summary(data$Debt_Income_ratio) # porcentage
summary(data$Other_Debt*1000) # *1000
summary(data$Credit_Debt*1000) # *1000

# DV Histogram
hist(data$Credit_Debt) #long-tail distribution
summary(data$Credit_Debt)
summary(data$Credit_Debt*1000)

?hist
hist(data$Credit_Debt, main = "",xlab = "Credit Card Debt")
hist(data$Credit_Debt*1000, main="", xlab = "Credit Card Debt")
hist(log(data$Credit_Debt*1000), main="", xlab = "Log Credit Card Debt") #quickly we get that we have a problem with the distribution of the DV


##############################################
# New Data frame
##############################################
#head(data)
#df = data
#df$Debt_Income_ratio = df$Debt_Income_ratio

#df$Household_Income = df$Household_Income*1000
#df$Other_Debt = df$Other_Debt*1000
#df$Credit_Debt = df$Credit_Debt*1000

#df$ratio = 100* (df$Credit_Debt + df$Other_Debt)/df$Household_Income
#View(df %>% filter(Credit_Debt < 0.50 * 1000))



#################################################
plot(data$Age,data$Credit_Debt)

names(data)

#Age
ggplot(data, aes(x=Age, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

#Education (categorical)
ggplot(data, aes(x=Education, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data, aes(x=as.factor(Education), y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

#Years_Current_Employ
ggplot(data, aes(x=Years_Current_Employ, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

#Years_Current_Address
ggplot(data, aes(x=Years_Current_Address, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

#Household_Income
ggplot(data, aes(x=Household_Income, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data, aes(x=log(Household_Income), y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

#Debt_Income_ratio
ggplot(data, aes(x=Debt_Income_ratio, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data, aes(x=log(Debt_Income_ratio), y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data, aes(x=Debt_Income_ratio^2, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)


#Other_Debt
ggplot(data, aes(x=Other_Debt, y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data, aes(x=log(Other_Debt), y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

#Previous_Defaulted
ggplot(data, aes(x=as.factor(Previous_Defaulted), y=Credit_Debt)) + 
  geom_point()+
  geom_smooth(method=lm)

#################################################
  
#check correlations

stargazer(data,
          title = "Statistical Descriptive",
          digits = 2,
          label = "tab1")

m = data %>% select(Credit_Debt, Other_Debt, Household_Income, Debt_Income_ratio)

cor(data)

stargazer(cor(data))  



# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(data,
      upper.panel = panel.smooth,    # Correlation panel
      lower.panel = panel.cor) # Smoothed regression lines

cr=cor(data)
corrplot(cr, type="lower")
corrplot(cr, type="lower", method = "number", tl.col = 'black')
?corrplot

## circle + colorful number
corrplot(cr, order = 'AOE', type = 'upper', tl.pos = 'lt', tl.col = 'black')
corrplot(cr, add = TRUE, type = 'lower', method = 'number', order = 'AOE',
         diag = FALSE, tl.pos = 'n', cl.pos = 'n')

##################################################################
boxplot(data$Age)
boxplot(data$Years_Current_Employ)
boxplot(log(data$Household_Income))
boxplot(log(data$Other_Debt))
