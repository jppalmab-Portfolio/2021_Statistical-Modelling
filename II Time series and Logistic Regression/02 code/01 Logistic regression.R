rm(list = ls())

library(pacman)
p_load(haven, stargazer, xtable, texreg, dplyr, ggplot2,hrbrthemes,psych,
       corrplot, ROCR, tidyverse, broom)

df <- read_sav("01 data/House Categories.sav")
head(df)


#######################################
#######################################
######## DATA UNDERSTANDING ###########
#######################################
#######################################
#‘2Expensive’ or ‘1Budget’
summary(df)

for(i in names(df[,1:9])){
  df[[i]] <- as.numeric(df[[i]])#, ordered=TRUE      
}

for(i in names(df[,10:13])){
  df[[i]] <- factor(df[[i]]) #, ordered=TRUE      
}

levels(df$PriceCat) <- c("Budget", "Expensive")
str(df)

df %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.)))) 

#######################################
#######################################
######### DESCRIPTIVE DATA  ###########
#######################################
#######################################

summary(df)
stargazer(as.data.frame(df), title = 'Statistical summary', label = 'stats', digits = 2)

pairs.panels(df[,1:9], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#########################
#########################
##########CORRELATION###########
#########################
#########################

cr=cor(df[,1:9])
#corrplot(cr, type="lower")
#corrplot(cr, type="lower", method = "number", tl.col = 'black')
#?corrplot

################## The best graph of cor
## circle + colorful number
corrplot(cr, order = 'AOE', type = 'upper', tl.pos = 'lt', tl.col = 'black')
corrplot(cr, add = TRUE, type = 'lower', method = 'number', order = 'AOE',
         diag = FALSE, tl.pos = 'n', cl.pos = 'n')


#####################

xtable(table(df$fuel,df$PriceCat))
xtable(table(df$waterfront,df$PriceCat))
xtable(table(df$newConstruction,df$PriceCat))
xtable(table(df$PriceCat,df$PriceCat))

xtable(prop.table(table(df$fuel,df$PriceCat)))
xtable(prop.table(table(df$waterfront,df$PriceCat)))
xtable(prop.table(table(df$newConstruction,df$PriceCat)))
xtable(prop.table(table(df$PriceCat,df$PriceCat)))




#########################
#########################
########## PCA ##########
#########################
#########################

#Read file and amend
#df_pc<-(df[,-c(10:13)]) #3 factors
#df_pc<-(df[,-c(2,10:13)])#2factors

df_pc<-(df[,-c(1,2,10:13)])#2factor
#df_pc<-(df[,-c(1,2,5,10:13)])
head(df_pc)

#check for number of components
fa.parallel(df_pc,fa="pc",n.iter = 100)

#Extract components
pc.df<-principal(df_pc,nfactors = 2,rotate = "none")
pc.df

#Rotate components
rc.df<-principal(df_pc,nfactors = 2,rotate="varimax")
rc.df

#Interpret rotated components
fa.diagram(rc.df)

#Factor Scores
rc.df$score
pca_score=as.data.frame(rc.df$score)

df$RC1=pca_score$RC1
df$RC2=pca_score$RC2
#principal components try to reduce dimension but age was alone so de idea was,
#do it again only with the house characteristics

####################
####################
####################  best fit PCA
df_pc_2<-(df[,c(1:4,8)])
head(df_pc_2)

fa.parallel(df_pc_2,fa="pc",n.iter = 100)
pc.df_2<-principal(df_pc_2,nfactors = 2,rotate = "none")
pc.df_2
rc.df_2<-principal(df_pc_2,nfactors = 2,rotate="varimax")
rc.df_2
fa.diagram(rc.df_2)

rc.df_2$score
pca_score_2=as.data.frame(rc.df_2$score)

df$RC1_2=pca_score_2$RC1
df$RC2_2=pca_score_2$RC2

################################################################################
################################################################################
#############################   MODELLING  #####################################
################################################################################
################################################################################

colnames(df)
#Fitting the model
housefit0<-glm(PriceCat~1, 
               data = df, family=binomial)
summary(housefit0)

housefit1<-glm(PriceCat~. -RC1 -RC2-RC1_2 -RC2_2, 
               data = df, family=binomial)
summary(housefit1)

texreg(list(housefit0, housefit1), caption = "Null Model vs. Full Model", 
       caption.above = TRUE, label = "null_full", scalebox = 0.8)

#age : not possible to remove
housefit2<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-age, 
               data = df, family=binomial)
summary(housefit2)

#pctCollege: not possible to remove
housefit3<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-pctCollege, 
               data = df, family=binomial)
summary(housefit3)

#bedrooms: not possible to remove
housefit4<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-bedrooms, 
               data = df, family=binomial)
summary(housefit4)

#fireplace:not possible to remove
housefit5<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-fireplaces, 
               data = df, family=binomial)
summary(housefit5)

#rooms:
housefit6<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-rooms, 
               data = df, family=binomial)
summary(housefit6)

#fuel:not possible to remove
housefit7<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-fuel, 
               data = df, family=binomial)
summary(housefit7)

#New construction:not possible to remove
housefit8<-glm(PriceCat~-RC1 -RC2-RC1_2 -RC2_2-newConstruction, 
               data = df, family=binomial)
summary(housefit8)


housefit10<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-fireplaces-rooms, 
               data = df, family=binomial)
summary(housefit10)

housefit11<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-fireplaces-rooms-bedrooms, 
                data = df, family=binomial)
summary(housefit11)

housefit12<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-fireplaces-rooms-bedrooms-fuel, 
                data = df, family=binomial)
summary(housefit12)

housefit13<-glm(PriceCat~.-RC1 -RC2-RC1_2 -RC2_2-fireplaces-rooms-bedrooms-fuel -newConstruction, 
                data = df, family=binomial)
summary(housefit13)

housefit14<-glm(PriceCat~.-age2-RC1 -RC2-RC1_2 -RC2_2-fireplaces-rooms-bedrooms-fuel-newConstruction-pctCollege, 
                data = df, family=binomial)
summary(housefit14)

housefit15<-glm(PriceCat~ age+lotSize+landValue+livingArea+bathrooms+waterfront+I(age^2), 
                data = df, family=binomial)
summary(housefit15)


df$age = df$age +1

housefit16<-glm(PriceCat~ age2+lotSize+landValue+livingArea+bathrooms+waterfront+age2:log(age2), 
                data = df, family=binomial)
summary(housefit16)


texreg(list(housefit13, housefit14, housefit15, house_pca_fit5), caption = "Model Building and Parsimony", 
       caption.above = TRUE, label = "parsimony", scalebox = 0.8)

print(paste(round(housefit1$deviance,2),round(housefit1$aic,2)))
print(paste(round(housefit2$deviance,2),round(housefit2$aic,2)))
print(paste(round(housefit3$deviance,2),round(housefit3$aic,2)))
print(paste(round(housefit4$deviance,2),round(housefit4$aic,2)))
print(paste(round(housefit5$deviance,2),round(housefit5$aic,2))) #can be
print(paste(round(housefit6$deviance,2),round(housefit6$aic,2)))
print(paste(round(housefit7$deviance,2),round(housefit7$aic,2)))
print(paste(round(housefit8$deviance,2),round(housefit8$aic,2)))


housefit24<-glm(PriceCat~.-landValue-livingArea-RC1 -RC2-RC1_2 -RC2_2-fireplaces-rooms-bedrooms-fuel-newConstruction-pctCollege-age2, 
                data = df, family=binomial)
summary(housefit24)

################################################################################
# PCA 
################################################################################

house_pca_fit1<-glm(PriceCat~.-livingArea-rooms-bedrooms-bathrooms-pctCollege
                    -landValue-fireplaces-RC1_2 -RC2_2, 
               data = df, family=binomial)
summary(house_pca_fit1)

#age
house_pca_fit2<-glm(PriceCat~.-livingArea-rooms-bedrooms-bathrooms-pctCollege
                    -landValue-fireplaces -age-RC1_2 -RC2_2, 
                    data = df, family=binomial)
summary(house_pca_fit2)

#fuel:
house_pca_fit3<-glm(PriceCat~.-livingArea-rooms-bedrooms-bathrooms-pctCollege
                    -landValue-fireplaces -fuel-RC1_2 -RC2_2, 
                    data = df, family=binomial)
summary(house_pca_fit3)

#newConstruction
house_pca_fit4<-glm(PriceCat~.-livingArea-rooms-bedrooms-bathrooms-pctCollege
                    -landValue-fireplaces -newConstruction-RC1_2 -RC2_2-age2, 
                    data = df, family=binomial)
summary(house_pca_fit4)


house_pca_fit5<-glm(PriceCat~lotSize+age+waterfront+RC1+RC2, 
                    data = df, family=binomial)
summary(house_pca_fit5)

##############################
#############################
house_pca_fit6<-glm(PriceCat~ waterfront+RC1_2+RC2_2, 
                    data = df, family=binomial)
summary(house_pca_fit6)


house_pca_fit7<-glm(PriceCat~ age+bathrooms+waterfront+RC2_2, 
                    data = df, family=binomial)
summary(house_pca_fit7)



house_pca_fit8<-glm(PriceCat~lotSize+age+waterfront+RC1+RC2, 
                    data = df, family=binomial)
summary(house_pca_fit8)


house_pca_fit9<-glm(PriceCat~lotSize+age+waterfront+RC1+RC2+I(age^2), 
                    data = df, family=binomial)
summary(house_pca_fit9)


print(paste(round(house_pca_fit1$deviance,2),round(house_pca_fit1$aic,2)))
print(paste(round(house_pca_fit2$deviance,2),round(house_pca_fit2$aic,2)))
print(paste(round(house_pca_fit3$deviance,2),round(house_pca_fit3$aic,2)))
print(paste(round(house_pca_fit4$deviance,2),round(house_pca_fit4$aic,2)))

###################################
### PERFORMANCE AND CONFUSION MATRIX

res_nom = predict(housefit14, df, type='response')
xtable(table(RealValue=df$PriceCat, PredictedValue=res_nom>0.49), 
       caption = "Confusion Matrix Logistic Regression", label = "Confusion_norm")
sum(diag(table(RealValue=df$PriceCat, PredictedValue=res_nom>0.49)))/1709
#### best performance 14

res_pca = predict(house_pca_fit5, df, type='response')
xtable(table(RealValue=df$PriceCat, PredictedValue=res_pca>0.5),
       caption = "Confusion Matrix Logistic Regression with PCA", label = "Confusion_pca")
sum(diag(table(RealValue=df$PriceCat, PredictedValue=res_pca>0.5)))/1709
#### best performance 6

ROCRPred = prediction(res_nom,df$PriceCat)
ROCRPerf = performance(ROCRPred, "tpr","fpr")

plot(ROCRPerf, colorize=TRUE,printcutoffs.at=seq(0.1,by=0.1))



###########################################
# OTHER EVALUATIONS

#Omnibus test
xtable(anova(housefit1, test="Chisq"), caption = 'Omnibus Test', 
       label = "omnibus", digits = 2)


#Odds Ratios
coef(housefit14)
exp(coef(housefit14))


xtable(table(coef(housefit14)))
xtable(table(exp(coef(housefit14))))


#PseudoRSquared Statistics
library(DescTools)
PseudoR2(housefit14, which = "all")

#Check for Multicollinearity
library(car)
vif(housefit14)
xtable(table(vif(housefit14)))

#Check Linearity of the Logit
housefit14$coefficients
summary(housefit14)

# Select only numeric predictors
mydata <- df %>% 
  dplyr::select_if(is.numeric) 

#mydata$log_age = log(mydata$age)

mydata = mydata %>% select(lotSize,age,landValue,livingArea,bathrooms)

predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
probabilities <- predict(housefit14, type = "response")

mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Checking the standardised residuals and identifying Influential data points
rstandard(housefit14)
cooks.distance(housefit14)

#######PLOTTING

plot(housefit14, which = 4, id.n = 3)

# Extract model results
model.data <- augment(housefit14) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = PriceCat), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)

geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
