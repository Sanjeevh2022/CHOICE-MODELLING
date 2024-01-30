#CHOICE MODELLING

#Load the required libraries
library(mlogit)
library(conjoint)
library(dplyr)
library(ggplot2)

options(scipen = 999)

#Load the dataset and quickly examine it
housingmarket<-read.csv(file.choose()) # load housingmarket.csv
str(housingmarket, give.attr = FALSE)
head(housingmarket)

unique(housingmarket$caseid)
unique(housingmarket$respondentid)
unique(housingmarket$houseviewed)
unique(housingmarket$option)
unique(housingmarket$fireplaces)
unique(housingmarket$bedrooms)
unique(housingmarket$preference)
unique(housingmarket$averagecostpersquarefootcomparisons)
unique(housingmarket$cashprice)

hm.ml <- mlogit.data(housingmarket, shape = 'long', choice = 'choice',
                     alt.var = 'option')

#xtabs
#Plot crosstabs of fireplaces
chosen_by_fireplaces <- xtabs(preference ~ fireplaces, data=housingmarket)
chosen_by_fireplaces
barplot(chosen_by_fireplaces)

#crosstabs of bedrooms 
chosen_by_bedrooms <- xtabs(preference ~ bedrooms, data=housingmarket)
chosen_by_bedrooms
barplot(chosen_by_bedrooms)

#crosstabs of ave_sqft_comparison
ave_sqft_comp <-xtabs(preference ~ averagecostpersquarefootcomparisons, data=housingmarket)
ave_sqft_comp
barplot(ave_sqft_comp)

#crosstabs of cashprice
chosen_by_cashprice <- xtabs(preference ~ cashprice, data=housingmarket)
chosen_by_cashprice
barplot(chosen_by_cashprice)

#converting the data into mlogit data format
housingmarket.ml<-mlogit.data(housingmarket,
                              choice = 'preference',shape = 'long',
                              alt.var='option')

#converting 'preference' into a factor
housingmarket$preference<-as.factor(housingmarket$preference)

#mlogit model with'0' as coeff
housingmarket_model<-mlogit(preference~0 +bedrooms+cashprice + 
                              averagecostpersquarefootcomparisons, data=housingmarket.ml)
summary(housingmarket_model)

#Finding the Willingness To Pay
WTP<-coef(housingmarket_model)/coef(housingmarket_model)[1]
WTP

#marketshare projections
## Predict choice share based on model
predict_mnl<-function(model,products) {
  data.model<-model.matrix(update(model$formula,0~.),
                           data=products)[,-1]
  utility<-data.model%*%model$coef
  share<-exp(utility)/sum(exp(utility))
  cbind(share,products)
}

#reading the test file
housingmarket_test<-read.csv(file.choose())
prod<-housingmarket_test[1:3,]

#Predict choice shares of hypothetical 3-option sports car selection
shares<-predict_mnl(housingmarket_model,prod)
shares

#Plotting the results
ggplot(shares,aes(x=option,y=share,fill=option))+geom_bar(stat="identity")+
  xlab("Predicted Market share")+ylab("Housing")+
  ggtitle("Housing Choice")
