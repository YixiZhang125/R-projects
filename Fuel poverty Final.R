##-------data cleaning--------
library(haven)
library(readr)
fp2018<-read_csv("~/Downloads/ASSIGNMENT ESDA/built environment- final/spss25/fp2018.csv")
#setwd("~/Downloads/ASSIGNMENT ESDA/built environment- final/spss25/fuel_poverty_data_2018_end_user_licence_rev_12_2020.sav")
library(dplyr) 
library(ggplot2)
library(caTools)
library(caret)
#install.packages("ROSE") 
library(ROSE)
library(pROC)
library(tidyverse)
##compare：lilee and 10% standard very similar results
standard<-fp2018%>%
  select("fpflgb","fpLIHCflg")%>%
  rename("basic_income_defination"="fpflgb","low_income_high_costs_measure"="fpLIHCflg")
str(standard)
standard$basic_income_defination<-as.factor(standard$basic_income_defination)
standard$low_income_high_costs_measure<-as.factor(standard$low_income_high_costs_measure)
# 0.8797 similarity
##plot
confusionMatrix(standard$basic_income_defination, standard$low_income_high_costs_measure)
#remove all the vairables that's strongly correlated with the indicator, set 10%, drop NAs
#remove all the repetitive variables
fp<-rename(fp2018,c("annual_income"="fpfullinc","fuel_costs"="fuelexpn","Dwelling_type"="DWtype","Dwelling_age"="DWage","Number_of_members_of_the_household"="hhsize","Longtermill"="hhsick",
                    "Household_composition"="hhcompx","Under_occupied"="Unoc","Region"="gorehs", "Tenure"="tenure4x","Head_Working_Status"="emphrp3x",
                    "Head_Ethnic_Origin"="ethhrp2x","Age_of_youngest"="Ageyng","Age_of_oldest"="Ageold"))
str(fp)
fp<-fp%>%
  mutate(annual_income=as.numeric(annual_income),
         percent_fuel_cost=fuel_costs/annual_income,
         in_fuel_poverty= if_else(percent_fuel_cost >= 0.1, '1', '0'))%>%
  select("in_fuel_poverty","annual_income","fuel_costs","Dwelling_type","Dwelling_age","FloorArea","Mainfueltype","WallType","Number_of_members_of_the_household","Longtermill","Household_composition","EPC","Ongas","Under_occupied","Region","Tenure","Head_Working_Status","Head_Ethnic_Origin","Age_of_youngest","Age_of_oldest") 
drop_na(fp)
fp$in_fuel_poverty<-as.factor(fp$in_fuel_poverty)
str(fp)
#19 variables left


##-----Normalize and balance data-----
head(fp)
library('e1071')
library('caret')
#install.packages("irr")
library('irr')
table(fp$in_fuel_poverty) #not balanced-table
fp_balanced_over<-ovun.sample(in_fuel_poverty ~ ., data = fp, method = "over",N = 22000)$data
table(fp_balanced_over$in_fuel_poverty)
x<-scale(fp_balanced_over[,2:20])
##covariance table- rough feature selection-original dataset- including y
##not significant because, 1.y binary classification 2.can only show numeric variables, not accurate
#install.packages('readxl')
#install.packages('glmnet')
#install.packages('corrplot')
#install.packages('Metrics')
#install.packages('ggplot2')
library(readxl);library(glmnet);library(corrplot)
library(Metrics);library(ggplot2)
fp_covariance <- fp %>%
  mutate(in_fuel_poverty=as.numeric(fp$in_fuel_poverty))
str(fp_covariance)
names(fp_covariance)=c("Y",'V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14','V15','V16','V17','V18','V19')
dataset_cor <- cor(fp_covariance)
corrplot.mixed(dataset_cor,tl.col="black",tl.pos = "d",number.cex = 0.7)
##---------------------------------------- SVM visualisation----------------------------------------------------
library(e1071);library(ggplot2);library(RColorBrewer)
#write.table(fp_balanced_over,"fp_balanced_over.csv",row.names=FALSE,col.names=TRUE,sep=",")
pcaseed <- princomp(fp_balanced_over[,2:20], cor = TRUE)
seed_score <- as.data.frame(pcaseed$scores[,1:3])
seed_score$in_fuel_povery <- fp_balanced_over$in_fuel_poverty

seed_score2 <- as.data.frame(seed_score[,-2])
seed_score2$in_fuel_povery <- fp_balanced_over$in_fuel_poverty

seed_score3 <- as.data.frame(seed_score[,-1])
seed_score3$in_fuel_povery <- fp_balanced_over$in_fuel_poverty

head(seed_score2)
ggplot(seed_score,aes(x=Comp.1,y= Comp.2,colour = fp_balanced_over$in_fuel_poverty,shape = fp_balanced_over$in_fuel_poverty))+
  geom_point()+theme(legend.position = "right")+
  labs(x = "1st Principal component score", y = "2nd Principal component score", title = "Scattered plots of principal components in reduced dimensions" )

ggplot(seed_score2,aes(x=Comp.1,y= Comp.3,colour = fp_balanced_over$in_fuel_poverty,shape = fp_balanced_over$in_fuel_poverty))+
  geom_point()+theme(legend.position = "right")+
  labs(x = "1st Principal component score", y = "3rd Principal component score", title = "Scattered plots of principal components in reduced dimensions" )

ggplot(seed_score3,aes(x=Comp.2,y= Comp.3,colour = fp_balanced_over$in_fuel_poverty,shape = fp_balanced_over$in_fuel_poverty))+
  geom_point()+theme(legend.position = "right")+
  labs(x = "2nd Principal component score", y = "3rd Principal component score", title = "Scattered plots of principal components in reduced dimensions" )
##3d plot
install.packages("plotly")
library(plotly)

p <- plot_ly(seed_score, x = ~seed_score$Comp.1, y = ~seed_score$Comp.2, z = ~seed_score$Comp.3, color = ~seed_score$in_fuel_povery,
             colors = c("red","black"),
             marker = list(size = 3)) %>%
  add_markers(alpha=0.8)
p


##-------10k-cross-validation visualisation-----------------------------------------------------

ctr <- trainControl(method= "repeatedcv",number=10,repeats=3)
## poly kernal SVM
svm.fit1 <- train(in_fuel_poverty ~., fp,method="svmPoly",trControl=ctr)
## plot
plot(svm.fit1)
## radical SVM
grid <- data.frame(sigma = seq(0.01,0.1,0.01),
                   C = seq(0.01,0.5,0.05))
svm.fit2 <- train(in_fuel_poverty ~., fp,method="svmRadial",
                  trControl=ctr,tuneGrid = grid)
## plot
plot(svm.fit2)
##-------------------------------------SVM model---------------------------------------------------------------
##scale the over sample data
x2<-scale(fp_balanced_over[,2:20])
y<-as.factor(fp_balanced_over[,1])

##split into training and test data
x2<-data.frame(x2)
idx<-sample(nrow(x2),nrow(x2)*0.75)
y<-data.frame(y)
x_train<-x2[idx,]
y_train<-y[idx,]
x_test<-x2[-idx,]
y_test<-y[-idx,]
###rough model
model<-svm(x_train,y_train,scale=FALSE,kernel='radial',gamma=2,cost=0.2)
y_pred<-predict(model,x_test)
confusionMatrix(y_pred,y_test,positive="1")

###cross validation
folds<-createFolds(y_train,k=10)
comp1<-seq(-5,15,2)
comp2<-seq(-15,3,2)
C_crude<-2*10^comp1
gamma_crude<-2*10^comp2
accuracy_crude<-matrix(0,length(comp1),length(comp2))

###Rough grid search
for(i in 1:length(C_crude)){
  C<-C_crude[i];
  for(j in 1:length(gamma_crude)){
    gamma<-gamma_crude[j];
    
    cv_result<-lapply(folds,function(x){
      xk_test<-x_train[x,]
      yk_actual<-y_train[x]
      xk_train<-x_train[-x,]
      yk_train<-y_train[-x]
      model<-svm(xk_train,yk_train,scale=FALSE,kernel='radial',gamma=gamma,cost=C);
      yk_pred<-predict(model,xk_test);
                       
      kappa<-kappa2(data.frame(yk_actual,yk_pred))$value
     return(kappa);
   
  })
      accuracy_crude[i,j]=mean(unlist(cv_result));
  }
}               
    
##Find the most accurate hyperparameters
temp<-which(accuracy_crude==max(accuracy_crude))
col<-ceiling(temp/nrow(accuracy_crude))
row<-temp-(col-1)*nrow(accuracy_crude)
res_gamma<-gamma_crude[col]
res_cost<-C_crude[row]
num1<-log10(res_gamma/2)
num2<-log10(res_cost/2)

##precise grid searching
#use e1071 package, tune algorithm
gamma_fine<-2*10^seq((num1-1),num1+1,0.25)
cost_fine<-2*10^seq((num2-1),num2+1,0.25)
obj<-tune.svm(x=x_train,y=y__train,gamma=gamma_fine,cost=cost_fine)

##test the model
model_best<-svm(x_train,y_train,scale=FALSE,kernel='radial',gamma=obj$best.parameters$gamma,
                cost=obj$best.paramaters$cost)
y_pred<-predict(model_best,x_test)
result<-consusionMatrix(y_test,y_pred,positive='1')


##feature IMPORTANCE--last part--RFE-recursive feature elimination--------------
#install.packages("mlbench")
library(MASS)
library(reshape2)
library(kernlab)
library(caret)
set.seed(123)
rfecntl<-rfeControl(functions=lrFuncs,method='cv',number=10)
y_train_data<-data.frame(y_train)
str(y_train_data)
combine_train_data=data.frame(y_train_data,x_train)
head(combine_train_data)
svm_feature<-rfe(combine_train_data[,2:20],combine_train_data[,1],sizes=c(12,11,10,9,7,6,5,5,4,3,2),rfeControl =rfecntl,method='svmLinear' )
svm_feature
