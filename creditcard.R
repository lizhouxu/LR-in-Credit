data<- 'https://raw.githubusercontent.com/frankhlchi/R-scorecard/b092981b89a90f8cdf83046138acc31b478d3dcc/german_credit.csv'
german_credit<- read.csv(data,header = T)

#将数据集按75%,25%分成训练集(train)和验证集(validation)
library(caret)
set.seed(2000)
train1 <-createDataPartition(y=german_credit$Creditability,p=0.75,list=FALSE)
train <- german_credit[train1, ]
test <- german_credit[-train1, ]

#Exploratory Data Analysis:
hist(german_credit[,3]) #?????????????????????
hist(german_credit[,14]) #???????????????hist(german_credit[,6]) #?????????????????????
hist(german_credit[,1], probability = T) #?????????????????????

# using supervised discretizaion将连续变量分类
library(smbinning)
Durationresult=smbinning(df=train,y="Creditability",x="Duration",p=0.05)
CreditAmountresult=smbinning(df=train,y="Creditability",x="CreditAmount",p=0.05) 
Ageresult=smbinning(df=train,y="Creditability",x="Age",p=0.05) 

smbinning.plot(CreditAmountresult,option="WoE",sub="CreditAmount") #??????WoE
smbinning.plot(Durationresult,option="WoE",sub="Duration")
smbinning.plot(Ageresult,option="WoE",sub="Age")

#计算有经济含义的变量的woe，对已分类数据进行清理
library(woe)
AccountBalancewoe=woe(train, "AccountBalance",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
ggplot(AccountBalancewoe, aes(x = BIN, y = -WOE)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "AccountBalance")
ValueSavingswoe=woe(train, "ValueSavings",Continuous = F, "Creditability",C_Bin = 5,Good = "1",Bad = "0")
ggplot(ValueSavingswoe, aes(x = BIN, y = -WOE)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "ValueSavings") #5?????????,?????????
Lengthofcurrentemploymentwoe=woe(train, "Lengthofcurrentemployment",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
ggplot(Lengthofcurrentemploymentwoe, aes(x = BIN, y = -WOE)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "Lengthofcurrentemployment") 
for(i in 1:750){
  if(train$Lengthofcurrentemployment[i]==5){train$Lengthofcurrentemployment[i]=4}
}
for(i in 1:750){
  if(train$Lengthofcurrentemployment[i]==1){train$Lengthofcurrentemployment[i]=2}
}
Lengthofcurrentemploymentwoe=woe(train, "Lengthofcurrentemployment",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
ggplot(Lengthofcurrentemploymentwoe, aes(x = BIN, y = -WOE)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "Lengthofcurrentemployment")

#按照上述分类计算IV
library(corrplot)
cor1<-cor(train)
corrplot(cor1,tl.cex = 0.5)

for(i in 1:1000){
  if(german_credit$Duration[i]<=11){german_credit$Duration[i]=1}
  else if(german_credit$Duration[i]<=33){german_credit$Duration[i]=2}
  else{german_credit$Duration[i]=3}
}
for(i in 1:750){
  if(train$Duration[i]<=11){train$Duration[i]=1}
  else if(train$Duration[i]<=33){train$Duration[i]=2}
  else{train$Duration[i]=3}
}
for(i in 1:250){
  if(test$Duration[i]<=11){test$Duration[i]=1}
  else if(test$Duration[i]<=33){test$Duration[i]=2}
  else{test$Duration[i]=3}
}

for(i in 1:1000){
  if(german_credit$CreditAmount[i]<=7678){german_credit$CreditAmount[i]=1}
  else {german_credit$CreditAmount[i]=2}
}

for(i in 1:750){
  if(train$CreditAmount[i]<=7678){train$CreditAmount[i]=1}
  else {train$CreditAmount[i]=2}
}

for(i in 1:250){
  if(test$CreditAmount[i]<=7678){test$CreditAmount[i]=1}
  else {test$CreditAmount[i]=2}
}

for(i in 1:1000){
  if(german_credit$Age[i]<=33){german_credit$Age[i]=1}
  else {german_credit$Age[i]=2}
}

for(i in 1:750){
  if(train$Age[i]<=33){train$Age[i]=1}
  else {train$Age[i]=2}
}

for(i in 1:250){
  if(test$Age[i]<=33){test$Age[i]=1}
  else {test$Age[i]=2}
}

for(i in 1:1000){
  if(german_credit$Lengthofcurrentemployment[i]==5){german_credit$Lengthofcurrentemployment[i]=4}
}
for(i in 1:1000){
  if(german_credit$Lengthofcurrentemployment[i]==1){german_credit$Lengthofcurrentemployment[i]=2}
}
for(i in 1:250){
  if(test$Lengthofcurrentemployment[i]==5){test$Lengthofcurrentemployment[i]=4}
}
for(i in 1:250){
  if(test$Lengthofcurrentemployment[i]==1){test$Lengthofcurrentemployment[i]=2}
}

AccountBalancewoe=woe(train, "AccountBalance",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Durationwoe=woe(train, "Duration",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
PaymentStatusofPreviousCreditwoe=woe(train, "PaymentStatusofPreviousCredit",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Purposewoe = woe(train, "Purpose",Continuous = F, "Creditability",C_Bin = 11,Good = "1",Bad = "0")
CreditAmountwoe= woe(train, "CreditAmount",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
ValueSavingswoe =woe(train, "ValueSavings",Continuous = F, "Creditability",C_Bin = 5,Good = "1",Bad = "0")
Lengthofcurrentemploymentwoe=woe(train, "Lengthofcurrentemployment",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
Instalmentpercentwoe=woe(train, "Instalmentpercent",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Sex.Marital.Statuswoe=woe(train, "Sex.Marital.Status",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Guarantorswoe=woe(train, "Guarantors",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
DurationinCurrentaddresswoe=woe(train, "DurationinCurrentaddress",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Mostvaluableavailableassetwoe=woe(train, "Mostvaluableavailableasset",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Agewoe=woe(train, "Age",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
ConcurrentCreditswoe=woe(train, "ConcurrentCredits",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
Typeofapartmentwoe=woe(train, "Typeofapartment",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
NoofCreditatthisBankwoe=woe(train, "NoofCreditatthisBank",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
Occupationwoe=woe(train, "Occupation",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Noofdependentswoe=woe(train, "Noofdependents",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
Telephonewoe=woe(train, "Telephone",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
ForeignWorkerwoe=woe(train, "ForeignWorker",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")

va = c("AccountBalance",	"Duration",	"PaymentStatusofPreviousCredit",	"Purpose",	"CreditAmount",	"ValueSavings",	"Lengthofcurrentemployment",	"Instalmentpercent","Sex.Marital.Status","Guarantors","DurationinCurrentaddress",	"Mostvaluableavailableasset",	"Age","ConcurrentCredits","Typeofapartment",	"NoofCreditatthisBank","Occupation","Noofdependents",	"Telephone"	,"ForeignWorker")
iv=c(sum(AccountBalancewoe$IV),sum(Durationwoe$IV),sum(PaymentStatusofPreviousCreditwoe$IV),sum(Purposewoe$IV),sum(CreditAmountwoe$IV),sum(ValueSavingswoe$IV),sum(Lengthofcurrentemploymentwoe$IV) ,sum(Instalmentpercentwoe$IV), sum(Sex.Marital.Statuswoe$IV) ,sum(Guarantorswoe$IV)  ,sum(DurationinCurrentaddresswoe$IV) ,sum(Mostvaluableavailableassetwoe$IV),sum(Agewoe$IV),sum(ConcurrentCreditswoe$IV),sum(Typeofapartmentwoe$IV),sum(NoofCreditatthisBankwoe$IV),sum(Occupationwoe$IV), sum(Noofdependentswoe$IV), sum(Telephonewoe$IV),sum(ForeignWorkerwoe$IV))
infovalue = data.frame(va,iv)
ggplot(infovalue, aes(x = va, y = iv)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "Information value")+ theme(axis.text.x=element_text(angle=90,colour="black",size=10))
#去除低iv的特征
#Durationincurrentaddress,ForeignWorker,Guarantors,NoofCreditatthisBank,Noofdependents,occupation,sex.maritial.status,Telephone delete.

german_credit$DurationinCurrentaddress=NULL
german_credit$Guarantors=NULL
german_credit$ForeignWorker=NULL
german_credit$Noofdependents=NULL
german_credit$Telephone=NULL
german_credit$Occupation=NULL
german_credit$Sex.Marital.Status=NULL
german_credit$NoofCreditatthisBank=NULL
train$DurationinCurrentaddress=NULL
train$Guarantors=NULL
train$ForeignWorker=NULL
train$Noofdependents=NULL
train$Telephone=NULL
train$Occupation=NULL
train$Sex.Marital.Status=NULL
train$NoofCreditatthisBank=NULL
test$DurationinCurrentaddress=NULL
test$Guarantors=NULL
test$ForeignWorker=NULL
test$Noofdependents=NULL
test$Telephone=NULL
test$Occupation=NULL
test$Sex.Marital.Status=NULL
test$NoofCreditatthisBank=NULL

AccountBalancewoe=woe(train, "AccountBalance",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Durationwoe=woe(train, "Duration",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
PaymentStatusofPreviousCreditwoe=woe(train, "PaymentStatusofPreviousCredit",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Purposewoe = woe(train, "Purpose",Continuous = F, "Creditability",C_Bin = 11,Good = "1",Bad = "0")
CreditAmountwoe= woe(train, "CreditAmount",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
ValueSavingswoe =woe(train, "ValueSavings",Continuous = F, "Creditability",C_Bin = 5,Good = "1",Bad = "0")
Lengthofcurrentemploymentwoe=woe(train, "Lengthofcurrentemployment",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
Instalmentpercentwoe=woe(train, "Instalmentpercent",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Mostvaluableavailableassetwoe=woe(train, "Mostvaluableavailableasset",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Agewoe=woe(train, "Age",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
ConcurrentCreditswoe=woe(train, "ConcurrentCredits",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
Typeofapartmentwoe=woe(train, "Typeofapartment",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")

#将原有的分类数据替换成woe
for(i in 1:1000){
  for(s in 1:4){
    if(german_credit$AccountBalance[i]==s){
      german_credit$AccountBalance[i]=-AccountBalancewoe$WOE[s]
    }
  }

  for(s in 1:3){
    if(german_credit$Duration[i]==s){
      german_credit$Duration[i]=-Durationwoe$WOE[s]
    }
  }
  
  for(s in 0:4){
    if(german_credit$PaymentStatusofPreviousCredit[i]==s){
      german_credit$PaymentStatusofPreviousCredit[i]=-PaymentStatusofPreviousCreditwoe$WOE[s+1]
    }
  }
  
  for(s in 0:10){
    if(s<=6){
      if(german_credit$Purpose[i]==s){
        german_credit$Purpose[i]=-Purposewoe$WOE[s+1]
      }
    }else{
      if(german_credit$Purpose[i]==s){
        german_credit$Purpose[i]=-Purposewoe$WOE[s]
      }
    }
  }
  
  for(s in 1:2){
    if(german_credit$CreditAmount[i]==s){
      german_credit$CreditAmount[i]=-CreditAmountwoe$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(german_credit$ValueSavings[i]==s){
      german_credit$ValueSavings[i]=-ValueSavingswoe$WOE[s]
    }
  }
  
  for(s in 2:4){
    if(german_credit$Lengthofcurrentemployment[i]==s){
      german_credit$Lengthofcurrentemployment[i]=-Lengthofcurrentemploymentwoe$WOE[s-1]
    }
  }
  
  for(s in 1:4){
    if(german_credit$Instalmentpercent[i]==s){
      german_credit$Instalmentpercent[i]=-Instalmentpercentwoe$WOE[s]
    }
  }
  
  for(s in 1:4){
    if(german_credit$Mostvaluableavailableasset[i]==s){
      german_credit$Mostvaluableavailableasset[i]=-Mostvaluableavailableassetwoe$WOE[s]
    }
  }
  
  for(s in 1:2){
    if(german_credit$Age[i]==s){
      german_credit$Age[i]=-Agewoe$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(german_credit$ConcurrentCredits[i]==s){
      german_credit$ConcurrentCredits[i]=-ConcurrentCreditswoe$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(german_credit$Typeofapartment[i]==s){
      german_credit$Typeofapartment[i]=-Typeofapartmentwoe$WOE[s]
    }
  }
}  

for(i in 1:750){
  for(s in 1:4){
    if(train$AccountBalance[i]==s){
      train$AccountBalance[i]=-AccountBalancewoe$WOE[s]
    }
  }
  
  for(s in 1:3){
    if(train$Duration[i]==s){
      train$Duration[i]=-Durationwoe$WOE[s]
    }
  }
  
  for(s in 0:4){
    if(train$PaymentStatusofPreviousCredit[i]==s){
      train$PaymentStatusofPreviousCredit[i]=-PaymentStatusofPreviousCreditwoe$WOE[s+1]
    }
  }
  
  for(s in 0:10){
    if(s<=6){
      if(train$Purpose[i]==s){
        train$Purpose[i]=-Purposewoe$WOE[s+1]
      }
    }else{
      if(train$Purpose[i]==s){
        train$Purpose[i]=-Purposewoe$WOE[s]
      }
    }
  }
  
  for(s in 1:2){
    if(train$CreditAmount[i]==s){
      train$CreditAmount[i]=-CreditAmountwoe$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(train$ValueSavings[i]==s){
      train$ValueSavings[i]=-ValueSavingswoe$WOE[s]
    }
  }
  
  for(s in 2:4){
    if(train$Lengthofcurrentemployment[i]==s){
      train$Lengthofcurrentemployment[i]=-Lengthofcurrentemploymentwoe$WOE[s-1]
    }
  }
  
  for(s in 1:4){
    if(train$Instalmentpercent[i]==s){
      train$Instalmentpercent[i]=-Instalmentpercentwoe$WOE[s]
    }
  }
  
  for(s in 1:4){
    if(train$Mostvaluableavailableasset[i]==s){
      train$Mostvaluableavailableasset[i]=-Mostvaluableavailableassetwoe$WOE[s]
    }
  }
  
  for(s in 1:2){
    if(train$Age[i]==s){
      train$Age[i]=-Agewoe$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(train$ConcurrentCredits[i]==s){
      train$ConcurrentCredits[i]=-ConcurrentCreditswoe$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(train$Typeofapartment[i]==s){
      train$Typeofapartment[i]=-Typeofapartmentwoe$WOE[s]
    }
  }
}  

#将woe化后的数据进行逐步逻辑回归
fit<-glm(Creditability~ AccountBalance + Duration +PaymentStatusofPreviousCredit +Purpose + CreditAmount + ValueSavings + Lengthofcurrentemployment + Instalmentpercent + Mostvaluableavailableasset + Age + ConcurrentCredits + Typeofapartment,train,family = "binomial")
backwards = step(fit)

#Creditability ~ AccountBalance + Duration + PaymentStatusofPreviousCredit + Purpose + CreditAmount + ValueSavings + Instalmentpercent + Mostvaluableavailableasset + Age
fit2<-glm(Creditability~ AccountBalance + Duration + PaymentStatusofPreviousCredit + Purpose + CreditAmount + ValueSavings + Instalmentpercent + Mostvaluableavailableasset + Age,train,family = "binomial")
summary(fit2)

library(car)
vif(fit2, digits =3)
#不受多重共线性影响

#用验证集验证模型
AccountBalancewoe2=woe(test, "AccountBalance",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Durationwoe2=woe(test, "Duration",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
PaymentStatusofPreviousCreditwoe2=woe(test, "PaymentStatusofPreviousCredit",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Purposewoe2 = woe(test, "Purpose",Continuous = F, "Creditability",C_Bin = 11,Good = "1",Bad = "0")
CreditAmountwoe2= woe(test, "CreditAmount",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
ValueSavingswoe2 =woe(test, "ValueSavings",Continuous = F, "Creditability",C_Bin = 5,Good = "1",Bad = "0")
Lengthofcurrentemploymentwoe2=woe(test, "Lengthofcurrentemployment",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
Instalmentpercentwoe2=woe(test, "Instalmentpercent",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Mostvaluableavailableassetwoe2=woe(test, "Mostvaluableavailableasset",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
Agewoe2=woe(test, "Age",Continuous = F, "Creditability",C_Bin = 2,Good = "1",Bad = "0")
ConcurrentCreditswoe2=woe(test, "ConcurrentCredits",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")
Typeofapartmentwoe2=woe(test, "Typeofapartment",Continuous = F, "Creditability",C_Bin = 3,Good = "1",Bad = "0")

for(i in 1:250){
  for(s in 1:4){
    if(test$AccountBalance[i]==s){
      test$AccountBalance[i]=-AccountBalancewoe2$WOE[s]
    }
  }
  
  for(s in 1:3){
    if(test$Duration[i]==s){
      test$Duration[i]=-Durationwoe2$WOE[s]
    }
  }
  
  for(s in 0:4){
    if(test$PaymentStatusofPreviousCredit[i]==s){
      test$PaymentStatusofPreviousCredit[i]=-PaymentStatusofPreviousCreditwoe2$WOE[s+1]
    }
  }
  
  for(s in 0:10){
    if(s<=6){
      if(test$Purpose[i]==s){
        test$Purpose[i]=-Purposewoe2$WOE[s+1]
      }
    }else{
      if(test$Purpose[i]==s){
        test$Purpose[i]=-Purposewoe2$WOE[s]
      }
    }
  }
  
  for(s in 1:2){
    if(test$CreditAmount[i]==s){
      test$CreditAmount[i]=-CreditAmountwoe2$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(test$ValueSavings[i]==s){
      test$ValueSavings[i]=-ValueSavingswoe2$WOE[s]
    }
  }
  
  for(s in 2:4){
    if(test$Lengthofcurrentemployment[i]==s){
      test$Lengthofcurrentemployment[i]=-Lengthofcurrentemploymentwoe2$WOE[s-1]
    }
  }
  
  for(s in 1:4){
    if(test$Instalmentpercent[i]==s){
      test$Instalmentpercent[i]=-Instalmentpercentwoe2$WOE[s]
    }
  }
  
  for(s in 1:4){
    if(test$Mostvaluableavailableasset[i]==s){
      test$Mostvaluableavailableasset[i]=-Mostvaluableavailableassetwoe2$WOE[s]
    }
  }
  
  for(s in 1:2){
    if(test$Age[i]==s){
      test$Age[i]=-Agewoe2$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(test$ConcurrentCredits[i]==s){
      test$ConcurrentCredits[i]=-ConcurrentCreditswoe2$WOE[s]
    }
  }
  
  for(s in 1:5){
    if(test$Typeofapartment[i]==s){
      test$Typeofapartment[i]=-Typeofapartmentwoe2$WOE[s]
    }
  }
}  

prediction <- predict(fit2,newdata=test)
for (i in 1:250) {
  if(prediction[i]>0.99){
    prediction[i]=1}
  else
  {prediction[i]=0}
}

confusionmatrix<- as.table(table(test$Creditability,prediction))
confusionmatrix[2,2]/(confusionmatrix[2,2]+confusionmatrix[1,2])

#sensitivity=90.7%，模型效果良好
