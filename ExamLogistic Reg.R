dfr <- read.csv("Train9.csv")
dfr
View(dfr)
is.na(dfr)
nrow(dfr)
attach(dfr)
dfr$default = as.factor(dfr$default)
lm.fit <- glm(default~age+ed+employ+address+income+dbtinc+creddebt+otherdebt,family=binomial)
summary(lm.fit)
lm.fit2 <- glm(default~employ+address+dbtinc+creddebt,family=binomial)
summary(lm.fit2)

train_dfr = dfr[1:650,]
test_dfr = dfr[651:701,]

class(dfr$default)

prediction = predict(lm.fit2,test_dfr,type="response")
predictio
table(test_dfr$default,prediction>0.5)
prediction_train = predict(lm.fit2,train_dfr,type="response")
table(train_dfr$default,prediction_train>0.5)










library(glmnet)#...cross validation
library(boot)
dfr$default = as.numeric(dfr$default)
lm.fita=lm(default~poly(age,2),data=dfr)
summary(lm.fita)
lm.fita1=lm(default~poly(ed,2),data=dfr)
summary(lm.fita1)
lm.fita2=lm(default~poly(employ,2),data=dfr)
summary(lm.fita2)
lm.fita3=lm(default~poly(address,2),data=dfr)
summary(lm.fita3)
lm.fita4=lm(default~poly(income,2),data=dfr)
summary(lm.fita4)
lm.fita5=lm(default~poly(dbtinc,2),data=dfr)
summary(lm.fita5)
lm.fita6=lm(default~poly(creddebt,2),data=dfr)
summary(lm.fita6)
lm.fita7=lm(default~poly(otherdebt,2),data=dfr)
summary(lm.fita7)
lm.fita8=lm(default~.,data=dfr)
summary(lm.fita8)


 


#significant varibales are age, employ of order 2 and address,dbt.,cred. of order1

glm.fits66a=glm(default~poly(age,2),data=dfr )
cv.errors24=cv.glm(dfr,glm.fits66a,K=5)$delta[1]
cv.errors24
glm.fits68a=glm(default~poly(employ,2),data=dfr)
cv.errors26=cv.glm(dfr,glm.fits68a,K=5)$delta[1]
cv.errors26
glm.fits71a=glm(default~dbtinc,data=dfr)#...best model of cross validation
cv.errors29=cv.glm(dfr,glm.fits71a,K=5)$delta[1]
cv.errors29
glm.fits72a=glm(default~creddebt,data=dfr)
cv.errors30=cv.glm(dfr,glm.fits72a,K=5)$delta[1]
cv.errors30
glm.fits72ab=glm(default~address,data=dfr)
cv.errors30=cv.glm(dfr,glm.fits72ab,K=5)$delta[1]
cv.errors30
glm.fits73a=glm(default~poly(age,2)*poly(employ,2)*dbtinc*creddebt*address,data=dfr)
cv.errors31=cv.glm(dfr,glm.fits73a,K=5)$delta[1]
cv.errors31




