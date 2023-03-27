###############################################################
###############################################################
# Project - MythAnalysis

library(tidyverse)
library(readxl)
library(gmodels)

data <- read_xlsx("MosquitoBiteMyth_Dataset.xlsx")
data

nrow(data) #18302
ncol(data) #18

summary(data)

data_1 <- filter(data, Y %in% c(1,2))
data_1

nrow(data_1) #15164

summary(data_1)

# removing WOMEN_ID column and getting needed column names 
data_ <- select(data_1, -c(WOMEN_ID))
data_ 

# finding unique values of each column
column_names_ <- colnames(data_)
column_names_
for (column_names_ in data_){
  sort_unique <- sort(unique(column_names_))
  print(sort_unique)
}

# finding the missing values in any of the column
sapply(data_, function(x) sum(is.na(x))) 

# data exploration

CrossTable(data_$X1, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X2, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X3, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X4, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X5, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X6, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X7, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X8, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X9, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X10, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X11, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X12, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X13, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X14, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X15, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(data_$X16, data_$Y, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)


# Finding the important variables

# use x12 by excluding x9,x10 and x11
data_3 <- select(data_, -c(X9, X10, X11))
data_3

# conduct chi-square test for selected other variables and Y1
chisq.test(table(data_3$X1,data_3$Y))
chisq.test(table(data_3$X2,data_3$Y))
chisq.test(table(data_3$X3,data_3$Y))
chisq.test(table(data_3$X4,data_3$Y))
chisq.test(table(data_3$X5,data_3$Y))
chisq.test(table(data_3$X6,data_3$Y))
chisq.test(table(data_3$X7,data_3$Y))
chisq.test(table(data_3$X8,data_3$Y))
chisq.test(table(data_3$X12,data_3$Y))
chisq.test(table(data_3$X13,data_3$Y))
chisq.test(table(data_3$X14,data_3$Y))
chisq.test(table(data_3$X15,data_3$Y))
chisq.test(table(data_3$X16,data_3$Y))

# select X1, X2, X3, X4, X7, X8, X12, X15, X16

# record 1 to 1 and 2 to 0
d1 = data_3 %>% filter(Y=="1")
d2 = data_3 %>% filter(Y=="2")

d3 = d1 %>% mutate(Y1 = Y*1)
d4 = d2 %>% mutate(Y1 = Y*0)

data_7 <- bind_rows(d3,d4) 
data_7

data_used = data_7 %>% rename(Response=Y1)
data_used

# Model building 
# forward selection 

model2 <- glm(Response~as.factor(X1), data=data_used, family=binomial)
summary(model2)
model3 <- glm(Response~as.factor(X2), data=data_used, family=binomial)
summary(model3)
model4 <- glm(Response~as.factor(X3), data=data_used, family=binomial)
summary(model4)
model5 <- glm(Response~as.factor(X4), data=data_used, family=binomial)
summary(model5)
model6 <- glm(Response~as.factor(X7), data=data_used, family=binomial)
summary(model6)
model7 <- glm(Response~as.factor(X8), data=data_used, family=binomial)
summary(model7)
model8 <- glm(Response~as.factor(X12), data=data_used, family=binomial)
summary(model8)
model9 <- glm(Response~as.factor(X15), data=data_used, family=binomial)
summary(model9)
model10 <- glm(Response~as.factor(X16), data=data_used, family=binomial)
summary(model10)

1-pchisq(67,2)
1-pchisq(145,8)
1-pchisq(78,5)
1-pchisq(109,5)
1-pchisq(13,2)
1-pchisq(336,5)
1-pchisq(9,2)
1-pchisq(8,1)
1-pchisq(149,4)

model11 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X1), data=data_3, family=binomial)
summary(model11)
model12 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2), data=data_3, family=binomial)
summary(model12)
model13 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X3), data=data_3, family=binomial)
summary(model13)
model14 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X4), data=data_3, family=binomial)
summary(model14)
model15 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X7), data=data_3, family=binomial)
summary(model15)
model16 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X12), data=data_3, family=binomial)
summary(model16)
model17 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X15), data=data_3, family=binomial)
summary(model17)
model18 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X16), data=data_3, family=binomial)
summary(model18)

1-pchisq(34,2)
1-pchisq(132,8)
1-pchisq(42,5)
1-pchisq(63,6)
1-pchisq(13,2)
1-pchisq(3,2)
1-pchisq(1,1)
1-pchisq(28,4)

model19 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X1), data=data_3, family=binomial)
summary(model19)
model20 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X3), data=data_3, family=binomial)
summary(model20)
model21 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4), data=data_3, family=binomial)
summary(model21)
model22 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X7), data=data_3, family=binomial)
summary(model22)
model23 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X12), data=data_3, family=binomial)
summary(model23)
model24 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X15), data=data_3, family=binomial)
summary(model24)
model25 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X16), data=data_3, family=binomial)
summary(model25)

1-pchisq(15,2)
1-pchisq(36,5)
1-pchisq(51,6)
1-pchisq(11,2)
1-pchisq(3,2)
1-pchisq(0,1)
1-pchisq(27,4)

model26 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X1), data=data_3, family=binomial)
summary(model26)
model27 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X3), data=data_3, family=binomial)
summary(model27)
model28 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X7), data=data_3, family=binomial)
summary(model28)
model29 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X12), data=data_3, family=binomial)
summary(model29)
model30 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X15), data=data_3, family=binomial)
summary(model30)
model31 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16), data=data_3, family=binomial)
summary(model31)

1-pchisq(2,2)
1-pchisq(5,2)
1-pchisq(9,2)
1-pchisq(3,2)
1-pchisq(0,1)
1-pchisq(24,4)

model32 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X1), data=data_3, family=binomial)
summary(model32)
model33 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X3), data=data_3, family=binomial)
summary(model33)
model34 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X7), data=data_3, family=binomial)
summary(model34)
model35 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X12), data=data_3, family=binomial)
summary(model35)
model36 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X15), data=data_3, family=binomial)
summary(model36)

1-pchisq(0,2)
1-pchisq(5,5)
1-pchisq(10,2)
1-pchisq(5,2)
1-pchisq(0,1)

model37 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X7)+as.factor(X1), data=data_3, family=binomial)
summary(model37)
model38 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X7)+as.factor(X3), data=data_3, family=binomial)
summary(model38)
model39 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X7)+as.factor(X12), data=data_3, family=binomial)
summary(model39)
model40 <- glm(as.factor(Y)~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X7)+as.factor(X15), data=data_3, family=binomial)
summary(model40)

1-pchisq(0,2)
1-pchisq(5,5)
1-pchisq(5,2)
1-pchisq(0,1)

model_fitted <- glm(Response~as.factor(X8)+as.factor(X2)+as.factor(X4)+as.factor(X16)+as.factor(X7), data=data_used, family=binomial)
summary(model_fitted)



# checking multi-collinearity 
car::vif(model_fitted)

# influential cases

plot(model_fitted, which=4, id.n=3)
library(broom)
model.data <- augment(model_fitted) %>% mutate(index=1:n())
model.data %>% top_n(3, .cooksd)
library(ggplot2)
ggplot(model.data, aes(index, .std.resid))+geom_point(aes(color=data_used$Response), alpha=0.5)+theme_bw()
model.data %>% filter(abs(.std.resid)>3)

