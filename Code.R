library(readr)
dataset <- read.csv(file.choose())

#Data Preparation
set.seed(45)
summary(dataset)
View(dataset)
levels(dataset$status)
sum(is.na(dataset))

#Mengganti variabel kategori menjadi numerik
dataset$status = ifelse(dataset$status == 'Not Placed', 0, 1)

dataset$gender = ifelse(dataset$gender == 'M',1, 0)
dataset$ssc_board = ifelse(dataset$ssc_board == 'Central', 1, 2)
dataset$hsc_board = ifelse(dataset$hsc_board == 'Central',1, 2)

dataset$hsc_subject = factor(dataset$hsc_subject,levels = 
                                   c('Arts', 'Commerce', 'Science'),
                                 labels = c(1, 2, 3))
dataset$undergrad_degree = factor(dataset$undergrad_degree,levels = 
                                   c('Comm&Mgmt', 'Others', 'Sci&Tech'),
                                 labels = c(1, 2, 3))

dataset$work_experience = ifelse(dataset$work_experience == 'No',1, 2)
dataset$specialisation = ifelse(dataset$specialisation == 'Mkt&Fin', 1, 2)


#melakukan pengecekan level yang ada dalam variabel
levels(dataset$status)
levels(dataset$undergrad_degree)
levels(housing$ssc_board)

levels(dataset$status)lapply(dataset[c('status', 'gender', 'ssc_percentage','ssc_board','hsc_percentage','hsc_board')], unique)
lapply(dataset[c('hsc_subject', 'degree_percentage','undergrad_degree')], unique)


#---regresi logistic biner---#
model <- glm(status ~ gender +ssc_percentage +ssc_board +hsc_percentage +hsc_board 
             +hsc_subject +degree_percentage+ undergrad_degree + work_experience
             +emp_test_percentage +specialisation +mba_percent
             , data=dataset, family = "binomial")
summary(model)

#---uji signifikansi---#
#uji serentak
#H0 = beta 1 = beta 2 = ... = beta p
#H1 = minimal ada satu beta i yang != 0
library(lmtest)
lrtest(model)

#uji signifikansi parsial
#H0 = beta i = 0
#H1 = beta i != 0
summary(model)

dataset$hsc_subject = as.character(dataset$hsc_subject)
dataset$hsc_subject = as.array(dataset$hsc_subject)

c1 <- c('1')
dataset$hsc_subject[!(dataset$hsc_subject %in% c1)] <- 'others'
dataset$hsc_subject = factor(dataset$hsc_subject, levels = c('1','others'),labels = c('1','2&3'))

levels(dataset$hsc_subject)

dataset$`hsc_subject` = ifelse(dataset$`hsc_subject` == '2', 'other', dataset$`hsc_subject`)
dataset$`hsc_subject` = ifelse(dataset$`hsc_subject` == '3', 'other', dataset$`hsc_subject`)



model1 <- glm(status ~ gender +ssc_percentage +hsc_percentage +hsc_board 
             +hsc_subject +degree_percentage+ undergrad_degree + work_experience
             +emp_test_percentage +specialisation +mba_percent
             , data=dataset, family = "binomial")
summary(model1)
model2 <- glm(status ~ gender+ssc_percentage +hsc_percentage +hsc_board 
              +hsc_subject +degree_percentage+ undergrad_degree + work_experience
              +emp_test_percentage +mba_percent
              , data=dataset, family = "binomial")
summary(model2)
model3 <- glm(status ~ gender +ssc_percentage +hsc_percentage +hsc_board 
              +hsc_subject +degree_percentage+ undergrad_degree + work_experience
              +mba_percent
              , data=dataset, family = "binomial")
summary(model3)
model4 <- glm(status ~ gender +ssc_percentage +hsc_percentage 
              +hsc_subject +degree_percentage+ undergrad_degree + work_experience
              +mba_percent
              , data=dataset, family = "binomial")
summary(model4)
model5 <- glm(status ~ gender +ssc_percentage +hsc_percentage 
              +degree_percentage+ undergrad_degree + work_experience
              +mba_percent
              , data=dataset, family = "binomial")
summary(model5)
model6 <- glm(status ~ gender +ssc_percentage +hsc_percentage 
              +degree_percentage + work_experience
              +mba_percent
              , data=dataset, family = "binomial")
summary(model6)
model7 <- glm(status ~ ssc_percentage +hsc_percentage 
              +degree_percentage + work_experience
              +mba_percent
              , data=dataset, family = "binomial")
summary(model7)
model$coefficients
model7$coefficients


#---uji kelayakan model---
# H0 = model sudah sesuai/layak
# H1 = model belum sesuai/belum layak
library(ResourceSelection)
hoslem.test(dataset$status, fitted(model7))
#karena p-value = 0.9953 > alpha = 0.05, maka gagal tolak H0
#MODEL SESUAI, jadi model dapat dijadikan sebagai alternatif model karena sudah sesuai


#---regresi logistic biner dengan menggunakan Bayesian---#
library(rstanarm)
modelBayes <- stan_glm(status ~ gender +ssc_percentage +ssc_board +hsc_percentage +hsc_board 
                       +hsc_subject +degree_percentage+ undergrad_degree + work_experience
                       +emp_test_percentage +specialisation +mba_percent
                       , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes)

#---uji signifikansi parameter---#
modelBayes1 <- stan_glm(status ~ gender +ssc_percentage +hsc_percentage +hsc_board 
                       +hsc_subject +degree_percentage+ undergrad_degree + work_experience
                       +emp_test_percentage +specialisation +mba_percent
                       , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes1)

modelBayes2 <- stan_glm(status ~ gender +ssc_percentage +hsc_percentage +hsc_board 
                        +hsc_subject +degree_percentage+ undergrad_degree + work_experience
                        +emp_test_percentage +mba_percent
                        , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes2)

modelBayes3 <- stan_glm(status ~ gender +ssc_percentage +hsc_percentage +hsc_board 
                        +hsc_subject +degree_percentage+ undergrad_degree + work_experience
                       +mba_percent
                        , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes3)

modelBayes4 <- stan_glm(status ~ gender +ssc_percentage +hsc_percentage 
                        +hsc_subject +degree_percentage+ undergrad_degree + work_experience
                        +mba_percent
                        , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes4)

modelBayes5 <- stan_glm(status ~ gender +ssc_percentage +hsc_percentage 
                       +degree_percentage+ undergrad_degree + work_experience
                        +mba_percent
                        , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes5)

modelBayes6 <- stan_glm(status ~ gender +ssc_percentage +hsc_percentage 
                        +degree_percentage + work_experience
                        +mba_percent
                        , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes6)

modelBayes7 <- stan_glm(status ~ ssc_percentage +hsc_percentage 
                        +degree_percentage + work_experience
                        +mba_percent
                        , data = dataset, prior = normal(), prior_intercept = normal())
posterior_interval(modelBayes7)

modelBayes$coefficients
modelBayes7$coefficients

library(rstanarm)
modelBayes <- stan_glm(status ~ gender , data= dataset)
posterior_interval(modelBayes)

#---model terbaik---#
#model logistik
library(caret)
yhat = fitted(model7)
y = dataset$status
prediksi = as.factor(ifelse(round(yhat)==1, "Placed","Not Placed"))
aktual = as.factor(ifelse(y==1,"Placed","Not Placed"))

confusionMatrix(prediksi, aktual)


library(caret)
yhat1 = fitted(modelBayes7)
y = dataset$status
prediksibayes = as.factor(ifelse(round(yhat1)==1, "Placed","Not Placed"))
aktualbayes = as.factor(ifelse(y==1,"Placed","Not Placed"))

confusionMatrix(prediksibayes, aktualbayes)


#mape
library(Metrics)
ypredlog = predict(model7, dataset)
smape(dataset$status, ypredlog)

ypredbayes = predict(modelBayes7, dataset)
smape(dataset$status, ypredbayes)

#mse
mse(dataset$status, ypredlog)
mse(dataset$status, ypredbayes)
