# download
setwd("C:/Users/rladb/Documents/R_review")
install.packages(c("tidyverse", "data.table","caret"))
install.packages("ROCR")
install.packages('rpart')
# packages
library(dplyr)
library(tidyverse)
library(data.table)
library(devtools)
library(car)
library(ROCR)
library(pROC)
library(rpart)
# data
data <- read.csv("3_LoL_esports_match_data.csv", header = T)
data <- data[,-1]
summary(data)
nrow(data)
ncol(data)
colnames(data)
head(data)
## data preprocessing
# 자료형 변환
data <- data %>% mutate_at(`.vars` = c("result","playoffs","firstblood" ,"firstbloodkill" ,"firstdragon","firstherald","firstbaron","firsttower" ,"firstmidtower", "firsttothreetowers","firsttower"), `.funs` = as.factor)
data %>% str()

# data completeness가 완료된 것만 가져오기.
data <- data[data$datacompleteness == "complete",]
table(data$datacompleteness)

# team data만 가져오기
data <- data[data$position=="team",]
table(data$position)

# 필요한 변수 가져오기
#'result','top_dtpm','jng_ka','mid_dpm','bot_dpm','sup_apg','kills','deaths','assists','firstblood','team kpm','ckpm','firstdragon','dragons','elders','firstherald','heralds','firstbaron','barons','firsttower','towers','firsttothreetowers','inhibitors','dpm','wpm','totalgold','goldat10','xpat10','csat10','golddiffat10','xpdiffat10','csdiffat10','killsat10','assistsat10','deathsat10','goldat15','xpat15','csat15','golddiffat15','xpdiffat15','csdiffat15','killsat15','assistsat15','deathsat15'
data_l <- data[,c(30,18,19,20,21,22,31,32,33,40,44,45,46,47,58,60,61,63,64,66,67,70,73,76,81,87,99,100,101,105,106,107,108,109,110,114,115,116,120,121,122,123,124,125)]
#data_l$result <- as.numeric(data_l$result)
#data_l$result <- data_l$result-1
# na omit
table(is.na(data_l))
clean_data <- na.omit(data_l)

str(clean_data)
View(clean_data)
table(data$result)
# train test
index <- caret::createDataPartition(y = clean_data$result, p = 0.8, list = FALSE)
train <- clean_data[index, ]
test <- clean_data[-index, ]

train %>% head()
test %>% head()

## logistic regression
log_fit <- glm(result~.,train,family = binomial)
summary(log_fit)


# 재정의 - summary 
log_fit1 <- glm(result~top_dtpm+jng_ka+bot_dpm+sup_apg+kills+deaths+assists+team.kpm+ckpm+firstdragon+dragons+elders+firstbaron+barons+towers+firsttothreetowers+inhibitors+totalgold+goldat15+golddiffat15+killsat15+deathsat15,train,family = binomial)
summary(log_fit1)

# 다중 공선성 확인
vif(log_fit1)

#재정의-다중 공선성
log_fit2 <- glm(result~top_dtpm+jng_ka+bot_dpm+sup_apg+firstherald+heralds+firstdragon+dragons+elders+firstbaron+barons+towers+firsttothreetowers+inhibitors+totalgold+goldat15+golddiffat15+killsat15+deathsat15,train,family = binomial)
summary(log_fit2)

# step으로 변수 선택
stplog2 <- step(log_fit2, direction = "backward")
summary(stplog2)

# 중요도
caret::varImp(log_fit2) 
caret::varImp(stplog2) 

# coufusion matrix 보여주기
printCM(stplog2) # vif step

# coufusion matrix function
printCM <- function(x){
  # test 세트로 예측
  predict_value <- predict(x, test, type = "response")%>% tibble(predict_value = .)
  predict_check <- test %>% select(result) %>% dplyr::bind_cols(., predict_value) 
  predict_cutoff <- predict_check %>% mutate(predict_value_cutoff = as.factor(ifelse(predict_value > 0.5, 1, 0)))
  # 변수
  predicted <- predict_cutoff$predict_value_cutoff
  actual <-  as.factor(predict_cutoff$result)
  # confusion matrix
  caret::confusionMatrix(predicted,actual,positive = '1')}

# ## 원래 비교하려했는데 그냥 끝냈다
# # 결정나무
# tree_fit <- rpart(formula = result~., data = train, method = "class")
# summary(tree_fit)
# 
# # coufusion matrix
# predict_value <- predict(tree_fit, test, type = "class") %>%tibble(predict_value = .)
# predict_check <- test %>% select(result) %>% bind_cols(., predict_value)
# cm <- caret::confusionMatrix(predict_value$predict_value, test$result);cm
