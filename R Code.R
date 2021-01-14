install.packages("neuralnet")
library(neuralnet)

nn=neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
             linear.output = FALSE)
setwd("/Users/kars-jangiesen/Desktop/Master DSMA/Block 2/Seminar for Data Science and Marketing Analytics/Assignments/Individual Assignment 2")


url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases//haberman/haberman.data'

Hab_Data <- read_csv(file = url,
                     col_names = c('Age', 'Operation_Year', 
                                   'Number_Pos_Nodes','Survival')) %>%
  na.omit() %>%
  mutate(Survival = ifelse(Survival == 2, 0, 1),
         Survival = factor(Survival))

scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

Hab_Data <- Hab_Data %>%
  mutate(Age = scale01(Age), 
         Operation_Year = scale01(Operation_Year), 
         Number_Pos_Nodes = scale01(Number_Pos_Nodes), 
         Survival = as.numeric(Survival)-1)
Hab_Data <- Hab_Data %>%
  mutate(Survival = as.integer(Survival) - 1, 
         Survival = ifelse(Survival == 1, TRUE, FALSE))

set.seed(123)
Hab_NN1 <- neuralnet(Survival ~ Age + Operation_Year + Number_Pos_Nodes, 
                     data = Hab_Data, 
                     linear.output = FALSE, 
                     err.fct = 'ce', 
                     likelihood = TRUE)

Hab_NN2 <- neuralnet(Survival ~ Age + Operation_Year + Number_Pos_Nodes, 
                     data = Hab_Data, 
                     linear.output = FALSE, 
                     err.fct = 'ce', 
                     likelihood = TRUE)
plot(Hab_NN2)


smalldatatrain <- Churn_scaled[1:500,]
smalldatatest <- Churn_scaled[500:650,]

Hab_NN1 <- neuralnet(Exited ~ ., 
                     data = smalldata, 
                     linear.output = FALSE, 
                     err.fct = 'ce')
plot(Hab_NN1)

predicted.nn3.values <- neuralnet::compute(Hab_NN1,smalldatatest[,-1])
predicted.nn3.values$net.result <- sapply(predicted.nn3.values$net.result,round,digits=0)
table(smalldatatest$Exited,predicted.nn3.values$net.result)



Hab_NN2 <- neuralnet(Exited ~ ., 
                     data = Churn.test, 
                     linear.output = FALSE, 
                     err.fct = 'ce')
plot(Hab_NN2)

predicted.nn.values <- neuralnet::compute(Hab_NN2,Churn.test[,-1])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(Churn.test$Exited,predicted.nn.values$net.result)


Hab_NN3 <- neuralnet(Exited ~ ., 
                     data = Churn.test, 
                     linear.output = FALSE, 
                     err.fct = 'ce',hidden = c(2,1))
plot(Hab_NN3)

predicted.nn.values <- neuralnet::compute(Hab_NN3,Churn.test[,-1])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(Churn.test$Exited,predicted.nn.values$net.result)

NN4 <- neuralnet(Exited ~ ., 
                     data = Churn.test, 
                     linear.output = FALSE, 
                     err.fct = 'ce',hidden = c(1,2))
plot(NN4)

predicted.nn.values <- neuralnet::compute(NN4,Churn.test[,-1])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(Churn.test$Exited,predicted.nn.values$net.result)

NN5 <- neuralnet(Exited ~ ., 
                 data = Churn.test, 
                 linear.output = FALSE, 
                 err.fct = 'ce',hidden = c(2,2))
plot(NN5)

predicted.nn.values <- neuralnet::compute(NN5,Churn.test[,-1])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(Churn.test$Exited,predicted.nn.values$net.result)


#Hello new line!

plot.nnet(m4a)


