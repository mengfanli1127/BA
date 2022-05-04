install.packages("tidyverse")
library(tidyverse)
install.packages("data.table")
library(data.table)
#install.packages("sampling")
#library(sampling)
dir = "/Users/limengfan-eleanor/Desktop/task/iso-per-test-results 2"
file_list = list.files(path = dir,pattern = "*.csv$",recursive = TRUE,full.names = TRUE)
store_csv = paste(dir,"merge.csv")
df <- list()
for (i in file_list) {
  df[i] = read_csv(i)
  #write_csv(df,store_csv,append=TRUE,col_names= FALSE)
  colnames(df[i]) <- c('test_name','test_result','time_to_run_test','run_num','machine_id','test_class_order_md5sum','num_test_class','module_path','slug','sha')
}
#mergedata <- read.csv("E:/R/data/iso-per-test-results 2 merge.csv")
mergedata <- bind_rows(df)
#colnames(mergedata) <- c('test_name','test_result','time_to_run_test','run_num','machine_id','test_class_order_md5sum','num_test_class','module_path','slug','sha')

input <- mergedata[,c('test_result','time_to_run_test','run_num','machine_id')]
input$test_result <- ifelse(test = input$test_result == "pass",yes = 1, no = 0)
input$test_result <- as.factor(input$test_result)
input$time_to_run_test <- as.factor(input$time_to_run_test)
input$run_num <- as.factor(input$run_num)
input$machine_id <- as.factor(input$machine_id)
any(is.na(input))
str(input)

#samp <- sample(20000,20000)
#testinput <- input[samp,]
#dim(input)
#summary(input)

fit_all = glm(test_result~time_to_run_test+run_num+machine_id,binomial(link = 'logit'),data = testinput)
summary(fit_all)
fit_num_id = glm(test_result ~ run_num+machine_id,binomial(link = 'logit'),data = testinput)
summary(fit_num_id)
coef(fit_num_id)
a <- summary(fit_all)
View(a)

logistic.a = glm(test_result ~run_num, binomial(link = 'logit'),data = input)
summary(logistic.a)
logistic.b = glm(test_result~machine_id,binomial(link="logit"),data = input)
summary(logistic.b)
logistic.c = glm(test_result ~time_to_run_test, binomial(link = 'logit'),data = testinput)
summary(logistic.c)
