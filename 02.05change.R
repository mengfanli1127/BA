## comment: it is better to only install packages conditionally, i.e., if they are required but haven't been installed before
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

## comment: I removed the data.table package, as it's not needed by your solution

## comment: relative paths are preferable
dir = "./iso-per-test-results"
file_list = list.files(path = dir,pattern = "*.csv$",recursive = TRUE,full.names = TRUE)
df <- list()
for (i in file_list) {
  df[[i]] <- read_csv(i,col_names=F)
  #write_csv(df,store_csv,append=TRUE,col_names= FALSE)
  colnames(df[[i]]) <- c('test_name','test_result','time_to_run_test','run_num','machine_id','test_class_order_md5sum','num_test_class','module_path','slug','sha')
}
mergedata <- bind_rows(df)

## comment: you can use dplyr for the following. personally I find that easier to remember. test_name was missing; see further below
## input <- mergedata[,c('test_result','time_to_run_test','run_num','machine_id')]
## input$test_result <- ifelse(test = input$test_result == "pass",yes = 1, no = 0)
input <- mergedata %>%
  select(test_name, test_result, time_to_run_test, run_num, machine_id) %>%
  mutate(test_result = ifelse(test_result == "pass", 0, 1))

## comment: no need for factors
## input$test_result <- as.factor(input$test_result)
## input$time_to_run_test <- as.factor(input$time_to_run_test)
## input$run_num <- as.factor(input$run_num)
## input$machine_id <- as.factor(input$machine_id)

## comment: checks are good! I just commented those out, because they do not show by default on the console, anyway. maybe better to use assertions
## any(is.na(input))
## str(input)

## comment: your fits fail, because you try to fit the model across all data. unless we have good reason to believe that test results are independent of the actual tests, we should create one model for each test separately.
## fit_all = glm(test_result~time_to_run_test+run_num+machine_id,binomial(link = 'logit'),data = input)
## summary(fit_all)
## fit_num_id = glm(test_result ~ run_num+machine_id,binomial(link = 'logit'),data = input)
## summary(fit_num_id)
## coef(fit_num_id)
## a <- summary(fit_all)
## View(a)
## logistic.a = glm(test_result ~run_num, binomial(link = 'logit'),data = input)
## summary(logistic.a)
## logistic.b = glm(test_result~machine_id,binomial(link="logit"),data = input)
## summary(logistic.b)
## logistic.c = glm(test_result ~time_to_run_test, binomial(link = 'logit'),data = input)
## summary(logistic.c)

output <- input %>%
  group_by(test_name) %>%
  filter(n_distinct(test_result)>1) %>%
  summarize(runs = n(), failures = sum(test_result), logit = list(glm(test_result ~ time_to_run_test + run_num + machine_id, family = "binomial")))

## comment: the above puts the glm objects directly into the dataframe along with the test names. this then needs further processing. for instance, we may want to know which of these tests have any significant coefficients.

sig_output <- output %>%
  rowwise %>%
  mutate(logit = list(as_tibble(coef(summary(logit)), rownames = "param"))) %>%
  filter(any(logit$`Pr(>|z|)` < 0.05))


## here begins the method of the logistf
install.packages("logistf")
library(logistf)

fit<- input %>%
  group_by(test_name) %>%
  filter(n_distinct(test_result)>1) %>%
  summarise(logit = list(logistf(test_result ~ time_to_run_test + run_num + machine_id,firth = TRUE,pl = TRUE)))

sig_output_fit <- fit %>%
  rowwise %>%
  mutate(logit = list(as_tibble(coef(summary(logit)), rownames = "param"))) %>%
  filter(any(logit$`Pr(>|z|)` < 0.05))

##another try of the method one
fit<- input %>%
  group_by(test_name) %>%
  filter(n_distinct(test_result)>1) %>%
  summarise(runs = n(), failures = sum(test_result),logit = list(logistf(test_result ~ time_to_run_test + run_num + machine_id,firth = TRUE,pl = TRUE)))



####the final version of the method 01

test_data_final <- input %>% filter(test_name != "org.java_websocket.issues.Issue256Test.runReconnectScenario1")

logit_final <- test_data_final %>% 
  group_by(test_name) %>% 
  filter(n_distinct(test_result)>1) %>% 
  summarise(runs = n(), failures = sum(test_result),logit = list(logistf(test_result ~ time_to_run_test + run_num + machine_id,firth = TRUE,pl = TRUE)))





## here begins the method of the zelig(just a try,still need more considerations)
devtools::install_github('IQSS/Zelig')
install.packages("zeligverse")
library(Zelig)


fit_two <- input %>%
  group_by(test_name) %>%
  filter(n_distinct(test_result)>1) %>%
  mutate(tau = sum(test_result == 1)/n()) %>%
  summarize(runs = n(), failures = sum(test_result),logit = list(zelig(test_result ~ time_to_run_test + run_num + machine_id,data = input, model = "ls",cite = FALSE,tau = tau)))

sig_output_two <-
  mutate(logit = list(as_tibble(coef(summary(logit)), rownames = "param"))) %>%
  filter(any(logit$`Pr(>|z|)` < 0.05))


##another try for the method two
fit_two <-  input %>%
  group_by(test_name) %>%
  filter(n_distinct(test_result)>1)

z.out1 <- zelig(test_result ~ time_to_run_test + run_num + machine_id,data = fit_two, model = "relogit",tau = 32765/428000)
summary(z.out1)








