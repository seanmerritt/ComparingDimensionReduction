pacman::p_load(tidyverse)
setwd("../../Output")
Regression_CV_Scores <- read_csv("Regression_CV_Scores.csv") %>% 
  na.omit()
Classification_CV_Scores <- read_csv("Classification_CV_Scores.csv") %>% 
  na.omit()

data_info <- read_csv("data_info.csv")


Classification_Scores_info <- Classification_CV_Scores %>% 
  pivot_longer(Logit:XGB, names_to = "Algorithm", values_to = "Accuracy") %>% 
  rename(ReductionMethod = Model) %>% 
  select(-'...1') %>% 
  left_join(data_info) %>% 
  select(-'...1') %>% 
  mutate(ReductionMethod = ifelse(ReductionMethod == "Non", "ANON", ReductionMethod))


methods <- unique(Classification_Scores_info$ReductionMethod)

lapply(seq_along(methods), function(i){
  
  classification_data <- Classification_Scores_info %>%
    filter(ReductionMethod == methods[i])
  
  Attritbutes_classification <- lm(Accuracy ~ Attributes + Algorithm , classification_data) 
  N_classification  <- lm(Accuracy ~ N + Algorithm , classification_data) 
  kurt_classification <- lm(Accuracy ~ mean_kurt + Algorithm , classification_data)
  
})

Attritbutes_classification <- lm(Accuracy ~ ReductionMethod*Attributes + Algorithm , Classification_Scores_info) 
N_classification  <- lm(Accuracy ~ ReductionMethod*N + Algorithm , Classification_Scores_info) 
kurt_classification <- lm(Accuracy ~ ReductionMethod*mean_kurt + Algorithm , Classification_Scores_info)




Regression_Scores_info <- Regression_CV_Scores %>% 
  pivot_longer(LASSO:XGB, names_to = "Algorithm", values_to = "MSE") %>% 
  group_by(Data) %>% 
  mutate(MSE = (MSE - mean(MSE))/sd(MSE)) %>% 
  rename(ReductionMethod = Model) %>% 
  select(-'...1') %>% 
  left_join(data_info) %>% 
  select(-'...1') %>%  
  mutate(ReductionMethod = ifelse(ReductionMethod == "Non", "ANON", ReductionMethod))

Attritbutes_regression<- lm(MSE ~ ReductionMethod*Attributes + Algorithm , Regression_Scores_info)

N_Regression <- lm(MSE ~ ReductionMethod*N + Algorithm , Regression_Scores_info)

kurt_Regression <- lm(MSE ~ ReductionMethod*mean_kurt + Algorithm , Regression_Scores_info)

Scores_info <- Classification_CV_Scores %>% 
  pivot_longer(Logit:XGB, names_to = "Algorithm", values_to = "Accuracy") %>% 
  rename(ReductionMethod = Model) %>% 
  select(-'...1') %>% 
  left_join(data_info) %>% 
  select(-'...1') %>% 
  filter(Data != "MNIST") 

summary(lm(Accuracy ~ ReductionMethod*Attributes + Algorithm , Scores_info))  
summary(lm(Accuracy ~ ReductionMethod*N + Algorithm , Scores_info)) 
summary(lm(Accuracy ~ ReductionMethod*mean_kurt + Algorithm , Scores_info)) 
summary(lm(Accuracy ~ ReductionMethod*var_kurt + Algorithm , Scores_info))


stargazer::stargazer(Attritbutes_classification, N_classification,kurt_classification, Attritbutes_regression, N_Regression, kurt_Regression)
