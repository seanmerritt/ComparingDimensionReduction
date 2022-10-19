pacman::p_load(tidyverse)
Regression_CV_Scores <- read_csv("../../Output/Regression_CV_Scores.csv") %>% 
  na.omit()


Regression_CV_Scores %>% 
  pivot_longer(LASSO:XGB, names_to = "Algorithm", values_to = "RMSE") %>% 
  group_by(Data) %>% 
  mutate(RMSE = abs(RMSE)) %>%
  # mutate(RMSE = (RMSE - mean(RMSE))/sd(RMSE)) %>% 
  rename(ReductionMethod = Model) %>% 
  jmv::ANOVA(formula = RMSE ~ ReductionMethod +Algorithm + Data, postHoc = list("ReductionMethod"), effectSize = "partEta", postHocES = 'd')




for (data in unique(Regression_CV_Scores$Data)){
  print(data)
  results <- Regression_CV_Scores %>% 
    pivot_longer(LASSO:XGB, names_to = "Algorithm", values_to = "RMSE") %>% 
    mutate(RMSE = abs(RMSE)) %>%
    # mutate(RMSE = (RMSE - mean(RMSE))/sd(RMSE)) %>% 
    rename(ReductionMethod = Model) %>% 
    filter(Data == data) %>% 
    jmv::ANOVA(formula = RMSE ~ ReductionMethod + Algorithm, postHoc = list("ReductionMethod"), effectSize = "partEta", postHocES = 'd')
  print(results)
  
}


pd <- position_dodge(0.9)

Regression_CV_Scores %>% 
  View()
  pivot_longer(lasso:XGB, names_to = "Algorithm", values_to = "MSE") %>% 
  View()

Regression_CV_Scores %>% 
  rename(RFR = "RFC") %>% 
  pivot_longer(LASSO:XGB, names_to = "Algorithm", values_to = "RMSE") %>%
  mutate(RMSE = abs(RMSE)) %>% 
  dplyr::group_by(Model, Data,Algorithm) %>% 
  summarise(Mean_RMSE = mean(RMSE), se = sd(RMSE)/sqrt(5), se_top = Mean_RMSE + se, se_bottom = Mean_RMSE - se) %>% 
  rename(ReductionMethod = Model) %>% 
  ggplot(aes(fill = ReductionMethod, y = Mean_RMSE, x = Algorithm))+
  geom_bar(alpha = 0.90, stat = "identity", position = "dodge", color = "black", size = .1)+
  geom_errorbar(aes(ymin = se_bottom, ymax = se_top, group = ReductionMethod), width = .3, position = pd)+
  facet_wrap(~Data, scales = "free")+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(y = "RMSE", x = "", fill = "Reduction Method") +
  scale_fill_manual(
    values = c(
      "#264653", "#ff9b54",
      "#e9c46a", "#e76f51",
      "#2a9d8f"
    )
  )

ggsave(
  "../../Output/Figures/Regression_MSE.png",
  height = 8, width = 10, dpi = 600
)

# Check out best performing model + algorithm for each dataset
Regression_CV_Scores %>% 
  pivot_longer(LASSO:XGB, names_to = "Algorithm", values_to = "RMSE") %>% 
  mutate(RMSE = abs(RMSE)) %>%
  group_by(Data, Algorithm, Model) %>%
  summarize(RMSE = round(mean(RMSE, na.rm = TRUE), 3)) %>%
  as.data.frame()

