pacman::p_load(tidyverse)
Classification_CV_Scores <- read_csv("../../Output/Classification_CV_Scores.csv") %>% 
  na.omit()


Classification_CV_Scores %>% 
  pivot_longer(Logit:XGB, names_to = "Algorithm", values_to = "Accuracy") %>% 
  rename(ReductionMethod = Model) %>% 
  jmv::ANOVA(formula = Accuracy ~ ReductionMethod + Algorithm + Data, postHoc = list("ReductionMethod"), effectSize = "partEta", postHocES = 'd' )



for (data in unique(Classification_CV_Scores$Data)){
  print(data)
  results <- Classification_CV_Scores %>% 
    pivot_longer(Logit:XGB, names_to = "Algorithm", values_to = "Accuracy") %>% 
    rename(ReductionMethod = Model) %>% 
    filter(Data == data) %>% 
    jmv::ANOVA(formula = Accuracy ~ ReductionMethod + Algorithm, postHoc = list("ReductionMethod"), effectSize = "partEta", postHocES = 'd' )
  print(results)
  
}


pd <- position_dodge(0.9)

Classification_CV_Scores %>% 
  pivot_longer(Logit:XGB, names_to = "Algorithm", values_to = "Accuracy") %>%
  dplyr::group_by(Model, Data, Algorithm) %>% 
  summarise(Mean_accuracy = mean(Accuracy), se = sd(Accuracy), se_top = Mean_accuracy + se, se_bottom = Mean_accuracy - se) %>% 
  rename(ReductionMethod = Model) %>% 
  ggplot(aes(fill = ReductionMethod, y = Mean_accuracy, x = Algorithm)) +
  geom_bar(alpha = 0.90, stat = "identity", position = "dodge", color = "black", size = .1)+
  geom_errorbar(aes(ymin = se_bottom, ymax = se_top, group = ReductionMethod), width = .3, position = pd)+
  facet_wrap(~Data)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(y = "Accuracy", x = "", fill = "Reduction Method") +
  scale_fill_manual(
    values = c(
      "#264653", "#ff9b54",
      "#e9c46a", "#e76f51",
      "#2a9d8f"
    )
  ) +
  scale_y_continuous(
    limits = c(0.65, 1.0),
    oob = scales::rescale_none,
    breaks = seq(0.70, 1, 0.10),
    labels = formatC(
      seq(0.70, 1, 0.10),
      format = "f", digits = 2, flag = "0"
    )
  )

ggsave(
  "../../Output/Figures/Classifcation_accuracy.png",
  height = 8, width = 10, dpi = 600
)

# Check out best performing model + algorithm for each dataset
Classification_CV_Scores %>% 
  pivot_longer(Logit:XGB, names_to = "Algorithm", values_to = "Accuracy") %>% 
  group_by(Data, Algorithm, Model) %>%
  summarize(Accuracy = round(mean(Accuracy, na.rm = TRUE), 3)) %>%
  as.data.frame()


