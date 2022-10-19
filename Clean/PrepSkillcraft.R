pacman::p_load(tidyverse, EGAnet)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
SkillCraft <- read_csv("Raw/SkillCraft/SkillCraft1_Dataset.csv")

SkillCraft <-  SkillCraft %>% 
  rename(target = LeagueIndex) %>% 
  select(-GameID, -Age, -TotalHours, -HoursPerWeek )

dat <- SkillCraft %>% 
  select(-target) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat, impute = 'mean')
ega_dat <- scores$std.scores
ega_dat$target = SkillCraft$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- SkillCraft$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, dat)
trg <- data.frame(trg, SkillCraft)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:16), var_explained[1:16]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0,1)

pca_dat <- trg %>% 
  select(PC1:PC4, target)

ica_results <- ica(dat , nc = 15, method = "fast")

qplot(c(1:15),ica_results$vafs[1:15]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 6, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- SkillCraft$target

write.csv(ica_dat,"Prepped/ICA/Regression/SkillCraft_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Regression/SkillCraft_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Regression/SkillCraft_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Regression/SkillCraft_UVA.csv")

SkillCraft %>% 
  write.csv('Prepped/Non-reduced/Regression/SkillCraft_FULL.csv')
