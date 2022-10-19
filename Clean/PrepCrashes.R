pacman::p_load(tidyverse, EGAnet,ica)
# setwd("../../Data")
park <- read_csv("Raw/Parkinsons/parkinsons_updrs.data")

park <-  park %>% 
  rename(target = total_UPDRS) %>% 
  select(-`subject#`, -age, -sex, -test_time) %>% 
  na.omit()

dat <- park %>% 
  select(-target) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat, impute = 'mean')
ega_dat <- scores$std.scores
ega_dat$target = park$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- ega_dat$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, dat)
trg <- data.frame(trg, park)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:18), var_explained[1:18]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0,1)

pca_dat <- trg %>% 
  select(PC1, target)

ica_results <- ica(dat , nc = 17, method = "fast")

qplot(c(1:50),ica_results$vafs[1:50]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 3, method = "fast")



write.csv(pca_dat,"Prepped/PCA/Classification/crashes_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Classification/crashes_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Classification/crashes_UVA.csv")

park %>% 
  write.csv('Prepped/Non-reduced/Classifcation/crashes_FULL.csv')
