pacman::p_load(tidyverse, EGAnet)
# setwd("../../Data")
cancer <- read_delim("Raw/BreastCancer/wdbc.data", delim = ",", col_names = F)

cancer <- cancer %>% 
  rename(target = X2) %>% 
  mutate(target = ifelse(target == "M",0,1)) %>% 
  select(-X1)

dat <- cancer %>% 
  select(-target) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat)
ega_dat <- scores$std.scores
ega_dat$target = cancer$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- ega_dat$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, cancer)
trg <- data.frame(trg, cancer)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:30), var_explained[1:30]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)

pca_dat <- trg %>% 
  select(PC1:PC7, target)

ica_results <- ica(dat, nc = 30, method = "fast")

qplot(c(1:59),ica_results$vafs[1:59]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 5, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- cancer$target

write.csv(ica_dat,"Prepped/ICA/Classification/Cancer_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Classification/cancer_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Classification/cancer_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Regression/cancer_UVA.csv")

cancer %>% 
  write.csv('Prepped/Non-reduced/Classifcation/cancer_FULL.csv')
