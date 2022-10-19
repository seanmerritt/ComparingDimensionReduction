pacman::p_load(tidyverse, EGAnet)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
heart_train <- read_delim("Raw/Heart/SPECTF.train", 
                      delim = ",", escape_double = FALSE, trim_ws = TRUE, col_names = F)

heart_test <- read_delim("Raw/Heart/SPECTF.test", 
                          delim = ",", escape_double = FALSE, trim_ws = TRUE, col_names = F)

heart <-  heart_train %>% 
  rbind(heart_test) %>% 
  rename(target = X1)

dat <- heart %>% 
  select(-target) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat, impute = 'mean')
ega_dat <- scores$std.scores
ega_dat$target = heart$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- ega_dat$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, dat)
trg <- data.frame(trg, heart)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:54), var_explained[1:54]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0,1)

pca_dat <- trg %>% 
  select(PC1:PC5, target)

ica_results <- ica(dat , nc = 44, method = "fast")

qplot(c(1:44),ica_results$vafs[1:44]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 5, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- heart$target

write.csv(ica_dat,"Prepped/ICA/Classification/Heart_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Classification/heart_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Classification/heart_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Classification/heart_UVA.csv")

heart %>% 
  write.csv('Prepped/Non-reduced/Classifcation/heart_FULL.csv')
