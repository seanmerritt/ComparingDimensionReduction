pacman::p_load(tidyverse, EGAnet)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
sc <- read_csv("Raw/Superconducter/train.csv")

sc <-  sc %>% 
  rename(target = critical_temp) 

dat <- sc %>% 
  select(-target) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat, impute = 'mean')
ega_dat <- scores$std.scores
ega_dat$target = sc$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- sc$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, dat)
trg <- data.frame(trg, sc)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:81), var_explained[1:81]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0,1)

pca_dat <- trg %>% 
  select(PC1:PC6, target)

ica_results <- ica(dat , nc = 81, method = "fast")

qplot(c(1:81),ica_results$vafs[1:81]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 4, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- sc$target

write.csv(ica_dat,"Prepped/ICA/Regression/SC_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Regression/SC_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Regression/SC_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Regression/SC_UVA.csv")

sc %>% 
  write.csv('Prepped/Non-reduced/Regression/SC_FULL.csv')
