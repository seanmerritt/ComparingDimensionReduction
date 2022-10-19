pacman::p_load(tidyverse, EGAnet,ica)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
wine <- read_delim("Raw/Wine/wine.data", delim = ',', col_names = FALSE) %>% 
  rename(target = X1)

dat <- wine %>% 
  select(-target) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat)
ega_dat <- scores$std.scores
ega_dat$target = wine$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- wine$target

results <- prcomp(dat, scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, wine)
trg <- data.frame(trg, wine)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:14), var_explained[1:14]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)

pca_dat <- trg %>% 
  select(PC1:PC4, target)


ica_results <- ica(dat, nc = 13, method = "fast")

qplot(c(1:14), dat.ica$vafs[1:14]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 5, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- wine$target
  
write.csv(ica_dat,"Prepped/ICA/Classification/Wine_ICA.csv")

write.csv(ega_dat,"Prepped/EGA/Classification/Wine_EGA.csv")

write.csv(uva_dat,"Prepped/UVA/Classification/Wine_UVA.csv")

wine %>% 
  write.csv('Prepped/Non-reduced/Wine_FULL.csv')
