pacman::p_load(tidyverse, EGAnet,ica)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
divorce <- read_delim("Raw/Divorce/divorce.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)



divorce <- divorce %>% 
  rename(target = Class)

dat <- divorce %>% 
  select(-target) 



ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat)
ega_dat <- scores$std.scores
ega_dat$target = divorce$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- ega_dat$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, divorce)
trg <- data.frame(trg, divorce)


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
  select(PC1:PC2, target)

ica_results <- ica(dat, nc = 54, method = "fast")

qplot(c(1:54),ica_results$vafs[1:54]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 2, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- divorce$target

write.csv(ica_dat,"Prepped/ICA/Classification/divorce_ICA.csv")


write.csv(pca_dat,"Prepped/PCA/Classification/divorce_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Classification/divorce_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Classification/divorce_UVA.csv")


divorce %>% 
  write.csv('Prepped/Non-reduced/Classifcation/divorce_FULL.csv')
