pacman::p_load(tidyverse, EGAnet,ica)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
mnist_train <- read_csv("Raw/MNIST/mnist_train.csv")
mnist_test <- read_csv("Raw/MNIST/mnist_test.csv")

combined <- mnist_train %>% 
  rbind(mnist_test) %>% 
  rename(target = label)



columns  <- combined %>% 
  summarize_all(mean) %>% 
  select(-target) %>% 
  pivot_longer(`1x1`:`28x28`) %>% 
  filter(value != 0 , value !=1)

dat <- combined %>% 
  select(unlist(unique(columns$name))) 

ega.dat <-  dat %>% 
  EGA()


scores <- net.scores(data = dat, A = ega.dat, impute = "mean")
ega_dat <- scores$std.scores
ega_dat$target = combined$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- combined$target

results <- prcomp(dat, scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, combined)
trg <- data.frame(trg, combined)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:100), var_explained[1:100]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.10)

pca_dat <- trg %>% 
  select(PC1:PC25, target)

ica_results <- ica(dat, nc = 100, method = "fast")

qplot(c(1:100),ica_results$vafs[1:100]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 25, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- combined$target

write.csv(ica_dat,"Prepped/ICA/Classification/MNIST_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Classification/MNIST_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Classification/MNIST_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Classification/MNIST_UVA.csv")

combined %>% 
  select(target,unlist(unique(columns$name)))  %>% 
  write.csv('Prepped/Non-reduced/MNIST_FULL.csv')

