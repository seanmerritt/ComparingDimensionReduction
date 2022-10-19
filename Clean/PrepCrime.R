pacman::p_load(tidyverse, EGAnet, R.matlab)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")

communities <- read_csv("Raw/Crime/communities.data", 
                        col_names = FALSE)

write.csv(communities,"communities.csv")

communities <- read_csv("Raw/Crime/communities.csv")

communities <- communities %>% 
  rename(target = X128) %>% 
  mutate_at(vars(X6:X127), ~replace_na(.,mean(., na.rm = TRUE)))
  

dat <- communities %>% 
  select(-X1,-X2, -X3, -X4, -X5, -target)



ega.dat <- dat %>% 
  EGA()


scores <- net.scores(data = dat , A = ega.dat)
ega_dat <- scores$std.scores
ega_dat$target = communities$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- communities$target

results <- prcomp(dat, scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, communities)
trg <- data.frame(trg, communities)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:12), var_explained[1:12]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)

pca_dat <- trg %>% 
  select(PC1:PC6, target)

ica_results <- ica(dat, nc = 120, method = "fast")

qplot(c(1:59),ica_results$vafs[1:59]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 5, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- communities$target

write.csv(ica_dat,"Prepped/ICA/Regression/Crime_ICA.csv")


write.csv(pca_dat,"Prepped/PCA/Regression/Crime_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Regression/Crime_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Regression/Crime_UVA.csv")

communities %>% 
  select(-`...1`,-X1,-X2, -X3, -X4, -X5) %>% 
  write.csv('Prepped/Non-reduced/Crime_Full.csv')
