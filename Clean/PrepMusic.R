pacman::p_load(tidyverse, EGAnet)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
music <- read_csv("Raw/Music/Acoustic Features.csv")



music <- music %>% 
  rename(target = Class) %>% 
  mutate(target = case_when(target == "relax" ~ 0,
                            target == "happy" ~ 1,
                            target == "sad" ~ 2,
                            target == "angry" ~ 3))

dat <- music %>% 
  select(-target) 



ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat, impute = "mean")
ega_dat <- scores$std.scores
ega_dat$target = music$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- music$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, music)
trg <- data.frame(trg, music)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:50), var_explained[1:50]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0,1)

pca_dat <- trg %>% 
  select(PC1:PC4, target)

ica_results <- ica(dat , nc = 50, method = "fast")

qplot(c(1:50),ica_results$vafs[1:50]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 4, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- music$target

write.csv(ica_dat,"Prepped/ICA/Classification/Music_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Classification/music_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Classification/music_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Classification/music_UVA.csv")

music %>% 
  write.csv('Prepped/Non-reduced/Classifcation/music_FULL.csv')
