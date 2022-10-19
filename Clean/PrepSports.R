pacman::p_load(tidyverse, EGAnet)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
sports <- read_csv("Raw/Sports/features.xls.csv")

sports <- sports %>%
  select(-TextID, -URL,  -JJS, -NNP, -TOs, -WP, -WRB, -ellipsis, -sentence1st, -sentencelast) %>% 
  rename(target = Label) %>% 
  mutate(target = ifelse(target == "objective",1,0))

dat <- sports %>% 
  select( CC:compsupadjadv) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat)
ega_dat <- scores$std.scores
ega_dat$target = sports$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- sports$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, sports)
trg <- data.frame(trg, sports)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:43), var_explained[1:43]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)

pca_dat <- trg %>% 
  select(PC1:PC4, target)

ica_results <- ica(dat , nc = 43, method = "fast")

qplot(c(1:43),ica_results$vafs[1:43]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 5, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- sports$target

write.csv(ica_dat,"Prepped/ICA/Classification/Sports_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Classification/sports_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Classification/sports_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Classification/sports_UVA.csv")


sports %>% 
  select(target, CC:compsupadjadv) %>% 
  write.csv('Prepped/Non-reduced/Classifcation/sports_FULL.csv')
