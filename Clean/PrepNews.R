pacman::p_load(tidyverse, EGAnet)
# setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data")
news <- read_csv("Raw/News/OnlineNewsPopularity.csv")

news <-  news %>% 
  rename(target = shares) %>% 
  select(-url, -timedelta, -weekday_is_monday, -weekday_is_tuesday, -weekday_is_wednesday, -weekday_is_thursday, -weekday_is_friday, -weekday_is_saturday, -weekday_is_sunday, -is_weekend )

dat <- news %>% 
  select(-target) 

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat, impute = 'mean')
ega_dat <- scores$std.scores
ega_dat$target = news$target

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- news$target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, dat)
trg <- data.frame(trg, news)


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
  select(PC1:PC12, target)

ica_results <- ica(dat , nc = 50, method = "fast")

qplot(c(1:50),ica_results$vafs[1:50]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 3, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- news$target

write.csv(ica_dat,"Prepped/ICA/Regression/News_ICA.csv")

write.csv(pca_dat,"Prepped/PCA/Regression/News_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Regression/News_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Regression/News_UVA.csv")

news %>% 
  write.csv('Prepped/Non-reduced/Regression/News_FULL.csv')