pacman::p_load(tidyverse, EGAnet, ica)
# setwd("../../Data/Raw/Blog")
blog <- read_csv("./Raw/Blog/blogData_train.csv",col_names = F)

dat <- blog %>% 
  select(-X8, -X13, -X28, -X33, -X38, -X43, -X63, -X64, -X65, -X66, -X67, -X68, -X69, -X70, -X71, -X72, -X73, -X74, -X75, -X76, -X77, -X78, -X79, -X80, -X81, -X82, -X83, -X84, -X85, -X86, -X87, -X88, -X89, -X90, -X91, -X92, -X93, -X94, -X95, -X96, -X97, -X98, -X99, -X100, -X101, -X102, -X103, -X104, -X105, -X106, -X107, -X108, -X109, -X110, -X111, -X112, -X113, -X114, -X115, -X116, -X117, -X118, -X119, -X120, -X121, -X122, -X123, -X124, -X125, -X126, -X127, -X128, -X129, -X130, -X131, -X132, -X133, -X134, -X135, -X136, -X137, -X138, -X139, -X140, -X141, -X142, -X143, -X144, -X145, -X146, -X147, -X148, -X149, -X150, -X151, -X152, -X153, -X154, -X155, -X156, -X157, -X158, -X159, -X160, -X161, -X162, -X163, -X164, -X165, -X166, -X167, -X168, -X169, -X170, -X171, -X172, -X173, -X174, -X175, -X176, -X177, -X178, -X179, -X180, -X181, -X182, -X183, -X184, -X185, -X186, -X187, -X188, -X189, -X190, -X191, -X192, -X193, -X194, -X195, -X196, -X197, -X198, -X199, -X200, -X201, -X202, -X203, -X204, -X205, -X206, -X207, -X208, -X209, -X210, -X211, -X212, -X213, -X214, -X215, -X216, -X217, -X218, -X219, -X220, -X221, -X222, -X223, -X224, -X225, -X226, -X227, -X228, -X229, -X230, -X231, -X232, -X233, -X234, -X235, -X236, -X237, -X238, -X239, -X240, -X241, -X242, -X243, -X244, -X245, -X246, -X247, -X248, -X249, -X250, -X251, -X252, -X253, -X254, -X255, -X256, -X257, -X258, -X259, -X260, -X261, -X262, -X263, -X264, -X265, -X266, -X267, -X268, -X269, -X270, -X271, -X272, -X273, -X274, -X275, -X276, -X278) %>%
  distinct(X1,.keep_all = TRUE) %>% 
  rename(target = X281) 

target <- dat$target

dat <- dat %>% 
  select(-target)

ega.dat <- dat %>% 
  EGA()

scores <- net.scores(data = dat, A = ega.dat)
ega_dat <- scores$std.scores
ega_dat$target = target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, dat)
trg <- data.frame(trg, dat)

uva_dat <- dat %>% 
  UVA(reduce.method = "sum")

scores <- uva_dat$reduced$data
uva_dat <- as.data.frame(scores)
uva_dat$target <- target

results <- prcomp(dat %>% na.omit(), scale = TRUE)
results$rotation <- -1*results$rotation

trg <- predict(results, dat)
trg <- data.frame(trg, dat)


#display principal components
results$rotation

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:59), var_explained[1:59]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)

pca_dat <- trg %>% 
  select(PC1:PC8)

pca_dat$target = target

ica_results <- ica(dat, nc = 59, method = "fast")

qplot(c(1:59),ica_results$vafs[1:59]) + 
  geom_line() + 
  xlab("Independent Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


ica_results <- ica(dat, nc = 3, method = "fast")

ica_dat <- data.frame(ica_results$S)
ica_dat$target <- target

setwd("../../../Data")
write.csv(ica_dat,"Prepped/ICA/Regression/Blog_ICA.csv")
write.csv(pca_dat,"Prepped/PCA/Regression/blog_PCA.csv")
write.csv(ega_dat,"Prepped/EGA/Regression/blog_EGA.csv")
write.csv(uva_dat,"Prepped/UVA/Regression/blog_UVA.csv")

dat$target <- target

dat %>% 
  write.csv('Prepped/Non-reduced/Regression/blog_FULL.csv')
