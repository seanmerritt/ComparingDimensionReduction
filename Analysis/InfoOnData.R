pacman::p_load(tidyverse,moments, EGAnet)
setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Data/Raw")


blog <- read_csv("Blog/blogData_train.csv",col_names = F) %>% 
  select(-X8, -X13, -X28, -X33, -X38, -X43, -X63, -X64, -X65, -X66, -X67, -X68, -X69, -X70, -X71, -X72, -X73, -X74, -X75, -X76, -X77, -X78, -X79, -X80, -X81, -X82, -X83, -X84, -X85, -X86, -X87, -X88, -X89, -X90, -X91, -X92, -X93, -X94, -X95, -X96, -X97, -X98, -X99, -X100, -X101, -X102, -X103, -X104, -X105, -X106, -X107, -X108, -X109, -X110, -X111, -X112, -X113, -X114, -X115, -X116, -X117, -X118, -X119, -X120, -X121, -X122, -X123, -X124, -X125, -X126, -X127, -X128, -X129, -X130, -X131, -X132, -X133, -X134, -X135, -X136, -X137, -X138, -X139, -X140, -X141, -X142, -X143, -X144, -X145, -X146, -X147, -X148, -X149, -X150, -X151, -X152, -X153, -X154, -X155, -X156, -X157, -X158, -X159, -X160, -X161, -X162, -X163, -X164, -X165, -X166, -X167, -X168, -X169, -X170, -X171, -X172, -X173, -X174, -X175, -X176, -X177, -X178, -X179, -X180, -X181, -X182, -X183, -X184, -X185, -X186, -X187, -X188, -X189, -X190, -X191, -X192, -X193, -X194, -X195, -X196, -X197, -X198, -X199, -X200, -X201, -X202, -X203, -X204, -X205, -X206, -X207, -X208, -X209, -X210, -X211, -X212, -X213, -X214, -X215, -X216, -X217, -X218, -X219, -X220, -X221, -X222, -X223, -X224, -X225, -X226, -X227, -X228, -X229, -X230, -X231, -X232, -X233, -X234, -X235, -X236, -X237, -X238, -X239, -X240, -X241, -X242, -X243, -X244, -X245, -X246, -X247, -X248, -X249, -X250, -X251, -X252, -X253, -X254, -X255, -X256, -X257, -X258, -X259, -X260, -X261, -X262, -X263, -X264, -X265, -X266, -X267, -X268, -X269, -X270, -X271, -X272, -X273, -X274, -X275, -X276, -X278,-X281) %>%
  distinct(X1,.keep_all = TRUE)

#blog_lct <- blog %>% LCT(forcePD = T)

cancer <- read_delim("BreastCancer/wdbc.data", delim = ",", col_names = F) %>% 
  select(-X2)

#cancer_lct <- cancer %>% LCT()

communities <- read_csv("Crime/communities.csv") %>% 
  mutate_at(vars(X6:X127), ~replace_na(.,mean(., na.rm = TRUE))) %>% 
  select(-X1,-X2, -X3, -X4, -X5, -X128)

#communities_lct <- communities %>% LCT()

divorce <- read_delim("Divorce/divorce.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

#divorce_lct <- divorce %>% LCT()


facebook <- read_delim("FacebookMetrics/dataset_Facebook.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE) %>% 
  select(-Type)

#facebook_lct <- facebook %>% LCT()


heart_train <- read_delim("Heart/SPECTF.train", 
                          delim = ",", escape_double = FALSE, trim_ws = TRUE, col_names = F)

heart_test <- read_delim("Heart/SPECTF.test", 
                         delim = ",", escape_double = FALSE, trim_ws = TRUE, col_names = F)

heart <-  heart_train %>% 
  rbind(heart_test)

#heart_lct <- heart %>% LCT()

mnist_train <- read_csv("MNIST/mnist_train.csv")
mnist_test <- read_csv("MNIST/mnist_test.csv")

combined <- mnist_train %>% 
  rbind(mnist_test) %>% 
  rename(target = label)

#combined_lct <- combined %>% LCT()

music <- read_csv("Music/Acoustic Features.csv") %>% 
  select(-Class)

#music_lct <- music %>% LCT()

news <- read_csv("News/OnlineNewsPopularity.csv") %>% 
  select(-url)

#news_lct <- news %>% LCT()

SkillCraft <- read_csv("SkillCraft/SkillCraft1_Dataset.csv") %>% 
  select(-GameID, -Age, -TotalHours, -HoursPerWeek)

#SkillCraft_lct <- SkillCraft %>% LCT()
         
sports <- read_csv("Sports/features.xls.csv") %>% 
  select(-TextID, -URL, - Label)

#sports_lct <- sports %>% LCT()

sc <- read_csv("Superconducter/train.csv")

#sc_lct <- sc %>% LCT()

wine <- read_delim("Wine/wine.data", delim = ',', col_names = FALSE)

#wine_lct <- wine %>% LCT()

park <- read_csv("Parkinsons/parkinsons_updrs.DATA")

park <-  park %>% 
  rename(target = total_UPDRS) %>% 
  select(-`subject#`, -age, -sex, -test_time) %>% 
  na.omit()

#park_lct <- park %>% LCT()

N <- c(dim(wine)[1], dim(sc)[1], dim(sports)[1], dim(blog)[1],dim(cancer)[1],dim(communities)[1],dim(divorce)[1],dim(facebook)[1], dim(heart)[1], dim(combined)[1], dim(music)[1], dim(news)[1], dim(SkillCraft)[1], dim(park)[1])

mean(N)
var(N)
min(N)
max(N)

mean_kurt <- c(mean(kurtosis(wine)), mean(kurtosis(sc)), mean(kurtosis(sports), na.rm = T), mean(kurtosis(blog), na.rm = T),mean(kurtosis(cancer), na.rm = T), mean(kurtosis(communities), na.rm = T), mean(kurtosis(divorce), na.rm = T),mean(kurtosis(facebook), na.rm = T), mean(kurtosis(heart), na.rm = T), mean(kurtosis(combined), na.rm = T), mean(kurtosis(music), na.rm = T), mean(kurtosis(news), na.rm = T), mean(kurtosis(SkillCraft), na.rm = T), mean(kurtosis(park), na.rm = T))
var_kurt <- c(var(kurtosis(wine)), var(kurtosis(sc)), var(kurtosis(sports), na.rm = T), var(kurtosis(blog), na.rm = T),var(kurtosis(cancer), na.rm = T),var(kurtosis(communities), na.rm = T),var(kurtosis(divorce), na.rm = T),var(kurtosis(facebook), na.rm = T), var(kurtosis(heart), na.rm = T), var(kurtosis(combined), na.rm = T), var(kurtosis(music), na.rm = T), var(kurtosis(news), na.rm = T), var(kurtosis(SkillCraft), na.rm = T), var(kurtosis(park)))

#network <- c(wine_lct$bootsrap, sc_lct$bootsrap, sports_lct$bootsrap, blog_lct$bootsrap, cancer_lct$bootsrap, communities_lct$bootsrap, divorce_lct$bootsrap, facebook_lct$bootsrap, heart_lct$bootsrap, combined_lct$bootsrap, music_lct$bootsrap, news_lct$bootsrap, SkillCraft_lct$bootsrap, park_lct$bootsrap)

info <- data.frame(wine = dim(wine)[2], SC = dim(sc)[2], Sports = dim(sports)[2], blog =  dim(blog)[2],Cancer = dim(cancer)[2], Crime = dim(communities)[2], Divorce = dim(divorce)[2], Facebook = dim(facebook)[2], Heart = dim(heart)[2], MNIST = dim(combined)[2], Music =  dim(music)[2], News = dim(news)[2], Skillcraft = dim(SkillCraft)[2], Parkinsons = dim(park)[2])

mean(A)
var(A)
min(A)
max(A)

setwd("C:/Users/seanm/Dropbox/Research/EGA_vs_PCA/Output")

data_info <- info %>% 
  pivot_longer(wine:Parkinsons, names_to = "Data", values_to = "Attributes") %>% 
  cbind(N) %>% 
  cbind(mean_kurt) %>% 
  cbind(var_kurt) %>% 
  write.csv("data_info.csv")




