##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##++++++++++++++++++ NORM KOLLEKTIVE IMPORTIEREN +++++++++++++++++++++++++++##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
# Data importation
library(janitor)
library(readxl)


# Data cleaning and preprocessing
##+++++++++++ Calculating the standard deviation score - Control db ++++++++###
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
#1) subsetting control databank according to sex
#2) than subsetting according to group ages
# For each subset we will do the following operations:
#3) filter out irrelevant columns and categorical variables
#4) log 10 transformtion
#5) Standard deviation Score calculation
#6) transpose and convert to dataframe

# STEP 1: subsetting databank according to group age
library(janitor)
# Neonates Database control
library(readxl)
urine_24h_NormBMI_Hochberg_Tonou_TF <- read_excel("~/urine_24h_NormBMI_Hochberg_Tonou_TF.xlsx", 
                                                  sheet = "Sheet2")
bb.DB_ct <- urine_24h_NormBMI_Hochberg_Tonou_TF
bb.DB_ct <- clean_names(bb.DB_ct)

# change sex column factors from w to f
bb.DB_ct$sex[bb.DB_ct$sex=="w"]= "f"

# age group < 2,5 years factor = 1
bb.DB_ct$age_group = 1

#+ Children > 2,5years Database control 
urine_24h_NormBMI_Hochberg_Tonou_Sp_TF <- read_excel("C:/Users/Anwender/Downloads/urine_24h_NormBMI_Hochberg_Tonou_Sp_TF.xlsx", 
                                                     sheet = "Sheet1 Sp")
DB_ct <- urine_24h_NormBMI_Hochberg_Tonou_Sp_TF
DB_ct <- clean_names(DB_ct)

# structure of databank: - check data types for every variable
str(DB_ct)

# Filtering out unwanted columns
library(tidyverse)
DB_ct <- select(DB_ct,-c(2,3,6,8,9,10,11))

# STEP 2: subsetting according to group age
DB_ct$age_group[DB_ct$age>=2.5 & DB_ct$age<=4.49]=2
DB_ct$age_group[DB_ct$age>=4.5 & DB_ct$age<=6.49]=3
DB_ct$age_group[DB_ct$age>=6.5 & DB_ct$age<=8.49]=4
DB_ct$age_group[DB_ct$age>=8.5 & DB_ct$age<=10.49]=5
DB_ct$age_group[DB_ct$age>=10.5 & DB_ct$age<=12.49]=6
DB_ct$age_group[DB_ct$age>=12.5 & DB_ct$age<=14.49]=7
DB_ct$age_group[DB_ct$age>=14.5 & DB_ct$age<=16.49]=8
DB_ct$age_group[DB_ct$age>=16.5 & DB_ct$age<=18.49]=9


# merge neonate and children DB
DB_control <- rbind(bb.DB_ct,DB_ct)

## scaling according to sex and age - standard deviation score
# convert age_group and sex to factor variables
DB_control$age_group <- as.factor(DB_control$age_group)
DB_control$sex <- as.factor(DB_control$sex)

# filter out the internal standards from control DB
DB_control <- select(DB_control, -c(5,28))
DB_control<-select(DB_control,1,2,41,3:40) # reorder the dataframe

# is it necessary to log transform the data? What about other transformation techniques
# Like square root transformation
# if yes,which value should be added before the log-trans
#DB_control<- log10(DB_control[,6:41]+0.1)
# standard deviation score calculation
zscore <- by(log10(DB_control[,6:41]+0.1),list(DB_control$sex,DB_control$age_group),scale)
zscore_DB_control<-do.call(rbind,zscore)
zscore_DB_control <- as.data.frame(zscore_DB_control)

# From zscore object retreive the means and std for each age group and sex respectively
# Age group 1 
female_1_mean <- attr(zscore[[1]],"scaled:center")
female_1_std <- attr(zscore[[1]],"scaled:scale")
male_1_mean<- attr(zscore[[2]],"scaled:center")
male_1_std <- attr(zscore[[2]],"scaled:scale")

# Age group 2
female_2_mean <- attr(zscore[[3]],"scaled:center")
female_2_std <- attr(zscore[[3]],"scaled:scale")
male_2_mean<- attr(zscore[[4]],"scaled:center")
male_2_std <- attr(zscore[[4]],"scaled:scale")

# Age group 3
female_3_mean <- attr(zscore[[5]],"scaled:center")
female_3_std <- attr(zscore[[5]],"scaled:scale")
male_3_mean<- attr(zscore[[6]],"scaled:center")
male_3_std <- attr(zscore[[6]],"scaled:scale")

# Age group 4
female_4_mean <- attr(zscore[[7]],"scaled:center")
female_4_std <- attr(zscore[[7]],"scaled:scale")
male_4_mean<- attr(zscore[[8]],"scaled:center")
male_4_std <- attr(zscore[[8]],"scaled:scale")

# Age group 5
female_5_mean <- attr(zscore[[9]],"scaled:center")
female_5_std <- attr(zscore[[9]],"scaled:scale")
male_5_mean<- attr(zscore[[10]],"scaled:center")
male_5_std <- attr(zscore[[10]],"scaled:scale")

# Age group 6
female_6_mean <- attr(zscore[[11]],"scaled:center")
female_6_std <- attr(zscore[[11]],"scaled:scale")
male_6_mean<- attr(zscore[[12]],"scaled:center")
male_6_std <- attr(zscore[[12]],"scaled:scale")

# Age group 7
female_7_mean <- attr(zscore[[13]],"scaled:center")
female_7_std <- attr(zscore[[13]],"scaled:scale")
male_7_mean<- attr(zscore[[14]],"scaled:center")
male_7_std <- attr(zscore[[14]],"scaled:scale")

# Age group 8
female_8_mean <- attr(zscore[[15]],"scaled:center")
female_8_std <- attr(zscore[[15]],"scaled:scale")
male_8_mean<- attr(zscore[[16]],"scaled:center")
male_8_std <- attr(zscore[[16]],"scaled:scale")

# Age group 9
female_9_mean <- attr(zscore[[17]],"scaled:center")
female_9_std <- attr(zscore[[17]],"scaled:scale")
male_9_mean<- attr(zscore[[18]],"scaled:center")
male_9_std <- attr(zscore[[18]],"scaled:scale")

# female = 1 and male = 2
DB_control$sex <- ifelse(DB_control$sex == "f",1,2)


#NaN.z.databank_ct <- z.databank_ct[,-c(7:9,21)] # dropping out estrogens & testosterone columns
                                                # because of numerous NaNs

summary(zscore_DB_control) %>% 
  kable() %>% 
  kable_styling()
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##+++++++ TUMOR PATIENTEN DATAFRAME IMPORTIEREN ++++++++++++++++++++++++++++##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
library(janitor) #library for clean_names func

# read tumor DB
Diagnosen_Tumore_Final_List_Sp_TF <- read_excel("~/Diagnosen_Tumore_Final_List_Sp_TF.xlsx",
                                                sheet = "Sheet3 Sp")
DB_tp <-Diagnosen_Tumore_Final_List_Sp_TF
DB_tp <- clean_names(DB_tp) # transforms col names to more friendly names for MLA
variable.names(DB_tp) # checking out the column names of the dataframe

#data.table::setnames(DB_tp,'diagnose_a_k','diagnose')
#data.table::setnames(DB_tp,'e_3','e3')

##+++++++++++ Calculating the standard deviation score - Tumor db ++++++++###
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
#1) subsetting control databank according to sex
#2) than subsetting according to group ages
# For each subset we will do the following operations:
#3) filter out irrelevant columns and categorical variables
#4) log 10 transformtion
#5) Standard deviation Score calculation
#6) transpose and convert to dataframe

# STEP 1: subsetting according to group age
DB_tp$age_group[DB_tp$age < 2.5] = 1
DB_tp$age_group[DB_tp$age>=2.5 & DB_tp$age<=4.49]=2
DB_tp$age_group[DB_tp$age>=4.5 & DB_tp$age<=6.49]=3
DB_tp$age_group[DB_tp$age>=6.5 & DB_tp$age<=8.49]=4
DB_tp$age_group[DB_tp$age>=8.5 & DB_tp$age<=10.49]=5
DB_tp$age_group[DB_tp$age>=10.5 & DB_tp$age<=12.49]=6
DB_tp$age_group[DB_tp$age>=12.5 & DB_tp$age<=14.49]=7
DB_tp$age_group[DB_tp$age>=14.5 & DB_tp$age<=16.49]=8
DB_tp$age_group[DB_tp$age>=16.5 & DB_tp$age<=18.49]=9

# filter out unnecessary columns  
DB_tp <- select(DB_tp,-c(1:3,6:9,11:23))

# female = 1 and male = 2
DB_tp$sex <- ifelse(DB_tp$sex == 0,2,1)

# log transforming quantitative variables
log_DB_tp <- log10(DB_tp[,-c(1:3,40)]+0.1)
log_DB_tp$age_group <- DB_tp$age_group
log_DB_tp$sex <- DB_tp$sex
log_DB_tp$diagnose <- DB_tp$diagnose_a_k

# Define a function to calculate the standard deviation score 
std_score <- function(tp.data1, mean.data2, std.data3){
  name <- names(tp.data1[,-c(37:39)])
  tp.data1[name] <- Map(`-`,tp.data1[name],mean.data2[name])
  tp.data1[name] <- Map(`/`,tp.data1[name],std.data3[name])
  return(tp.data1)
}

# build small subsets based on age group and sex,
# before doing the normalisation
tp1 <- subset(log_DB_tp,age_group == 1 & sex == 1)
tp1 <- std_score(tp1,female_1_mean,female_1_std)

tp2 <- subset(log_DB_tp,age_group == 1 & sex == 2)
tp2 <- std_score(tp2,male_1_mean,male_1_std)

tp3 <- subset(log_DB_tp,age_group == 2 & sex == 1)
tp3 <- std_score(tp3,female_2_mean,female_2_std)

tp4 <- subset(log_DB_tp,age_group == 2 & sex == 2)
tp4 <- std_score(tp4,male_2_mean,male_2_std)

tp5 <- subset(log_DB_tp,age_group == 3 & sex == 1)
tp5 <- std_score(tp5,female_3_mean,female_3_std)

tp6 <- subset(log_DB_tp,age_group == 3 & sex == 2)
tp6 <- std_score(tp6,male_3_mean,male_3_std)

tp7 <- subset(log_DB_tp,age_group == 4 & sex == 1)
tp7 <- std_score(tp7,female_4_mean,female_4_std)

tp8 <- subset(log_DB_tp,age_group == 4 & sex == 2)
tp8 <- std_score(tp8,male_4_mean,male_4_std)

tp9 <- subset(log_DB_tp,age_group == 5 & sex == 1)
tp9 <- std_score(tp9,female_5_mean,female_5_std)

tp10 <- subset(log_DB_tp,age_group == 5 & sex == 2)
tp10 <- std_score(tp10,male_5_mean,male_5_std)

tp11 <- subset(log_DB_tp,age_group == 6 & sex == 1)
tp11 <- std_score(tp11,female_6_mean,female_6_std)

tp12 <- subset(log_DB_tp,age_group == 6 & sex == 2)
tp12 <- std_score(tp12,male_6_mean,male_6_std)

tp13 <- subset(log_DB_tp,age_group == 7 & sex == 1)
tp13 <- std_score(tp13,female_7_mean,female_7_std)

tp14 <- subset(log_DB_tp,age_group == 7 & sex == 2)
tp14 <- std_score(tp14,male_7_mean,male_7_std)

tp15 <- subset(log_DB_tp,age_group == 8 & sex == 1)
tp15 <- std_score(tp15,female_8_mean,female_8_std)

tp16 <- subset(log_DB_tp,age_group == 8 & sex == 2)
tp16 <- std_score(tp16,male_8_mean,male_8_std)

tp17 <- subset(log_DB_tp,age_group == 9 & sex == 1)
tp17 <- std_score(tp17,female_9_mean,female_9_std)

tp18 <- subset(log_DB_tp,age_group == 9 & sex == 2)
tp18 <- std_score(tp18,male_9_mean,male_9_std)

# The following subsets have no observations [tp6,tp12,tp14,tp16,tp18], 
# so can be excluded from the merging

zscore_DB_Tumor <- bind_rows(tp1,tp2,tp3,tp4,tp5,tp7,tp8,tp9,tp10,tp11,tp13,tp15,tp17)

#NaN.z.databank_tp <- z.databank_tp[,-c(7:9,21)] # estrogens und testosterone ausgeschlossen!!
#NaN.z.databank_tp <- na.omit(NaN.z.databank_tp)

## Explore the datasets
install.packages("kableExtra")
library(kableExtra)

# convert diagnose to a factor variable
zscore_DB_Tumor$diagnose <- as.factor(zscore_DB_Tumor$diagnose)

# summary of the tumor DB
summary(zscore_DB_Tumor) %>% 
  kable() %>% 
  kable_styling()
boxplot(zscore_DB_Tumor)
# Historgram for each attribute - NORM 
library(tidyr)
zscore_DB_control%>%
  select(-c(7:9,21)) %>% 
  gather(Attributes, value, 1:20) %>% 
  ggplot(aes(x=value,)) + 
  geom_histogram(fill = "lightblue2", color = "black", bins = 15) +
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram for each variable in Control database")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Attributes, scales = "free") +
  labs(x = "Value", y = "Frequency")


# Historgram for each attribute - databank_tp
library(tidyr)
zscore_DB_Tumor%>%
  select(-c(7:9,21)) %>% 
  gather(Attributes, value, 1:20) %>% 
  ggplot(aes(x=value,)) + 
  geom_histogram(fill = "lightblue2", color = "black", bins = 15) +
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram for each variable in Tumor Patients database")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Attributes, scales = "free") +
  labs(x = "Value", y = "Frequency")

# Hist, cor und scatterplots 
install.packages("psych")
library(psych) 
pairs.panels(zscore_DB_Tumor[1:6], gap = 0, bg = "red", pch = 21) # Hist, cor und scatterplots

#**********************************************************************
#*OPTIONAL
# ths group boxplot 
ggplot(zscore_DB_Tumor, aes(y=ths, color=diagnose)) +
  geom_boxplot(fill="white")+
  ggtitle("THS distribution in ACA, ACC and ACX")
# f gorup boxplot
ggplot(zscore_DB_Tumor, aes(x=f, color=diagnose)) +
  geom_histogram(fill="white", bins = 30)+
  ggtitle("THS distribution in ACA, ACC and ACX")

#************************************************************************************


# data preprocessing
#databank$sex <- as.factor(databank$sex)
#databank$diagnose <- as.factor(databank$diagnose)
#databank<-na.omit(databank) #remove ROWS with missing values

#----------------------------------------------------------------------------#
############ HIERACHICAL ANALYSE(CLUSTERING) #################################
#----------------------------------------------------------------------------#
## find optimal cluster number
library(factoextra)
library(cluster)
library(ggplot2)

fviz_nbclust(zscore_DB_Tumor[,-c(7:9,21,37:39)],
             kmeans,
             method = "silhouette")
fviz_nbclust(zscore_DB_Tumor[,-c(7:9,21,37:39)], kmeans, nstart = 25,  method = "gap_stat", nboot = 150)+
  labs(subtitle = "Gap statistic method")

## visualize K-MEANS

km<-kmeans(zscore_DB_Tumor[,-c(7:9,21,37:39)],
           centers = 3,
           nstart = 20)
fviz_cluster(km, data = zscore_DB_Tumor[,-c(7:9,21,37:39)])



#------------------------------------------------------------------------------#
####################### HEATMAP ################################################
#------------------------------------------------------------------------------#

## import required libraries
install.packages("heatmaply")
library(heatmaply)
library(ggplot2)
library(plotly)

## Heatmap

gradient_col<- ggplot2::scale_fill_gradient2(
  low = "blue",
  high = "red",
  midpoint = 0
  #limits=c(-41.95,41.95)
)
heatmaply(zscore_DB_Tumor[,-c(7:9,21,37:38)],
          xlab = "Metaboliten",
          ylab = "Probe Nr",
          main = "Heatmap Tumor patients",
          grid_gap = 0,
          scale = "column",
          scale_fill_gradient_fun = gradient_col,
          colors = viridis(n=200, alpha = 1, begin = 0, end = 1, option = "viridis"),
          row_text_angle = 0,
          column_text_angle = 45)


#+++++++++++++++ MERGE NORM KOLLEKTIVE WITH -TUMOR PATIENT DB ++++++++++#

# merge control- & Tp databank
zscore_DB_control$diagnose <- "Control"
databank <- bind_rows(zscore_DB_control, zscore_DB_Tumor[,-c(37:38)])


#+++++++++++++++++ HEATMAP ++++++++++++++++++++++++++++++++++++#
## import required libraries
install.packages("heatmaply")
library(heatmaply)
library(ggplot2)
library(plotly)

## Heatmap

gradient_col<- ggplot2::scale_fill_gradient2(
  low = "blue",
  high = "red",
  midpoint = 0
  #limits=c(-41.95,41.95)
)
heatmaply(databank[,-c(7:9,21,38)],
          grid_gap = 0,
          xlab = "Metaboliten",
          ylab = "Probe Nr",
          main = "Heatmap Control and Tumor Patients",
          scale = "column",
          scale_fill_gradient_fun = gradient_col,
          colors = viridis(n=200, alpha = 1, begin = 0, end = 1, option = "viridis"),
          row_text_angle = 0,
          column_text_angle = 45)

### heatmap correlation
library(GGally)

# correlation between the variables
ggcorr(databank[,-c(7:9,21,37,38)], #excluded diagnose
       label = TRUE,label_size = 1.9, label_round = 2,
       method = c("everything", "pearson"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
################### GROUP BOXPLOTS ####################################
install.packages("reshape2") # Install reshape2 package
library("reshape2")
library(ggplot2)
#dev.off() # run when the graphice device is messed up

#----------------------+ databank_long +--------------------------#

# Androgens
databank_short<- databank[,c(1:6,37)]
databank_long<-melt(databank_short, id = "diagnose") #convert wide format to a long format
bplt_a <- ggplot(databank_long, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  ggtitle("Andogens und Präkursor")+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25),plot.title = element_text(hjust = 0.5))+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
#coord_flip()
bplt_a
bplt_a.stats <- ggplot_build(bplt_a)$data # print out stats from geom.boxplot

#------------------------+ databank_long_1 +-----------------------#
# Androgens
databank_short_1 <- databank[,c(9,11:12,14,18,37)]

databank_long_1<-melt(databank_short_1, id ="diagnose") #convert wide format to a long format
bplt_b <- ggplot(databank_long_1, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25), plot.title = element_text(hjust = 0.5))+
  ggtitle("Androgens und Präkursor")+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
  #coord_flip()
bplt_b
bplt_b.stats <- ggplot_build(bplt_b)$data # stats results

#-------------------------+ databank_long_2 +----------------------------------#
# progesteron
databank_short_2 <- databank[,c(10,13,15:16,37)]

databank_long_2<-melt(databank_short_2, id ="diagnose") #convert wide format to a long format
bplt_c <- ggplot(databank_long_2, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  ggtitle("Progesteron und Präkursor")+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25), plot.title = element_text(hjust = 0.5))+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
  #coord_flip()
bplt_c
bplt_c.stats <- ggplot_build(bplt_b)$data # stats results

#------------------+ databank_long_3 +-----------------------------------------#
#progesteron
databank_short_3 <- databank[,c(17,19,22:23,37)]

databank_long_3<-melt(databank_short_3, id ="diagnose") #convert wide format to a long format
bplt_d <- ggplot(databank_long_3, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  ggtitle("Progesteron und Präkursor")+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25),plot.title = element_text(hjust = 0.5))+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
  #coord_flip()
bplt_d
bplt_d.stats <- ggplot_build(bplt_d)$data # stats results

#------------------+ databank_long_4 +-----------------------------------------#
# Mineralocorticoid
databank_short_4 <- databank[,c(25:27,20,37)]

databank_long_4<-melt(databank_short_4, id ="diagnose") #convert wide format to a long format
bplt_e <- ggplot(databank_long_4, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  ggtitle("Mineralocorticoids und Präkursor")+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25),plot.title = element_text(hjust = 0.5))+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
  #coord_flip()
bplt_e
bplt_e.stats <- ggplot_build(bplt_e)$data # stats results

#------------------+ databank_long_5 +-----------------------------------------#
# Glucocorticoid
databank_short_5 <- databank[,c(24,28:31,37)]

databank_long_5 <-melt(databank_short_5, id ="diagnose") #convert wide format to a long format
#label <- labs(title = "Glucocorticoids", tag = "center")
bplt_f <- ggplot(databank_long_5, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25), plot.title = element_text(hjust = 0.5))+
  ggtitle("Glucorticoids und Präkursor")+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
  #coord_flip()
bplt_f
bplt_f.stats <- ggplot_build(bplt_f)$data # stats results

#-------------------+ databank_long_6 +----------------------------------------#
# Glucocorticoids

databank_short_6 <- databank[,c(32:36,33)]
databank_long_6 <-melt(databank_short_6, id ="diagnose") #convert wide format to a long format
bplt_g <- ggplot(databank_long_6, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25), plot.title = element_text(hjust = 0.5))+
  ggtitle("Glucorticoids und Präkursor")+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
  #coord_flip()
bplt_g
bplt_g.stats <- ggplot_build(bplt_g)$data
#------------------+ databank_long_7 +-----------------------------------------#
# Estrogens
databank_short_7 <- databank[,c(7:8,21,37)]

databank_long_7 <-melt(databank_short_7, id ="diagnose") #convert wide format to a long format
bplt_h <- ggplot(databank_long_7, aes(x = variable, y = value, color = diagnose))+# ggplot function
  scale_color_discrete('diagnose')+
  stat_boxplot(geom = "errorbar", # Boxplot with error bars
               width = 0.2)+
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25))+
  geom_boxplot(alpha = 0.9, outlier.colour = "cyan")+
  stat_summary(fun = mean,
               geom = "point",
               size = 0.9,
               color = "black")
#coord_flip()
bplt_h
bplt_h.stats <- ggplot_build(bplt_h)$data # stats results

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
############# SUPERVISED MACHINE LEARNING #####################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

################### 1) Logistic Regression ###################################
#binomial regression 
#glm_model0 <- glm(diagnose ~ ., data = databank, family = binomial())

#glm_model1 <- glm(diagnose ~ an, data = databank, family = binomial()) 


################## 2) DECISION TREE ##########################################

#import required libraries
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
# decision tree model - Differentiating all 3 Tumor classes
tree_model_1 <- rpart(diagnose ~ ., zscore_DB_Tumor)
rpart.plot(tree_model_1, type = 1, yesno = 2, extra = 1)
tree_model_1$variable.importance

# decision tree model - Differentiating ACC from ACA
tree_model_2 <- rpart(diagnose ~ ., zscore_DB_Tumor[-c(8,10,17),]) # excluding ACX patients
rpart.plot(tree_model_2, type = 1, yesno = 2, extra = 1)
tree_model_2$variable.importance

# decision tree model - gesamte databank
tree_model_3 <- rpart(diagnose ~ ., databank)
rpart.plot(tree_model_3, type = 1, yesno = 2, extra = 1)
tree_model_3$variable.importance



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++++++++++++ sparse partial least square discriminant analysis +++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
install.packages("mdatools")
library(mdatools)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("mixOmics")
library(mixOmics)
# importing dependencies of mixOmics package
library(MASS)
library(lattice)
library(ggplot2)

# Creating the x and y components for the splsda model
x <- as.matrix(na.omit(databank[,-c(7:9,21,37:38)])) # ACX rows excluded
c  <- as.factor(databank$diagnose) #[-c(155,157,164),]

# generate splsda model/object
spls.model <- mixOmics::splsda(x, c, keepX = c(30,30),ncomp = 2, scale = T,
                     max.iter = 500, tol = 1e-06)
background <- background.predict(spls.model, comp.predicted=2,
                                 dist = "max.dist") 
plotIndiv(spls.model, comp = 1:2, group = d,
          ind.names = FALSE, title = "Maximum distance with predicted background",
          legend = TRUE,  background = background)

# performance         #dist = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"),
performance <- perf(spls.model,
     dist = "all",
     choices = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"),
     validation = c("Mfold", "loo"),
     signif.threshold = 0.05,
     folds = 5, nrepeat =2, auc = TRUE, progressBar = TRUE, cpus = 2)
plot(performance)

# Clustering using splsda object derived from splsda method - individual names appear     #ind.names = c
plotIndiv(spls.model, group = c, legend = TRUE, ellipse =TRUE,
          ellipse.level = 0.99, cex = 4, centroid = TRUE,star=F, 
          title = "SUPERVISED CLUSTERING - ACA,ACC & CTRL")

#plot variables 
plotVar(spls.model, style = "ggplot2", plot = TRUE, var.names = TRUE)

#select variables
selectVar(spls.model, comp=1)$name 
selectVar(spls.model, comp=2)$name 
# look at the contribution (median) for each variable
#plot.contrib <- plotLoadings(spls.model, comp = 1, method = 'median', plot = TRUE, 
                            #contrib = "max",max.name.length = 50)

#####################################################################
#++++++++++++++++++++ tumor databank +++++++++++++++++++++++++++++++#
#+
# spls-da model using all 03 Tumor types - ACC, ACA, ACX
# generate x component (data matrix), y component (factor variable)
y <- as.matrix(na.omit(zscore_DB_Tumor[,-c(7:9,21,37:39)]))
d  <- as.factor(zscore_DB_Tumor$diagnose)

# Create splsda model
spls.model.tp <- mixOmics::splsda(y, d, keepX = c(30,30),ncomp = 2, scale = T,
                               max.iter = 500, tol = 1e-06)

# Predicted Background - Tumor patients (ACA;ACX & ACC)
# backgroud.predict and plotIndiv should be run all at once
background <- background.predict(spls.model.tp, comp.predicted=2,
                                 dist = "max.dist") 
plotIndiv(spls.model.tp, comp = 1:2, group = d,
          ind.names = FALSE, title = "Maximum distance with predicted background",
          legend = TRUE,  background = background)
# by graphic problems, run the code
dev.off()
# Clustering from the splsda object generated
plotIndiv(spls.model.tp, group = d, 
          legend = TRUE, ellipse =TRUE, ellipse.level = 0.99,#95% CI
          cex = 4, star = T,title = "CLUSTERING - Tumor patients")

# plot variables 
plotVar(spls.model.tp, style = "ggplot2", plot = TRUE, var.names = TRUE)

# look at the contribution (median) for each variable
plot.contrib.tp = plotLoadings(spls.model.tp, comp = 1, method = 'median', plot = TRUE,
                            contrib = "max",name.var = colnames(y))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++ ACA AND ACC Patients ++++++++++++++++++++++++++++++++++++++#
y1 <- as.matrix(na.omit(zscore_DB_Tumor[-c(8,10,17),-c(7:9,21,37:39)])) 
# ACX excluded
acx_zscore_DB_Tumor <- zscore_DB_Tumor[-c(8,10,17),]
d1  <- as.factor(acx_zscore_DB_Tumor$diagnose)

# Create splsda model
spls.acx.tp <- mixOmics::splsda(y1, d1, keepX = c(30,30),ncomp = 2, scale = T,
                                max.iter = 500, tol = 1e-06)

# Predicted Background - Tumor patients (ACA & ACC)
# backgroud.predict and plotIndiv should be run all at once
background <- background.predict(spls.acx.tp, comp.predicted=2,
                                 dist = "max.dist") 
plotIndiv(spls.acx.tp, comp = 1:2, group = d1,
          ind.names = FALSE, title = "Maximum distance with predicted background",
          legend = TRUE,  background = background)

# Clustering from the splsda object generated
plotIndiv(spls.acx.tp, group = d1, 
          legend = TRUE, ellipse =TRUE, ellipse.level = 0.99,#95% CI
          cex = 4, star = T,title = "CLUSTERING - Tumor patients")

# ROC CURVE
auc.plsda <- auroc(spls.model)
auc.plsda.tp <- auroc(spls.model.tp)
auc.spls.acx.tp <- auroc(spls.acx.tp)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
############### 3) Random Forest ############################################

databank_tp_model.scaled = scale(databank_tp_model[,-1]) #tp_databank scalieren au?er diagnose Spalte
data <- cbind(databank_tp_model$diagnose, databank_tp_model.scaled) #f?gt die diagnose Spalte hinzu
data <- as.data.frame(data) # wandelt die databank in einem dataframe um
data.table::setnames(data, "V1","diagnose") #benennen die V1 spalte um

#import libraries
install.packages("randomForest")
library(randomForest)
library(caret)
library(lattice)


#+++++++++ random forest model +++++++++++++++++++++++++++++++++++++

# training the data set using random forest method

modelFit <- train( diagnose ~ ., data = databank, method="rf", 
                   metric = "Accuracy",
                   importance = TRUE, tuneLength = 20)
mostImp <- varImp(modelFit) # list the 20 most important variables
plot(mostImp)

xy <- predict(modelFit, databank_acx)
table(xy,databank_acx$diagnose)
confusionMatrix(reference = databank_acx$diagnose, data = xy, mode = "everything")

# based on Mean decrease Accuracy: How the Accuracy level is affected when a variable is left out 
# Mean Gini Impurity Index: HOW frequent is a variable used in classifying the dependent variable
#rf variable importance

#only 20 most important variables shown (out of 36)

#Importance


# Random forest Model mit den 7 wichtigsten Metaboliten 
# f + a_thf + ths + tha + x6b_oh_f + x11_o_pt + a_thb
set.seed(222)
rf_7 <- randomForest(diagnose ~ f+ths+an+a_thf+x16a_oh_dhea+x11_o_pt+a_thb, 
                     data = NaN.z.databank_tp,
                     mtry = 2,
                     ntree = 96, 
                     importance = TRUE)
print(rf_7)
plot(rf_7)

#++++++++++++++++++ PREDICTIONS ++++++++++++++++++++++++
#+
# predict with the - ACX dataset
predictions.rf_7 <- predict(rf_7,databank_acx)
table(databank_acx$diagnose, predictions.rf_7)
confusionMatrix(reference = databank_acx$diagnose, data = predictions.rf_7, mode = "everything")

#Random forest Model mit den 4 wichtigsten Metaboliten
#f + x6b_oh_f + a_thb + po_5a_3a
set.seed(222)
rf_4 <- randomForest(diagnose ~ f+x20a_dhf+ths+a_thf, 
                     data = NaN.z.databank_tp,
                     mtry = 2,
                     ntree = 54, 
                     importance = TRUE)
print(rf_4)
plot(rf_4)
varImpPlot(rf_4)
#Random forest Model mit den 3 wichtigsten Metaboliten
# x6b_oh_f + f + a_thb
set.seed(222)
rf_6 <- randomForest(diagnose ~ an+x11_o_pt+ths+f+x20a_dhf+a_thb, 
                     data = NaN.z.databank_tp,
                     #mtry = 1,
                     ntree = 63, 
                     importance = TRUE)
print(rf_6)
plot(rf_6)
varImpPlot(rf_6)
# Random forest Model using all 32 Metaboliten
set.seed(123)                           
rf <- randomForest(diagnose ~ ., data = databank,
                   mtry = 3,
                   ntree = 120, 
                   importance = TRUE)
print(rf)
plot(rf)
varImpPlot(rf)

#++++++++++++++++++ PREDICTIONS ++++++++++++++++++++++++
#+
# predict with the - ACX dataset
predictions.rf <- predict(rf,databank_acx)
myCF <- table(databank_acx$diagnose,predictions.rf)
myCF
confusionMatrix(reference = databank_acx$diagnose, data = predictions.rf, mode = "everything")

#++++++++++++++ TUNING HYPERPARAMETERS +++++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#searching for the optimal mtry ie No of random choosen variables 
#used by every decision tree in the random forest

mtry <- tuneRF(NaN.z.databank_tp[-33],NaN.z.databank_tp$diagnose, ntreeTry=120,
               stepFactor=1.5,improve=0.1, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


#+++++++++++++++++++ Model evaluation - ROC CURVE ++++++++++++++++++#
#library importation
library(pROC)
par(pty = 's')
roc(NaN.z.databank_tp$diagnose,
    rf$votes[,1],
    main = "ROC CURVE",
    plot = T,
    percent = T,
    col = "#377eb8",
    print.auc = T,
    print.auc.x = 70,
    print.auc.y = 34,
    lwd = 2,
    legacy.axes = T)

# comparing the rf_7 model with rf
plot.roc(NaN.z.databank_tp$diagnose,
         rf_7$votes[,1],
         percent = T,
         col = "#4daf4a",
         lwd = 2,
         print.auc = T,
         add = T,
         print.auc.y = 26,
         print.auc.x = 70 )
plot.roc(NaN.z.databank_tp$diagnose,
         rf_4$votes[,1],
         percent = T,
         col = "brown1",
         lwd = 2,
         print.auc = T,
         add = T,
         print.auc.y = 18,
         print.auc.x = 70 )
plot.roc(NaN.z.databank_tp$diagnose,
         rf_6$votes[,1],
         percent = T,
         col = "blueviolet",
         lwd = 2,
         print.auc = T,
         add = T,
         print.auc.y = 10,
         print.auc.x = 70 )
legend("bottomright",
       legend = c("rf_33","rf_7","rf_4","rf_6"),
       col = c("#377eb8","#4daf4a","brown1","blueviolet"),
       lwd = 2)

##---------------------------------------------------------------------------##
## DIMENSION REDUCTION TECHNIQUE - PRINCIPAL COMPONENT ANALYSIS (PCA) 
##---------------------------------------------------------------------------##

# Libraries
install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR)
library(factoextra)
library(ggplot2)
# get the PCA of tumor DB
pca_comp.tp <- prcomp(zscore_DB_Tumor[,-c(7:9,21,37:39)], scale. = TRUE, center = T)
pca_comp.tp
summary(pca_comp.tp)

# using the - gesamte db
pca_comp <- prcomp(na.omit(databank[,-c(7:9,37:38)]), scale. = TRUE, center = T)
pca_comp
summary(pca_comp)

# scree plot
fviz_eig(pca_comp, addlabels = TRUE, ylim = c(0, 50))
# OR
plot(pca_comp, type = "l", main = "SCREE PLOT TUMOR")

# Biplopt
biplot(pca_comp.tp, scale = 0, main = "BIPLOT TUMOR") #xlim = c(-2,2))

# biplot using factoextra
library(factoextra)
fviz_pca_biplot(pca_comp.tp, repel=TRUE, pointsize=2, pointshape=21, col.var="red", arrowsize=0.6, labelsize=5,
                col.ind = zscore_DB_Tumor$diagnose, palette=c("green2", "skyblue2","cyan"), addEllipses=TRUE, ellipse.type="confidence",ellipse.level=0.95)
# Total contribution on PC1 and PC2
fviz_contrib(pca_comp.tp, choice = "var", axes = 1:3)

############## Save all plots in R ######################################################
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:\\Users\\togui1\\Downloads\\R_Plots")


#-------------------------------------------------------------------#
######################### BOXPLOTS ##################################
#-------------------------------------------------------------------#
install.packages("reshape2") # Install reshape2 package
library("reshape2")
library(ggplot2)

### Subsetting metabolites according to their steroid classes
variable.names(databank)

# Estrogens C18-Steroids
estrogens1<-databank_tp[,c(10:11,24)]
#estrogens1 <- as.numeric(unlist(estrogens1))
#median(estrogens1)
sc_estrogens1<-scale(estrogens1)
a1<-boxplot(estrogens1, main = "Tumor_patient ESTROGENS; C-18 STEROIDS", 
            col = "green3", 
            xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot

a1$stats
med_a1 <- a1$stats[3, ] #median 
liqr_a1<- a1$stats[2, ] #lower IQR 
uiqr_a1<- a1$stats[4, ] #upper IQR 
med_a1 <- round(med_a1, 2)  
liqr_a1 <- round(liqr_a1, 2) 
uiqr_a1 <- round(uiqr_a1, 2) 
paste0(med_a1, " [", liqr_a1,"-" ,uiqr_a1,"]")

#----------------------------------------------------------------------------#
# Androgens C19-Steroids

androgens1<-databank_tp[,c(4:9,12,14:15,17,21)]
androgens1<- as.numeric(unlist(androgens1))
variable.names(androgens1)
b1<-boxplot(log(androgens1+1), main = "Tumor_patient ANDROGENS; C-19 STEROIDS",
            horizontal = TRUE,
            col = "cyan",
            scale = TRUE,
            xlab = "Metaboliten", ylab = "Conc")
b1$stats
# extract median and IQR from boxplot

med_b1 <- b1$stats[3, ] #median 
liqr_b1<- b1$stats[2, ] #lower IQR 
uiqr_b1<- b1$stats[4, ] #upper IQR 
med_b1 <- round(med_b1, 2)  
liqr_b1 <- round(liqr_b1, 2) 
uiqr_b1 <- round(uiqr_b1, 2) 
paste0(med_b1, " [", liqr_b1,"-" ,uiqr_b1,"]")

#-----------------------------------------------------------------------------#
# Progestagens C21-Steroids

progestagens1<-databank_tp[,c(13,16,18:20,22,25:26)]
c1<-boxplot(log10(progestagens1+1), main = "Tumor_patient PROGESTAGENS; C-21 STEROIDS", 
            col = "magenta", 
            xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot

med_c1 <- c1$stats[3, ] #median 
liqr_c1<- c1$stats[2, ] #lower IQR 
uiqr_c1<- c1$stats[4, ] #upper IQR 
med_c1 <- round(med, 2)  
liqr_c1 <- round(liqr_c1, 2) 
uiqr_c1 <- round(uiqr_c1, 2) 
paste0(med_c1, " [", liqr_c1,"-" ,uiqr_c1,"]")

#-----------------------------------------------------------------------------#
# Glucocorticoids C21-Steroids

glucocorticoids1<-databank[,c(27,31:39)]
d1<-boxplot(log10(glucocorticoids1+1), main = "Tumor_patient GLUCOCORTICOIDS; C-21 STEROIDS", 
            col = "purple", 
            xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot

med_d1 <- d1$stats[3, ] #median 
liqr_d1<- d1$stats[2, ] #lower IQR 
uiqr_d1<- d1$stats[4, ] #upper IQR 
med_d1 <- round(med_d1, 2)  
liqr_d1 <- round(liqr_d1, 2) 
uiqr_d1 <- round(uiqr_d1, 2) 
paste0(med_d1, " [", liqr_d1,"-" ,uiqr_d1,"]")

#-----------------------------------------------------------------------------#
# Mineralocorticoids C21-Steroids

mineralocorticoids1<-databank[,c(23,28:30)]
e1<-boxplot(log10(mineralocorticoids1+1), main = "Tumor_patient MINERALOCORTICOIDS; C-21 STEROIDS", 
            col = "orange", 
            xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot

med_e1 <- e1$stats[3, ] #median 
liqr_e1<- e1$stats[2, ] #lower IQR 
uiqr_e1<- e1$stats[4, ] #upper IQR 
med_e1<- round(med_e1, 2)  
liqr_e1 <- round(liqr_e1, 2) 
uiqr_e1 <- round(uiqr_e1, 2) 
paste0(med_e1, " [", liqr_e1,"-" ,uiqr_e1,"]")



#-----------------------------------------------------------------------------#

# Estrogens C18-Steroids
#estrogens<-z.databank_ct[,c(7:8,21)]
#a<-boxplot(estrogens, main = "CONTROL_ESTROGENS; C-18 STEROIDS", 
#col = "green3", 
#xlab = "Metaboliten", ylab = "std score")

# extract median and IQR from boxplot

#a$stats
#med_a <- a$stats[3, ] #median 
#liqr_a<- a$stats[2, ] #lower IQR 
#uiqr_a<- a$stats[4, ] #upper IQR 
#med_a <- round(med_a, 2)  
#liqr_a <- round(liqr_a, 2) 
#uiqr_a <- round(uiqr_a, 2) 
#paste0(med_a, " [", liqr_a," - " ,uiqr_a,"]")

#----------------------------------------------------------------------------#
# Androgens C19-Steroids

androgens<-databank_ct[,c(4:9,12,14:15,17,21)]
b<-boxplot(androgens, main = "CONTROL_ANDROGENS; C-19 STEROIDS",
           horizontal = FALSE,
           col = "skyblue", 
           xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot
b$stats
med_b <- b$stats[3, ] #median 
liqr_b<- b$stats[2, ] #lower IQR 
uiqr_b<- b$stats[4, ] #upper IQR 
med_b <- round(med_b, 2)  
liqr_b <- round(liqr_b, 2) 
uiqr_b <- round(uiqr_b, 2) 
paste0(med_b, " [", liqr_b," - " ,uiqr_b,"]")

#-----------------------------------------------------------------------------#
# Progestagens C21-Steroids

progestagens<-databank_ct[,c(16:20,22,25:26)]
c<-boxplot(progestagens, main = "CONTROL_PROGESTAGENS; C-21 STEROIDS", 
           col = "magenta", 
           xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot

med_c <- c$stats[3, ] #median 
liqr_c<- c$stats[2, ] #lower IQR 
uiqr_c<- c$stats[4, ] #upper IQR 
med_c <- round(med_c, 2)  
liqr_c <- round(liqr_c, 2) 
uiqr_c <- round(uiqr_c, 2) 
paste0(med_c, " [", liqr_c,"-" ,uiqr_c,"]")

#-----------------------------------------------------------------------------#
# Glucocorticoids C21-Steroids

glucocorticoids<-databank_ct[,c(27,31:39)]
d<-boxplot(glucocorticoids, main = "CONTROL_GLUCOCORTICOIDS; C-21 STEROIDS", 
           col = "purple", 
           xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot

med_d <- d$stats[3, ] #median 
liqr_d<- d$stats[2, ] #lower IQR 
uiqr_d<- d$stats[4, ] #upper IQR 
med_d <- round(med_d, 2)  
liqr_d <- round(liqr_d, 2) 
uiqr_d <- round(uiqr_d, 2) 
paste0(med_d, " [", liqr_d,"-" ,uiqr_d,"]")

#-----------------------------------------------------------------------------#
# Mineralocorticoids C21-Steroids

mineralocorticoids<-databank_ct[,c(23,28:30)]
e<-boxplot(mineralocorticoids, main = "CONTROL_MINERALOCORTICOIDS; C-21 STEROIDS", 
           col = "orange", 
           xlab = "Metaboliten", ylab = "Conc")

# extract median and IQR from boxplot

med_e <- e$stats[3, ] #median 
liqr_e<- e$stats[2, ] #lower IQR 
uiqr_e<- e$stats[4, ] #upper IQR 
med_e<- round(med_e, 2)  
liqr_e <- round(liqr_e, 2) 
uiqr_e <- round(uiqr_e, 2) 
paste0(med_e, " [", liqr_e,"-" ,uiqr_e,"]")

#**************************************************************************************
#*OPTIONAL 
# Boxplot - Tumor Werte
bplt_tp <- boxplot(zscore_DB_Tumor[,-c(7:9,21,37:39)], 
                   horizontal = F, 
                   main = "TUMOR PATIENTEN", 
                   col = rainbow(ncol(zscore_DB_Tumor[,-c(7:9,21,37:39)])))
tp.stats <- bplt_tp$stats
colnames(tp.stats) <- c("an", "et","a5_3b_17a","dhea","a5_3b_17b","x11_o_an","po_5b_3a","x11_oh_an",  
                        "x11_oh_et","po_5a_3a","x16a_oh_dhea","pd" ,"pt", "p5d","a5t_16a" ,"ths" ,        
                        "th_doc", "x11_o_pt", "p5t_17a","the" , "tha","thb","a_thb","thf",         
                        "a_thf" , "a_cl" ,"b_c","b_cl", "a_c" , "f" ,"x6b_oh_f","x20a_dhf")

row.names(tp.stats) <- c("min","25th Perz","median","75th Perz","max")
tp.stats <- as.data.frame(tp.stats)
tp.stats$diagnose <- "TUMOR"

# Vergleichtabelle - gesamte stats
comparing.df <- cbind(t(tp.stats),t(ctrl.stats))
comparing.df <- comparing.df[c(33,1:32),]
comparing.df <- comparing.df[,c(1,6,2,7,3,8,4,9,5,10)]
comparing.df <- t(comparing.df)

# Vergleichtabelle - Median
med.stats <- rbind(ctrl.stats[3,],tp.stats[3,])
row.names(med.stats) <- c("median.ctrl","median.tp")
med.stats
fach.med.stats <- med.stats[,-33]
fach <- fach.med.stats[2,]/fach.med.stats[1,]

# difference in median btw control and tumor patients
diff.median <- apply(fach.med.stats,2, function(x) diff(x))
diff.median

# Retrieve the top 10 largest difference btw the two groups
head(sort(diff.median,decreasing=TRUE), n = 10)

## Sub-setting metabolites in their corresponding steroid classes
variable.names(NaN.z.databank_ct)



