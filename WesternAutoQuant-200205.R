#Testing Zone

#Mission - put up graph

library(tidyverse)
library(dplyr)
library(ggplot2)
library(wesanderson)

#Plotting western blot quantitation.

#Read the file containing the western informaiton. 
input<-read.csv("Tables/fig1Aquantifiied.csv", header = TRUE, sep=",")

#Filter the file and create a data frame that extracts the intensity values for the background signal.
bg<-input %>% 
  group_by(protein, condition, rep_num) %>%
  filter(condition=='bg')

#Merge the two data frames to create a column of bg intensity values next to the vaules of the conditions from the same protein and replicate number
df_bg <-full_join(input, bg, by = c("protein", "rep_num"), all = TRUE)

#Subtract bg vaules from signal for each intensity
df_norm <- data.frame(df_bg, transmute(df_bg, norm = df_bg$intensity.x - df_bg$intensity.y))

#Create a data frame that specifically filters the WT conditions grouped by protein, condition and replicate number
test1 <- df_norm %>% 
  group_by(protein, condition.x, rep_num) %>%
  filter(condition.x=='WT')

#Get mean vaules from the normalized WT measurements for each protein
test2 <- test1 %>%
  group_by(protein, condition.x) %>%
  summarise(mean(norm))

#Add column of mean vaules given protein conditions
df_withmean <- full_join(df_norm, test2, by = c("protein"))

#Normalize all values to WT condition for each protein
df_rel <- data.frame(df_withmean, rel=df_withmean$norm/df_withmean$`mean(norm)`*100)

#Filter the file by selecting for the relevant columns and removing condition = bg
forplot <- df_rel %>%
  transmute(protein, condition.x.x, rep_num, rel) %>%
  filter(condition.x.x != "bg")

#Set colors


#Plot 
g1 <- ggplot(forplot, aes(x=protein, y=rel, fill=condition.x.x))+
  geom_dotplot(binaxis = 'y', stackdir='center', position=position_dodge(0.3))+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black", position = position_dodge(0.3))
  
#Print Plot
g1+scale_fill_brewer(palette = 11)


