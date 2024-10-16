# setwd("SYP")

library(tidyverse)
library(tidymodels)

syp <- read_csv("Data/finaldata.csv", skip = 1)
colnames(syp) <- str_replace_all(colnames(syp), pattern = "[ %]", replacement = "")
colnames(syp) <- gsub("\\s*\\(.*?\\)", "", colnames(syp))
change_cols <- colnames(syp)[str_detect(colnames(syp), "Change")]
pre_cols <- colnames(syp)[str_detect(colnames(syp), "Pre")]
post_cols <- colnames(syp)[str_detect(colnames(syp), "Post")]

# fix percentages
syp <- syp %>% 
  mutate(across(all_of(change_cols), ~ as.numeric(gsub("%", "", .x)))) 

# age fix
syp <- syp %>% 
  mutate(Age = ifelse(Age <= 14, "10-14", "15-17"))

# Demographics

ch <- 3.28084
cw <- 2.20462

syp %>% 
  group_by(Sex) %>% 
  summarise(Count = n(),
            Height_Min = ch*min(Height),
            Height_Max = ch*max(Height),
            Weight_Min = cw*min(Weight),
            Weight_Max = cw*max(Weight),
            )

syp %>% 
  group_by(Age) %>% 
  summarise(Count = n())

syp %>% 
  group_by(Sex) %>% 
  summarise(Count = n(),
            AVG_Init_Jump = mean(AVGPostJumpDistance),
            AVG_Change = mean(JumpDistanceChange))

syp %>% 
  group_by(Sex, Age) %>% 
  summarise(AVG_Init_Jump = mean(AVGPostJumpDistance),
            AVG_Change = mean(JumpDistanceChange))

# Group views, sample size, summary statistics ---------------------------------------------

syp %>% 
  group_by(Treatment, Sex) %>% 
  summarise(Count = n(),
            Mean_Jump = mean(JumpDistanceChange),
            SE_Jump = sd(JumpDistanceChange)/sqrt(Count)) %>% 
  arrange(Treatment)

