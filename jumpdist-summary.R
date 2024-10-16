library(tidyverse)
library(tidymodels)

setwd("Fall 2024")

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

# Descriptive Graphs --------------------

d1 <- ggplot(syp, mapping = aes(x = Treatment,
                                color = Sex,
                                y = JumpDistanceChange)) +
  geom_jitter(position = position_dodge(width = .2)) +
  theme_classic() +
  theme(legend.position = 'bottom') +
  labs(title = "Change % based on Sex and Treatment",
       subtitle = "N = 41")

png(filename = "Visuals/Univariate/PreAnalysis.png", width = 9, height = 5, units = "in", res = 540)

print(d1)

dev.off()

# Graphs on Improvement ---------------------

# TREATMENT PAIRWISE

source("jumpdist-analysis.R")

pw_names <- rownames(tmt_pairwise)
pw_tmt_df <- as_tibble(data.frame(pw_names, tmt_pairwise))
pw_tmt_df[3,2:4] <- abs(pw_tmt_df[3,2:4])
pw_tmt_df[3,1] <- "External-Internal"

f3 <- ggplot(data = pw_tmt_df,
       mapping = aes(x = pw_names, y = diff)) +
  geom_errorbar(aes(ymin = lwr, 
                    ymax = upr), 
                width = .1, position = position_dodge(0.25), 
                show.legend = F) +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  geom_point(position = position_dodge(0.25), size = 3) +
  labs(title = "Treatment Comparisons and Jump Distance Improvement",
       subtitle = "95% CIs for Treatment Effects",
       y = "% Change", x = "Treatment Comparison") +
  theme_classic()

png(filename = "Visuals/Univariate/PWEffectsGraph.png", width = 9, height = 5, units = "in", res = 540)

print(f3)

dev.off()



# Group views, sample size, summary statistics ---------------------------------------------

ggplot() +
  geom_histogram(mapping = aes(x = syp$Change), bins = 15) +
  labs(title = "Distribution of % Change",
       x = "Change % in Jump Distance",
       subtitle = paste("AVG Jump % Change: ", round(mean(syp$Change), 2), "%", sep = ""))

syp %>% 
  group_by(Treatment, Sex) %>% 
  summarise(Count = n(),
            Mean_Jump = mean(Change),
            SE_Jump = sd(Change)/sqrt(Count)) %>% 
  arrange(Treatment)

