library(tidyverse)
library(tidymodels)
library(scales)
library(ggcorrplot)
library(GGally)

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

# if I want to go from percent change to difference ---------

syp_diff <- syp %>%
  mutate(
    JumpDistanceChange = AVGPostJumpDistance - AVGPreJumpDistance,
    MaxHipAngleChange = MaxHipAnglePost - MaxHipAnglePre,
    MaxAnkleAngleChange = MaxAnkleAnglePost - MaxAnkleAnglePre,
    MaxKneeAngleChange = MaxKneeAnglePost - MaxKneeAnglePre,
    StanceTimeChange = StanceTimePost - StanceTimePre
  )


# Descriptive Visuals ---------------------------------------------------

syp_viz <- syp %>% 
  select(SubjectID, Age, Treatment, Sex, all_of(change_cols)) %>% 
  pivot_longer(
    cols = all_of(change_cols),  
    names_to = "Var", 
    values_to = "Change" 
  ) 

syp_viz_means <- syp_viz %>% 
  group_by(Treatment, Var) %>% 
  summarise(Mean = mean(Change), Median = median(Change))

syp_viz_means_sex <- syp_viz %>% 
  group_by(Treatment, Var, Sex) %>% 
  summarise(Mean = mean(Change))

d1 <- ggplot(data = syp_viz, mapping = aes(x = Treatment,
                                     y = Change/100)) +
  geom_jitter(mapping = aes(color = Sex),
              position = position_dodge(width = .4)) +
  geom_label(data = syp_viz_means, mapping = aes(y = Mean/100,
                                                 label = percent(Mean/100, accuracy = .1)),
             size = 3, show.legend = F) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  facet_wrap(~ Var, nrow = 1) +
  theme(legend.position = 'bottom') +
  labs(title = "Change % based on Sex and Treatment",
       y = "Change",
       subtitle = "N = 39")

# d2 <- ggplot(data = syp_viz, mapping = aes(x = Treatment,
#                                            color = Sex,
#                                            y = Change/100)) +
#   geom_jitter(position = position_dodge(width = .4)) +
#   geom_label(data = syp_viz_means_sex, mapping = aes(y = Mean/100,
#                                                  label = percent(Mean/100, accuracy = 1)),
#              size = 2.5, show.legend = F,
#              position = position_dodge(width = 1)) +
#   scale_y_continuous(labels = percent) +
#   theme_classic() +
#   facet_wrap(~ Var, nrow = 1) +
#   theme(legend.position = 'bottom') +
#   labs(title = "Change % based on Sex and Treatment",
#        y = "Change",
#        subtitle = "N = 39")


png(filename = "Visuals/Multivariate/PreAnalysis.png", width = 9, height = 5, units = "in", res = 540)

print(d1)

dev.off()

# png(filename = "Visuals/Multivariate/PreAnalysis2.png", width = 9, height = 5, units = "in", res = 540)
# 
# print(d2)
# 
# dev.off()

# Age (instead of Sex)

# syp_viz_means_age <- syp_viz %>% 
#   group_by(Treatment, Var, Age) %>% 
#   summarise(Mean = mean(Change))
# 
# d3 <- ggplot(data = syp_viz, mapping = aes(x = Treatment,
#                                            color = Age,
#                                            y = Change/100)) +
#   geom_jitter(position = position_dodge(width = .4)) +
#   geom_label(data = syp_viz_means_age, mapping = aes(y = Mean/100,
#                                                      label = percent(Mean/100, accuracy = 1)),
#              size = 2.5, show.legend = F,
#              position = position_dodge(width = 1)) +
#   scale_y_continuous(labels = percent) +
#   theme_classic() +
#   scale_color_manual(values = c("brown", "yellow4")) +
#   facet_wrap(~ Var, nrow = 1) +
#   theme(legend.position = 'bottom') +
#   labs(title = "Change % based on Age and Treatment",
#        y = "Change",
#        subtitle = "N = 41")
# 
# 
# png(filename = "Visuals/Multivariate/PreAnalysis3.png", width = 9, height = 5, units = "in", res = 540)
# 
# print(d3)
# 
# dev.off()


# Pairwise Differences by Response ----------------------------------------
source("jumpmv-analysis.R")
pw_df <- pairwise_results %>%
  tibble() %>% 
  unnest() %>% 
  pull(.) %>% 
  as_tibble() %>% 
  mutate(y = rep(change_cols, each = 3)) %>%
  mutate(tmt = rep(pw_names, times = 5)) %>% 
  set_names(c("fit", "lwr", "upr", "p", "y", "tmt")) %>% 
  mutate(tmt = case_when(
    tmt == "Internal-Control" ~ "Int-Con",
    tmt == "External-Control" ~ "Ext-Con",
    tmt == "External-Internal" ~ "Ext-Int",
  ))

f_pw <- ggplot(data = pw_df,
       mapping = aes(x = tmt, y = fit/100)) +
  geom_errorbar(aes(ymin = lwr/100, 
                    ymax = upr/100), 
                width = .1, position = position_dodge(0.25), 
                show.legend = F) +
  facet_wrap(~ y, scales = "free", ncol = 3) +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  scale_y_continuous(label = percent) +
  geom_point(position = position_dodge(0.25), size = 3) +
  labs(title = "Treatment Comparisons and Multiple Responses",
       subtitle = "95% CIs for Treatment Comparison Effects",
       y = "% Change", x = "Treatment Comparison") +
  theme_classic()

png(filename = "Visuals/Multivariate/PWEffectsGraph.png", width = 9, height = 5, units = "in", res = 540)

print(f_pw)

dev.off()



# Corrplot ----------------------------------------------------------------

corrs <- syp %>% 
  select(all_of(change_cols)) %>% 
  cor()

c1 <- ggcorrplot(corrs, lab = T, colors = c("pink4", "white", "lightblue")) +
  theme_classic() +
  labs(x = "", y = "",
       title = "Correlation Matrix - % Changes") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, vjust = 1))

png(filename = "Visuals/Multivariate/CorrMatPercent.png", width = 9, height = 5, units = "in", res = 540)

print(c1)

dev.off() 

corrs <- syp_diff %>% 
  select(all_of(change_cols)) %>% 
  cor()

c2 <- ggcorrplot(corrs, lab = T, colors = c("pink4", "white", "lightblue")) +
  theme_classic() +
  labs(x = "", y = "",
       title = "Correlation Matrix - Raw Differences") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, vjust = 1))

png(filename = "Visuals/Multivariate/CorrMatDiff.png", width = 9, height = 5, units = "in", res = 540)

print(c2)

dev.off() 

