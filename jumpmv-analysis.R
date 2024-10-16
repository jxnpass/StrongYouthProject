library(tidyverse)
library(tidymodels)

# setwd("Fall 2024")

syp <- read_csv("Data/finaldata.csv", skip = 1)
colnames(syp) <- str_replace_all(colnames(syp), pattern = "[ %]", replacement = "")
colnames(syp) <- gsub("\\s*\\(.*?\\)", "", colnames(syp))
change_cols <- colnames(syp)[str_detect(colnames(syp), "Change")]
pre_cols <- colnames(syp)[str_detect(colnames(syp), "Pre")]
post_cols <- colnames(syp)[str_detect(colnames(syp), "Post")]

# fix percentages
syp <- syp %>% 
  mutate(across(all_of(change_cols), ~ as.numeric(gsub("%", "", .x)))) 

# if I want to go from percent change to difference ---------

syp_diff <- syp %>%
  mutate(
    JumpDistanceChange = AVGPostJumpDistance - AVGPreJumpDistance,
    MaxHipAngleChange = MaxHipAnglePost - MaxHipAnglePre,
    MaxAnkleAngleChange = MaxAnkleAnglePost - MaxAnkleAnglePre,
    MaxKneeAngleChange = MaxKneeAnglePost - MaxKneeAnglePre,
    StanceTimeChange = StanceTimePost - StanceTimePre
  )


# Multivariate analysis ---------------------------------------------------

mva <- manova(cbind(JumpDistanceChange, 
             MaxHipAngleChange, 
             MaxAnkleAngleChange, 
             MaxKneeAngleChange, 
             StanceTimeChange) ~ 
         Treatment*Sex + 
           AVGPreJumpDistance + 
         Sex:AVGPreJumpDistance, 
       data = syp)

summary(mva)
summary(mva, test = "Wilks")
summary.aov(mva)

library(biotools)
mva_pw <- mvpaircomp(mva, factor1 = "Treatment",
                     test = "Wilks", adjust = "bonferroni")
mva_pw
mva_pw_sex <- mvpaircomp(mva, factor1 = "Treatment", nesting.factor = "Sex",
                     test = "Wilks", adjust = "bonferroni")
mva_pw_sex



# Multiple PW Values ------------------------------------------------------

# Initialize a list to store pairwise results
pairwise_results <- list()
syp_pw <- syp %>% # or change to syp_diff
  mutate(Treatment = factor(Treatment, levels = c("Control", "Internal", "External")))

for (resp_col in change_cols) {
  formula <- as.formula(paste(resp_col, "~ Treatment * Sex"))
  
  y <- syp_pw %>% pull(resp_col)
  tmt <- syp_pw %>% pull(Treatment)
  
  # Bonferroni p-values
  pvals <- pairwise.t.test(y, tmt, p.adj = 'bonferroni')$p.value %>% 
    as.vector() 
  pvals <- pvals[!is.na(pvals)] * 5 # Remove any NA values (if any)
  pvals <- ifelse(pvals > 1, 1, pvals)
  
  # pairwise comparison values
  tmt_pairwise <- TukeyHSD(aov(formula, data = syp_pw))$Treatment
  tmt_pairwise[, 4] <- pvals # Update p-values with Bonferroni correction
  
  pairwise_results[[resp_col]] <- tmt_pairwise %>% round(digits = 4)
}

# Display
for (resp_col in names(pairwise_results)) {
  pairwise_results[[resp_col]][3,]
  cat("\n### Pairwise Comparisons for:", resp_col, "###\n")
  print(pairwise_results[[resp_col]])
}

pairwise_results
pw_names <- rownames(pairwise_results$JumpDistanceChange)








# # can also run raw differences
# mva <- manova(cbind(JumpDistanceChange, 
#              MaxHipAngleChange, 
#              MaxAnkleAngleChange, 
#              MaxKneeAngleChange, 
#              StanceTimeChange) ~ 
#          Treatment*Sex + 
#            AVGPreJumpDistance + 
#          Sex:AVGPreJumpDistance, 
#        data = syp_diff)
# 
# summary(mva)
# summary(mva, test = "Wilks")
# summary.aov(mva)

