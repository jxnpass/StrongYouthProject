setwd("Fall 2024")
source("functions.R")

library(tidyverse)
library(tidymodels)
library(GGally)
library(ggrepel)
select <- dplyr::select

syp <- read_csv("Data/finaldata.csv", skip = 1)
colnames(syp) <- str_replace_all(colnames(syp), pattern = "[ %]", replacement = "")
colnames(syp) <- gsub("\\s*\\(.*?\\)", "", colnames(syp))
change_cols <- colnames(syp)[str_detect(colnames(syp), "Change")]
pre_cols <- colnames(syp)[str_detect(colnames(syp), "Pre")]
post_cols <- colnames(syp)[str_detect(colnames(syp), "Post")]

# fix percentages
syp <- syp %>% 
  mutate(across(all_of(change_cols), ~ as.numeric(gsub("%", "", .x)))) 


# CUSTOM MANOVA  ----------------------------------------------------------

x <- syp %>% 
  select(all_of(change_cols)) %>% 
  as.matrix()

grp <- syp %>% 
  select(Treatment) %>% 
  as.matrix()

mva <- mymanova(x,grp, extract = 'mva') # verified with jumpmv-analysis.R (Wilk's Lambda)

mats <- mymanova(x, grp, extract = "EH")
print(mats)
E <- mats$E
H <- mats$H
# STEPS


# DISCRIMINANT ANALYSIS ---------------------------------------------------

# solve(sqrtmat(E)) = E^-1/2
E_invhalf <- solve(sqrtmat(E))
# C = 5x5 eigenvector matrix eigen(E^-1/2%*%H%*%E^-1/2)
C = eigen(E_invhalf%*%H%*%E_invhalf)
# E^-1/2 %*% C = D
D = E_invhalf %*% C$vectors
# D is 5x5, look at 1st column d1
# then....
# visualize
# X %*% d1 = 1st discriminant score = z1
z1 = x %*% D[,1]
# X %*% d2 = 2nd discriminant score = z2
z2 = x %*% D[,2]
# how important is each variable in each row and how they separate groups?
# create scatterplot of z1 and z2 
ggplot(mapping = aes(x = z1, y = z2, color = grp)) +
  geom_point() +
  theme_classic() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs(title = "Discriminant Analysis on SYP Features",
       subtitle = "N = 39")

# Size of elements in Z1 (tells us how dif ext is from internal and contr)
# C$vector


Z = x %*% D
colnames(Z) <- c("z1", "z2", "z3", "z4", "z5")

Z = data.frame(SubjectID = syp$SubjectID, grp, Z)
custom_point <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_point(size = 4, ...) + 
    geom_label(aes(label = SubjectID), size = 2) 
}

disc_plot <- ggpairs(Z, columns = grep("z", colnames(Z))[1:2], 
        aes(color = Treatment, alpha = 0.01),
        lower = list(continuous = wrap(custom_point)),
        upper = list(continuous = wrap(custom_point))) +
  theme_classic() +
  scale_color_manual(values = c("red", "green", "blue")) +
  labs(title = "Discriminant Decomposition for Multivariate Outcomes",
       subtitle = "Treatment Group Separation")

png(filename = "Visuals/Multivariate/DiscAnalysis.png", width = 9, height = 5, units = "in", res = 540)

print(disc_plot)

dev.off()


# interpret
# diag(E) %*% D = D* (standardized version)
D_star = diag(sqrt(diag(E))) %*% D
print(D_star)
# look at first column of D*
D_star[,1]
D_star[,2]
# relative importance of each story of D1 and D2 or z1 and z2
(C$values/sum(C$values)) %>% round(digits = 2)


