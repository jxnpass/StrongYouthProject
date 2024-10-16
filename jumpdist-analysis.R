library(tidyverse)
library(tidymodels)

# Any improvement among treatment groups? ------------

t_test_results <- lapply(split(syp$JumpDistanceChange, 
                               interaction(syp$Treatment, syp$Sex)), 
                         function(x) {
  t.test(x, mu = 0, alternative = "greater")$p.value  # Test if mean > 0
})
t_test_results <- p.adjust(t_test_results, method = "bonferroni")
t_test_results

# ANOVA Univariate Analsyis ----------------------------------------------------------------



# also tried raw difference (Diff) but didn't see much difference
# using change % 
lm_cjump <- lm(JumpDistanceChange ~ Treatment*Sex + AVGPreJumpDistance +
                  Sex:AVGPreJumpDistance,
               data = syp)
summary(lm_cjump)

anv_cjump <- anova(lm_cjump)
anv_cjump

pairwise.t.test(syp$JumpDistanceChange, 
                syp$Treatment, p.adj='bonferroni')
TukeyHSD(aov(lm(JumpDistanceChange ~ Treatment*Sex, data = syp))) 
tmt_pairwise <- TukeyHSD(aov(lm(JumpDistanceChange ~ Treatment*Sex, data = syp)))$Treatment
tmtsex_pairwise <- TukeyHSD(aov(lm(JumpDistanceChange ~ Treatment*Sex, data = syp)))$`Treatment:Sex`

# get residuals from lm_cjump
# check standard deviation from residuals from each treatment groups 

res <- resid(lm_cjump)
tmt <- syp$Treatment

cbind(res, tmt) %>% 
  as.data.frame() %>% 
  mutate(res = as.numeric(res)) %>% 
  group_by(tmt) %>% 
  summarise(SD = sd(res))

# 3:1 ratio for SD seem good




# read up on multivariate analysis methods
## include each (post) angle, stance time, and % change jump dist
## angle + stance post + % change ~ 
# Treatment*Sex + AVGInitialJumpDistance + Sex:AVGInitialJumpDistance 
## wilk's lambda



