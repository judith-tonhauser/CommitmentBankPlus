# 13_explicitIgnorance
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(lme4)

# load helper functions
source('../../../helpers.R')

# load cleaned data
d = read_tsv("../data/cd.tsv")

names(d)
table(d$context)

length(unique(d$participantID)) #370 participants

# predict acceptability rating in comparison to least acceptable (continue) ----

# target data: explicit ignorance context
t = d %>%
  filter(context == "explicitIgnorance")

# merge the two controls into one
table(t$expression)
t = t %>%
  mutate(expression = recode(expression, "controlGood1" = "controls", "controlGood2" = "controls"))

# set reference level
# create item as combination of predicate and complement clause
# cd$item = as.factor(paste(cd$verb,cd$content))

# model
str(t$expression)
t$expression <- as.factor(t$expression)
t$expression <- relevel(t$expression, ref = "continue")
names(t)
m = lmer(response ~ expression + (1|participantID), data = t, REML=F)
summary(m)

# predict acceptability rating from predicate and context ----

# target data: predicates
t = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  mutate(expression = recode(expression, "controlGood1" = "controls", "controlGood2" = "controls")) %>%
  mutate(context2 = recode(context, "factL" = "neutral", "factH" = "neutral"))

# set reference levels
# create item as combination of predicate and complement clause
# cd$item = as.factor(paste(cd$verb,cd$content))

# reference level for expression is "see" as there is smallest difference 
str(t$expression)
t$expression <- as.factor(t$expression)
t$expression <- relevel(t$expression, ref = "see")

# reference level for context is "explicitIgnorance"
t$context2 <- as.factor(t$context2)
t$context2 <- relevel(t$context2, ref = "explicitIgnorance")

# model
m = lmer(response ~ expression * context2 + (1|participantID), data = t, REML=F)
summary(m)

#######

# main analysis of interest

# the model reported in the paper
m = lmer(response ~ cai * cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.1)

# get p-values via likelihood ratio tests
m.mr.0a = lmer(projective ~ cai + cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0a)

m.mr.0b = lmer(projective ~ cai + cai : cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0b)

m.mr.0c = lmer(projective ~ cblock_ai + cai : cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0c)

anova(m.mr.0a,m.mr.1) #p-value for interaction
anova(m.mr.0b,m.mr.1) #p-value for block
anova(m.mr.0c,m.mr.1) #p-value for at-issueness

# simple effects for interaction interpretation
m.mr.simple = lmer(projective ~ ai * block_ai - ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.simple)

# pairwise comparisons on short_trigger using tukey (reported in table 3)
# run the model again with trigger as fixed effect so you can do multiple comparisons (with no at-issueness or block effects)
m.mr.fixedtrigger = lmer(projective ~ short_trigger + (1|workerid), data=t_nomc, REML=F)

pc = lsmeans(m.mr.fixedtrigger, revpairwise~short_trigger, adjust="tukey")
pc

