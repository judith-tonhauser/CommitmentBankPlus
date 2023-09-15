# 13_explicitIgnorance
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(emmeans)

# load cleaned data
d = read_tsv("../data/cd.tsv")

names(d)
table(d$context)

length(unique(d$participantID)) #370 participants

# predict naturalness rating from expression in explicit ignorance context ----

# target data: explicit ignorance context, no controls
t = d %>%
  filter(context == "explicitIgnorance") %>%
  filter(expression != "controlGood1" & expression != "controlGood2")

# sort the expressions by mean naturalness rating (for ease of reading multiple comparisons)
means = t %>%
  group_by(expression) %>%
  summarize(mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),mean)) 
means

t = t %>%
  mutate(expression = fct_relevel(expression,levels(means$expression)))
levels(t$expression)

# set reference level (continue as the least natural)
t = t %>% mutate(expression = fct_relevel(expression, "continue"))

# model
m = lmer(response ~ expression + (1|participantID) + (1|cc), data = t, REML=F)
summary(m)

# pairwise comparisons on expression using tukey
pc = lsmeans(m, revpairwise~expression, adjust="tukey",lmerTest.limit = 6660,pbkrtest.limit = 6660)
options(max.print=2000)
pc

