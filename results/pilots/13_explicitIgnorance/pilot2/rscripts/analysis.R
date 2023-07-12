# 13_explicitIgnorance
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)

# load helper functions
source('../../../helpers.R')

d = read_tsv("../data/cd.tsv")

names(d)
table(d$context)

# target data: explicit ignorance context
t = d %>%
  filter(context == "explicitIgnorance")



# center the block and at-issueness variables
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("block_ai","ai")]))

# main analysis of interest: predict projectivity from at-issueness, while controlling for block (random effects by subject, lexical content, and target expression)

# the model reported in the paper
m.mr.1 = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
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

