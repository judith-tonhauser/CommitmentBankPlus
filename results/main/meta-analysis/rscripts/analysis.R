# meta-analyses
# projection: comparing embedding under polar questions, negation, "perhaps", conditionals
# at-issueness diagnostics

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
require(tidyverse)
library(ggrepel)
library(lmerTest)

# read in the data
polar = read_csv(file="../../1_projaiQ/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="polar")
length(unique(polar$workerid)) #242

neg = read_csv(file="../../2_projaiN/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="neg")
length(unique(neg$workerid)) #274 THERE ARE STILL PROBLEMS IN THE READ-IN FILE

modal = read_csv(file="../../3_projaiM/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="modal")
length(unique(modal$workerid)) #281 THERE ARE STILL PROBLEMS IN THE READ-IN FILE

nrow(polar)
nrow(neg)
nrow(modal)

# aggregate

polar_a = polar %>%
  group_by(short_trigger) %>%
  summarise(AImean_P = mean(ai), ci.low_AI_P=ci.low(ai), ci.high_AI_P=ci.high(ai),
            Projmean_P = mean(projective), ci.low_Proj_P=ci.low(projective), ci.high_Proj_P=ci.high(projective))
  

neg_a = neg %>%
  group_by(short_trigger) %>%
  summarise(AImean_N = mean(ai), ci.low_AI_N=ci.low(ai), ci.high_AI_N=ci.high(ai),
            Projmean_N = mean(projective), ci.low_Proj_N=ci.low(projective), ci.high_Proj_N=ci.high(projective))

modal_a = modal %>%
  group_by(short_trigger) %>%
  summarise(AImean_M = mean(ai), ci.low_AI_M=ci.low(ai), ci.high_AI_M=ci.high(ai),
            Projmean_M = mean(projective), ci.low_Proj_M=ci.low(projective), ci.high_Proj_M=ci.high(projective))

d = polar_a %>%
  inner_join(neg_a,by=("short_trigger")) %>%
  inner_join(modal_a,by=("short_trigger"))
View(d)
names(d)

# correlations
# projection
cor(d$Projmean_P,d$Projmean_N) # .95
cor(d$Projmean_P,d$Projmean_M) # .92
cor(d$Projmean_N,d$Projmean_M) # .81

# not-at-issueness
cor(d$AImean_P,d$AImean_N) # .72
cor(d$AImean_P,d$AImean_M) # .77
cor(d$AImean_N,d$AImean_M) # .82


# plot projection against projection ----
ggplot(d, aes(x=Projmean_P,y=Projmean_N)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  #geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  xlab("Mean certainty rating polar") +
  ylab("Mean certainty rating negation") +
  xlim(0,1) +
  ylim(0,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("../graphs/projection-polar-against-negation.pdf",width=6,height=6)

ggplot(d, aes(x=Projmean_P,y=Projmean_M)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  #geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  xlab("Mean certainty rating polar") +
  ylab("Mean certainty rating modal") +
  xlim(0,1) +
  ylim(0,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("../graphs/projection-polar-against-modal.pdf",width=6,height=6)

ggplot(d, aes(x=Projmean_N,y=Projmean_M)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  #geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  xlab("Mean certainty rating negation") +
  ylab("Mean certainty rating modal") +
  xlim(0,1) +
  ylim(0,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("../graphs/projection-negation-against-modal.pdf",width=6,height=6)

# plot ai against ai ----
ggplot(d, aes(x=AImean_P,y=AImean_N)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  #geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  xlab("Mean not-at-issueness rating polar (asking whether)") +
  ylab("Mean not-at-issueness rating negation (sure that)") +
  xlim(0,1) +
  ylim(0,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("../graphs/nai-polar-against-negation.pdf",width=6,height=6)

ggplot(d, aes(x=AImean_P,y=AImean_M)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  #geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  xlab("Mean not-at-issueness rating polar (asking whether)") +
  ylab("Mean not-at-issueness rating modal (sure that)") +
  xlim(0,1) +
  ylim(0,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("../graphs/nai-polar-against-modal.pdf",width=6,height=6)

ggplot(d, aes(x=AImean_N,y=AImean_M)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  #geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  xlab("Mean not-at-issueness rating negation (sure that)") +
  ylab("Mean not-at-issueness rating modal (sure that)") +
  xlim(0,1) +
  ylim(0,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("../graphs/nai-negation-against-modal.pdf",width=6,height=6)

######### HAVENT INTEGRATED ERROR BARS YET
d = agr1 %>%
  inner_join(agr2,by=c("short_trigger","SubExperiment"))
nrow(d)
d = as.data.frame(d) %>%
  filter(short_trigger != "MC") %>%
  mutate(YMin=mean_exp2 - ci.low_exp2, YMax=mean_exp2 + ci.high_exp2, XMin=mean_exp1 - ci.low_exp1, XMax=mean_exp1+ci.high_exp1)

# correlations overall and by sub-experiment
cor(d$mean_exp1,d$mean_exp2) # .62
d %>%
  group_by(SubExperiment) %>%
  summarise(r=cor(mean_exp1,mean_exp2))

cbPalette <- c("#0072B2", "#D55E00")

# figure 11
ggplot(d, aes(x=mean_exp1,y=mean_exp2,color=SubExperiment)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  scale_color_manual(values=cbPalette,name="Sub-experiment") +
  xlab("Mean not-at-issueness ('asking whether', Exp. 1)") +
  ylab("Mean not-at-issueness ('are you sure', Exp. 2)") +
  xlim(.3,1) +
  ylim(.3,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("graphs/correlation-bytrigger.pdf",width=6,height=6)


# aggregate by both trigger and content
agr1 = exp1 %>%
  group_by(short_trigger,content,SubExperiment) %>%
  summarise(mean_exp1 = mean(ai), ci.low_exp1=ci.low(ai), ci.high_exp1=ci.high(ai))
agr2 = exp2 %>%
  group_by(short_trigger,content,SubExperiment) %>%
  summarise(mean_exp2 = mean(response), ci.low_exp2=ci.low(response), ci.high_exp2=ci.high(response))

d = agr1 %>%
  inner_join(agr2,by=c("short_trigger","content","SubExperiment"))
nrow(d)
d = as.data.frame(d) %>%
  filter(short_trigger != "MC") %>%
  mutate(YMin=mean_exp2 - ci.low_exp2, YMax=mean_exp2 + ci.high_exp2, XMin=mean_exp1 - ci.low_exp1, XMax=mean_exp1+ci.high_exp1)

# correlations overall and by sub-experiment
cor(d$mean_exp1,d$mean_exp2) # .31
d %>%
  group_by(SubExperiment) %>%
  summarise(r=cor(mean_exp1,mean_exp2))

d$cmean_exp1 = myCenter(d$mean_exp1)
d$SubExp = ifelse(d$SubExperiment == "a",0,1)
d$cSubExperiment = myCenter(d$SubExp)

# how much does exp 1 measure explain exp 2 measure? (footnote 13)
m = lmer(mean_exp2 ~ 1+cmean_exp1 * cSubExperiment + (1|content), data = d)
summary(m)

m.simple = lmer(mean_exp2 ~ 1+cmean_exp1 * SubExperiment - cmean_exp1 + (1|content), data = d)
summary(m.simple)


