# meta-analyses
# projection: comparing embedding under polar questions, negation, "perhaps", conditionals
# different at-issueness diagnostics

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
require(tidyverse)
library(ggrepel)
library(lmerTest)

# read in the data

# at-issueness diagnostics: Q: asking whether, N/M/C: sure that
polar_1 = read.csv(file="../../1_projaiQ/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="polar_1")

neg_2 = read.csv(file="../../2_projaiN/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="neg_2")

modal_3 = read.csv(file="../../3_projaiM/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="modal_3")

cond_4 = read.csv(file="../../4_projaiC/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="conditional_4")

# at-issueness diagnostic: assent with positive continuation

polar_5 = read.csv(file="../../5_projaiQ/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="polar_5")

neg_6 = read.csv(file="../../6_projaiN/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="neg_6")

modal_7 = read.csv(file="../../7_projaiM/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="modal_7")

cond_8 = read.csv(file="../../8_projaiC/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="conditional_8")

# at-issueness diagnostic: assent with adversative continuation

polar_9 = read.csv(file="../../9_projaiQ/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="polar_9")

neg_10 = read.csv(file="../../10_projaiN/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="neg_10")

modal_11 = read.csv(file="../../11_projaiM/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="modal_11")

cond_12 = read.csv(file="../../12_projaiC/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="conditional_12")

# aggregate

polar_1a = polar_1 %>%
  group_by(short_trigger) %>%
  summarise(AImean_P1 = mean(ai), ci.low_AI_P=ci.low(ai), ci.high_AI_P=ci.high(ai),
            Projmean_P1 = mean(projective), ci.low_Proj_P=ci.low(projective), ci.high_Proj_P=ci.high(projective))


neg_2a = neg_2 %>%
  group_by(short_trigger) %>%
  summarise(AImean_N2 = mean(ai), ci.low_AI_N=ci.low(ai), ci.high_AI_N=ci.high(ai),
            Projmean_N2 = mean(projective), ci.low_Proj_N=ci.low(projective), ci.high_Proj_N=ci.high(projective))

modal_3a = modal_3 %>%
  group_by(short_trigger) %>%
  summarise(AImean_M3 = mean(ai), ci.low_AI_M=ci.low(ai), ci.high_AI_M=ci.high(ai),
            Projmean_M3 = mean(projective), ci.low_Proj_M=ci.low(projective), ci.high_Proj_M=ci.high(projective))

cond_4a = cond_4 %>%
  group_by(short_trigger) %>%
  summarise(AImean_C4 = mean(ai), ci.low_AI_M=ci.low(ai), ci.high_AI_M=ci.high(ai),
            Projmean_C4 = mean(projective), ci.low_Proj_M=ci.low(projective), ci.high_Proj_M=ci.high(projective))

polar_5a = polar_5 %>%
  group_by(short_trigger) %>%
  summarise(AImean_P5 = mean(ai), ci.low_AI_P=ci.low(ai), ci.high_AI_P=ci.high(ai),
            Projmean_P5 = mean(projective), ci.low_Proj_P=ci.low(projective), ci.high_Proj_P=ci.high(projective))


neg_6a = neg_6 %>%
  group_by(short_trigger) %>%
  summarise(AImean_N6 = mean(ai), ci.low_AI_N=ci.low(ai), ci.high_AI_N=ci.high(ai),
            Projmean_N6 = mean(projective), ci.low_Proj_N=ci.low(projective), ci.high_Proj_N=ci.high(projective))

modal_7a = modal_7 %>%
  group_by(short_trigger) %>%
  summarise(AImean_M7 = mean(ai), ci.low_AI_M=ci.low(ai), ci.high_AI_M=ci.high(ai),
            Projmean_M7 = mean(projective), ci.low_Proj_M=ci.low(projective), ci.high_Proj_M=ci.high(projective))

cond_8a = cond_8 %>%
  group_by(short_trigger) %>%
  summarise(AImean_C8 = mean(ai), ci.low_AI_M=ci.low(ai), ci.high_AI_M=ci.high(ai),
            Projmean_C8 = mean(projective), ci.low_Proj_M=ci.low(projective), ci.high_Proj_M=ci.high(projective))

polar_9a = polar_9 %>%
  group_by(short_trigger) %>%
  summarise(AImean_P9 = mean(ai), ci.low_AI_P=ci.low(ai), ci.high_AI_P=ci.high(ai),
            Projmean_P9 = mean(projective), ci.low_Proj_P=ci.low(projective), ci.high_Proj_P=ci.high(projective))

neg_10a = neg_10 %>%
  group_by(short_trigger) %>%
  summarise(AImean_N10 = mean(ai), ci.low_AI_N=ci.low(ai), ci.high_AI_N=ci.high(ai),
            Projmean_N10 = mean(projective), ci.low_Proj_N=ci.low(projective), ci.high_Proj_N=ci.high(projective))

modal_11a = modal_11 %>%
  group_by(short_trigger) %>%
  summarise(AImean_M11 = mean(ai), ci.low_AI_M=ci.low(ai), ci.high_AI_M=ci.high(ai),
            Projmean_M11= mean(projective), ci.low_Proj_M=ci.low(projective), ci.high_Proj_M=ci.high(projective))

cond_12a = cond_12 %>%
  group_by(short_trigger) %>%
  summarise(AImean_C12 = mean(ai), ci.low_AI_M=ci.low(ai), ci.high_AI_M=ci.high(ai),
            Projmean_C12 = mean(projective), ci.low_Proj_M=ci.low(projective), ci.high_Proj_M=ci.high(projective))

d = polar_1a %>%
  inner_join(neg_2a,by=("short_trigger")) %>%
  inner_join(modal_3a,by=("short_trigger")) %>%
  inner_join(cond_4a,by=("short_trigger")) %>%
  inner_join(polar_5a,by=("short_trigger")) %>%
  inner_join(neg_6a,by=("short_trigger")) %>%
  inner_join(modal_7a,by=("short_trigger")) %>%
  inner_join(cond_8a,by=("short_trigger")) %>%
  inner_join(polar_9a,by=("short_trigger")) %>%
  inner_join(neg_10a,by=("short_trigger")) 
#%>%
#inner_join(modal_7a,by=("short_trigger")) %>%
#inner_join(cond_8a,by=("short_trigger"))


View(d)
names(d)
View(polar_9a)



# how much variability?
max(polar_1a$Projmean_P1) - min(polar_1a$Projmean_P1) #.83
max(neg_2a$Projmean_N2) - min(neg_2a$Projmean_N2) #.78
max(modal_3a$Projmean_M3) - min(modal_3a$Projmean_M3) #.53
max(cond_4a$Projmean_C4) - min(cond_4a$Projmean_C4) #.6
max(polar_5a$Projmean_P5) - min(polar_5a$Projmean_P5) #.87
max(neg_6a$Projmean_N6) - min(neg_6a$Projmean_N6) #.77
max(modal_7a$Projmean_M7) - min(modal_7a$Projmean_M7) #.54
max(cond_8a$Projmean_C8) - min(cond_8a$Projmean_C8) #.57
max(polar_9a$Projmean_P9) - min(polar_9a$Projmean_P9) #.
max(neg_10a$Projmean_N10) - min(neg_10a$Projmean_N10) #.44
max(modal_11a$Projmean_M11) - min(modal_11a$Projmean_M11) #
max(cond_12a$Projmean_C12) - min(cond_12a$Projmean_C12) #.

max(polar_1a$AImean_P1) - min(polar_1a$AImean_P1) #.74
max(neg_2a$AImean_N2) - min(neg_2a$AImean_N2) #.36
max(modal_3a$AImean_M3) - min(modal_3a$AImean_M3) #.59
max(cond_4a$AImean_C4) - min(cond_4a$AImean_C4) #.21
max(polar_5a$AImean_P5) - min(polar_5a$AImean_P5) #.88
max(neg_6a$AImean_N6) - min(neg_6a$AImean_N6) #.53
max(modal_7a$AImean_M7) - min(modal_7a$AImean_M7) #.42
max(cond_8a$AImean_C8) - min(cond_8a$AImean_C8) #.25
max(polar_9a$AImean_P9) - min(polar_9a$AImean_P9) #.
max(neg_10a$AImean_N10) - min(neg_10a$AImean_N10) #.22
max(modal_11a$AImean_M11) - min(modal_11a$AImean_M11) #
max(cond_12a$AImean_C12) - min(cond_12a$AImean_C12) #.


## correlations ----

# projection correlations across embeddings (same not-at-issueness diagnostics)
cor(d$Projmean_P1,d$Projmean_N2) # .72
cor(d$Projmean_P1,d$Projmean_M3) # .85
cor(d$Projmean_N2,d$Projmean_M3) # .82
cor(d$Projmean_P1,d$Projmean_C4) # .62
cor(d$Projmean_N2,d$Projmean_C4) # .54
cor(d$Projmean_M3,d$Projmean_C4) # .73

cor(d$Projmean_P5,d$Projmean_N6) # .92
cor(d$Projmean_P1,d$Projmean_M7) # .72
cor(d$Projmean_N6,d$Projmean_M7) # .81
cor(d$Projmean_P5,d$Projmean_C8) # .81
cor(d$Projmean_N6,d$Projmean_C8) # .79
cor(d$Projmean_M7,d$Projmean_C8) # .78

cor(d$Projmean_P9,d$Projmean_N10) # .83
cor(d$Projmean_P9,d$Projmean_M11) # .
cor(d$Projmean_N10,d$Projmean_M11) # .
cor(d$Projmean_P9,d$Projmean_C12) # .
cor(d$Projmean_N10,d$Projmean_C12) # .
cor(d$Projmean_M11,d$Projmean_C12) # .

# projection correlations for the same embeddings across the experiments
cor(d$Projmean_P1,d$Projmean_P5) # .83
cor(d$Projmean_P5,d$Projmean_P9) # .96

cor(d$Projmean_N2,d$Projmean_N6) # .91
cor(d$Projmean_N6,d$Projmean_N10) # .88

cor(d$Projmean_M3,d$Projmean_M7) # .72
cor(d$Projmean_M7,d$Projmean_M11) # .

cor(d$Projmean_C4,d$Projmean_C8) # .78
cor(d$Projmean_C8,d$Projmean_C12) # .

# not-at-issueness correlations across embeddings ("same" not-at-issueness diagnostics)
cor(d$AImean_P1,d$AImean_N2) # .59
cor(d$AImean_P1,d$AImean_M3) # .47
cor(d$AImean_N2,d$AImean_M3) # .50
cor(d$AImean_P1,d$AImean_C4) # .3
cor(d$AImean_N2,d$AImean_C4) # .52
cor(d$AImean_M3,d$AImean_C4) # .1

cor(d$AImean_P5,d$AImean_N6) # -.17
cor(d$AImean_P1,d$AImean_M7) # .18
cor(d$AImean_N6,d$AImean_M7) # .13
cor(d$AImean_P5,d$AImean_C8) # .18
cor(d$AImean_N6,d$AImean_C8) # .49
cor(d$AImean_M7,d$AImean_C8) # .49

cor(d$AImean_P9,d$AImean_N10) # -03
cor(d$AImean_P9,d$AImean_M11) # .
cor(d$AImean_N10,d$AImean_M11) # .
cor(d$AImean_P9,d$AImean_C12) # .
cor(d$AImean_N10,d$AImean_C12) # .
cor(d$AImean_M11,d$AImean_C12) # .

# not-at-issueness correlations for the same embeddings across the experiments (different diagnostics)
cor(d$AImean_P1,d$AImean_P5) # .74
cor(d$AImean_P5,d$AImean_P9) # -.34

cor(d$AImean_N2,d$AImean_N6) # -.18
cor(d$AImean_N6,d$AImean_N10) # -.41

cor(d$AImean_M3,d$AImean_M7) # .55
cor(d$AImean_M7,d$AImean_M11) # .

cor(d$AImean_C4,d$AImean_C8) # -.05
cor(d$AImean_C8,d$AImean_C12) # .

# correlation between projection and at-issueness in the 12 experiments
cor(d$Projmean_P1,d$AImean_P1) # .79
cor(d$Projmean_N2,d$AImean_N2) # .29
cor(d$Projmean_M3,d$AImean_M3) # .24
cor(d$Projmean_C4,d$AImean_C4) # .2
cor(d$Projmean_P5,d$AImean_P5) # .47
cor(d$Projmean_N6,d$AImean_N6) # -.35
cor(d$Projmean_M7,d$AImean_M7) # .09
cor(d$Projmean_C8,d$AImean_C8) # -.11
cor(d$Projmean_P9,d$AImean_P9) # .28
cor(d$Projmean_N10,d$AImean_N10) # .01
cor(d$Projmean_M11,d$AImean_M11) # .
cor(d$Projmean_C12,d$AImean_C12) # .

# how much variability is there in the individual diagnostics?



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


