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
exp1q = read.csv(file="../../1_projaiQ/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp1q")

exp1n = read.csv(file="../../2_projaiN/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp1n")

exp1m = read.csv(file="../../3_projaiM/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp1m")

exp1c = read.csv(file="../../4_projaiC/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp1c")

# at-issueness diagnostic: assent with positive continuation

exp2q = read.csv(file="../../5_projaiQ/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp2q")

exp2n = read.csv(file="../../6_projaiN/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp2n")

exp2m = read.csv(file="../../7_projaiM/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp2m")

exp2c = read.csv(file="../../8_projaiC/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp2c")

# at-issueness diagnostic: assent with adversative continuation

exp3q = read.csv(file="../../9_projaiQ/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp3q")

exp3n = read.csv(file="../../10_projaiN/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp3n")

exp3m = read.csv(file="../../11_projaiM/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp3m")

exp3c = read.csv(file="../../12_projaiC/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  filter(short_trigger != "MC") %>%
  spread(question_type,response) %>%
  mutate(SubExperiment="exp3c")

# aggregate

exp1qA = exp1q %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp1q = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp1q = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp1q = AImean_exp1q - ci.low_AI, YMax_AI_exp1q = AImean_exp1q + ci.high_AI, 
         YMin_Proj_exp1q = Projmean_exp1q - ci.low_Proj, YMax_Proj_exp1q = Projmean_exp1q + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))
#View(exp1qA)

exp1nA = exp1n %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp1n = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp1n = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp1n = AImean_exp1n - ci.low_AI, YMax_AI_exp1n = AImean_exp1n + ci.high_AI, 
         YMin_Proj_exp1n = Projmean_exp1n - ci.low_Proj, YMax_Proj_exp1n = Projmean_exp1n + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp1mA = exp1m %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp1m = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp1m = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp1m = AImean_exp1m - ci.low_AI, YMax_AI_exp1m = AImean_exp1m + ci.high_AI, 
         YMin_Proj_exp1m = Projmean_exp1m - ci.low_Proj, YMax_Proj_exp1m = Projmean_exp1m + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp1cA = exp1c %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp1c = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp1c = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp1c = AImean_exp1c - ci.low_AI, YMax_AI_exp1c = AImean_exp1c + ci.high_AI, 
         YMin_Proj_exp1c = Projmean_exp1c - ci.low_Proj, YMax_Proj_exp1c = Projmean_exp1c + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp2qA = exp2q %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp2q = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp2q = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp2q = AImean_exp2q - ci.low_AI, YMax_AI_exp2q = AImean_exp2q + ci.high_AI, 
         YMin_Proj_exp2q = Projmean_exp2q - ci.low_Proj, YMax_Proj_exp2q = Projmean_exp2q + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp2nA = exp2n %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp2n = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp2n = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp2n = AImean_exp2n - ci.low_AI, YMax_AI_exp2n = AImean_exp2n + ci.high_AI, 
         YMin_Proj_exp2n = Projmean_exp2n - ci.low_Proj, YMax_Proj_exp2n = Projmean_exp2n + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp2mA = exp2m %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp2m = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp2m = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp2m = AImean_exp2m - ci.low_AI, YMax_AI_exp2m = AImean_exp2m + ci.high_AI, 
         YMin_Proj_exp2m = Projmean_exp2m - ci.low_Proj, YMax_Proj_exp2m = Projmean_exp2m + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp2cA = exp2c %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp2c = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp2c = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp2c = AImean_exp2c - ci.low_AI, YMax_AI_exp2c = AImean_exp2c + ci.high_AI, 
         YMin_Proj_exp2c = Projmean_exp2c - ci.low_Proj, YMax_Proj_exp2c = Projmean_exp2c + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp3qA = exp3q %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp3q = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp3q = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp3q = AImean_exp3q - ci.low_AI, YMax_AI_exp3q = AImean_exp3q + ci.high_AI, 
         YMin_Proj_exp3q = Projmean_exp3q - ci.low_Proj, YMax_Proj_exp3q = Projmean_exp3q + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp3nA = exp3n %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp3n = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp3n = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp3n = AImean_exp3n - ci.low_AI, YMax_AI_exp3n = AImean_exp3n + ci.high_AI, 
         YMin_Proj_exp3n = Projmean_exp3n - ci.low_Proj, YMax_Proj_exp3n = Projmean_exp3n + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp3mA = exp3m %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp3m = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp3m = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp3m = AImean_exp3m - ci.low_AI, YMax_AI_exp3m = AImean_exp3m + ci.high_AI, 
         YMin_Proj_exp3m = Projmean_exp3m - ci.low_Proj, YMax_Proj_exp3m = Projmean_exp3m + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

exp3cA = exp3c %>%
  group_by(short_trigger) %>%
  summarise(AImean_exp3c = mean(ai), ci.low_AI=ci.low(ai), ci.high_AI=ci.high(ai),
            Projmean_exp3c = mean(projective), ci.low_Proj=ci.low(projective), ci.high_Proj=ci.high(projective)) %>%
  mutate(YMin_AI_exp3c = AImean_exp3c - ci.low_AI, YMax_AI_exp3c = AImean_exp3c + ci.high_AI, 
         YMin_Proj_exp3c = Projmean_exp3c - ci.low_Proj, YMax_Proj_exp3c = Projmean_exp3c + ci.high_Proj) %>%
  select(-c(ci.low_AI,ci.high_AI,ci.low_Proj,ci.high_Proj))

d = exp1qA %>%
  inner_join(exp1nA,by=("short_trigger")) %>%
  inner_join(exp1mA,by=("short_trigger")) %>%
  inner_join(exp1cA,by=("short_trigger")) %>%
  inner_join(exp2qA,by=("short_trigger")) %>%
  inner_join(exp2nA,by=("short_trigger")) %>%
  inner_join(exp2mA,by=("short_trigger")) %>%
  inner_join(exp2cA,by=("short_trigger")) %>%
  inner_join(exp3qA,by=("short_trigger")) %>%
  inner_join(exp3nA,by=("short_trigger")) %>%
  inner_join(exp3mA,by=("short_trigger")) %>%
  inner_join(exp3cA,by=("short_trigger"))
  
summary(d)  
  
# add VeridicalityGroup and colors for plotting
# define colors for the predicates
d$VeridicalityGroup = as.factor(
  ifelse(d$short_trigger %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))
d$Colors =  ifelse(d$VeridicalityGroup == "F", "tomato1", "black")

# how much variability?
max(polar_1a$Projmean_P1) - min(polar_1a$Projmean_P1) #.74 !
max(neg_2a$Projmean_N2) - min(neg_2a$Projmean_N2) #.78
max(modal_3a$Projmean_M3) - min(modal_3a$Projmean_M3) #.53
max(cond_4a$Projmean_C4) - min(cond_4a$Projmean_C4) #.6
max(polar_5a$Projmean_P5) - min(polar_5a$Projmean_P5) #.76 !
max(neg_6a$Projmean_N6) - min(neg_6a$Projmean_N6) #.69 !
max(modal_7a$Projmean_M7) - min(modal_7a$Projmean_M7) #.56 !
max(cond_8a$Projmean_C8) - min(cond_8a$Projmean_C8) #.67 !
max(polar_9a$Projmean_P9) - min(polar_9a$Projmean_P9) #.
max(neg_10a$Projmean_N10) - min(neg_10a$Projmean_N10) #.44
max(modal_11a$Projmean_M11) - min(modal_11a$Projmean_M11) #
max(cond_12a$Projmean_C12) - min(cond_12a$Projmean_C12) #.

max(polar_1a$AImean_P1) - min(polar_1a$AImean_P1) #.75 !
max(neg_2a$AImean_N2) - min(neg_2a$AImean_N2) #.36
max(modal_3a$AImean_M3) - min(modal_3a$AImean_M3) #.59
max(cond_4a$AImean_C4) - min(cond_4a$AImean_C4) #.21
max(polar_5a$AImean_P5) - min(polar_5a$AImean_P5) #.72 !
max(neg_6a$AImean_N6) - min(neg_6a$AImean_N6) #.31 !
max(modal_7a$AImean_M7) - min(modal_7a$AImean_M7) #.4 !
max(cond_8a$AImean_C8) - min(cond_8a$AImean_C8) #.29 !
max(polar_9a$AImean_P9) - min(polar_9a$AImean_P9) #.
max(neg_10a$AImean_N10) - min(neg_10a$AImean_N10) #.22
max(modal_11a$AImean_M11) - min(modal_11a$AImean_M11) #
max(cond_12a$AImean_C12) - min(cond_12a$AImean_C12) #.


## correlations ----

# projection correlations across embeddings (same not-at-issueness diagnostics)

# exp1
cor(d$Projmean_exp1q,d$Projmean_exp1n) # .95
cor(d$Projmean_exp1q,d$Projmean_exp1m) # .92
cor(d$Projmean_exp1q,d$Projmean_exp1c) # .96

cor(d$Projmean_exp1n,d$Projmean_exp1m) # .81
cor(d$Projmean_exp1n,d$Projmean_exp1c) # .88

cor(d$Projmean_exp1m,d$Projmean_exp1c) # .94

# exp2
cor(d$Projmean_exp2q,d$Projmean_exp2n) # .93
cor(d$Projmean_exp2q,d$Projmean_exp2m) # .92
cor(d$Projmean_exp2q,d$Projmean_exp2c) # .94

cor(d$Projmean_exp2n,d$Projmean_exp2m) # .81
cor(d$Projmean_exp2n,d$Projmean_exp2c) # .87

cor(d$Projmean_exp2m,d$Projmean_exp2c) # .94

# exp3
cor(d$Projmean_exp3q,d$Projmean_exp3n) # .95
cor(d$Projmean_exp3q,d$Projmean_exp3m) # .92
cor(d$Projmean_exp3q,d$Projmean_exp3c) # .94

cor(d$Projmean_exp3n,d$Projmean_exp3m) # .81
cor(d$Projmean_exp3n,d$Projmean_exp3c) # .84

cor(d$Projmean_exp3m,d$Projmean_exp3c) # .94

# not-at-issueness correlations across embeddings ("same" not-at-issueness diagnostics)
cor(d$AImean_P1,d$AImean_N2) # .59
cor(d$AImean_P1,d$AImean_M3) # .47
cor(d$AImean_N2,d$AImean_M3) # .50
cor(d$AImean_P1,d$AImean_C4) # .3
cor(d$AImean_N2,d$AImean_C4) # .52
cor(d$AImean_M3,d$AImean_C4) # .1

# for Moscow talk:
cor(d$AImean_P1,d$AImean_N6) #-.55

cor(d$AImean_P5,d$AImean_N6) # -.45 !
cor(d$AImean_P1,d$AImean_M7) # .54 !
cor(d$AImean_N6,d$AImean_M7) # .09 !
cor(d$AImean_P5,d$AImean_C8) # .14 !
cor(d$AImean_N6,d$AImean_C8) # .54 !
cor(d$AImean_M7,d$AImean_C8) # .69 !

cor(d$AImean_P9,d$AImean_N10) # -03
cor(d$AImean_P9,d$AImean_M11) # .
cor(d$AImean_N10,d$AImean_M11) # .
cor(d$AImean_P9,d$AImean_C12) # .
cor(d$AImean_N10,d$AImean_C12) # .
cor(d$AImean_M11,d$AImean_C12) # .

# not-at-issueness correlations for the same embeddings across the experiments (different diagnostics)
cor(d$AImean_P1,d$AImean_P5) # .94 !
cor(d$AImean_P5,d$AImean_P9) # -.34

cor(d$AImean_N2,d$AImean_N6) # -.18
cor(d$AImean_N6,d$AImean_N10) # -.41

cor(d$AImean_M3,d$AImean_M7) # .55
cor(d$AImean_M7,d$AImean_M11) # .

cor(d$AImean_C4,d$AImean_C8) # -.05
cor(d$AImean_C8,d$AImean_C12) # .

# correlation between projection and at-issueness in the 12 experiments
cor(d$Projmean_P1,d$AImean_P1) # .79 !
cor(d$Projmean_N2,d$AImean_N2) # .29
cor(d$Projmean_M3,d$AImean_M3) # .24
cor(d$Projmean_C4,d$AImean_C4) # .2
cor(d$Projmean_P5,d$AImean_P5) # .63 !
cor(d$Projmean_N6,d$AImean_N6) # -.69 !
cor(d$Projmean_M7,d$AImean_M7) # -.13 !
cor(d$Projmean_C8,d$AImean_C8) # -.74 !
cor(d$Projmean_P9,d$AImean_P9) # .28
cor(d$Projmean_N10,d$AImean_N10) # .01
cor(d$Projmean_M11,d$AImean_M11) # .
cor(d$Projmean_C12,d$AImean_C12) # .

# how much variability is there in the individual diagnostics?
summary(d)
d$Colors


# plot projection against projection ----
options(ggrepel.max.overlaps = Inf)

ggplot(d, aes(x=Projmean_exp1q,y=Projmean_exp1n)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp1q,xmax=YMax_Proj_exp1q),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp1n,ymax=YMax_Proj_exp1n),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=2,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 1q") +
  ylab("Mean certainty rating Exp 1n") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp1q-against-exp1n.pdf",width=5,height=5)

ggplot(d, aes(x=Projmean_exp1q,y=Projmean_exp1m)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp1q,xmax=YMax_Proj_exp1q),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp1m,ymax=YMax_Proj_exp1m),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=2,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 1q") +
  ylab("Mean certainty rating Exp 1m") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp1q-against-exp1m.pdf",width=5,height=5)

ggplot(d, aes(x=Projmean_exp1q,y=Projmean_exp1c)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp1q,xmax=YMax_Proj_exp1q),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp1c,ymax=YMax_Proj_exp1c),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=2,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 1q") +
  ylab("Mean certainty rating Exp 1c") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp1q-against-exp1c.pdf",width=5,height=5)

ggplot(d, aes(x=Projmean_exp1n,y=Projmean_exp1m)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp1n,xmax=YMax_Proj_exp1n),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp1m,ymax=YMax_Proj_exp1m),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=2,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 1n") +
  ylab("Mean certainty rating Exp 1m") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp1n-against-exp1m.pdf",width=5,height=5)

ggplot(d, aes(x=Projmean_exp1n,y=Projmean_exp1c)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp1n,xmax=YMax_Proj_exp1n),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp1c,ymax=YMax_Proj_exp1c),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=2,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 1n") +
  ylab("Mean certainty rating Exp 1c") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp1n-against-exp1c.pdf",width=5,height=5)

ggplot(d, aes(x=Projmean_exp1m,y=Projmean_exp1c)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp1m,xmax=YMax_Proj_exp1m),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp1c,ymax=YMax_Proj_exp1c),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=2,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 1m") +
  ylab("Mean certainty rating Exp 1c") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp1m-against-exp1c.pdf",width=5,height=5)


ggplot(d, aes(x=Projmean_exp2n,y=Projmean_exp2m)) +
  geom_point(shape=21) +
  #scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp2n,xmax=YMax_Proj_exp2n),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp2m,ymax=YMax_Proj_exp2m),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger,color=d$Colors),nudge_x=1,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 2n") +
  ylab("Mean certainty rating Exp 2m") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp2n-against-exp2m.pdf",width=5,height=5)

ggplot(d, aes(x=Projmean_exp2n,y=Projmean_exp1n)) +
  geom_point(shape=21) +
  #scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_exp2n,xmax=YMax_Proj_exp2n),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_exp1n,ymax=YMax_Proj_exp1n),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger,color=d$Colors),nudge_x=1,size=5,show.legend=F) +
  #scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating Exp 2n") +
  ylab("Mean certainty rating Exp 1n") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-exp2n-against-exp1n.pdf",width=5,height=5)


########

ggplot(d, aes(x=Projmean_P1,y=Projmean_N6,fill=VeridicalityGroup)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_P1,xmax=YMax_Proj_P1),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_N6,ymax=YMax_Proj_N6),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger,color=d$Colors),nudge_x=-.05,size=5,show.legend=F) +
  scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating (Exp 1, polar Q)") +
  ylab("Mean certainty rating (Exp 3, negation)") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-P1-against-N6.pdf",width=5,height=5)

ggplot(d, aes(x=Projmean_P5,y=Projmean_N6,fill=VeridicalityGroup)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_Proj_P5,xmax=YMax_Proj_P5),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_Proj_N6,ymax=YMax_Proj_N6),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger,color=d$Colors),nudge_x=-.05,size=5,show.legend=F) +
  scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean certainty rating (Exp 2, polar Q)") +
  ylab("Mean certainty rating (Exp 3, negation)") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-P5-against-N6.pdf",width=5,height=5)

# plot ai against ai ----
ggplot(d, aes(x=AImean_P1,y=AImean_P5,fill=VeridicalityGroup)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_AI_P1,xmax=YMax_AI_P1),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_AI_P5,ymax=YMax_AI_P5),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger,color=d$Colors),nudge_x=-.05,size=5,show.legend=F) +
  scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean not-at-issue rating (Exp 1: Q, \"asking whether\")") +
  ylab("Mean not-at-issue rating (Exp 2: Q, \"yes, p\")") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/nai-P1-against-P5.pdf",width=5,height=5)

ggplot(d, aes(x=AImean_P1,y=AImean_N6,fill=VeridicalityGroup)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_AI_P1,xmax=YMax_AI_P1),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_AI_N6,ymax=YMax_AI_N6),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger,color=d$Colors),nudge_x=-.05,size=5,show.legend=F) +
  scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean not-at-issue rating (Exp 1: Q, \"asking whether\")") +
  ylab("Mean not-at-issue rating (Exp 3: Neg, assent)") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/nai-P1-against-N6.pdf",width=5,height=5)

ggplot(d, aes(x=AImean_P5,y=AImean_N6,fill=VeridicalityGroup)) +
  geom_point(shape=21) +
  scale_fill_manual(values=c("tomato1","black")) +
  geom_errorbarh(aes(xmin=YMin_AI_P5,xmax=YMax_AI_P5),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin_AI_N6,ymax=YMax_AI_N6),alpha=.8,color="gray") +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger,color=d$Colors),nudge_x=-.05,size=5,show.legend=F) +
  scale_colour_manual(values=c("black", "tomato1")) +
  xlab("Mean not-at-issue rating (Exp 2: Q, \"yes, p\")") +
  ylab("Mean not-at-issue rating (Exp 3: Neg, assent)") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/nai-P5-against-N6.pdf",width=5,height=5)



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


