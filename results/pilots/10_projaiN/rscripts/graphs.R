# 10_projaiN
# negation: projection and at-issueness (assent with positive continuation)
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(ggrepel)
theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

d = read_csv("../data/data_preprocessed.csv")

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1"))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response)

cd = t

table(cd$short_trigger)
# change cd verb names to match veridicality names
cd = cd %>%
  mutate(verb=recode(short_trigger, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# mean not-at-issueness by predicate, including the main clause controls
ai.means = cd %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
ai.means

# define colors for the predicates
cols = data.frame(V=levels(ai.means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))
         
levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(ai.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "tomato1", "black")
                      
cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(ai.means$verb)),]$V, ordered = TRUE)
levels(cols$V)

ai.means$VeridicalityGroup = as.factor(
  ifelse(ai.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))


ai.subjmeans = cd %>%
  group_by(short_trigger,workerid) %>%
  summarize(Mean = mean(ai)) 
ai.subjmeans$verb <- factor(ai.subjmeans$short_trigger, levels = unique(levels(ai.means$verb)))
levels(ai.subjmeans$verb)


# plot of means, 95% CIs and participants' ratings 
ggplot(ai.means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=ai.subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("tomato1","black")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Mean not-at-issueness rating") +
  xlab("Predicate (polar question)") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/nai-by-predicate.pdf",height=4,width=7)


# mean projectivity by predicate, including the main clause controls
proj.means = cd %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj.means


# define colors for the predicates
cols = data.frame(V=levels(proj.means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))
         
levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "tomato1", "black")
                      
cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
levels(cols$V)

proj.means$VeridicalityGroup = as.factor(
  ifelse(proj.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))

proj.subjmeans = cd %>%
  group_by(short_trigger,workerid) %>%
  summarize(Mean = mean(projective)) 
proj.subjmeans$verb <- factor(proj.subjmeans$short_trigger, levels = unique(levels(proj.means$verb)))
levels(proj.subjmeans$verb)


# plot of means, 95% CIs and participants' ratings 
ggplot(proj.means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=proj.subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("tomato1","black")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Mean certainty rating") +
  xlab("Predicate (polar question)") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/projection-by-predicate.pdf",height=4,width=7)


# plot mean at-issueness ratings against mean projectivity ratings
tmp.ai = ai.means %>%
  rename(AIMean="Mean",AIYMin="YMin",AIYMax="YMax") 
tmp.proj = proj.means %>%
  rename(ProjMean="Mean",ProjYMin="YMin",ProjYMax="YMax") 
toplot = tmp.ai %>%
  left_join(tmp.proj, by=c("short_trigger"))

ggplot(toplot, aes(x=AIMean,y=ProjMean,group=1)) +
  geom_text_repel(aes(label=short_trigger),alpha=.5,color="blue",size=3) +
  geom_errorbar(aes(ymin=ProjYMin,ymax=ProjYMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=AIYMin,xmax=AIYMax),color="gray50",alpha=.5) +
  geom_point() +
  xlab("Mean not-at-issueness rating ('asking whether')") +
  ylab("Mean certainty ratings") +
  xlim(0,1) +
  ylim(0,1) 
ggsave("../graphs/proj-by-ai-polar-Q.pdf",width=4.2,height=3.5)

# correlation between at-issueness and projectivity by predicate:
toplot <- toplot %>% filter(short_trigger != "MC") %>% droplevels()
table(toplot$short_trigger)
cor(toplot$AIMean,toplot$ProjMean) #.01

# correlation between at-issueness and projectivity by predicate and item:
means = t %>%
  filter(short_trigger != "MC") %>%
  droplevels() %>%
  group_by(short_trigger, content) %>%
  summarize(AIMean = mean(ai), ProjMean = mean(projective))
cor(means$AIMean,means$ProjMean) #.01

# by-participant projection by not-at-issueness
ggplot(t, aes(x=ai,y=projective)) +
  geom_smooth(method="lm") +
  geom_point() +
  xlab("Not-at-issueness rating ('sure that')") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~short_trigger) +
  theme(legend.position="top")
ggsave("../graphs/by-participant-correlation-polar-Q.pdf",width=20,height=25)
