# 13_explicitIgnorance
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(ggrepel)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

# load cleaned data
d = read_tsv("../data/cd.tsv")

names(d)
table(d$context)

length(unique(d$participantID)) #370 participants

# Fig 1: plot of mean certainty ratings from Exp 1a of Degen & Tonhauser 2022 -----
# import data from repo
cd <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
summary(cd)

# mean projectivity by predicate, including the main clause controls
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
means

# define colors for the predicates
cols = data.frame(V=levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("MC"),"MC","NF")))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#D55E00",
                      ifelse(cols$VeridicalityGroup == "NF", "#009E73",'black'))


cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = factor(x=
                                   ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
                                          ifelse(means$verb  %in% c("MC"),"MC","NF")),levels=rev(c("F","NF","MC")))

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
subjmeans$VeridicalityGroup = factor(x=
                                   ifelse(subjmeans$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
                                          ifelse(subjmeans$verb  %in% c("MC"),"MC","NF")),levels=rev(c("F","NF","MC")))

levels(subjmeans$verb)
#view(subjmeans)

# version of Figure 2 Language paper
# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_violin(data=subjmeans,scale="width",linewidth = 0, alpha = .3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax, fill=VeridicalityGroup, shape=VeridicalityGroup),width=0.1,color="black") +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  scale_fill_manual(values=rev(c("#D55E00","#009E73","black")),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/mean-certainty-by-predicateType.pdf",height=4.5,width=7)

# Fig 3: plot of mean naturalness ratings in explicit ignorance context ----

# target data: explicit ignorance context
table(d$expression)
t = d %>%
  filter(context == "explicitIgnorance") %>%
  mutate(expression = recode(expression, "controlGood1" = "controls", "controlGood2" = "controls")) %>%
  filter(expression != "controls")

# calculate mean naturalness rating by expression, including the fillers, in explicit ignorance context
nat.meansEIC = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
nat.meansEIC
levels(nat.meansEIC$expression)

# color code the expressions
factives <- c("know", "discover", "be annoyed", "reveal", "see")
fillers <- c("too", "also","cleft","again","stop", "continue")

nat.meansEIC$ps = ifelse(nat.meansEIC$expression %in% fillers, "filler",
                                ifelse(nat.meansEIC$expression %in% factives, "factive", "other"))

t$ps = ifelse(t$expression %in% fillers, "filler",
                     ifelse(t$expression %in% factives, "factive", "other"))

table(nat.meansEIC$ps, nat.meansEIC$expression)

text.color <- ifelse(nat.meansEIC$expression[order(nat.meansEIC$Mean)] %in% factives, '#D55E00',
                     ifelse(nat.meansEIC$expression[order(nat.meansEIC$Mean)] %in% fillers, "black", "#009E73"))
text.color

t$expression = factor(t$expression, levels = nat.meansEIC$expression[order(nat.meansEIC$Mean)], ordered = TRUE)

# plot of naturalness means, with participants' individual responses
ggplot(nat.meansEIC, aes(x=expression, y=Mean)) +
  geom_violin(data=t[t$context == "explicitIgnorance",],aes(x=expression, y=response),
               scale="width",color="gray80", fill = "gray80") +
  geom_point(aes(group = ps, fill = ps), shape=21,stroke=.5,size=3, color="black") +
  scale_fill_manual(values=c('#D55E00','black','#009E73')) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  guides(fill=FALSE) +
  theme(legend.position="top", panel.grid.major.x = element_blank()) +
  ylab("Mean naturalness rating \n in explicit ignorance context") +
  xlab("Expression") +  
  #theme_dark() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = text.color)) 
ggsave("../graphs/explicit-ignorance-naturalness-by-predicate.pdf",height=4,width=7)


# Fig 4.1: plot of mean naturalness ratings in by context ----
# for 20 clause-embedding predicates only

# calculate mean naturalness rating by predicate and context
table(d$expression)
table(d$context)  #explicit ignorance / factL / factH

nat.means = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  group_by(expression,context) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  ungroup %>%
  select(-c(CILow,CIHigh)) %>%
  mutate(context = as.factor(context))
nat.means
table(nat.means$context)
nat.means$expression <- as.factor(nat.means$expression)
levels(nat.means$expression)

t = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  mutate(context = as.factor(context))
levels(t$context)

# order predicates by mean naturalness rating in EIC
tmp <- t %>%
  filter(context == "explicitIgnorance") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
tmp
levels(tmp$expression)

nat.means$expression = factor(nat.means$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
t$expression = factor(t$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
levels(nat.means$expression)
levels(t$expression)

# order the contexts: EIC, low, high
levels(nat.means$context)
nat.means$context = factor(nat.means$context, levels = c("explicitIgnorance", "factL", "factH"))
levels(t$context)
t$context = factor(t$context, levels = c("explicitIgnorance", "factL", "factH"))

fill.color <- ifelse(levels(nat.means$expression) %in% factives, '#D55E00', "#009E73")
fill.color

# to color the facets differently
library(ggh4x)

strip <- strip_themed(background_x = elem_list_rect(fill = fill.color))

# violinplot
ggplot(nat.means, aes(x=context, y=Mean)) +
  geom_violin(data=t, aes(x=context, y=response, fill = context), scale="width", linewidth = 0) +
  geom_point(aes(fill = context), shape=21,stroke=.5,size=2, color="black") +
  scale_fill_manual(values=c('gray80',"#56B4E9",'#F0E442'), 
                    name = "Context", 
                    labels=c('explicit ignorance', 'lower prior probability','higher prior probability')) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating") +
  xlab("Context") +
  facet_wrap2(. ~ expression, nrow = 2, strip = strip) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "black")) 
ggsave("../graphs/naturalness-by-context-and-predicate.pdf",height=4,width=9)

# Fig 4.2: plot of mean naturalness ratings in by context ----
# for 20 clause-embedding predicates only
# with statistics output

# calculate mean naturalness rating by predicate and context
table(d$expression)
table(d$context)  #explicit ignorance / factL / factH

nat.means = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
         expression != "stop" & expression != "continue") %>%
  group_by(expression,context) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  ungroup %>%
  select(-c(CILow,CIHigh)) %>%
  mutate(context = as.factor(context))
nat.means
table(nat.means$context)
nat.means$expression <- as.factor(nat.means$expression)
levels(nat.means$expression)

t = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  mutate(context = as.factor(context))
levels(t$context)

# order predicates by mean naturalness rating in EIC
tmp <- t %>%
  filter(context == "explicitIgnorance") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
tmp
levels(tmp$expression)
  
nat.means$expression = factor(nat.means$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
t$expression = factor(t$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
levels(nat.means$expression)
levels(t$expression)

# order the contexts: EIC, low, high
levels(nat.means$context)
nat.means$context = factor(nat.means$context, levels = c("explicitIgnorance", "factL", "factH"))
levels(t$context)
t$context = factor(t$context, levels = c("explicitIgnorance", "factL", "factH"))

fill.color <- ifelse(levels(nat.means$expression) %in% factives, '#D55E00', "#009E73")
fill.color

# to color the facets differently
library(ggh4x)

strip <- strip_themed(background_x = elem_list_rect(fill = fill.color))

nat.means

# join results of statistical analysis with nat.means
contrasts = read_csv("../data/contrasts.csv")
view(contrasts)
contrasts = contrasts %>%
  select(-c(contrast)) %>%
  select(c(expression,context,x,xend,y,yend,linetype)) %>%
  mutate(expression = recode(expression,"be.annoyed" = "be annoyed", "be.right" = "be right")) %>%
  filter(linetype != 0)
contrasts$context = factor(contrasts$context, levels = c("explicitIgnorance", "factL", "factH"))
contrasts$expression = as.factor(contrasts$expression)
contrasts$linetype <- as.integer(contrasts$linetype)

contrasts$expression = factor(contrasts$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)

# violinplot
ggplot() +
  geom_violin(data=t, aes(x=context, y=response, fill = context), scale="width", linewidth = 0) +
  geom_point(data=nat.means, aes(x=context, y=Mean, fill = context), shape=21,stroke=.5,size=2, color="black") +
  geom_errorbar(data=nat.means,aes(x=context,ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_fill_manual(values=c('gray80',"#56B4E9",'#F0E442'),
                    name = "Context",
                    labels=c('explicit ignorance', 'lower prior probability','higher prior probability')) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(limits = c(-.15,1), breaks = seq(0,1,.2), labels = c("0",".2",".4",".6",".8","1")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(legend.position="top") +
  guides(linetype = "none") +
  ylab("Mean naturalness rating") +
  xlab("Context") +
  facet_wrap2(. ~ expression, nrow = 2, strip = strip) +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "black")) +
  geom_segment(data = contrasts, aes(x=x,xend=xend,y=y,yend=yend, linetype = as.factor(linetype)))
ggsave("../graphs/naturalness-by-context-and-predicate-with-stats.pdf",height=4.5,width=9)


# Fig 5: plot of mean certainty ratings for "Julian dances salsa" from Exp 1a of Degen & Tonhauser 2022 -----
# import data from repo
cd <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
summary(cd)
table(cd$content)
table(cd$verb)

# mean projectivity by predicate, including the main clause controls
means = cd %>%
  filter(verb != "MC") %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
means

# define colors for the predicates
cols = data.frame(V=levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("MC"),"MC","NF")))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#D55E00",
                      ifelse(cols$VeridicalityGroup == "NF", "#009E73",'black'))


cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = factor(x=
                                   ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
                                          ifelse(means$verb  %in% c("MC"),"MC","NF")),levels=rev(c("F","NF","MC")))

subjmeans = cd %>%
  filter(content == "Julian dances salsa") %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
subjmeans$VeridicalityGroup = factor(x=
                                       ifelse(subjmeans$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
                                              ifelse(subjmeans$verb %in% c("MC"),"MC","NF")),levels=rev(c("F","NF","MC")))

levels(subjmeans$verb)
#view(subjmeans)

# version of Figure 2 Language paper
# plot of means, 95% CIs and participants' ratings 
ggplot(subjmeans, aes(x=verb, y=Mean)) +
  #geom_point(data=subjmeans,scale="width",linewidth = 0, alpha = .3) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax, fill=VeridicalityGroup, shape=VeridicalityGroup),width=0.1,color="black") +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22)),labels=rev(c("factive","nonfactive")),name="Predicate type") +
  scale_fill_manual(values=rev(c("#D55E00","#009E73")),labels=rev(c("factive","nonfactive")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Certainty rating") +
  xlab("Predicate + 'Julian dances salsa'") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/mean-certainty-by-predicateType-JULIAN.pdf",height=4.5,width=7)

# auxiliary plots

#### plot of mean naturalness ratings against mean certainty ratings ----

# get data from Degen & Tonhauser, 2022 (Language)
# https://github.com/judith-tonhauser/projective-probability/tree/master/results/5-projectivity-no-fact
dt <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
summary(dt)
table(dt$verb)

# target data
dt <- dt %>%
  filter(verb != "MC")
table(dt$verb)

dt.means = dt %>%
  group_by(verb) %>%
  summarize(MeanCertain = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinCertain = MeanCertain - CILow, YMaxCertain = MeanCertain + CIHigh, expression = fct_reorder(as.factor(verb),MeanCertain))
dt.means
levels(dt.means$expression)

# reduce the acceptability data to the 20 predicates in the explicit ignorance context
t = d %>%
  filter(context == "explicitIgnorance") %>%
  filter(expression != "controlGood1" & expression != "controlGood2" & expression != "again"
         & expression != "also" & expression != "too" & expression != "continue"
         & expression != "stop" & expression != "cleft")

# calculate mean naturalness rating by expression
nat.means = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
nat.means

# bind the data
data = left_join(nat.means, dt.means, by = "expression")
data
summary(data)

# plot of naturalness means against certainty means
ggplot(data, aes(x=Mean, y=MeanCertain),label = expression) +
  geom_point(shape=21,stroke=.5,size=2,color="black") +
  #geom_smooth(method="lm") +
  geom_text_repel(aes(label = expression),
                   #box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMin,xmax=YMax),height=.01,color="black") +
  geom_errorbar(aes(ymin=YMinCertain,ymax=YMaxCertain),width=.01,color="black") +
  #geom_point(data = nt[nt$context != "explicitIgnorance",], aes(x=expression, y=response), shape=21,fill="gray60", alpha=.5, color="blue") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  guides(fill=FALSE) +
  geom_abline(intercept = 1,slope = -1, col="red", lty = "dashed") + 
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  #theme(legend.position="top") +
  coord_fixed() +
  xlab("Mean naturalness rating \n in explicit ignorance context") +
  ylab("Mean certainty rating \n (from Degen & Tonhauser 2022)") 
  #theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/mean-acceptability-against-mean-certainty.pdf",height=5,width=5)

# calculate Spearman rank correlation
corr <- cor.test(x=data$Mean, y=data$MeanCertain, method = 'spearman', exact = FALSE)
corr

# Spearman's rank correlation rho
# 
# data:  data$Mean and data$MeanCertain
# S = 2152, p-value = 0.003681
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.6180451

  