# 13_explicitIgnorance
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

d = read_tsv("../data/cd.tsv")

names(d)
table(d$context)

# target data: explicit ignorance context
t = d %>%
  filter(context == "explicitIgnorance")

# calculate mean naturalness rating by expression, including the controls
nat.means = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
nat.means

# plot of naturalness means
ggplot(nat.means, aes(x=expression, y=Mean)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(data = t, aes(x=expression, y=response), shape=21,fill="gray60", alpha=.1, color="gray40") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating in explicit ignorance context") +
  xlab("Expression") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/explicit-ignorance-naturalness-by-predicate.pdf",height=4,width=7)

# no explicit ignorance context
nt = d %>%
  filter(context != "explicitIgnorance")

# calculate mean naturalness rating by expression, including the controls
nat.means = nt %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
nat.means
levels(nat.means$expression)

# plot of naturalness means
ggplot(nat.means, aes(x=expression, y=Mean)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  #geom_point(data = t, aes(x=expression, y=response), shape=21,fill="gray60", alpha=.1, color="gray40") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating in neutral context") +
  xlab("Expression") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/neutral-context-naturalness-by-predicate.pdf",height=4,width=7)

