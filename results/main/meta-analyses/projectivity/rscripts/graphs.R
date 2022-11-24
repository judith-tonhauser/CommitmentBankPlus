
### summary statistics and graphs for projection data ------

# set wd to script dir
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data
data <- read.csv("../data/data_combined.csv", header = TRUE, sep = ",")

# load helper functions
source('../../../helpers.R')

# libraries for manipulating dataframes, and plotting
library(tidyverse)
library(ggplot2)

# projectivity by operator -----
proj_means_op = data %>% group_by(op) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), 
            CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         op = fct_reorder(as.factor(op),Mean)) %>% ungroup()

proj_means_op %>% mutate(op = fct_reorder(op, Mean, 
                                          .fun = mean)) %>% 
  ggplot(aes(x = op, y=Mean)) +
  # coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(shape = op), size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  labs(title = "Mean projectivity by operator")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/proj-by-op.pdf",height=4,width=5)



# projectivity by verb -----
proj_means_v = data %>% group_by(verb) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), 
            CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

proj_means_v %>% mutate(verb = fct_reorder(verb, Mean, 
                                           .fun = mean)) %>% 
  ggplot(aes(x=verb, y=Mean)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "blue") +
  geom_line() + 
  labs(title = "Mean projectivity by predicate")+
  theme_bw()

ggsave("../graphs/proj-by-verb.pdf",height=6.5,width=14)


# projectivity by verb and operator ----
proj_means = data %>% group_by(verb, op) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), 
            CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

#proj_means %>% mutate(verb = fct_reorder(verb, Mean, 
#                                         .fun = mean)) %>% 
#  mutate(op = fct_reorder(op, Mean, .fun = mean)) %>% 
#  ggplot(aes(x=verb, y=Mean, group = op, color = op)) +
#  coord_cartesian(ylim=c(0,1)) +
#  geom_point(aes(shape = op), size = 4) + 
#  scale_shape_manual(values = c("M", "N", "Q", "C")) +
#  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
#  geom_line() + 
#  labs(title = "Mean projectivity by predicate and operator")+
#  theme_bw() +
#  scale_color_brewer(palette = "PRGn")
proj_means <- mutate(proj_means, verb = fct_reorder(verb, Mean, .fun = mean))
proj_means <- mutate(proj_means, op = fct_reorder(op, Mean, .fun = mean))

ggplot(proj_means, aes(x=verb, y=Mean, group = op, color = op)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = op), size = 3) + 
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  guides(shape = "none") +
  scale_colour_discrete(labels = c("Modal", "Negation", "Question", "Conditional")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  labs(title = "Mean certainty for predicate by operator", color = "Operator")

ggsave("../graphs/proj-by-both.pdf",height=8,width=14)



proj_means %>% mutate(op = fct_reorder(op, Mean, 
                                          .fun = mean)) %>% 
  mutate(verb = fct_reorder(verb, Mean, 
                            .fun = mean)) %>% 
  ggplot(aes(x = op, y=Mean, group = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(vars(verb)) +
  geom_point(size = 1, color = "lightblue") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  scale_x_discrete(labels=c("m" = "modal", "n" = "negation", "q" = "question", "c" = "conditional")) +
  labs(title = "Mean projectivity by operator, for each verb")+
  theme_bw() + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
ggsave("../graphs/proj-by-op-for-verb.pdf",height=8,width=10)

proj_means %>% mutate(verb = fct_reorder(verb, Mean, 
                                       .fun = mean)) %>% 
  
  ggplot(aes(x = verb, y=Mean, color = op, group = op)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_grid(rows = vars(op)) +
  geom_point(aes(shape = op), size = 3) + 
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1) +
  geom_line() + 
  labs(title = "Mean projectivity by operator, for each verb")+
  theme_bw() + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  #scale_color_brewer(palette = "PRGn")

ggsave("../graphs/proj-by-verb-for-op.pdf",height=9,width=14)

# get the mean of participants' projectivity ratings by verb and operator 
subjomeans = data %>%
  group_by(verb,op, workerid) %>%
  summarize(Mean = mean(projective)) 
subjomeans$verb <- factor(subjomeans$verb, levels = unique(levels(pomeans$verb)))

# plot of projectivity means by verb and operator, 95% CIs and participants' ratings 
ggplot(pomeans, aes(x=verb, y=Mean)) +
  geom_violin(data=subjomeans,scale="width",color="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width = 0.5, color="black") +
  geom_point(size=0.5,color="black") +
  facet_wrap(~op, labeller = labeller(op = c("m" = "Modal", "n" = "Negation", "q" = "Question", "c" = "Conditional"))) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = -0.1, color=cols$Colors)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave(f="../graphs/projectivity-verb-operator-participant.pdf",height=6,width=8)


# means and confidence intervals for projectivity rating by verb
pmeans = data %>% group_by(verb) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), 
            CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean))

# get the mean of participants' projectivity ratings by verb 
subjmeans = data %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(projective)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(pmeans$verb)))

# plot of projectivity means, 95% CIs and participants' ratings 
ggplot(pmeans, aes(x=verb, y=Mean)) +
  geom_violin(data=subjmeans,scale="width",color="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
  geom_point(size=0.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave(f="../graphs/projectivity-verb-participant.pdf",height=6,width=8)

