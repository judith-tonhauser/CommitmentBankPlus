
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

proj_means %>% mutate(verb = fct_reorder(verb, Mean, 
                                         .fun = mean)) %>% 
  mutate(op = fct_reorder(op, Mean, .fun = mean)) %>% 
  ggplot(aes(x=verb, y=Mean, group = op, color = op)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = op), size = 4) + 
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() + 
  labs(title = "Mean projectivity by predicate and operator")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/proj-by-both.pdf",height=6.5,width=14)



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
  labs(title = "Mean projectivity by operator, for each verb")+
  theme_bw()

ggsave("../graphs/proj-by-op-for-verb.pdf",height=8,width=10)

proj_means %>% mutate(verb = fct_reorder(verb, Mean, 
                                       .fun = mean)) %>% 
  
  ggplot(aes(x = verb, y=Mean, color = op, group = op)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_grid(rows = vars(op)) +
  geom_point(aes(shape = op), size = 4) + 
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1) +
  geom_line() + 
  labs(title = "Mean projectivity by operator, for each verb")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/proj-by-verb-for-op.pdf",height=9,width=14)
