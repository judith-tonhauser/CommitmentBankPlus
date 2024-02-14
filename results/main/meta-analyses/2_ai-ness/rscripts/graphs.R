

### summary statistics and graphs for projection data ------

# set wd to script dir
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data
data <- read.csv("../../data_combined.csv", header = TRUE, sep = ",")

# load helper functions
source('../../../../helpers.R')

# libraries for manipulating dataframes, and plotting
library(dplyr)
library(forcats)
library(ggplot2)

### summary statistics and graphs for projection data ------

# revert ai ratings back to original scale orientation
data$ai <- 1-data$ai

# add labels for ai-ness diagnostics ----
data$exp = as.factor(paste(data$op, data$exp_block, ""))
data$test = as.factor(paste(data$op, data$exp_block, ""))
levels(data$test)
new_levels <- c("sure?", "assent", "A/R")
new_levels <- rep(new_levels, 3)
new_levels <- append(new_levels, c("whether", "assent", "A/R"))
levels(data$test) <- new_levels

# mean ainess by diagnostic -----
ai_means_test = data %>% group_by(test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         test = fct_reorder(as.factor(test),Mean)) %>% ungroup()

ai_means_test %>% mutate(test = fct_reorder(test, Mean, 
                                          .fun = mean)) %>% 
  ggplot(aes(x = test, y=Mean)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(shape = test), size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  labs(title = "Mean ai-ness by diagnostic")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/ai-by-test.pdf")

# mean ai-ness by operator -----
ai_means_op = data %>% group_by(op) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         op = fct_reorder(as.factor(op),Mean)) %>% ungroup()

ai_means_op %>% mutate(op = fct_reorder(op, Mean, 
                                        .fun = mean)) %>% 
  ggplot(aes(x = op, y=Mean)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(shape = op), size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  labs(title = "Mean ai-ness by operator")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/ai-by-op.pdf")

# ai-ness by verb -----
ai_means_v = data %>% group_by(verb) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

ai_means_v %>% mutate(verb = fct_reorder(verb, Mean, 
                                           .fun = mean)) %>% 
  ggplot(aes(x=verb, y=Mean)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "blue") +
  geom_line() + 
  labs(title = "Mean at-issueness by predicate")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/ai-by-pred.pdf", height=6.5, width=14)




# questions only: ai-ness by diagnostic ----
ai_means_q_test = data %>% filter(op == "q") %>% group_by(test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         test = fct_reorder(as.factor(test),Mean)) %>% ungroup()

ai_means_q_test %>% mutate(test = fct_reorder(test, Mean, 
                                            .fun = mean)) %>% 
  ggplot(aes(x = test, y=Mean)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(shape = test), size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  labs(title = "Questions: Mean ai-ness by diagnostic")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/q-ai-by-test.pdf")

# questions only: ai-ness by predicate and diagnostic ----
ai_means_q = data %>% filter(op == "q") %>% group_by(verb, test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

# Color blind friendly palette with black (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ai_means_q %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = test, color = test)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = test), size = 3) + 
  # scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values = palette) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean ai-ness rating") +
  # ylab("") +
  xlab("") +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/q-ai-by-test+pred.pdf",height=4.7,width=9)

# negation only: ai-ness by diagnostic ----
ai_means_n_test = data %>% filter(op == "n") %>% group_by(test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         test = fct_reorder(as.factor(test),Mean)) %>% ungroup()

ai_means_n_test %>% mutate(test = fct_reorder(test, Mean, 
                                              .fun = mean)) %>% 
  ggplot(aes(x = test, y=Mean)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(shape = test), size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  labs(title = "Negation: Mean ai-ness by diagnostic")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/n-ai-by-test.pdf")

# negation only: ai-ness by predicate and diagnostic ----
ai_means_n = data %>% filter(op == "n") %>% group_by(verb, test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

ai_means_n %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = test, color = test)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = test), size = 3) + 
  # scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values = palette) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean ai-ness rating") +
  # ylab("") +
  xlab("") +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/n-ai-by-test+pred.pdf",height=4.7,width=9)


# modals only: ai-ness by diagnostic ----
ai_means_m_test = data %>% filter(op == "m") %>% group_by(test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         test = fct_reorder(as.factor(test),Mean)) %>% ungroup()

ai_means_m_test %>% mutate(test = fct_reorder(test, Mean, 
                                              .fun = mean)) %>% 
  ggplot(aes(x = test, y=Mean)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(shape = test), size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  labs(title = "Modals: Mean ai-ness by diagnostic")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/m-ai-by-test.pdf")

# modals only: ai-ness by predicate and diagnostic ----
ai_means_m = data %>% filter(op == "m") %>% group_by(verb, test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

ai_means_m %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = test, color = test)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = test), size = 3) + 
  # scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values = palette) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean ai-ness rating") +
  # ylab("") +
  xlab("") +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/m-ai-by-test+pred.pdf",height=4.7,width=9)





# conditionals only: ai-ness by diagnostic ----
ai_means_c_test = data %>% filter(op == "c") %>% group_by(test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         test = fct_reorder(as.factor(test),Mean)) %>% ungroup()

ai_means_c_test %>% mutate(test = fct_reorder(test, Mean, 
                                              .fun = mean)) %>% 
  ggplot(aes(x = test, y=Mean)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(shape = test), size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") + 
  labs(title = "Conditionals: Mean ai-ness by diagnostic")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

ggsave("../graphs/c-ai-by-test.pdf")

# conditionals only: ai-ness by predicate and diagnostic ----
ai_means_c = data %>% filter(op == "c") %>% group_by(verb, test) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

ai_means_c %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = test, color = test)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = test), size = 3) + 
  # scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values = palette) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean ai-ness rating") +
  # ylab("") +
  xlab("") +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/c-ai-by-test+pred.pdf",height=4.7,width=9)










# sure? and asking whether only: ai-ness by predicate and operator ----
ai_means_test1 = data %>% filter(test == "sure?" | test == "whether") %>% group_by(verb, op) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

ai_means_test1 %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = op, color = op)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = op), size = 3) + 
  # scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values = palette) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean ai-ness rating") +
  # ylab("") +
  xlab("") +
  labs(title = "Sure? and asking whether?: Mean ai-ness by operator")+
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/test1-ai-by-op+pred.pdf",height=4.7,width=9)

# assent only: ai-ness by predicate and operator ----
ai_means_test2 = data %>% filter(test == "assent") %>% group_by(verb, op) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

ai_means_test2 %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = op, color = op)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = op), size = 3) + 
  # scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values = palette) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean ai-ness rating") +
  # ylab("") +
  xlab("") +
  labs(title = "Assent test: Mean ai-ness by operator")+
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/assent-ai-by-op+pred.pdf",height=4.7,width=9)

# affirmative rejection only: ai-ness by predicate and operator ----
ai_means_test3 = data %>% filter(test == "A/R") %>% group_by(verb, op) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), 
            CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         verb = fct_reorder(as.factor(verb),Mean)) %>% ungroup()

ai_means_test3 %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = op, color = op)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = op), size = 3) + 
  # scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values = palette) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean ai-ness rating") +
  # ylab("") +
  xlab("") +
  labs(title = "Affirmative rejection test: Mean ai-ness by operator")+
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

ggsave("../graphs/ar-ai-by-op+pred.pdf",height=4.7,width=9)




