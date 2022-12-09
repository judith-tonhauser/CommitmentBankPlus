
### summary statistics and graphs for projection data ------

# set wd to script dir
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data
data <- read.csv("../data/data_combined.csv", header = TRUE, sep = ",")

# load helper functions
source('../../../helpers.R')

# libraries for manipulating dataframes, and plotting
library(dplyr)
library(forcats)
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

proj_means %>% 
  ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = op, color = op)) +
  coord_cartesian(ylim=c(0,1)) +
  geom_point(aes(shape = op), size = 3) + 
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

ggsave("../graphs/proj-by-both.pdf",height=4.7,width=9)



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


# VERB PROFILES ---- 

proj_means %>%
  ggplot(aes(x = fct_reorder(op, Mean), y = Mean, group = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(vars(fct_reorder(verb, Mean))) +
  geom_point(size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profiles.pdf",height=6,width=8)


levels(proj_means$verb)
vgroups <- data.frame(verb = levels(proj_means$verb), profile = 
                        c("1",
                              # pretend
                          "7",
                                  # be right
                          "7",
                                  # suggest
                          "1",   
                                    # think
                          "say",    # say
                          "4",   # prove
                          "4",   # confirm
                          "4",   # establish
                          "demonstrate",   # demonstrate
                          "2",  # announce
                          "2",   # confess
                          "2",   # admit 
                          "2",   # reveal
                          "3",  # acknowledge
                          "6",  # hear
                          "5",  # inform
                          "6",  # see
                          "3",  # discover
                          "5",  # know
                          "5"  # be annoyed
                        ))

lookup_group <- function(verb_name){
  return(as.character(filter(vgroups, verb == verb_name)[2]))
}
proj_means = proj_means %>% rowwise() %>% mutate(groups = lookup_group(verb))

# all groups overview
proj_means %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(~groups) +
  geom_point(size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profiles-grouped.pdf",height=6,width=8)

# group 1 
proj_means %>% filter(groups == "1") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile1.pdf",height=3.7,width=4.5)
# N > M, Q, C

# group 2 
proj_means %>% filter(groups == "2") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile2.pdf",height=3.7,width=4.5)
# C > N, M, Q

# group 3 
proj_means %>% filter(groups == "3") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile3.pdf",height=3.7,width=4.5)
# Q, C > N, M

# group 4
proj_means %>% filter(groups == "4") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile4.pdf",height=3.7,width=4.5)
# C, M > N

# group 5
proj_means %>% filter(groups == "5") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile5.pdf",height=3.7,width=4.5)
# Q > N, C > M

# group 6
proj_means %>% filter(groups == "6") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile6.pdf",height=3.7,width=4.5)
# Q, C > N > M 

# group 7
proj_means %>% filter(groups == "7") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile7.pdf",height=3.7,width=4.5)
# C > Q

