

# 4 projectivity by predicate and operator --------------------------------
## means and confidence intervals for projection rating by predicate + operator






# 5 verb profiles ---------------------------------------------------------
## 5a projection by operator, for each verb (verb profiles)
predicate_operator_means %>%
  ggplot(aes(x = fct_reorder(operator, projection, .fun = "mean"), y = projection, color = fct_reorder(operator, projection, .fun = "mean"), shape = fct_reorder(operator, projection, .fun = "mean"))) +
  geom_violin(data = data, scale="width", alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width = 0.5, color="black", alpha = .6) +
  geom_point(size = 4) +
  facet_wrap(vars(fct_reorder(predicate, projection, .fun = "mean"))) +
  xlab("Operator") +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1), legend.position = "none")

ggsave("../figures/predicate-profiles-1.pdf", width=10, height=10)



predicate_operator_means$group = factor(
  ifelse(predicate_operator_means$predicate %in% c("think", "pretend"), "negation-high", 
  ifelse(predicate_operator_means$predicate  %in% 
           c("demonstrate", "prove", "confirm"), "negation-low", 
  ifelse(predicate_operator_means$predicate  %in%
           c("admit", "confess", "announce", "reveal"), "modal-low",
  ifelse(predicate_operator_means$predicate  %in% 
           c("hear", "know", "inform"), "conditional-high",
  ifelse(predicate_operator_means$predicate  %in% 
           c("discover", "see", "acknowledge"), "question-high",
  ifelse(predicate_operator_means$predicate  %in% c("suggest", "be_right"), "suggest",
  ifelse(predicate_operator_means$predicate  %in% c("establish"), "establish",
  ifelse(predicate_operator_means$predicate  %in% c("say"), "say",
  ifelse(predicate_operator_means$predicate  %in% c("be_annoyed"), "annoy",
  "N/A"))))))))),
  levels = c("negation-high","negation-low","modal-low","conditional-high",
             "question-high", "suggest", "establish", "say", "annoy"))

levels(predicate_operator_means$group)

# all groups overview ----
predicate_operator_means %>%
  mutate(predicate = fct_reorder(predicate, projection, .fun = "mean")) %>%
  ggplot(aes(x = operator, y = projection, color = predicate, group = predicate)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(~group) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("")

ggsave(f="../graphs/profiles-grouped.pdf",height=6,width=8)

# individual profiles ----

# Color blind friendly palette with black (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
cbbPalette2 <- c("#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

library("directlabels")

# group 1 ----
proj_means %>% filter(groups == "1") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 2.4) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.1),
  #                       method = list(cex=0.7, "first.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
                        method = list(cex=1, rot = 50,  "last.bumpup")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile1.pdf",height=4,width=4)


# N > M, Q, C

# group 2 ----
proj_means %>% filter(groups == "2") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 2.5) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .075),
  #                       method = list(cex=0.54, "last.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
                        method = list(cex=1,  "smart.grid")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile2.pdf",height=4,width=4)
# C > N, M, Q

# group 3 ----
proj_means %>% filter(groups == "3") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 3) + 
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.1),
                        method = list(cex=0.7, "first.bumpup")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile3.pdf",height=4,width=4)
# Q, C > N, M

# group 4 ----
proj_means %>% filter(groups == "4") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 3) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
  #                       method = list(cex=0.6, "last.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
                        method = list(cex=1,"smart.grid")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile4.pdf",height=4,width=4)
# C, M > N

# group 5 ----
proj_means %>% filter(groups == "5") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 3) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.08),
  #                       method = list(cex=0.63, "first.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.1),
                        method = list(cex=1,"smart.grid")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile5.pdf",height=4,width=4)
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

