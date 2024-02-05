##### GRAPHS FOR PROJECTION RATINGS #####

# 1 libraries, helpers, settings
# 2 projectivity by operator
# 3 projectivity by predicate
  ## 3a projectivity by predicate w distribution of participants' means
  ## 3b projectivity by predicate w distribution of observations
# 4 projectivity by predicate and operator
  ## 4b projectivity means with distributions for observations
  ## 4c projectivity means with distributions for paritipant means
# 5 verb profiles


# 1 Libraries,  helpers, settings -------------------------------------------

# R SETUP / PACKAGES / DEFINITIONS
library(tidyverse)
library(ggplot2)
source('../../../../helpers.R')
# Color blind friendly palette (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#D55E00", "#CC79A7", "#000000")
cbbRotate <- c("#0072B2", "#CC79A7", "#000000", "#D55E00", "#F0E442", "#E69F00", "#56B4E9", "#009E73")
graphcolor <- "#0072B2"
theme_set(theme_bw())

# LOAD DATA
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
data <- read.csv("../../data_combined.csv", header = TRUE, sep = ",")
data <- data %>% mutate(projection = as.numeric(projective), predicate = as.factor(verb), 
                        operator = as.factor(op), participant = as.factor(workerid), 
                        item = as.factor(content), .keep = "none")
levels(data$operator) <- c("conditional", "modal", "negation", "question")
  
str(data)
  

# 2 projection by operator ------------------------------------------------
# projection means and 95% CIs by operator 
operator_means <- data %>% group_by(operator) %>% 
  summarize(Mean = mean(projection), CILow = ci.low(projection), 
            CIHigh = ci.high(projection)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         operator = fct_reorder(as.factor(operator), Mean)) %>% ungroup()
operator_means
operator_means <- operator_means %>% mutate(projection = Mean, .keep = "unused")

# plot
operator_means %>% ggplot(aes(x = fct_reorder(operator, projection, .fun = "mean"), 
                              y = projection, label = round(projection, digits = 2))) +
  geom_violin(data = data, scale="width", fill = graphcolor, 
              alpha = .4, color = graphcolor) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "black") +
  geom_point(size=0.5, color = "black") +
  geom_text(hjust = 0, nudge_x = -0.08, nudge_y = -0.06) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  xlab("Operator")
  # + labs(title = "Projection by operator")
  
ggsave("../figures/operator-graph-1.pdf", width=3, height=4.5)


# 3 projection by predicate ---------------------------------------------
# means and confidence intervals for projection rating by predicate
predicate_means <- data %>% group_by(predicate) %>% 
  summarize(Mean = mean(projection), CILow = ci.low(projection), 
            CIHigh = ci.high(projection)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean)) %>% ungroup()
predicate_means
predicate_means <- predicate_means %>% mutate(projection = Mean, .keep = "unused")

# color coding by (semi-)factive vs non-factive
data$VeridicalityGroup = as.factor(
  ifelse(data$predicate %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))
predicate_means$VeridicalityGroup = as.factor(
  ifelse(predicate_means$predicate %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))
colors <- c("darkorchid", "dodgerblue")
pred_order <- levels(fct_reorder(predicate_means$predicate, predicate_means$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), "darkorchid", "dodgerblue")
  
# plot 
predicate_means %>% ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = data, scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = "Predicate") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="bottom", text = element_text(size=12)
  )

ggsave(f="../figures/predicate-graph-1.pdf", width=7, height=4.5)

# 4 projectivity by predicate and operator --------------------------------
## means and confidence intervals for projection rating by predicate + operator
predicate_operator_means <- data %>% group_by(predicate, operator) %>%
  summarize(Mean = mean(projection), CILow = ci.low(projection), 
            CIHigh = ci.high(projection)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         predicate = fct_reorder(as.factor(predicate),Mean)) %>%
  mutate(predicate = fct_reorder(predicate, Mean, .fun = mean)) %>%
  mutate(operator = fct_reorder(operator, Mean, .fun = mean)) %>% ungroup()
predicate_operator_means
predicate_operator_means <- predicate_operator_means %>% mutate(projection = Mean, .keep = "unused")

# color coding
predicate_operator_means$VeridicalityGroup = as.factor(
  ifelse(predicate_operator_means$predicate %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))
pred_order <- levels(fct_reorder(
  predicate_operator_means$predicate, 
  predicate_operator_means$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), "darkorchid", "dodgerblue")
  
# line plot for comparing the different operators in one plot
predicate_operator_means %>%
  ggplot(aes(x = fct_reorder(predicate, projection), y = projection, group = operator, color = operator)) +
  geom_point(aes(shape = operator), size = 4) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() +
  xlab("Predicate") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Mean certainty rating") +
  scale_shape_manual(values = c("M", "N", "C", "Q")) +
  scale_colour_manual(values=cbbPalette) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        legend.position="none", text = element_text(size=12)
  )
    
ggsave("../figures/predicate-operator-graph-1.pdf", width=7, height=4.5)
    
# projectivity means with distributions for observations
predicate_operator_means %>%
  ggplot(aes(x = fct_reorder(predicate, projection), y = projection)) +
  geom_violin(data = data, scale="width", fill = graphcolor, alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width = 0.5, color="black") +
  geom_point(size=0.5, color="black") +
  xlab("Predicate") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Mean certainty rating") +
  facet_wrap(~operator) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = -0.1, color=cols$Colors)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

ggsave("../figures/predicate-operator-graph-violin-1.pdf", width=10, height=5)


### Projection by predicate, negation only
# color coding
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "negation")$predicate, 
  subset(predicate_operator_means, operator == "negation")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), "darkorchid", "dodgerblue")

# plot
subset(predicate_operator_means, operator == "negation") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "negation"), scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="none", text = element_text(size=12)
  )

ggsave("../figures/ negation-predicate-graph-1.pdf", width=7, height=3.2)

### Projection by predicate, question only
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "question")$predicate, 
  subset(predicate_operator_means, operator == "question")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), "darkorchid", "dodgerblue")

subset(predicate_operator_means, operator == "question") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "question"), scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="none", text = element_text(size=12)
  )

ggsave("../figures/question-predicate-graph-1.pdf", width=7, height=3.2)

### Projection by predicate, modal only
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "modal")$predicate, 
  subset(predicate_operator_means, operator == "modal")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), "darkorchid", "dodgerblue")

subset(predicate_operator_means, operator == "modal") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "modal"), scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="none", text = element_text(size=12)
  )

ggsave("../figures/modal-predicate-graph-1.pdf", width=7, height=3.2)

### Projection by predicate, conditional only
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "conditional")$predicate, 
  subset(predicate_operator_means, operator == "conditional")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), "darkorchid", "dodgerblue")

subset(predicate_operator_means, operator == "conditional") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "conditional"), scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="bottom", text = element_text(size=12)
  )

ggsave("../figures/modal-predicate-graph-1.pdf", width=7, height=3.75)



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

