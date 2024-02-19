##### Graphs for certainty ratings #####

# 1 load libraries, data, etc --------------------------------------------------

library(tidyverse)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# summary + graphing
library(ggplot2)
library(ggh4x)
library(ggrepel)
source('../../../helpers.R')
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#D55E00", 
                "#CC79A7", "#000000")
graphcolor <- "#0072B2"
pinkk <- "#CC79A7"
pred_colors <- c(pinkk, "gray")
theme_set(theme_bw())

# LOAD DATA
data <- read.csv("../../data_combined.csv", header = TRUE, sep = ",")
data <- data %>% mutate(projection = as.numeric(projective), predicate = as.factor(verb), 
                        operator = as.factor(op), participant = as.factor(workerid), 
                        item = as.factor(content), .keep = "none")
levels(data$operator) <- c("conditional", "modal", "negation", "question")
str(data)


# 2 projection by operator (Figure 2) ------------------------------------------

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
  geom_violin(data = data, scale="width", color = "gray", fill = "gray", alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = "black") +
  geom_point(size=1, color = "black", shape = 21) +
  geom_text(hjust = 0, nudge_x = -0.08, nudge_y = -0.06) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  xlab("Operator")

ggsave("../graphs/certainty-operator.pdf", width=3, height=4)


# 3 projection by predicate (all predicates, this is not in the paper) ---------

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

pred_order <- levels(fct_reorder(predicate_means$predicate, predicate_means$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), pinkk, "black")

# plot 
predicate_means %>% ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = data, scale="width", aes(fill = VeridicalityGroup, color = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = "Predicate") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = pred_colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_color_manual(values = pred_colors, name="Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="bottom", text = element_text(size=12)
  )

ggsave(f="../graphs/predicate-graph.pdf", width=7, height=4.5)


# 4 projection by predicate and operator, used to be figure 3, now replaced ----

# means and confidence intervals for projection rating by predicate / operator
predicate_operator_means <- data %>% group_by(predicate, operator) %>%
  summarize(Mean = mean(projection), CILow = ci.low(projection), 
            CIHigh = ci.high(projection)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         predicate = fct_reorder(as.factor(predicate),Mean)) %>%
  mutate(predicate = fct_reorder(predicate, Mean, .fun = mean)) %>%
  mutate(operator = fct_reorder(operator, Mean, .fun = mean)) %>% ungroup()
predicate_operator_means
predicate_operator_means <- predicate_operator_means %>% mutate(projection = Mean, 
                                                                .keep = "unused")

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

ggsave("../graphs/predicate-operator-graph.pdf", width=7, height=4.5)

# 5 projection by predicate with violin plots, for each operator (fig. 4) ------

predicate_operator_means$VeridicalityGroup = as.factor(
  ifelse(predicate_operator_means$predicate %in% 
           c("know", "discover", "reveal", "see", "be_annoyed"), "F", "NF"))

# NEGATION
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "negation")$predicate, 
  subset(predicate_operator_means, operator == "negation")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% 
                        c("know", "discover", "reveal", "see", "be_annoyed"), pinkk, "black")

# plot
subset(predicate_operator_means, operator == "negation") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "negation"), scale="width",
              aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = pred_colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="none",
        text = element_text(size=12)
  )

ggsave("../graphs/negation-predicate-graph.pdf", width=7, height=3.2)

# QUESTION
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "question")$predicate, 
  subset(predicate_operator_means, operator == "question")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% 
                        c("know", "discover", "reveal", "see", "be_annoyed"), pinkk, "black")

subset(predicate_operator_means, operator == "question") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "question"), scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = pred_colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="none", text = element_text(size=12)
  )

ggsave("../graphs/question-predicate-graph.pdf", width=7, height=3.2)

# MODAL
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "modal")$predicate, 
  subset(predicate_operator_means, operator == "modal")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% 
                        c("know", "discover", "reveal", "see", "be_annoyed"), pinkk, "black")

subset(predicate_operator_means, operator == "modal") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "modal"), scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = pred_colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="none", text = element_text(size=12)
  )

ggsave("../graphs/modal-predicate-graph.pdf", width=7, height=3.2)


### CONDITIONALS
pred_order <- levels(fct_reorder(
  subset(predicate_operator_means, operator == "conditional")$predicate, 
  subset(predicate_operator_means, operator == "conditional")$projection, .fun = "mean"))
textcolors <-  ifelse(pred_order  %in% 
                        c("know", "discover", "reveal", "see", "be_annoyed"), pinkk, "black")

subset(predicate_operator_means, operator == "conditional") %>%
  ggplot(aes(x = fct_reorder(predicate, projection, .fun = "mean"), y = projection)) +
  geom_violin(data = subset(data, operator == "conditional"), scale="width", aes(fill = VeridicalityGroup), alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.3, color="black") +
  geom_point(size=2, aes(fill = VeridicalityGroup, shape = VeridicalityGroup), stroke=.5) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Certainty rating") +
  scale_fill_manual(values = pred_colors, name="Predicate type:", 
                    labels = c("(semi-)factives", "nonfactives")) +
  scale_shape_manual(values = c(21, 24, 25, 22, 23), name = "Predicate type:",
                     labels = c("(semi-)factives", "nonfactives")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = textcolors),
        panel.grid.major.x = element_blank(),
        legend.position="bottom", text = element_text(size=12)
  )

ggsave("../graphs/conditional-predicate-graph.pdf", width=7, height=3.75)



# 6 predicate profiles

negHigh <- c("pretend", "think")
roof1 <- c("acknowledge", "see", "discover", "hear")
roof2 <- c("inform", "know", "be_annoyed")
u1 <- c("establish", "confirm", "demonstrate", "prove")
u2 <- c("reveal", "admit", "confess", "announce")
qLow <- c("be_right", "say", "suggest")

predicate_operator_means <- predicate_operator_means %>% 
  mutate(predGroup = as.factor(case_when(
    predicate %in% negHigh ~ "(a) Negation high",
    predicate %in% qLow ~ "(b) Question low",
    predicate %in% u1 ~ "(c) Negation low",
    predicate %in% u2 ~ "(d) Conditional high",
    predicate %in% roof1 ~ "(e) Modal low",
    predicate %in% roof2 ~ "(f) Question high",
    TRUE ~ NA
  )))

predicate_operator_means <- predicate_operator_means %>%
  mutate(
    predicate = fct_reorder(predicate, projection, .fun = "mean"),
    operator = fct_reorder(operator, projection, .fun = "mean"),
    predGroup = fct_reorder(predGroup, projection, .fun = "mean"),
  )

# "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F"  "#CAB2D6"  "#B15928""#FFFF99"

print(levels(predicate_operator_means$predicate))
predFacets <- as.data.frame(rbind(
  # predicate, color, label x, label y
  # in order of overall mean ratings for predicates, to get the colors right
  c("pretend", "#6A3D9A", 3.85, 0.1),
  c("be_right", "#6A3D9A", 3.5, 0.1),
  c("suggest", "#FF7F00", 1.25, 0.15),
  c("think", "#FF7F00", 4, 0.3),
  c("say", "#E31A1C", 4, 0.47),
  c("prove", "#6A3D9A", 1.4, 0.17),
  c("confirm", "#FF7F00", 3.8, 0.24),
  c("establish", "#E31A1C", 1.3, 0.53),
  c("demonstrate", "#33A02C", 3.35, 0.58),
  c("announce", "#6A3D9A", 3.6, 0.47),
  c("confess", "#FF7F00", 1.5, 0.43),
  c("admit", "#E31A1C", 1, 0.65),
  c("reveal", "#33A02C", 3.9, 0.8),
  c("acknowledge", "#6A3D9A", 3.3, 0.57),
  c("hear", "#FF7F00", 1.1, 0.45),
  c("inform", "#6A3D9A", 1.1, 0.52),
  c("see", "#E31A1C", 3, 0.89),
  c("discover", "#33A02C", 1.27, 0.76),
  c("know", "#FF7F00", 4, 0.88),
  c("be_annoyed", "#E31A1C", 1.7, 0.95)
))
colnames(predFacets) <- c("predicate", "color", "labX", "labY")
predFacets <- predFacets %>%
  mutate(labX = as.numeric(labX), 
         labY = as.numeric(labY),
         predGroup = as.factor(case_when(
           predicate %in% negHigh ~ "(a) Negation high",
           predicate %in% qLow ~ "(b) Question low",
           predicate %in% u1 ~ "(c) Negation low",
           predicate %in% u2 ~ "(d) Conditional high",
           predicate %in% roof1 ~ "(e) Modal low",
           predicate %in% roof2 ~ "(f) Question high",
           TRUE ~ NA
         )))
str(predFacets)

predicate_operator_means %>%
  ggplot(aes(x = operator, y = projection, group = predicate, color = predicate, 
             fill = predicate, label = predicate)) +
  scale_colour_manual(values = predFacets$color) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.2) +
  geom_point(size=1) + 
  geom_line() + 
  geom_text(data = predFacets, aes(x = labX, y = labY), size = 2.8) + 
  facet_wrap(~predGroup, ncol = 6) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0","0.2","0.4","0.6","0.8","1"),
                     name = "Mean certainty rating") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        legend.position = "none") +
  xlab("Operator")

ggsave("../graphs/predicate-profiles.pdf", width=9, height=4)
