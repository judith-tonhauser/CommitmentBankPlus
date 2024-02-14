##### Graphs for certainty ratings #####

# 1 load libraries, data, etc --------------------------------------------------

library(tidyverse)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# summary + graphing
library(ggplot2)
source('../../../../helpers.R')
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#D55E00", 
                "#CC79A7", "#000000")
graphcolor <- "#0072B2"
pinkk <- "#CC79A7"
pred_colors <- c(pinkk, "black")
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

ggsave("../graphs/certainty-operator.pdf", width=3.3, height=5)


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

ggsave(f="../figures/predicate-graph-1.pdf", width=7, height=4.5)


# 4 projection by predicate and operator (Figure 3) ----------------------------

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

ggsave("../figures/predicate-operator-graph-1.pdf", width=7, height=4.5)

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
        legend.position="bottom",
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

ggsave("../graphs/question-predicate-graph-1.pdf", width=7, height=3.2)

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

ggsave("../graphs/modal-predicate-graph.pdf", width=7, height=3.75)
