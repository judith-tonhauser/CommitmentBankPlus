##### ANALYSIS BY OPERATOR FOR EACH PREDICATE #####

# 1 load libraries, data, etc --------------------------------------------------

library(tidyverse)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
library(xtable)

# analysis
library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)
library(brms)
library(tidybayes)
library(emmeans)

# summary + graphing
library(ggplot2)
library(ggh4x)
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

# 2 ANALYSIS -------------------------------------------------------------------
# USING BAYESIAN MIXED EFFECTS BETA REGRESSION

# Response: certainty ratings (rescale for beta regression)
# Scaling to transform from closed unit interval $[0,1]$ to open unit interval $(0,1)$,
# using method used in Degen & Tonhauser (2022), from Smithson & Verkuilen (2006), for proportional data.
# y' = (y * (n - 1) + 0.5) / n
data$betaresponse <- (data$projection*(nrow(data)-1) + .5)/nrow(data)
data$betaresponse <- as.numeric(data$betaresponse)
summary(data$betaresponse)

# beta regression w response: scaled projection ratings, fixed effect: operator
# random effects: item (embedded clause) intercepts and item slopes by operator
# (no participant ranodm effects, bc each part saw each item only once)
# also estimating these effects on beta precision parameter
beta_formula = bf(betaresponse ~ operator + (1 + operator | item),
                  phi ~ operator + (1 + operator | item), # beta distribution's precision
                  family = Beta())

# dummy coding for operator fixed effect w baseline modal  
data$operator <- relevel(data$operator, ref = "modal")
contrasts(data$operator)

# fit model for each predicate subset 
predicates <- levels(data$predicate)

# # fit and save models
# for (current_pred in predicates) {
#   print(current_pred)
#   pred_subset = data %>%
#     subset(predicate == current_pred)
#   assign(paste("m.b.", current_pred, sep=""),
#          brm(formula = beta_formula,
#              family = Beta(),
#              data = pred_subset,
#              cores = 4, iter = 4000, warmup = 700,
#              control = list(adapt_delta = .95,max_treedepth=15)))
#   print(paste("m.b.", current_pred, sep=""))
#   saveRDS(eval(parse(text = paste("m.b.",current_pred,sep=""))),
#           file=paste("../models/predicates/beta-model-mixed-", current_pred,".rds",sep=""))
# }

# load models
for (current_pred in predicates) {
  current_name = paste("m.b.", current_pred, sep="")
  assign(current_name, readRDS(paste("../models/predicates/beta-model-mixed-", current_pred,".rds",sep="")))
  summary(current_name)
}

# print fixed effects for each model
for (current_pred in predicates) {
  print(current_pred)
  print(kable(fixef(eval(parse(text = paste("m.b.", current_pred, sep="")))), "latex", booktabs = TRUE))
}

# 3 GET PAIRWISE DIFFERENCES ---------------------------------------------------

for (p in predicates) {
  assign(paste("pairwise.",p,sep=""), get(paste("m.b.",p,sep="")) %>%
           emmeans(~ operator) %>%
           contrast(method = "pairwise") %>%
           gather_emmeans_draws() %>%
           mean_hdi())
}

# create + save plots
for (p in predicates) {
  ggsave(
    paste("../graphs/contrasts/", p, "_contrasts.pdf", sep = ""), 
    ggplot(get(paste("pairwise.",p,sep="")), aes(x = .value, y = contrast)) +
      geom_point() +
      geom_linerange(aes(xmin = .lower, xmax = .upper)) +
      geom_vline(xintercept = 0, color = "red") +
      xlim(-2.5,2.5) +
      labs(x = p, y = NULL),
    width=3.3, height=5
  )
}

# 4 plot results as part of figure 5 -------------------------------------------

#### create data for presenting results as part of Figure 4 ----

# there are 6 lines in each facet (that is, for each predicate)
# one line for each combination of two operators (x = operator)
# geom_segment(aes(x=1, xend=2 ,y=-.05, yend=-.05), linetype = "solid")
# x, xend, y, yend, linetype: depend on predicate and contrast
# create data called "contrasts"
# with columns "predicate" and "contrast" (for binding and faceting with nat.means)
# and with columns "x", "xend", "y", "yend", and "linetype"

# means and confidence intervals for projection rating by predicate / operator
predicate_operator_means <- data %>% group_by(predicate, operator) %>%
  summarize(Mean = mean(projection), CILow = ci.low(projection), 
            CIHigh = ci.high(projection)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         predicate = fct_reorder(as.factor(predicate),Mean)) %>% ungroup()
predicate_operator_means
predicate_operator_means <- predicate_operator_means %>%
  mutate(projection = Mean, .keep = "unused") %>%
  mutate(predicate = fct_reorder(predicate, projection, .fun = "mean"),
         operator =  fct_reorder(operator, projection, .fun = "mean"))

levels(predicate_operator_means$operator)
levels(predicate_operator_means$predicate)

pred_order <- levels(predicate_operator_means$predicate)
textcolors <-  ifelse(pred_order  %in% c("know", "discover", "reveal", "see", "be_annoyed"), pinkk, "white")

data <- data %>% mutate(operator = fct_reorder(operator, projection, .fun = "mean"))
levels(data$operator)


# identify pairwise differences
contrasts = data.frame(predicate = character(), contrast = character(), mean = numeric(), lower = numeric(), upper = numeric())
contrasts

for (p in predicates) {
  for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
    # print(i)
    cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
    value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
    lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
    upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
    contrasts = contrasts %>%
      add_row(predicate = p, contrast = cntrst, mean = value, lower = lower, upper = upper)                    
  }
}
View(contrasts)


# Get table with all contrasts for supplement
print(xtable(contrasts),
                  #only.contents = T,
                  include.rownames=FALSE,
                  include.colnames=TRUE,
                  tabular.environment="longtable",
                  floating=FALSE,
                  hline.after = NULL,
                  latex.environments=NULL,
                  booktabs=TRUE,
                  sanitize.text.function = function(x){x},
                  comment = F
)


# show in figure
# linetypes
# 0 blank
# 1 solid
# 2 dashed
# 3 dotted

# changed the cutoff for solid lines to 1 instead of 1.5, because otherwise there would be no solid lines
contrasts <- contrasts %>% mutate(
  linetype = case_when(
    lower <= 0 & upper >= 0 ~ "none",
    abs(mean) >= 1 ~ "solid",
    abs(mean) > 0.5 ~ "longdash",
    abs(mean) > 0 ~ "dotted",
    TRUE ~ NA
  )
)

table(contrasts$linetype)

contrasts <- contrasts %>% mutate(
  xstart = case_when(
    contrast == "conditional - negation" ~ 2,
    contrast == "conditional - question" ~ 3,
    contrast == "modal - conditional" ~ 1,
    contrast == "modal - negation" ~ 1,
    contrast == "modal - question" ~ 1,
    contrast == "negation - question" ~ 2,
    TRUE ~ 666
  ),
  xend = case_when(
    contrast == "conditional - negation" ~ 4,
    contrast == "conditional - question" ~ 4,
    contrast == "modal - conditional" ~ 4,
    contrast == "modal - negation" ~ 2,
    contrast == "modal - question" ~ 3,
    contrast == "negation - question" ~ 3,
    TRUE ~ 666
  ),
  ystart = case_when(
    contrast == "conditional - negation" ~ .2,
    contrast == "conditional - question" ~ .05,
    contrast == "modal - conditional" ~ .25,
    contrast == "modal - negation" ~ .05,
    contrast == "modal - question" ~ .15,
    contrast == "negation - question" ~ .1,
    TRUE ~ 666
  ),
  yend = ystart
)
str(contrasts)

# join results of statistical analysis with predicate operator means
contrasts <- contrasts %>%
  mutate(ystart = -ystart, yend = -yend, 
         predicate = as.factor(predicate),
         linetype = as.factor(linetype)) %>%
  filter(linetype != "none") 

strip <- strip_themed(background_x = elem_list_rect(fill = textcolors))

# plot
predicate_operator_means %>%
  ggplot() +
  geom_point(aes(x = operator, y = projection), 
             shape = 21, stroke = .5, size = .5, color = "black") +
  geom_errorbar(aes(x = operator, y = projection, ymin = YMin, ymax = YMax), 
                width = 0.5, color = "black", alpha = .6) +
  geom_violin(data = data, aes(x = operator, y = projection, color = operator), 
              scale="width", alpha = .4) +
  geom_segment(data = contrasts, 
               aes(x = xstart, xend = xend, y = ystart, yend=yend, linetype = linetype)) +
  facet_wrap2(~ predicate, nrow = 4, strip = strip) +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank(),
        axis.title.x=element_blank(), 
        legend.position = "bottom") +
  scale_y_continuous(limits = c(-.25,1),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0",".2",".4",".6",".8","1"),
                     name = "Certainty rating") +
  scale_linetype_identity() +
  scale_colour_manual(values = cbbPalette, name = "Operator") +
  theme(legend.position="top") +
  theme(panel.grid.minor = element_blank()) +
  # theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "black")) +
  guides(linetype = "none")
  
ggsave("../graphs/analysis.pdf", width=10, height=10)


