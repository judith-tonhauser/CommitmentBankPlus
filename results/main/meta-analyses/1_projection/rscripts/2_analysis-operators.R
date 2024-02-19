# SuB paper
# by-operator analysis (section 2.2.1)

# load libraries
library(tidyverse)
#library(rstan)
#options(mc.cores=parallel::detectCores())
#rstan_options(auto_write=TRUE)
library(brms)
library(tidybayes)
library(emmeans)
library(xtable)

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data 
data <- read.csv("../../data_combined.csv", header = TRUE, sep = ",")
data <- data %>% mutate(projection = as.numeric(projective), predicate = as.factor(verb), 
                        operator = as.factor(op), participant = as.factor(workerid), 
                        item = as.factor(content), .keep = "none")
levels(data$operator) <- c("conditional", "modal", "negation", "question")
str(data)

# Bayesian beta regression ----

# set the reference level for operator
data = data %>%
  mutate(operator = fct_relevel(operator, "modal"))
levels(data$operator)

# rescale certainty ratings for beta regression
# Scaling to transform from closed unit interval $[0,1]$ to open unit interval $(0,1)$,
# using method used in Smithson & Verkuilen (2006)
# y' = (y * (n - 1) + 0.5) / n
data$betaresponse <- (data$projection*(nrow(data)-1) + .5)/nrow(data)
summary(data$betaresponse)

# beta regression w response: scaled projection ratings, fixed effect: operator
# random effects: random intercepts for item, participant, slopes for both by operator
# also estimating these effects on beta precision parameter
beta_formula = bf(betaresponse ~ operator + (1 + operator | predicate) + (1 | item),
                  phi ~ operator  + (1 + operator | predicate) + (1 | item), # beta distribution's precision
                  family = Beta())

m.b.operators <- brm(formula = beta_formula,
    family = Beta(),
    data = data,
    cores = 4, chains = 4, iter = 8000, warmup = 700,
    control = list(adapt_delta = .95, max_treedepth=15))

saveRDS(m.b.operators, "../models/beta-model-mixed-operators2.rds")
summary(m.b.operators)

# run posterior predictive checks
p1 <- pp_check(m.b.operators, type = "dens_overlay_grouped", group = "operator", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# pairwise comparison of operators ----

# read the model
m.b.operators <- readRDS("../models/beta-model-mixed-operators2.rds")

# draws of posterior distributions of estimated marginal means of pairwise differences
pairwise <- m.b.operators %>%
  emmeans(~ operator) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  mean_hdi()
pairwise

# save the pairwise comparison
write_csv(pairwise,file="../models/pairwise-operator-comparison.csv")

# select relevant columns for printing
pairwise_reduced = pairwise %>%
  select(c(contrast, .value, .lower, .upper))
pairwise_reduced

#### full model output for online supplement ----

table1 = print(xtable(pairwise_reduced),
                  #only.contents = T,
                  include.rownames=FALSE,
                  include.colnames=TRUE,
                  #tabular.environment="longtable",
                  floating=FALSE,
                  hline.after = NULL,
                  latex.environments=NULL,
                  booktabs=TRUE,
                  sanitize.text.function = function(x){x},
                  comment = F
)

# write the table, print in latex document in supplement
write(table1, "../models/table1.tex")

# Latex output for the Bayesian model in supplement ----

# model with centered block effect
printd = as.data.frame(summary(m.b.operators)$fixed)

# create column name for first column
printd <- rownames_to_column(printd, var = "tmp")

printd = printd %>%
  mutate(tmp = recode(tmp, "Intercept" = "Intercept (modal)")) %>%
  #mutate(tmp = gsub("short_trigger", "predicate_", tmp)) %>%
  rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
  select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
  mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
  mutate("95% CI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
  #rename("beta" = "Estimate") %>%
  rename("95% CI" = "95% CI") %>%
  select(-c(lower,upper))
printd

# change column names
colnames(printd) <- c("","Estimate","95% CI")

analysis1.model = print(xtable(printd),
                   include.rownames=FALSE,
                   include.colnames=TRUE,
                   tabular.environment="longtable",
                   floating=FALSE,
                   latex.environments=NULL,
                   booktabs=FALSE)
write(analysis1.model, "../models/analysis1.model")


