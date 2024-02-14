##### ANALYSIS BY OPERATORS #####

# 1 load libraries, data, etc --------------------------------------------------

library(tidyverse)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# analysis
library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)
library(brms)
library(tidybayes)
library(emmeans)


# LOAD DATA
data <- read.csv("../../data_combined.csv", header = TRUE, sep = ",")
data <- data %>% mutate(projection = as.numeric(projective), predicate = as.factor(verb), 
                        operator = as.factor(op), participant = as.factor(workerid), 
                        item = as.factor(content), .keep = "none")
levels(data$operator) <- c("conditional", "modal", "negation", "question")
str(data)


# 2 ANALYSIS -------------------------------------------------------------------
# BAYESIAN MIXED EFFECTS BETA REGRESSION

# Response: certainty ratings (rescale for beta regression)
# Scaling to transform from closed unit interval $[0,1]$ to open unit interval $(0,1)$,
# using method used in Degen & Tonhauser (2022), from Smithson & Verkuilen (2006), for proportional data.
# y' = (y * (n - 1) + 0.5) / n
data$betaresponse <- (data$projection*(nrow(data)-1) + .5)/nrow(data)
data$betaresponse <- as.numeric(data$betaresponse)
summary(data$betaresponse)

# beta regression w response: scaled projection ratings, fixed effect: operator
# random effects: random intercepts for item, participant, slopes for both by operator
# also estimating these effects on beta precision parameter
beta_formula = bf(betaresponse ~ operator + (1 | predicate) + (1 | item) + (1 | participant),
                  phi ~ operator + (1 | predicate) + (1 | item)  + (1 | participant), # beta distribution's precision
                  family = Beta())

# dummy coding for operator fixed effect w baseline modal  
data$operator <- relevel(data$operator, ref = "modal")
contrasts(data$operator)

m.b.operators <- brm(formula = beta_formula,
    family = Beta(),
    data = data,
    cores = 4, iter = 4000, warmup = 700,
    control = list(adapt_delta = .95,max_treedepth=15))

saveRDS(m.b.operators, "../models/beta-model-mixed-operators.rds")

# check the model
m.b.operators <- readRDS("../models/beta-model-mixed-operators.rds")


