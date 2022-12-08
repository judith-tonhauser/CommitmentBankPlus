
### data analysis for projection data ----

# set wd to script dir
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data
data <- read.csv("../data/data_combined.csv", header = TRUE, sep = ",")


# variables and contrasts for data analysis: ----
## our dependent variable: projective (sliding-scale ratings of speaker certainty that complement clause is true)
data$projective <- as.numeric(data$projective)

# our independent variables
## inference-triggering predicate, coded as "verb"
data$verb <- as.factor(data$verb)
levels(data$verb)
contrasts(data$verb)
### dummy coding with "be annoyed" as baseline
data$verb <- relevel(data$verb, ref = "be_annoyed")
contrasts(data$verb)

## embedding operator, coded as "op"
data$op <- as.factor(data$op)
levels(data$op)
contrasts(data$op)
### dummy coding with "n" (negation) as baseline
data$op <- relevel(data$op, ref = "n")
contrasts(data$op)

# coding random effects as factors
data$workerid <- as.factor(data$workerid)
data$content <- as.factor(data$content)


# exploring a couple of models (linear)
## library for linear mixed models
library(lme4)
library(lmerTest)
library(knitr)

# glmm1 <- lmer(projective ~ op * verb + (1 | workerid) +
#                  (1 | content), data=data)

# save.image("linear-models.RData")
load("linear-models.RData")

summary1 <- summary(glmm1)
print(summary1, cor=F, dig=3)
kable(summary1$coefficients, format = "latex", booktabs = TRUE)


### dummy coding with "discover" as baseline
data$verb <- relevel(data$verb, ref = "discover")
contrasts(data$verb)

# glmm2 <- lmer(projective ~ op * verb + (1 | workerid) +
#                  (1 | content), data=data)

# save.image("linear-models.RData")
load("linear-models.RData")

summary2 <- summary(glmm2)
print(summary2, cor=F, dig=3)
kable(summary2$coefficients, format = "latex", booktabs = TRUE)

