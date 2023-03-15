
### data analysis for projection data ----

# set wd to script dir
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data
data <- read.csv("../../data_combined.csv", header = TRUE, sep = ",")


# variables and contrasts for data analysis: ----
## our dependent variable: projective (sliding-scale ratings of speaker certainty that complement clause is true)
data$projective <- as.numeric(data$projective)

# our independent variables
## inference-triggering predicate, coded as "pred"
data$pred <- as.factor(data$verb)
levels(data$pred)
contrasts(data$pred)
### dummy coding with "be annoyed" as baseline
data$pred <- relevel(data$pred, ref = "be_annoyed")
contrasts(data$pred)

## embedding operator, coded as "op"
data$op <- as.factor(data$op)
levels(data$op)
contrasts(data$op)
### op dummy coding with "n" (negation) as baseline
# data$op <- relevel(data$op, ref = "q")
# contrasts(data$op)


# coding random effects as factors
data$workerid <- as.factor(data$workerid)
# data$content <- as.factor(data$content) 
## content should not be a random effect, since contents are uniquely paired w
## preds, which are independent variable

# exploring a couple of models (linear)
## library for linear mixed models
library(lme4)
library(lmerTest)
library(knitr)

# linear model w op as predictor only
# glmm0 <- lmer(projective ~ op + (1 | workerid), data = data)
summary0 <- summary(glmm0)
print(summary0, cor=F, dig=3)

### op dummy coding with "n" (negation) as baseline
data$op <- relevel(data$op, ref = "n")
contrasts(data$op)


# glmm1 <- lmer(projective ~ op * pred + (1 | workerid), data=data)
# save.image("linear-models.RData")
load("linear-models.RData")

summary1 <- summary(glmm1)
print(summary1, cor=F, dig=3)
kable(summary1$coefficients, format = "latex", booktabs = TRUE, dig=2)


### dummy coding with "know" as baseline
data$pred <- relevel(data$pred, ref = "know")
contrasts(data$pred)

# glmm2 <- lmer(projective ~ op * pred + (1 | workerid), data=data)
# save.image("linear-models.RData")
load("linear-models.RData")

summary2 <- summary(glmm2)
print(summary2, cor=F, dig=3)
kable(summary2$coefficients, format = "latex", booktabs = TRUE, dig = 2)



### dummy coding with "discover" as baseline
data$pred <- relevel(data$pred, ref = "discover")
contrasts(data$pred)

# glmm3 <- lmer(projective ~ op * pred + (1 | workerid), data=data)
# save.image("linear-models.RData")
load("linear-models.RData")

summary3 <- summary(glmm3)
print(summary3, cor=F, dig=3)
kable(summary3$coefficients, format = "latex", booktabs = TRUE, dig = 2)

