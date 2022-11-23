
### data analysis for projection data ----

# set wd to script dir
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data
data <- read.csv("../data/data_combined.csv", header = TRUE, sep = ",")


# variables and contrasts for data analysis: ----
## our dependent variable: projective (sliding-scale ratings of speaker certainty that complement clause is true)
data$projective <- numeric(data$projective)

# our independent variables
## inference-triggering predicate, coded as "verb"
data$verb <- as.factor(data$verb)
levels(data$verb)
contrasts(data$verb)

### dummy coding with "suggest" as baseline
data$verb <- relevel(data$verb, ref = "suggest")
contrasts(data$verb)

#### this was our choice for a baseline, because this is the verb with the 
#### smallest differences between by-operator projectivity (ostensibly, based on graph)

## embedding operator, coded as "op"
data$op <- as.factor(data$op)
levels(data$op)
contrasts(data$op)
### dummy coding with "c" (conditionals) as baseline

# coding random effects as factors
data$workerid <- as.factor(data$workerid)
data$content <- as.factor(data$content)


# exploring a couple of models (linear)
## library for linear mixed models
library(lme4)
library(lmertest)

# glmm1 <- lmer(projective ~ op * verb + (op | workerid) + 
#                  (op | content), data=data)

# save.image("linear-models.RData")
load("linear-models.RData")
print(summary(glmm1), cor=F, dig=3)




