# 13_explicitIgnorance
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
library(emmeans)
library(lme4)
library(lmerTest)
library(padr)
library(performance)
library(MuMIn)
library(xtable)

# load cleaned data
d = read_tsv("../data/cd.tsv")

# analysis 1: pairwise comparison of ratings in explicit ignorance context ----

# target data and set reference levels
t = d %>% 
  filter(context == "explicitIgnorance") %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4")
  
# set reference level
t = t %>%
  mutate(expression = fct_relevel(expression, "continue"))
levels(t$expression)

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y'' = (y' ?? (n ??? 1) + 0.5)/n
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
t$betaresponse = (t$response*(nrow(t)-1) + .5)/nrow(t)

prior = get_prior(betaresponse ~ expression + (1|participantID) + (1|cc),family = Beta(),data=t)
prior

betamodel = bf(betaresponse ~ expression + (1|participantID) + (1|cc),
               phi ~ expression + (1|participantID) + (1|cc), # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=t, 
          cores = 4, iter = 3000, warmup = 500,
          control = list(adapt_delta = .95,max_treedepth=15))

summary(m.b)

saveRDS(m.b,file="../models/beta-model-mixed1.rds")

# to load model
m.b = readRDS(file="../models/beta-model-mixed1.rds")

# draws of posterior distributions of estimated marginal means of pairwise differences
pairwise <- m.b %>%
  emmeans(~ expression) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  mean_hdi() %>%
  # create new column of "first" element in contrast
  mutate(first = gsub(" -.*", "", contrast)) %>%
  mutate(second = gsub(".* -", "", contrast)) %>%
  # sort by mean value
  mutate(contrast = fct_reorder(as.factor(contrast),.value))
pairwise

saveRDS(pairwise,file="../models/pairwise1.csv")
pairwise = readRDS(file="../models/pairwise1.csv")
pairwise
names(pairwise)

# select relevant columns for printing
pairwise_reduced = pairwise %>%
  select(c(contrast, .value, .lower, .upper))

#### table of model output in Appendix ----

tableApp1 = print(xtable(pairwise_reduced),
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

write(tableApp1, "../models/latex-tables/tableApp1.tex")
       
#### plot of model output in Appendix ----
theme_set(theme_bw())   
ggplot(pairwise, aes(x = .value, y = contrast)) +
  geom_point() +
  geom_linerange(aes(xmin = .lower, xmax = .upper)) +
  geom_vline(xintercept = 0, color = "red") +
  labs(x = "Average marginal effect of expression", y = NULL) +
  facet_wrap(. ~ first, scales = "free_y", ncol = 5, drop = TRUE)
ggsave("../graphs/comparisons-in-EIC.pdf",height=13,width=20)

#### create Table 1 ----

# select needed columns
tableInput = pairwise %>%
  select(c(contrast, .value, .lower, .upper, first, second)) %>%
  select(-c(contrast))
tableInput$second = trimws(tableInput$second)
tableInput

# create separate dataframes for each expression
predicates = unique(as.character(t$expression))
predicates
predicates <- replace(predicates, 9, "be.annoyed") 
predicates <- replace(predicates, 14, "be.right") 
predicates

tableInput <- as.data.frame(tableInput)
tableInput = tableInput %>%
  mutate(first = recode(first,"be annoyed" = "be.annoyed","be right" = "be.right")) %>%
  mutate(second = recode(second,"be annoyed" = "be.annoyed","be right" = "be.right"))
tableInput

for (p in predicates) {
  assign(paste("data.", p, sep=""), subset(tableInput, tableInput$first == p | tableInput$second == p))
  assign(paste("data.", p, sep=""), get(paste("data.", p, sep="")) %>% mutate(expression = c(p)))
  write(paste("data.",p,sep=""),file=paste("../models/latex-tables/data.",p,sep=""))
}

# change dataframes such that value, lower and upper is consistent by expression in first position

tableData = data.frame(expression = character(), comparisonExpression = character(), value = numeric(), lower = numeric(), upper = numeric())
tableData

for (p in predicates) {
  for (i in 1:nrow(get(paste("data.",p,sep="")))) {
    print(p)
    # define some expressions
    valueOld = get(paste("data.",p,sep=""))[i,]$.value
    lowerOld = get(paste("data.",p,sep=""))[i,]$.lower
    upperOld = get(paste("data.",p,sep=""))[i,]$.upper
    first = get(paste("data.",p,sep=""))[i,]$first
    second = get(paste("data.",p,sep=""))[i,]$second
    expression = get(paste("data.",p,sep=""))[i,]$expression
    # now fill the dataframe
    comparisonExpression = ifelse(expression == first, second, first)
    value = ifelse(expression == first, valueOld, -valueOld)
    lower = ifelse(expression == first, lowerOld, -upperOld)
    upper = ifelse(expression == first, upperOld, -lowerOld)
    tableData = tableData %>%
      add_row(expression = p, comparisonExpression = comparisonExpression, value = value, lower = lower, upper = upper)
  }
}

tableData

# sort dataframe by expression mean naturalness rating in explicit ignorance context
tmp = t %>%
  filter(context == "explicitIgnorance") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
tmp
tmp = tmp %>%
  mutate(expression = recode(expression,"be annoyed" = "be.annoyed","be right" = "be.right"))
tmp
levels(tmp$expression)
tableData$expression = factor(tableData$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
tableData
levels(tableData$expression)
tableData$expression = factor(tableData$expression, ordered = FALSE )
str(tableData$expression)
str(tmp$expression)

tableData = left_join(tableData, tmp)
tableData

tableData$comparisonExpression = factor(tableData$comparisonExpression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)

# sort by mean (first column) and comparisonExpression (second column)
tableData <- tableData %>% arrange(Mean, comparisonExpression)
tableData

# colorcode the cells

tableData$cellColor = ifelse(tableData$lower <= 0 & tableData$upper >= 0, "\\cellcolor{white}",
                       ifelse(tableData$lower < 0 & tableData$upper < 0 & tableData$value <= -1.5, "\\cellcolor{yellow1}",
                              ifelse(tableData$lower < 0 & tableData$upper < 0 & -1.5 < tableData$value & tableData$value <= -0.5, "\\cellcolor{yellow2}",
                                     ifelse(tableData$lower < 0 & tableData$upper < 0 & -.5 < tableData$value & tableData$value <= 0, "\\cellcolor{yellow3}",
                                            ifelse(tableData$lower > 0 & tableData$upper > 0 & tableData$value >= 1.5, "\\cellcolor{purple1}",
                                                   ifelse(tableData$lower > 0 & tableData$upper > 0 & 1.5 > tableData$value & tableData$value > 0.5, "\\cellcolor{purple2}",
                                                          ifelse(tableData$lower > 0 & tableData$upper > 0 & .5 > tableData$value & tableData$value >= 0, "\\cellcolor{purple3}", "error")))))))
#view(tableData)

# select relevant columsn to make the latex table
tableData = tableData %>%
  select(c(expression,comparisonExpression,cellColor))

# spread the data wide
tableData = tableData %>%
  spread(comparisonExpression,cellColor)

# replace NA with gray cells and expressions with color coded versions
tableData = tableData %>% mutate(across(everything(), ~replace_na(.x, "\\cellcolor{black}")))
tableData = tableData %>%
  mutate(expression = recode(expression,"be.annoyed" = "\\color{orange}be annoyed\\color{black}",
                             "know" = "\\color{orange}know\\color{black}",
                             "demonstrate" = "\\color{green}demonstrate\\color{black}",
                             "pretend" = "\\color{green}pretend\\color{black}",
                             "inform" = "\\color{green}inform\\color{black}",
                             "confess" = "\\color{green}confess\\color{black}",
                             "see" = "\\color{orange}see\\color{black}",
                             "acknowledge" = "\\color{green}acknowledge\\color{black}",
                             "discover" = "\\color{orange}discover\\color{black}",
                             "admit" = "\\color{green}admit\\color{black}",
                             "hear" = "\\color{green}hear\\color{black}",
                             "prove" = "\\color{green}prove\\color{black}",
                             "reveal" = "\\color{orange}reveal\\color{black}",
                             "announce" = "\\color{green}announce\\color{black}",
                             "be.right" = "\\color{green}be right\\color{black}",
                             "establish" = "\\color{green}establish\\color{black}",
                             "confirm" = "\\color{green}confirm\\color{black}",
                             "suggest" = "\\color{green}suggest\\color{black}",
                             "think" = "\\color{green}think\\color{black}",
                             "say" = "\\color{green}say\\color{black}"                             
                             ))

#view(tableData)

table1 = print(xtable(tableData),
               only.contents = T,
               include.rownames=FALSE,
               include.colnames=FALSE,
               floating=FALSE,
               hline.after = NULL,
               latex.environments=NULL,
               booktabs=TRUE,
               sanitize.text.function = function(x){x},
               comment = F
)

write(table1, "../models/latex-tables/table1.tex")


###########

# create plots
for (p in predicates) {
  print(paste("pairwise.", p, sep=""))
  assign(paste("plot.pairwise.",p,sep=""), ggplot(get(paste("pairwise.",p,sep="")), aes(x = .value, y = contrast)) +
           geom_point() +
           geom_linerange(aes(xmin = .lower, xmax = .upper)) +
           geom_vline(xintercept = 0, color = "red") +
           xlim(-2.5,2.5) +
           labs(x = p, y = NULL))
  print(paste("plot.pairwise.", p, sep=""))
}


# make first a factor and sort by mean naturalness
tableInput$first <- as.factor(tableInput$first)
tmp = t %>%
  filter(context == "explicitIgnorance") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
tmp

tableInput$first = factor(tableInput$first, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
levels(tableInput$first)

view(tableInput)



# for which pairs does the interval not include 0?
pairwise$significant = ifelse(pairwise$.lower < 0 & pairwise$.upper > 0, "n.d.", 
                              ifelse(pairwise$.lower < 0 & pairwise$.upper < 0, "*", "*"))

tmp = pairwise %>%
  select(c(contrast,significant)) %>%
  filter(significant == "n.d.")
print(n=60, tmp)

# analysis 2: effect of context by expression ----

names(t)

# target data: 20 predicates, all three contexts
t = d %>% 
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(cc != "noCC" )

# set reference levels
t = t %>%
  mutate(context = factor(context, levels = c("explicitIgnorance", "factL", "factH")))

table(t$expression)
table(t$context)

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y'' = (y' ?? (n ??? 1) + 0.5)/n
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
t$betaresponse = (t$response*(nrow(t)-1) + .5)/nrow(t)

# wrapper that computes the model and puts out plot for each predicate

predicates = unique(t$expression) 
predicates
predicates <- replace(predicates, 12, "be.annoyed") 
predicates <- replace(predicates, 15, "be.right") 
predicates

t = t %>%
  mutate(expression = recode(expression,"be annoyed" = "be.annoyed","be right" = "be.right"))
  
theme_set(theme_bw()) 

#prior = get_prior(betaresponse ~ context + (1|cc),family = Beta(),data=t[t$expression == p,])

# models with by-participant slope do not converge, even with 8000 iterations
for (p in predicates) {
  print(p)
  betamodel = bf(betaresponse ~ context + (1|cc),
                 phi ~ context + (1|cc), # beta distribution's precision 
                 family = Beta())
  assign(paste("m.b.", p, sep=""), brm(formula = betamodel,
                                family=Beta(),
                                data=t[t$expression == p,], 
                                cores = 4, iter = 4000, warmup = 500,
                                control = list(adapt_delta = .95,max_treedepth=15)))
  print(paste("m.b.", p, sep=""))
  saveRDS(paste("m.b.",p,sep=""),file=paste("../models/beta-model-mixed-",p,".rds",sep=""))
  assign(paste("pairwise.",p,sep=""), get(paste("m.b.",p,sep="")) %>%
    emmeans(~ context) %>%
    contrast(method = "pairwise") %>%
    gather_emmeans_draws() %>%
    mean_hdi())
}


# create plots
for (p in predicates) {
  print(paste("pairwise.", p, sep=""))
  assign(paste("plot.pairwise.",p,sep=""), ggplot(get(paste("pairwise.",p,sep="")), aes(x = .value, y = contrast)) +
    geom_point() +
    geom_linerange(aes(xmin = .lower, xmax = .upper)) +
    geom_vline(xintercept = 0, color = "red") +
    xlim(-2.5,2.5) +
    labs(x = p, y = NULL))
  print(paste("plot.pairwise.", p, sep=""))
}

pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.value
pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.value

pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.upper - pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.lower
pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.upper - pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.lower

# identify pairwise differences in analysis 2

values = data.frame(predicate = character(), contrast = character(), value = numeric())
values

for (p in predicates) {
  for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
    print(i)
    cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
    value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
    values = values %>%
      add_row(predicate = p, contrast = cntrst, value = value)                    
  }
}
values
ggplot(values, aes(x=value)) +
  geom_histogram() 
ggsave("../graphs/mean-certainty-by-predicateType.pdf",height=4.5,width=7)
    
    
diff2 = data.frame(predicate = character(), contrast = character(), significant = character())
diff2

for (p in predicates) {
  for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
    print(i)
    cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
    lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
    upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
    value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
    # significant = ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0 
    #                      & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper > 0, "n.d.", 
    #                      ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0 
    #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper < 0
    #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value < -1.5, "--", "+"))
    significant = ifelse(lower <= 0 & upper >= 0, "\\cellcolor{white}",
                         ifelse(lower < 0 & upper < 0 & value <= -1.5, "\\cellcolor{yellow1}",
                                ifelse(lower < 0 & upper < 0 & -1.5 < value & value <= -0.5, "\\cellcolor{yellow2}",
                                       ifelse(lower < 0 & upper < 0 & -.5 < value & value <= 0, "\\cellcolor{yellow3}",
                                              ifelse(lower > 0 & upper > 0 & value >= 1.5, "\\cellcolor{purple1}",
                                                     ifelse(lower > 0 & upper > 0 & 1.5 > value & value > 0.5, "\\cellcolor{purple2}",
                                                            ifelse(lower > 0 & upper > 0 & .5 > value & value >= 0, "\\cellcolor{purple3}", "error")))))))
    diff2 = diff2 %>%
      add_row(predicate = p, contrast = cntrst, significant = significant)
  }
}



diff2
diff2 = diff2 %>%
  spread(key = predicate, value = significant)

# order the columns by mean naturalness rating in explicit ignorance context, as in Table 1
tmp = t %>%
  filter(context == "explicitIgnorance") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
tmp
levels(tmp$expression)
col_order <- levels(tmp$expression)
diff2 <- diff2[, col_order]
diff2$contrast = c("higher prior - EIC", "lower prior - EIC", "higher - lower prior")
diff2 = diff2 %>%
  select(contrast,everything())
diff2


table2 = print(xtable(diff2),
               only.contents = T,
               include.rownames=FALSE,
               include.colnames=FALSE,
               floating=FALSE,
               hline.after = NULL,
               latex.environments=NULL,
               booktabs=TRUE,
               sanitize.text.function = function(x){x},
               comment = F
               #hline.after = c(2,2,22,42)
)

write(table2, "../models/latex-tables/table2.tex")

# combine all the plots
library(ggpubr)

combined_plot <- ggarrange(plot.pairwise.acknowledge,
                           plot.pairwise.admit,
                           plot.pairwise.announce,
                           plot.pairwise.be.annoyed,
                           plot.pairwise.be.right,
                           plot.pairwise.confess,
                           plot.pairwise.confirm,
                           plot.pairwise.demonstrate,
                           plot.pairwise.discover,
                           plot.pairwise.establish,
                           plot.pairwise.hear,
                           plot.pairwise.inform,
                           plot.pairwise.know,
                           plot.pairwise.pretend,
                           plot.pairwise.prove,
                           plot.pairwise.reveal,
                           plot.pairwise.say,
                           plot.pairwise.see,
                           plot.pairwise.suggest,
                           plot.pairwise.think,
                           ncol = 2,
                           nrow = 10)
combined_plot
ggsave("../graphs/context-comparisons.pdf",height=10,width=6)

### end loop, rest is left over code


prior = get_prior(betaresponse ~ context + (1|participantID) + (1|cc),family = Beta(),data=t[t$expression == "be annoyed",])
prior

# betamodel = bf(betaresponse ~ context + (1|participantID) + (1|cc),
#                phi ~ context + (1|participantID) + (1|cc), # beta distribution's precision 
#                family = Beta())

betamodel = bf(betaresponse ~ context + (1|cc),
               phi ~ context + (1|cc), # beta distribution's precision 
               family = Beta())

m.b.beannoyed = brm(formula = betamodel,
          family=Beta(),
          data=t[t$expression == "be annoyed",], 
          cores = 4, iter = 3000, warmup = 500,
          control = list(adapt_delta = .95,max_treedepth=15))

summary(m.b.beannoyed)

saveRDS(m.b.beannoyed,file="../models/beta-model-mixed-beannoyed.rds")

# to load model
m.b.beannoyed = readRDS(file="../models/beta-model-mixed-beannoyed.rds")

# draws of posterior distributions of estimated marginal means of pairwise differences
pairwise <- m.b.beannoyed %>%
  emmeans(~ context) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  mean_hdi() %>%
  # sort by mean value
  mutate(contrast = fct_reorder(as.factor(contrast),.value))
pairwise

assign(paste("m.b.", p, sep=""), ggplot(pairwise, aes(x = .value, y = contrast)) +
  geom_point() +
  geom_linerange(aes(xmin = .lower, xmax = .upper)) +
  geom_vline(xintercept = 0, color = "red") +
  labs(x = "be annoyed", y = NULL))


ggsave("../graphs/test.pdf",height=3,width=3)



# OLD STUFF WITH frequentist models ----

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
library(emmeans)
library(lme4)
library(lmerTest)
library(padr)
library(performance)
library(MuMIn)

# load cleaned data
d = read_tsv("../data/cd.tsv")

# target data
t = d %>%
  filter(context == "explicitIgnorance") %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") #%>%
# filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
#          expression != "stop" & expression != "continue")
table(t$expression)
table(t$context)

# create item
t$item = paste(t$expression,t$cc)
table(t$item)

# reorder expressions by mean
means = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
means
levels(means$expression)

t$expression <- factor(t$expression, levels = unique(levels(means$expression)))
levels(t$expression) 

# set reference levels
t = t %>% 
  mutate(expression = fct_relevel(expression, "continue")) #%>% 
#mutate(context = fct_relevel(context, "explicitIgnorance"))

# model (models with slopes do not converge)
m = lmer(response ~ expression + (1|participantID) + (1|cc), data = t, REML=F)
summary(m)

# pairwise comparison
comparison = lsmeans(m, pairwise~expression,adjust="tukey",lmerTest.limit = 7400,pbkrtest.limit = 7400)
options(max.print=2000)
comparison

# > comparison
# $lsmeans
# expression  lsmean     SE  df lower.CL upper.CL
# be annoyed   0.359 0.0195 361    0.321    0.397
# know         0.494 0.0195 361    0.456    0.533
# demonstrate  0.518 0.0195 361    0.480    0.556
# pretend      0.525 0.0195 361    0.487    0.563
# inform       0.557 0.0195 361    0.519    0.596
# confess      0.560 0.0195 361    0.522    0.599
# acknowledge  0.583 0.0195 361    0.544    0.621
# see          0.587 0.0195 361    0.548    0.625
# discover     0.589 0.0195 361    0.551    0.627
# prove        0.597 0.0195 361    0.558    0.635
# admit        0.597 0.0195 361    0.558    0.635
# reveal       0.618 0.0195 361    0.579    0.656
# hear         0.622 0.0195 361    0.584    0.661
# establish    0.650 0.0195 361    0.611    0.688
# announce     0.658 0.0195 361    0.620    0.696
# confirm      0.668 0.0195 361    0.630    0.706
# be right     0.690 0.0195 361    0.652    0.728
# suggest      0.727 0.0195 361    0.689    0.766
# think        0.737 0.0195 361    0.699    0.775
# say          0.765 0.0195 361    0.726    0.803
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                   estimate     SE   df t.ratio p.value
# be annoyed - know         -0.135317 0.0230 7034  -5.894  <.0001
# be annoyed - demonstrate  -0.158882 0.0230 7034  -6.921  <.0001
# be annoyed - pretend      -0.166253 0.0230 7034  -7.241  <.0001
# be annoyed - inform       -0.198405 0.0230 7035  -8.640  <.0001
# be annoyed - confess      -0.201397 0.0230 7033  -8.774  <.0001
# be annoyed - acknowledge  -0.223810 0.0230 7034  -9.749  <.0001
# be annoyed - see          -0.227754 0.0230 7033  -9.923  <.0001
# be annoyed - discover     -0.230093 0.0230 7037 -10.015  <.0001
# be annoyed - prove        -0.237839 0.0230 7034 -10.360  <.0001
# be annoyed - admit        -0.237587 0.0230 7033 -10.351  <.0001
# be annoyed - reveal       -0.258755 0.0230 7034 -11.272  <.0001
# be annoyed - hear         -0.263466 0.0230 7033 -11.479  <.0001
# be annoyed - establish    -0.290712 0.0230 7036 -12.655  <.0001
# be annoyed - announce     -0.299229 0.0230 7035 -13.031  <.0001
# be annoyed - confirm      -0.309145 0.0230 7033 -13.468  <.0001
# be annoyed - be right     -0.331165 0.0230 7034 -14.424  <.0001
# be annoyed - suggest      -0.368279 0.0230 7034 -16.043  <.0001
# be annoyed - think        -0.378270 0.0230 7033 -16.481  <.0001
# be annoyed - say          -0.405801 0.0230 7036 -17.667  <.0001
# know - demonstrate        -0.023565 0.0230 7033  -1.027  1.0000
# know - pretend            -0.030936 0.0230 7034  -1.347  0.9988
# know - inform             -0.063088 0.0230 7035  -2.748  0.3931
# know - confess            -0.066080 0.0230 7035  -2.878  0.3039
# know - acknowledge        -0.088492 0.0230 7035  -3.854  0.0170
# know - see                -0.092437 0.0230 7036  -4.024  0.0089
# know - discover           -0.094776 0.0230 7036  -4.126  0.0059
# know - prove              -0.102521 0.0230 7036  -4.464  0.0014
# know - admit              -0.102270 0.0230 7036  -4.452  0.0015
# know - reveal             -0.123438 0.0230 7034  -5.376  <.0001
# know - hear               -0.128148 0.0230 7034  -5.582  <.0001
# know - establish          -0.155395 0.0230 7035  -6.768  <.0001
# know - announce           -0.163912 0.0229 7032  -7.143  <.0001
# know - confirm            -0.173828 0.0230 7035  -7.570  <.0001
# know - be right           -0.195848 0.0230 7033  -8.533  <.0001
# know - suggest            -0.232962 0.0230 7035 -10.145  <.0001
# know - think              -0.242953 0.0230 7036 -10.577  <.0001
# know - say                -0.270484 0.0230 7035 -11.778  <.0001
# demonstrate - pretend     -0.007371 0.0230 7034  -0.321  1.0000
# demonstrate - inform      -0.039523 0.0230 7036  -1.721  0.9765
# demonstrate - confess     -0.042515 0.0230 7033  -1.852  0.9511
# demonstrate - acknowledge -0.064928 0.0230 7033  -2.829  0.3360
# demonstrate - see         -0.068872 0.0230 7037  -2.998  0.2327
# demonstrate - discover    -0.071211 0.0230 7034  -3.102  0.1810
# demonstrate - prove       -0.078956 0.0230 7035  -3.439  0.0701
# demonstrate - admit       -0.078705 0.0230 7035  -3.428  0.0725
# demonstrate - reveal      -0.099873 0.0230 7034  -4.350  0.0023
# demonstrate - hear        -0.104583 0.0230 7035  -4.554  0.0009
# demonstrate - establish   -0.131830 0.0230 7033  -5.744  <.0001
# demonstrate - announce    -0.140347 0.0230 7035  -6.112  <.0001
# demonstrate - confirm     -0.150263 0.0230 7033  -6.546  <.0001
# demonstrate - be right    -0.172283 0.0230 7033  -7.506  <.0001
# demonstrate - suggest     -0.209397 0.0230 7034  -9.121  <.0001
# demonstrate - think       -0.219388 0.0230 7034  -9.555  <.0001
# demonstrate - say         -0.246919 0.0230 7033 -10.758  <.0001
# pretend - inform          -0.032152 0.0230 7036  -1.400  0.9979
# pretend - confess         -0.035144 0.0230 7035  -1.531  0.9937
# pretend - acknowledge     -0.057556 0.0230 7035  -2.507  0.5786
# pretend - see             -0.061501 0.0230 7035  -2.678  0.4448
# pretend - discover        -0.063840 0.0230 7038  -2.778  0.3712
# pretend - prove           -0.071585 0.0230 7038  -3.115  0.1750
# pretend - admit           -0.071334 0.0230 7036  -3.106  0.1792
# pretend - reveal          -0.092501 0.0230 7036  -4.028  0.0088
# pretend - hear            -0.097212 0.0230 7034  -4.234  0.0038
# pretend - establish       -0.124459 0.0230 7037  -5.417  <.0001
# pretend - announce        -0.132976 0.0230 7036  -5.789  <.0001
# pretend - confirm         -0.142892 0.0230 7035  -6.222  <.0001
# pretend - be right        -0.164912 0.0230 7036  -7.180  <.0001
# pretend - suggest         -0.202026 0.0230 7035  -8.798  <.0001
# pretend - think           -0.212017 0.0230 7035  -9.233  <.0001
# pretend - say             -0.239548 0.0230 7035 -10.432  <.0001
# inform - confess          -0.002992 0.0230 7033  -0.130  1.0000
# inform - acknowledge      -0.025404 0.0230 7037  -1.106  0.9999
# inform - see              -0.029349 0.0230 7035  -1.278  0.9994
# inform - discover         -0.031688 0.0230 7036  -1.380  0.9983
# inform - prove            -0.039433 0.0230 7036  -1.717  0.9771
# inform - admit            -0.039182 0.0230 7036  -1.706  0.9786
# inform - reveal           -0.060350 0.0229 7032  -2.630  0.4818
# inform - hear             -0.065060 0.0230 7034  -2.834  0.3328
# inform - establish        -0.092307 0.0230 7037  -4.017  0.0091
# inform - announce         -0.100824 0.0230 7035  -4.391  0.0019
# inform - confirm          -0.110740 0.0230 7035  -4.823  0.0003
# inform - be right         -0.132760 0.0230 7034  -5.782  <.0001
# inform - suggest          -0.169874 0.0230 7034  -7.400  <.0001
# inform - think            -0.179865 0.0230 7035  -7.833  <.0001
# inform - say              -0.207396 0.0230 7037  -9.026  <.0001
# confess - acknowledge     -0.022413 0.0230 7035  -0.976  1.0000
# confess - see             -0.026357 0.0230 7034  -1.148  0.9999
# confess - discover        -0.028696 0.0230 7034  -1.250  0.9996
# confess - prove           -0.036441 0.0230 7034  -1.587  0.9904
# confess - admit           -0.036190 0.0230 7034  -1.576  0.9911
# confess - reveal          -0.057358 0.0230 7032  -2.499  0.5845
# confess - hear            -0.062068 0.0230 7034  -2.704  0.4254
# confess - establish       -0.089315 0.0230 7035  -3.889  0.0149
# confess - announce        -0.097832 0.0230 7035  -4.260  0.0034
# confess - confirm         -0.107748 0.0229 7032  -4.695  0.0005
# confess - be right        -0.129768 0.0230 7034  -5.652  <.0001
# confess - suggest         -0.166882 0.0230 7033  -7.270  <.0001
# confess - think           -0.176873 0.0230 7033  -7.706  <.0001
# confess - say             -0.204404 0.0230 7035  -8.901  <.0001
# acknowledge - see         -0.003945 0.0230 7035  -0.172  1.0000
# acknowledge - discover    -0.006284 0.0230 7036  -0.274  1.0000
# acknowledge - prove       -0.014029 0.0230 7036  -0.611  1.0000
# acknowledge - admit       -0.013777 0.0230 7034  -0.600  1.0000
# acknowledge - reveal      -0.034945 0.0230 7034  -1.522  0.9941
# acknowledge - hear        -0.039656 0.0230 7036  -1.727  0.9757
# acknowledge - establish   -0.066903 0.0230 7033  -2.915  0.2808
# acknowledge - announce    -0.075420 0.0230 7036  -3.284  0.1111
# acknowledge - confirm     -0.085335 0.0230 7033  -3.718  0.0278
# acknowledge - be right    -0.107356 0.0230 7034  -4.676  0.0005
# acknowledge - suggest     -0.144470 0.0230 7033  -6.295  <.0001
# acknowledge - think       -0.154461 0.0230 7033  -6.729  <.0001
# acknowledge - say         -0.181992 0.0230 7033  -7.930  <.0001
# see - discover            -0.002339 0.0230 7040  -0.102  1.0000
# see - prove               -0.010084 0.0230 7034  -0.439  1.0000
# see - admit               -0.009833 0.0230 7034  -0.428  1.0000
# see - reveal              -0.031000 0.0230 7033  -1.350  0.9987
# see - hear                -0.035711 0.0230 7033  -1.556  0.9924
# see - establish           -0.062958 0.0230 7038  -2.740  0.3988
# see - announce            -0.071475 0.0230 7036  -3.112  0.1762
# see - confirm             -0.081391 0.0230 7034  -3.545  0.0501
# see - be right            -0.103411 0.0230 7035  -4.503  0.0012
# see - suggest             -0.140525 0.0230 7033  -6.122  <.0001
# see - think               -0.150516 0.0230 7033  -6.557  <.0001
# see - say                 -0.178047 0.0230 7036  -7.752  <.0001
# discover - prove          -0.007745 0.0230 7037  -0.337  1.0000
# discover - admit          -0.007494 0.0230 7038  -0.326  1.0000
# discover - reveal         -0.028662 0.0230 7036  -1.248  0.9996
# discover - hear           -0.033372 0.0230 7039  -1.452  0.9967
# discover - establish      -0.060619 0.0230 7037  -2.639  0.4750
# discover - announce       -0.069136 0.0230 7037  -3.009  0.2268
# discover - confirm        -0.079052 0.0230 7035  -3.442  0.0694
# discover - be right       -0.101072 0.0230 7037  -4.400  0.0019
# discover - suggest        -0.138186 0.0230 7037  -6.015  <.0001
# discover - think          -0.148177 0.0230 7037  -6.450  <.0001
# discover - say            -0.175708 0.0230 7037  -7.648  <.0001
# prove - admit              0.000251 0.0230 7035   0.011  1.0000
# prove - reveal            -0.020916 0.0230 7034  -0.911  1.0000
# prove - hear              -0.025627 0.0230 7034  -1.116  0.9999
# prove - establish         -0.052874 0.0230 7036  -2.302  0.7333
# prove - announce          -0.061391 0.0230 7036  -2.673  0.4490
# prove - confirm           -0.071306 0.0230 7034  -3.106  0.1790
# prove - be right          -0.093327 0.0230 7035  -4.064  0.0076
# prove - suggest           -0.130441 0.0230 7035  -5.681  <.0001
# prove - think             -0.140432 0.0230 7034  -6.117  <.0001
# prove - say               -0.167963 0.0230 7036  -7.312  <.0001
# admit - reveal            -0.021168 0.0230 7035  -0.922  1.0000
# admit - hear              -0.025878 0.0230 7034  -1.127  0.9999
# admit - establish         -0.053125 0.0230 7035  -2.314  0.7252
# admit - announce          -0.061643 0.0230 7036  -2.684  0.4402
# admit - confirm           -0.071558 0.0229 7032  -3.118  0.1735
# admit - be right          -0.093578 0.0230 7033  -4.077  0.0072
# admit - suggest           -0.130692 0.0230 7034  -5.693  <.0001
# admit - think             -0.140683 0.0229 7032  -6.130  <.0001
# admit - say               -0.168214 0.0230 7034  -7.326  <.0001
# reveal - hear             -0.004711 0.0230 7034  -0.205  1.0000
# reveal - establish        -0.031957 0.0230 7035  -1.392  0.9981
# reveal - announce         -0.040475 0.0230 7034  -1.763  0.9698
# reveal - confirm          -0.050390 0.0230 7033  -2.195  0.8037
# reveal - be right         -0.072410 0.0230 7034  -3.154  0.1581
# reveal - suggest          -0.109524 0.0229 7032  -4.773  0.0003
# reveal - think            -0.119515 0.0230 7033  -5.207  <.0001
# reveal - say              -0.147046 0.0230 7035  -6.403  <.0001
# hear - establish          -0.027247 0.0230 7037  -1.186  0.9998
# hear - announce           -0.035764 0.0230 7036  -1.557  0.9923
# hear - confirm            -0.045680 0.0230 7034  -1.990  0.9072
# hear - be right           -0.067700 0.0230 7034  -2.949  0.2606
# hear - suggest            -0.104814 0.0230 7035  -4.564  0.0009
# hear - think              -0.114805 0.0230 7033  -5.002  0.0001
# hear - say                -0.142336 0.0230 7036  -6.197  <.0001
# establish - announce      -0.008517 0.0230 7036  -0.371  1.0000
# establish - confirm       -0.018433 0.0230 7033  -0.803  1.0000
# establish - be right      -0.040453 0.0230 7035  -1.762  0.9700
# establish - suggest       -0.077567 0.0230 7035  -3.378  0.0845
# establish - think         -0.087558 0.0230 7036  -3.812  0.0198
# establish - say           -0.115089 0.0230 7035  -5.012  0.0001
# announce - confirm        -0.009916 0.0230 7035  -0.432  1.0000
# announce - be right       -0.031936 0.0230 7033  -1.391  0.9981
# announce - suggest        -0.069050 0.0230 7034  -3.008  0.2276
# announce - think          -0.079041 0.0230 7037  -3.440  0.0698
# announce - say            -0.106572 0.0230 7035  -4.641  0.0006
# confirm - be right        -0.022020 0.0230 7034  -0.959  1.0000
# confirm - suggest         -0.059134 0.0230 7033  -2.576  0.5236
# confirm - think           -0.069125 0.0230 7033  -3.012  0.2253
# confirm - say             -0.096656 0.0230 7035  -4.209  0.0042
# be right - suggest        -0.037114 0.0230 7034  -1.617  0.9881
# be right - think          -0.047105 0.0230 7035  -2.051  0.8810
# be right - say            -0.074636 0.0229 7032  -3.252  0.1214
# suggest - think           -0.009991 0.0230 7034  -0.435  1.0000
# suggest - say             -0.037522 0.0230 7035  -1.634  0.9866
# think - say               -0.027531 0.0230 7035  -1.199  0.9998
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 20 estimates 

# predict response from expression and prior (as in Degen & Tonhauser 2021 Exp1) ----

# load cleaned data
d = read_tsv("../data/cd.tsv")

# target data without explicit ignorance context
t = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  filter(context != "explicitIgnorance")
table(t$expression)
table(t$context)

# create item
t$item = paste(t$expression,t$cc)
table(t$item)

# set reference levels
t = t %>% 
  mutate(expression = fct_relevel(expression, "be annoyed")) %>% 
  mutate(context = fct_relevel(context, "factL"))

# center the relevant variables (context)
t = t %>% 
  mutate(c_context = as.numeric(as.factor(context)) - mean(as.numeric(as.factor(context))))

# does high/low prob fact predict naturalness ratings?
m = lmer(response ~ c_context*expression + (1+c_context|cc) + (1+c_context|participantID), data=t, REML=F)
summary(m)

