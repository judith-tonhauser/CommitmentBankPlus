# 13_explicitIgnorance
# analysis.R
# analysis 1: section 2.2.1
# analysis 2: section 2.2.2
# this script also creates the visual outputs of the analyses reported in the paper

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

saveRDS(m.b,file="../models/analysis1/beta-model-mixed1.rds")

# to load model
m.b = readRDS(file="../models/analysis1/beta-model-mixed1.rds")

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

write_csv(pairwise,file="../models/analysis1/pairwise1.csv")
pairwise = read_csv(file="../models/analysis1/pairwise1.csv")
pairwise

# which differences are observed?
summary(pairwise$.value)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.6943 -1.0429 -0.2822 -0.2895  0.3865  2.7232 

# select relevant columns for printing
pairwise_reduced = pairwise %>%
  select(c(contrast, .value, .lower, .upper))
pairwise_reduced

#### full model output  ----

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

write(tableApp1, "../models/analysis1/fullModelOutput/analysis1.tex")
       
# #### plot of model output in Appendix
# theme_set(theme_bw())   
# ggplot(pairwise, aes(x = .value, y = contrast)) +
#   geom_point() +
#   geom_linerange(aes(xmin = .lower, xmax = .upper)) +
#   geom_vline(xintercept = 0, color = "red") +
#   labs(x = "Average marginal effect of expression", y = NULL) +
#   facet_wrap(. ~ first, scales = "free_y", ncol = 5, drop = TRUE)
# ggsave("../graphs/comparisons-in-EIC.pdf",height=13,width=20)

#### create latex input for Table 1 ----

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
  write(paste("data.",p,sep=""),file=paste("../models/analysis1/data.",p,sep=""))
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
  mutate(expression = recode(expression,
                             "continue" = "{\\bf continue}",
                             "too" = "{\\bf too}",
                             "also" = "{\\bf also}",
                             "again" = "{\\bf again}",
                             "stop" = "{\\bf stop}",
                             "cleft" = "{\\bf cleft}",
                             "be.annoyed" = "\\color{orange}{\\bf be annoyed}\\color{black}",
                             "know" = "\\color{orange}{\\bf know}\\color{black}",
                             "demonstrate" = "\\color{green}{\\bf demonstrate}\\color{black}",
                             "pretend" = "\\color{green}{\\bf pretend}\\color{black}",
                             "inform" = "\\color{green}{\\bf inform}\\color{black}",
                             "confess" = "\\color{green}{\\bf confess}\\color{black}",
                             "see" = "\\color{orange}{\\bf see}\\color{black}",
                             "acknowledge" = "\\color{green}{\\bf acknowledge}\\color{black}",
                             "discover" = "\\color{orange}{\\bf discover}\\color{black}",
                             "admit" = "\\color{green}{\\bf admit}\\color{black}",
                             "hear" = "\\color{green}{\\bf hear}\\color{black}",
                             "prove" = "\\color{green}{\\bf prove}\\color{black}",
                             "reveal" = "\\color{orange}{\\bf reveal}\\color{black}",
                             "announce" = "\\color{green}{\\bf announce}\\color{black}",
                             "be.right" = "\\color{green}{\\bf be right}\\color{black}",
                             "establish" = "\\color{green}{\\bf establish}\\color{black}",
                             "confirm" = "\\color{green}{\\bf confirm}\\color{black}",
                             "suggest" = "\\color{green}{\\bf suggest}\\color{black}",
                             "think" = "\\color{green}{\\bf think}\\color{black}",
                             "say" = "\\color{green}{\\bf say}\\color{black}"                             
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

write(table1, "../models/analysis1/table1.tex")


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
  saveRDS(paste("m.b.",p,sep=""),file=paste("../models/analysis2/beta-model-mixed-",p,".rds",sep=""))
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

# pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.value
# pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.value
# 
# pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.upper - pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.lower
# pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.upper - pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.lower


# identify pairwise differences in analysis 2
values = data.frame(predicate = character(), contrast = character(), mean = numeric(), lower = numeric(), upper = numeric())
values

for (p in predicates) {
  for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
    print(i)
    cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
    value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
    lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
    upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
    values = values %>%
      add_row(predicate = p, contrast = cntrst, mean = value, lower = lower, upper = upper)                    
  }
}
values

#### full model output  ----

analysis2 = print(xtable(values),
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

write(analysis2, "../models/analysis2/fullModelOutput/analysis2.tex")

#### create data for presenting results as part of Figure 4 ----

# there are three lines in each facet (that is, for each expression)
# one line for each context (x = context)
# geom_segment(aes(x=1,xend=2,y=-.05,yend=-.05), linetype = "solid")
# x, xend, y, yend, linetype: depend on expression and contrast
# create data called "contrasts"
# with columns "expression" and "context" (for binding and faceting with nat.means)
# and with columns "x", "xend", "y", "yend", and "linetype"

predicates = unique(t$expression) 
predicates
predicates <- replace(predicates, 12, "be.annoyed") 
predicates <- replace(predicates, 15, "be.right") 
predicates

contrasts = data.frame(expression = character(), contrast = character(), linetype = numeric())
contrasts

# linetypes
# 0 blank
# 1 solid
# 2 dashed
# 3 dotted
for (p in predicates) {
  for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
    print(i)
    cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
    lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
    upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
    value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
    l = ifelse(lower <= 0 & upper >= 0, 0,
          ifelse(lower < 0 & upper < 0 & value <= -1.5, 1,
            ifelse(lower < 0 & upper < 0 & -1.5 < value & value <= -0.5, 6,
              ifelse(lower < 0 & upper < 0 & -.5 < value & value <= 0, 3,
                ifelse(lower > 0 & upper > 0 & value >= 1.5, 1,
                  ifelse(lower > 0 & upper > 0 & 1.5 > value & value > 0.5, 6,
                    ifelse(lower > 0 & upper > 0 & .5 > value & value >= 0, 3, 666)))))))
    contrasts = contrasts %>%
      add_row(expression = p, contrast = cntrst, linetype = l)
  }
}
contrasts

contrasts = data.frame(contrasts)
names(contrasts)

# make column for context
contrasts = contrasts %>%
  mutate(context = case_when(contrast == "explicitIgnorance - factH" ~ "explicitIgnorance",
                             contrast == "explicitIgnorance - factL" ~ "factL",
                             contrast == "factL - factH" ~ "factH",
                             TRUE ~ "error"))

# now add the needed line values based on context
# explicitIgnorance (EIC-factH): x=1,xend=3,y=-.15,yend=-.15
# factL (EIC-factL): x=1,xend=2,y=-.05,yend=-.05
# factH (factL-factH): x=2,xend=3,y=-.1,yend=-.1
contrasts = contrasts %>%
  mutate(x = case_when(context == "explicitIgnorance" ~ 1,
                       context == "factL" ~ 1,
                       context == "factH" ~ 2,
                       TRUE ~ 666)) %>%
  mutate(xend = case_when(context == "explicitIgnorance" ~ 3,
                       context == "factL" ~ 2,
                       context == "factH" ~ 3,
                       TRUE ~ 666)) %>%
  mutate(y = case_when(context == "explicitIgnorance" ~ -.15,
                       context == "factL" ~ -.05,
                       context == "factH" ~ -.1,
                       TRUE ~ 666)) %>%
  mutate(yend = case_when(context == "explicitIgnorance" ~ -.15,
                       context == "factL" ~ -.05,
                       context == "factH" ~ -.1,
                       TRUE ~ 666))
write_csv(contrasts, file="../data/contrasts.csv")


# the remainder of this code is to create Table 2 (which has been removed from the paper because
# the results of the statistical comparison have been integrated to Fig 4)

# # what is the distribution of the values?
# ggplot(values, aes(x=value)) +
#   geom_histogram() 
#     
#     
# diff2 = data.frame(predicate = character(), contrast = character(), significant = character())
# diff2
# 
# for (p in predicates) {
#   for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
#     print(i)
#     cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
#     lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
#     upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
#     value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
#     # significant = ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0 
#     #                      & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper > 0, "n.d.", 
#     #                      ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0 
#     #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper < 0
#     #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value < -1.5, "--", "+"))
#     significant = ifelse(lower <= 0 & upper >= 0, "\\cellcolor{white}",
#                          ifelse(lower < 0 & upper < 0 & value <= -1.5, "\\cellcolor{yellow1}",
#                                 ifelse(lower < 0 & upper < 0 & -1.5 < value & value <= -0.5, "\\cellcolor{yellow2}",
#                                        ifelse(lower < 0 & upper < 0 & -.5 < value & value <= 0, "\\cellcolor{yellow3}",
#                                               ifelse(lower > 0 & upper > 0 & value >= 1.5, "\\cellcolor{purple1}",
#                                                      ifelse(lower > 0 & upper > 0 & 1.5 > value & value > 0.5, "\\cellcolor{purple2}",
#                                                             ifelse(lower > 0 & upper > 0 & .5 > value & value >= 0, "\\cellcolor{purple3}", "error")))))))
#     diff2 = diff2 %>%
#       add_row(predicate = p, contrast = cntrst, significant = significant)
#   }
# }
# 
# 
# 
# diff2
# diff2 = diff2 %>%
#   spread(key = predicate, value = significant)
# 
# # order the columns by mean naturalness rating in explicit ignorance context, as in Table 1
# tmp = t %>%
#   filter(context == "explicitIgnorance") %>%
#   group_by(expression) %>%
#   summarize(Mean = mean(response)) %>%
#   mutate(expression = fct_reorder(as.factor(expression),Mean))
# tmp
# levels(tmp$expression)
# col_order <- levels(tmp$expression)
# diff2 <- diff2[, col_order]
# diff2$contrast = c("higher prior - EIC", "lower prior - EIC", "higher - lower prior")
# diff2 = diff2 %>%
#   select(contrast,everything())
# diff2
# 
# #### Table 2 output 
# table2 = print(xtable(diff2),
#                only.contents = T,
#                include.rownames=FALSE,
#                include.colnames=FALSE,
#                floating=FALSE,
#                hline.after = NULL,
#                latex.environments=NULL,
#                booktabs=TRUE,
#                sanitize.text.function = function(x){x},
#                comment = F
#                #hline.after = c(2,2,22,42)
# )
# 
# write(table2, "../models/analysis2/table2.tex")

# # combine all the plots
# library(ggpubr)
# 
# combined_plot <- ggarrange(plot.pairwise.acknowledge,
#                            plot.pairwise.admit,
#                            plot.pairwise.announce,
#                            plot.pairwise.be.annoyed,
#                            plot.pairwise.be.right,
#                            plot.pairwise.confess,
#                            plot.pairwise.confirm,
#                            plot.pairwise.demonstrate,
#                            plot.pairwise.discover,
#                            plot.pairwise.establish,
#                            plot.pairwise.hear,
#                            plot.pairwise.inform,
#                            plot.pairwise.know,
#                            plot.pairwise.pretend,
#                            plot.pairwise.prove,
#                            plot.pairwise.reveal,
#                            plot.pairwise.say,
#                            plot.pairwise.see,
#                            plot.pairwise.suggest,
#                            plot.pairwise.think,
#                            ncol = 2,
#                            nrow = 10)
# combined_plot
# ggsave("../graphs/context-comparisons.pdf",height=10,width=6)

