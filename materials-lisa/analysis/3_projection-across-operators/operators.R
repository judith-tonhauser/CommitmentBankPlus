
### comparing projectivity across entailment-cancelling operators

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# for exps 1: compare Q / N / M / C; 1-4

# load data from exps 1
data_1q <- read.csv("../../../results/main/1_projaiQ/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_1n <- read.csv("../../../results/main/2_projaiN/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_1m <- read.csv("../../../results/main/3_projaiM/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_1c <- read.csv("../../../results/main/4_projaiC/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")

# load data from exps 2
data_2q <- read.csv("../../../results/main/5_projaiQ/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_2n <- read.csv("../../../results/main/6_projaiN/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_2m <- read.csv("../../../results/main/7_projaiM/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_2c <- read.csv("../../../results/main/8_projaiC/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")

# load data from exps 3
data_3q <- read.csv("../../../results/main/9_projaiQ/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_3n <- read.csv("../../../results/main/10_projaiN/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_3m <- read.csv("../../../results/main/11_projaiM/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")
data_3c <- read.csv("../../../results/main/12_projaiC/data/data_preprocessed.csv", 
                    header=TRUE, sep=",")

df_list <- list(data_1q=data_1q, data_1m=data_1m, data_1n=data_1n, data_1c=data_1c,
                  data_2q=data_2q, data_2m=data_2m, data_2n=data_2n, data_2c=data_2c,
                  data_3q=data_3q, data_3m=data_3m, data_3n=data_3n, data_3c=data_3c)

# spread responses over separate columns for projectivity and at-issueness
# require(tidyverse)

df_list <- lapply(df_list, function(df) {
  df = df %>%
    mutate(block_ai = ifelse(question_type == "ai", 
                             ifelse(block == "block1", "block1", "block2"), 
                             ifelse(block == "block1", "block2", "block1"))) %>%
    select(workerid,content,short_trigger,question_type,response,block_ai) %>%
    spread(question_type,response)
  df
})

# change cd verb names to match veridicality names
df_list <- lapply(df_list, function(df) {
  df = df %>%
    mutate(verb=recode(short_trigger, control = "MC", annoyed = "be_annoyed", 
                       be_right_that = "be_right", inform_Sam = "inform"))
  df
})

# add labels for operator, and experiment block
# use purr's map2 to take a list from the df_list, and its name from the list of names, 
# and add the name to new col

# library(stringr)

df_list = map2(df_list, names(df_list), function(df, name) {
  # print(df$content[1])
  operator <- str_sub(name, -1, -1)
  exp_block <- str_sub(name, -2, -2)
  df$op <- rep(operator, length(df[,1]))
  df$exp <- rep(exp_block, length(df[,1]))
  return(df)
})

View(df_list$data_1q)
View(df_list$data_2n)

library(rlist)

data_all <- list.rbind(df_list)

# data_all <- bind_rows(df_list)


### summarize data
# load helper functions
source('../../../results/main/helpers.R')

## mean projectivity by predicate, including the main clause controls

# df_list <- lapply(df_list, function(df) {
#   # summary statistics
#   df = df %>%
#     group_by(short_trigger) %>%
#     summarize(Mean = mean(projective), CILow = ci.low(projective), 
#               CIHigh = ci.high(projective)) %>%
#     mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = 
#              fct_reorder(as.factor(short_trigger),Mean))
#   # add label .. how can I get the name of a list object?? this doesn't work, 
#   # have to do this one by one :()
#   df = df %>% add_column(op = rep("q", length(proj_means_1q[1])))
#   df
# })

# 1
proj_means_1q = df_list$data_1q %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_1q = proj_means_1q %>% add_column(op = rep("q", length(proj_means_1q[1])))
proj_means_1q

proj_means_1n = df_list$data_1n %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_1n = proj_means_1n %>% add_column(op = rep("n", length(proj_means_1n[1])))
proj_means_1n

proj_means_1m = df_list$data_1m %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_1m = proj_means_1m %>% add_column(op = rep("m", length(proj_means_1m[1])))
proj_means_1m

proj_means_1c = df_list$data_1c %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_1c = proj_means_1c %>% add_column(op = rep("c", length(proj_means_1c[1])))
proj_means_1c

# 2
proj_means_2q = df_list$data_2q %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_2q = proj_means_2q %>% add_column(op = rep("q", length(proj_means_2q[1])))
proj_means_2q

proj_means_2n = df_list$data_2n %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_2n = proj_means_2n %>% add_column(op = rep("n", length(proj_means_2n[1])))
proj_means_2n

proj_means_2m = df_list$data_2m %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_2m = proj_means_2m %>% add_column(op = rep("m", length(proj_means_2m[1])))
proj_means_2m

proj_means_2c = df_list$data_2c %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_2c = proj_means_2c %>% add_column(op = rep("c", length(proj_means_2c[1])))
proj_means_2c

# 3
proj_means_3q = df_list$data_3q %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_3q = proj_means_3q %>% add_column(op = rep("q", length(proj_means_3q[1])))
proj_means_3q

proj_means_3n = df_list$data_2n %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_3n = proj_means_3n %>% add_column(op = rep("n", length(proj_means_3n[1])))
proj_means_3n

proj_means_3m = df_list$data_3m %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_3m = proj_means_3m %>% add_column(op = rep("m", length(proj_means_3m[1])))
proj_means_3m

proj_means_3c = df_list$data_3c %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
proj_means_3c = proj_means_3c %>% add_column(op = rep("c", length(proj_means_3c[1])))
proj_means_3c

# combine summaries for different operators
proj_means_1 <- rbind(proj_means_1q, proj_means_1n, proj_means_1m, proj_means_1c)
# exclude MC controls (too different for question/declaratives)
proj_means_1 = proj_means_1 %>% filter(short_trigger != "MC")
proj_means_1

proj_means_2 <- rbind(proj_means_2q, proj_means_2n, proj_means_2m, proj_means_2c)
# exclude MC controls (too different for question/declaratives)
proj_means_2 = proj_means_2 %>% filter(short_trigger != "MC")
proj_means_2

proj_means_3 <- rbind(proj_means_3q, proj_means_3n, proj_means_3m, proj_means_3c)
# exclude MC controls (too different for question/declaratives)
proj_means_3 = proj_means_3 %>% filter(short_trigger != "MC")
proj_means_3

proj_means_all <- rbind(proj_means_3q, proj_means_3n, proj_means_3m, proj_means_3c)
# exclude MC controls (too different for question/declaratives)
proj_means_3 = proj_means_3 %>% filter(short_trigger != "MC")
proj_means_3

# plot of means, 95% CIs and participants' ratings 
library(forcats)

proj_means_1 %>% mutate(verb = fct_reorder(verb, proj_means_1$Mean, 
                                                    .fun = 'mean')) %>% 
  ggplot(aes(x=verb, y=Mean, group = op, color = op)) +
    geom_point() + 
    geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
    geom_line() + 
    labs(title = "Mean projectivity for predicate by operator, exps 1")+
    theme_bw() +
    scale_color_brewer(palette = "PRGn")

proj_means_2 %>% mutate(verb = fct_reorder(verb, proj_means_1$Mean, 
                                           .fun = 'mean')) %>% 
  ggplot(aes(x=verb, y=Mean, group = op, color = op)) +
  geom_point() + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() + 
  labs(title = "Mean projectivity for predicate by operator, exps 2")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")

proj_means_3 %>% mutate(verb = fct_reorder(verb, proj_means_1$Mean, 
                                           .fun = 'mean')) %>% 
  ggplot(aes(x=verb, y=Mean, group = op, color = op)) +
  geom_point() + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() + 
  labs(title = "Mean projectivity for predicate by operator, exps 3")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")


# pooling data for operators across experiments
# to do: justify statistically? how??




proj_means_all %>% mutate(verb = fct_reorder(verb, proj_means_all$Mean, 
                                           .fun = 'mean')) %>% 
  ggplot(aes(x=verb, y=Mean, group = op, color = op)) +
  geom_point() + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
  geom_line() + 
  labs(title = "Mean projectivity for predicate by operator")+
  theme_bw() +
  scale_color_brewer(palette = "PRGn")
