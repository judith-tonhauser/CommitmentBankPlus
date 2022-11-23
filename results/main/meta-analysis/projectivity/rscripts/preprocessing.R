

### data prep for projection data

# set wd to script dir
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load data from exps into list
df_list <- list(data_1q = read.csv("../../1_projaiQ/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_1n = read.csv("../../2_projaiN/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_1m = read.csv("../../3_projaiM/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_1c = read.csv("../../4_projaiC/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_2q = read.csv("../../5_projaiQ/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_2n = read.csv("../../6_projaiN/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_2m = read.csv("../../7_projaiM/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_2c = read.csv("../../8_projaiC/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_3q = read.csv("../../9_projaiQ/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_3n = read.csv("../../10_projaiN/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_3m = read.csv("../../11_projaiM/data/data_preprocessed.csv", 
                                   header=TRUE, sep=","),
                data_3c = read.csv("../../12_projaiC/data/data_preprocessed.csv", 
                                   header=TRUE, sep=",")
)

# libraries for manipulating dataframes, strings, and lists
require(tidyverse)
library(stringr)
library(rlist)

# spread responses over separate columns for projectivity and at-issueness
# add label for which position at block appeared in
df_list <- lapply(df_list, function(df) {
  df = df %>%
    mutate(ai_block = ifelse(question_type == "ai", 
                             ifelse(block == "block1", "block1", "block2"), 
                             ifelse(block == "block1", "block2", "block1"))) %>%
    select(workerid,content,short_trigger,question_type,response,ai_block) %>%
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
df_list = map2(df_list, names(df_list), function(df, name) {
  operator <- str_sub(name, -1, -1)
  exp_block <- str_sub(name, -2, -2)
  df$op <- rep(operator, length(df[,1]))
  df$exp_block <- rep(exp_block, length(df[,1]))
  return(df)
})

# recode relevant vectors as factors
df_list <- lapply(df_list, function(df) {
  df$content <- as.factor(df$content)
  df$verb <- as.factor(df$verb)
  df$workerid <- as.factor(df$workerid)
  df$op <- as.factor(df$op)
  df$exp_block <- as.factor(df$exp_block)
  df
})

# remove data from MC controls
df_list <- lapply(df_list, function(df) {
  df = filter(df, verb != "MC")
  df$verb = droplevels(df$verb)
  df$content = droplevels(df$content)
  df
})

# combine data from all experiments
data_all <- list.rbind(df_list)

# save combined data frame into csv file
write.csv(data_all, "../data/data_combined.csv", row.names=FALSE)

