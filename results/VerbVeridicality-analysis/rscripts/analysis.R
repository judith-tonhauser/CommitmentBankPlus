# Analysis of veridicality and projection ratings from 
# Ross & Pavlick VerbVeridicality dataset

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
library(splitstackshape)
theme_set(theme_bw())

## preprocessing ----

# load raw data
d = read.delim(file='../data/verb_veridicality_evaluation.tsv')
#View(d)
head(d)
names(d)

# "Our final dataset contains 137 verb types across 1,498 sentences (2,996 pairs)."
nrow(d) #1498 (1498 rows with veridicality + projection info)
length(unique(d$index)) #1498
length(unique(d$verb)) #124

# remove BERT columns for easier handling
d = d %>%
    select(-bert_neg_contradiction_prob,-bert_neg_entailment_prob,-bert_neg_neutral_prob,-bert_pos_contradiction_prob,-bert_pos_entailment_prob,-bert_pos_neutral_prob)
names(d)
head(d)
tail(d)

# reformat columns containing turker ratings
table(d$turker_pos_ratings)
table(d$turker_neg_ratings)

# veridicality ratings (positive ratings)
d <- d %>% 
  mutate(pos_rating = str_split(as.character(turker_pos_ratings), ",")) %>% 
  unnest(pos_rating) 
head(d)
tail(d)
nrow(d) # 4490 (but this should be 4494 = 1498 x 3)
length(unique(d$index)) # 1498
length(unique(d$verb)) #124

# factivity ratings (negative ratings)
d <- d %>% 
  mutate(neg_rating = str_split(as.character(turker_neg_ratings), ",")) %>% 
  unnest(neg_rating) 
head(d)
tail(d)
nrow(d) # 13470 (but this should be 13483 = 4494 * 3)
length(unique(d$index)) # 1498
length(unique(d$verb)) #124

# "We have raters label entailment on a 5-point likert scale in which -2 means that h is definitely not true 
# given p and 2 means that h is definitely true given p.
table(d$pos_rating)
table(d$neg_rating)

table(d$verb)

# factive predicates in this dataset
factives <- c("discover", "know", "learn", "notice", "realize", "recognize", "remember", "reveal", "saw", "see", "understand")

# remove items with "to VP" embedded clause
table(d$task)
d <- droplevels(subset(d, d$task == "that"))

names(d)
length(unique(d$verb)) #78 verbs
length(unique(d$index)) #859 total items: each has sentence and its negated variant

# make ratings numeric
str(d$pos_rating)
d$pos_rating <- as.numeric(d$pos_rating)
d$neg_rating <- as.numeric(d$neg_rating)


# Fig 6: projection ratings  ----

v_means = d %>%
  group_by(verb) %>%
  summarize(Mean = mean(neg_rating), CILow = ci.low(neg_rating), CIHigh = ci.high(neg_rating)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
v_means
levels(v_means$verb) # verbs sorted by projection mean (...)

# define colors
cols = data.frame(V=levels(v_means$verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% factives, "F", "X"))
                                
cols$VeridicalityGroup <- factor(cols$VeridicalityGroup, levels=c("X", "F"))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", "black")

v_means$VeridicalityGroup = as.factor(
  ifelse(v_means$verb %in% factives, "F", "X"))
  
v_means$VeridicalityGroup <- factor(v_means$VeridicalityGroup, levels=c("X","F"))

# create data subset for our  predicates
v_meansFactives <- droplevels(subset(v_means,v_means$verb %in% factives))
v_meansFactives
str(v_meansFactives$verb)
levels(v_meansFactives$verb) # 

# check to get shapes and colors to work out right
levels(v_means$VeridicalityGroup)
# "X" "F"

size <- ifelse(v_means$VeridicalityGroup == "X", 1, 3)

# plot
ggplot(v_means, aes(x=verb, y=Mean)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="gray",alpha=.4) +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=size,color="black") +
  scale_shape_manual(values=rev(c(23, 24)),
                     labels=rev(c("factive","non-factive")),
                     name="Predicate type") +
  scale_fill_manual(values=rev(c("tomato","black")),
                    labels=rev(c("factive", "non-factive")),
                    name="Predicate type") +
  theme(panel.grid.major.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position="bottom") +
  guides(color = "none") +
  geom_text_repel(data=v_meansFactives,aes(x=verb,y=Mean,label=verb,
                                      color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.6) +
  scale_color_manual(values=rev(c("tomato","black"))) +
  scale_y_continuous(limits = c(-1,2),breaks = c(-1,0,1,2)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/projection-factive-non-factive.pdf",height=4,width=9)

