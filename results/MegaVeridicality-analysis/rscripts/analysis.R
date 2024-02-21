# Analysis of veridicality and projection ratings from 
# White & Rawlins' MegaVeridicality I dataset 

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

## preprocessing ----

# load raw data
# MV1: judgments of "did that thing happen?" for positive and negated predicates with "that" complements
mv1 = read.csv("../data/mega-veridicality-v1/mega-veridicality-v1.csv")

# MV2: judgments of "did that thing happen?" for pos/neg predicates with nonfinite complements
# not relevant for our paper or comparison
#mv2 = read.csv("../data/mega-veridicality-v2/mega-veridicality-v2.csv")

nrow(mv1) #21760
#nrow(mv2) #50260

### exclude nonAmerican English speakers
length(unique(mv1$participant)) #291
#length(unique(mv2$participant)) #635

table(mv1$nativeenglish)
#table(mv2$nativeenglish)

str(mv1$nativeenglish)

mv1 <- droplevels(subset(mv1, (mv1$nativeenglish != "False")))
#mv2 <- droplevels(subset(mv2, (mv2$nativeenglish != "False")))

length(unique(mv1$participant)) #290
#length(unique(mv2$participant)) #630

# exclude 3 rows in mv2 where their java script created NA in veridicality response
table(mv1$veridicality)
#table(mv2$veridicality)

#mv2 <- droplevels(subset(mv2,(mv2$veridicality != "")))

# continuing to work only with mv1

# recode their responses to numerical values
# MV has ratings on a 3-point Likert scale in response to "Did that thing happen?"
# "veridicality" codes the response options: "yes" (happened) "no" (didn't happen) "maybe" (maybe happened)

# create veridicality_num which codes "yes" as 1, "no" as -1 and "maybe" as 0
table(mv1$veridicality)
str(mv1$veridicality)

mv1$veridicality_num[mv1$veridicality == "maybe"] <- 0 
mv1$veridicality_num[mv1$veridicality == "no"] <- -1
mv1$veridicality_num[mv1$veridicality == "yes"] <- 1
str(mv1$veridicality_num)

# check that all is in order
table(mv1$veridicality)
table(mv1$veridicality_num)

table(mv1$verb)

# create items for mv1 

table(mv1$verb) #verb
table(mv1$frame) #only that_S
table(mv1$voice) #active, passive
table(mv1$polarity) #negative, positive
table(mv1$conditional) #true, false

str(mv1$conditional)
str(mv1$conditional2)
mv1$conditional2 <- as.character(mv1$conditional)
mv1$conditional2[mv1$conditional2 == "True"] <- "conditional"
mv1$conditional2[mv1$conditional2 == "False"] <- "matrix"
table(mv1$conditional2) #conditional, matrix

mv1$item <- paste(mv1$verb,mv1$frame,mv1$voice,mv1$polarity,mv1$conditional2, sep = "-")
table(mv1$item)

# save data
write.csv(mv1, "../data/mv1.csv")
nrow(mv1) #21692

# plot

# load data
mv1 = read.csv("../data/mv1.csv")
nrow(mv1) #21692

# create data relevant to investigate projection (embedding is negation, conditional or both)
mv1_tmp <- droplevels(subset(mv1, mv1$polarity == "negative" | mv1$conditional2 == "conditional"))
t <- table(mv1_tmp$verb)
min(t)
mean(t)
max(t) #29-60 ratings per predicate under negation, cond or both
length(unique(mv1_tmp$verb)) #517 verbs
length(unique(mv1_tmp$participant)) #290 participants gave ratings

table(mv1_tmp$polarity, mv1_tmp$conditional2)

mv1_tmp$embedding = case_when(mv1_tmp$polarity == "positive" & mv1_tmp$conditional2 == "conditional" ~ "condPos",
                              mv1_tmp$polarity == "negative" & mv1_tmp$conditional2 == "conditional" ~ "condNeg",
                              mv1_tmp$polarity == "negative" & mv1_tmp$conditional2 == "matrix" ~ "matrixNeg",
                              TRUE ~ "error")

table(mv1_tmp$embedding)

# calculate mean for all 517 predicates, and embedding
p_means = mv1_tmp %>%
  group_by(verb, embedding) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = as.factor(verb), embedding = as.factor(embedding))
# options(tibble.print_max = Inf)

p_means$verb <- fct_reorder(p_means$verb, p_means$Mean, .fun = "mean")
levels(p_means$verb) # verbs sorted by projectivity mean 

# define subsets for selection

# 7 predicates -- Karttunen's factives and emotive predicates ("announce", "inform",)
factives <- c("annoy", "disappoint", "know", "please", "regret", "reveal", "surprise")
# 6 predicates -- Karttunen's semi-factives and cognitive predictes
semifactives <- c("discover", "find_out", "notice", "realize", "see", "understand")
# 14 predicates -- non-factives from experiment (except be_right)
nonfactives <- c("acknowledge", "admit", "announce", 
                 "confess", "confirm", "claim", "demonstrate", 
                 "establish", "hear", "inform", "prove", 
                 "pretend", "say", "suggest", "think")



# create data subsets for the selected predicates 
p_meansSelection <- droplevels(subset(p_means, p_means$verb %in% factives | p_means$verb %in% semifactives | p_means$verb %in% nonfactives))
str(p_meansSelection$verb)
levels(p_meansSelection$verb) # sorted by projectivity mean (pretend...be annoyed)

p_meansSelection <- mutate(p_meansSelection, verb = fct_reorder(verb, Mean, .fun = mean))
p_meansSelection$embedding <- fct_reorder(p_meansSelection$embedding, p_meansSelection$Mean, .fun = "mean")
levels(p_meansSelection$verb)
levels(p_meansSelection$embedding)

# Color blind friendly palette with black (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
cbbPalette <- c("#000000", "#56B4E9", "#E69F00", "#009E73", "#0072B2", "#CC79A7", "#D55E00","#F0E442")

textcolors <- case_when(
  levels(p_meansSelection$verb) %in% factives ~ "#CC79A7",
  levels(p_meansSelection$verb) %in% semifactives ~ "#CC79A7",
  levels(p_meansSelection$verb) %in% nonfactives ~ "black",
)
str(textcolors)

p_meansSelection %>%
  ggplot(aes(x = verb, y = Mean, group = embedding, fill = embedding, color = embedding)) +
  geom_line() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.4, alpha = .8) +
  coord_cartesian(ylim=c(-1,1)) +
  geom_point(aes(shape = embedding, fill = embedding), size = 2.5) + 
  scale_shape_manual(values = rev(c(23, 24, 25, 21)),
                     # labels= c("cond/neg/polar", "negation",  "cond/polar"),
                     name="Embedding") +
  scale_fill_manual(values = cbbPalette,
                    # labels= c("cond/neg/polar", "negation",  "cond/polar"),
                    name="Embedding") +
  scale_colour_manual(values = cbbPalette) +
  guides(color = "none") +
  scale_y_continuous(limits = c(-1,2),breaks = c(-1,0,1,2)) +
  ylab("Mean projection rating") +
  xlab("Predicate") + 
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),# color = textcolors),
         text = element_text(size=12)
  )

ggsave("../graphs/megaveridicality.pdf",height=4.7,width=12)
