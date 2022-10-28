# 4_projaiC
# Exp 1c
# projection and at-issueness (sure that)
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# load helpers
source('../../helpers.R')

# set theme
theme_set(theme_bw())

# how long did the experiment take?
time = read_csv("../data/experiment-merged.csv")
mean(time$time_in_minutes) #10.4 minutes
median(time$time_in_minutes) #9.1 minutes

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
nrow(d) #15600 / 300 = 52 trials
head(d)
summary(d)

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
nrow(ds)

# look at Turkers' comments
summary(ds)
unique(ds$comments)

# remove comments column (something in it creates a problem)
#ds <- ds %>% select(-comments)
#names(ds)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# how many participants?
length(unique(d$workerid)) #300

# participant info
table(d$age) #18-58 

# remove wacky ages from consideration
d <-  d %>% mutate(age = replace(age, age>150, NA))

length(which(is.na(d$age))) # 104 missing values
mean(d$age,na.rm=TRUE) #25.9

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#249 female, 45 male, 6 other, 0 undeclared

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 5 to 57
d$trial = d$slide_number_in_experiment - 4
unique(d$trial) # trial numbers from 1 to 53 (27 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 52

## exclude participants' data ----

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(ds$language))) #0 missing responses
table(d$language) 

d <- d %>%
  filter(language != "Language" & language != "Spanish" & language != "Jamaican Patois") %>%  droplevels()
length(unique(d$workerid)) #297 (data from 3 participants excluded)

# exclude non-American English speakers
length(unique(d$workerid))# 297
length(which(is.na(d$american))) #0
table(d$american) 

# exclude participant who didn't say "yes"
d <- d %>%
  filter(american == "Yes") %>%  droplevels()
length(unique(d$workerid)) #294 participants (3 participant excluded)

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #3528 (294 Turkers x 6 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #1764

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #.94 (because speaker is committed to content of positive indicative sentences)

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels()
nrow(d.MC.AI) #1764

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2) #.08 (because main clause content is at-issue, coded as 0)

# calculate each Turkers mean response to the projection of main clauses
ai.means = d.MC.AI %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

ggplot(ai.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# look at Turkers whose response mean on projection and ainess of main clauses is more than 2
# standard deviations away from the overall mean

# get the Turkers who are more than 2 standard deviations below the mean on projection 
p <- p.means[p.means$Mean < (mean(p.means$Mean) - 2*sd(p.means$Mean)),]
p #18

# get the Turkers who are more than 2 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai #19

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #312 = 26 participants

# exclude all outliers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) #268 remaining Turkers (26 Turkers excluded)

# variance

# exclude participants who always clicked on roughly the same point on the scale 
# ie participants whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
table(d$trigger)
table(d$question_type)

variances = d %>%
  filter(trigger != "MC") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 2 participants had lower mean variance

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

table(d$trigger_class)

ggplot(lvw,aes(x=Participant,y=response,color=trigger_class)) +
  geom_jitter()

# exclude 2 participants with really low variance 
d <- droplevels(subset(d, !(d$workerid == "1168")))
d <- droplevels(subset(d, !(d$workerid == "1253")))
#d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #266 participants remain

# write cleaned data to file
write_csv(d, file="../data/data_preprocessed.csv")

# info on remaining participants
table(d$age) #18-58
length(which(is.na(d$age))) # 52 missing values
mean(d$age,na.rm=TRUE) #24.8

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#235 female, 25 male, 6 other, 0 undeclared

