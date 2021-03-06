# 2_projaiN
# Exp 1n
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
mean(time$time_in_minutes) #9.8 minutes
median(time$time_in_minutes) #8.5 minutes

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
nrow(d) #15600 / 300 = 52 trials
head(d)
summary(d)

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
nrow(ds) #300
head(ds)
summary(ds)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# how many participants?
length(unique(d$workerid)) #300

# look at Turkers' comments
unique(ds$comments)

# participant info
table(d$age) #18-74
length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #33.2

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#150 female, 145 male, 5 other, 0 undeclared

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
length(which(is.na(ds$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language != "Spanish" & language != "Vietnamese") %>%  droplevels()
length(unique(d$workerid)) # (data from 5 Turker excluded, 295 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid))# 295
length(which(is.na(d$american))) #1 person didn't respond
table(d$american) 
d <- d %>%
  filter(d$american == "Yes") %>%  droplevels()
length(unique(d$workerid)) #292 (3 Turkers excluded)

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #3504 (292 Turkers x 6 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #1752 (292 Turkers x 6 controls)

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #.95 (because speaker is committed to content of positive indicative sentences)

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
nrow(d.MC.AI) #1752

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2) #.04 (because main clause content is at-issue, coded as 0)

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
p #11 participants

# get the Turkers who are more than 2 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai #11 participants

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
length(unique(outliers$workerid)) #17 outlier Turkers

# exclude all outliers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 275 remaining Turkers (17 Turkers excluded)

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
lowvarworkers # 1 participants had lower mean variance (249)

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

table(d$trigger_class)

ggplot(lvw,aes(x=Participant,y=response,color=trigger_class)) +
  geom_jitter()

# exclude 1 participant with really low variance (249)
d <- droplevels(subset(d, !(d$workerid == "249")))
#d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #274 participants remain

# write cleaned data to file
write_csv(d, file="../data/data_preprocessed.csv")

# info on remaining participants
table(d$age) #18-74
length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #33.3

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#141 female, 128 male, 5 other, 0 undeclared
