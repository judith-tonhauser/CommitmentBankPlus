# 6_projaiN
# Exp 2n
# projection and at-issueness (assent with positive continuation)
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
median(time$time_in_minutes) #8.6 minutes

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
nrow(d) #13000 / 250 = 52 trials (the experiment was done 250 times, as planned)
head(d)
summary(d)

length(unique(d$workerid)) #249

# participant 1877 took the experiment twice:
# View(table(d$workerid))

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #249
nrow(ds) #250
head(ds)
summary(ds) 

# look at participants' comments
unique(ds$comments)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# remove data from participant 1877 since no information on which was first take
d <- d %>%
  filter(workerid != "1877") %>%
  droplevels()
length(unique(d$workerid)) #248

# participant info
table(d$age) #18-69

str(d$age)
# turn non-numeric age information into NA
d$age <- as.numeric(as.character(d$age))
table(d$age)

length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #33.2

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#127 female, 114 male, 6 other, 1 undeclared


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

### exclude non-American English speakers
length(unique(d$workerid)) #248
length(which(is.na(d$language))) #no missing responses
table(d$language) 

# exclude anybody who didn't include English among languages spoken
# nobody removed
#d <- d %>%
#  filter(language != "Korean") %>%
#  droplevels()
# length(unique(d$workerid)) #248 (data from 0 participant excluded)

# exclude non-American English speakers
length(unique(d$workerid))# 248
length(which(is.na(d$american))) #52
table(d$american) 

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) #244 (data from 4 participants excluded)

# exclude participants based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #2928 / 244 participants = 12 (6 main clause controls in each of the two blocks)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #1464

# group projection mean (all participants, all clauses)
round(mean(d.MC.Proj$response),2) #.96

# calculate each participants mean response to the projection of main clauses
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
nrow(d.MC.AI) #1464

# group not-at-issueness mean (all participants, all clauses)
round(mean(d.MC.AI$response),2) #.22

# calculate each participants mean response to the projection of main clauses
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

# look at participants whose response mean on projection and ainess of main clauses is more than 2
# standard deviations away from the overall mean

# get the participants who are more than 2 standard deviations below the mean on projection 
p <- p.means[p.means$Mean < (mean(p.means$Mean) - 2*sd(p.means$Mean)),]
p #13 participants 

# get the participants who are more than 2 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai #16 participants

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #348 / 12 = 29 outlier participants

# exclude all outlier participants identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 215 remaining participants (29 participants excluded)

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
lowvarworkers # 0 participants had lower mean variance

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_jitter()

# exclude 0 participant with really low variance 
d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #215 participants remain

# write cleaned data to file
write_csv(d, file="../data/data_preprocessed.csv")

# info on remaining participants
table(d$age) #18-69
length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #23

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#113 female, 95 male, 6 other, 1 undeclared
