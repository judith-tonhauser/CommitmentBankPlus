# question: projection and at-issueness
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

theme_set(theme_bw())

# load required packages for pre-processing data
library(tidyverse)

# read in the raw data
d = read_csv("../data/experiment-trials.csv")

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# how many participants?
length(unique(d$workerid)) #9

# look at Turkers' comments
unique(ds$comments)

# participant info
table(d$age) #25-57
length(which(is.na(d$age))) # 0 missing values
median(d$age,na.rm=TRUE) #32

#no gender info collected in this experiment
#d %>% 
#  select(gender, workerid) %>% 
#  unique() %>% 
#  group_by(gender) %>% 
#  summarize(count=n())


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

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(ds$language))) #no missing responses
table(ds$language) 


d <- d %>%
  filter(language != "Chinese" & language != "Russian" & language != "telugu") %>%  droplevels()
length(unique(d$workerid)) # (data from 0 Turker excluded, 9 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid))# 9
length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #108 (9 Turkers x 6 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #54 (9 Turkers x 6 controls)

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #.08 (because speaker is not committed to content of questions)

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
nrow(d.MC.AI) #54

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

# get the Turkers who are more than 2 standard deviations above the mean on projection 
p <- p.means[p.means$Mean > (mean(p.means$Mean) + 2*sd(p.means$Mean)),]
p

# get the Turkers who are more than 2 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #24 (2 unique outlier Turkers x 12 = 6 main clauses x 2 questions)

# exclude all outliers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 7 remaining Turkers (2 Turkers excluded)

# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(short_trigger != "MC") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 0 turkers consistently clicked on roughly the same point on the scale

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point()

# exclude the Turkers identified above
d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #7 Turkers remain

# age and gender of remaining participants
table(d$age) #25-57
length(which(is.na(d$age))) # 0 missing values
median(d$age,na.rm=TRUE) #31

# no gender info collected

write.csv(d, file="../data/data_preprocessed.csv",row.names=F,quote=F)

