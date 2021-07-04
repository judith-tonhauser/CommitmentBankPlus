# set working directory to point to 'rscripts' (default when opening file in RStudio)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load required packages
require(tidyverse)

# load data
d = read.csv("../norms.csv")

# rating scale:
# from 1 (valence: happy, arousal: excited) to 9 (valence: unhappy; arousal: calm)

head(d)
nrow(d)

emotions = c("mad","frustrated","glad","worried","proud","pleased","embarrassed","giddy","scared","delighted","thrilled","surprised","proud","sad","excited","disappoinnted","furious","concerned","intrigued","grateful","happy","cheerful","angry","grouchy","mad","calm","confident","horrified","anxious","irritated","aggravated","disgusted","guilty","jealous","ecstatic","brave","appreciative","blissful","contented","joyful","serene","nervous","miserable","glum","amused","ashamed","astonished","threatened","terrified","suspicious","shocked","resentful","remorseful","overwhelmed","envious")

targets = d %>% 
  filter(Word %in% emotions) %>% 
  select(Word,V.Mean.Sum,A.Mean.Sum) %>% 
  mutate(Candidate = case_when(A.Mean.Sum > 4.7 & A.Mean.Sum < 6.5 ~ "yes",
                               TRUE ~ "no"),
         Valence = case_when(V.Mean.Sum >=4.5 ~ "positive",
                             TRUE ~ "negative"))
targets

table(targets$Candidate,targets$Valence)

ggplot(targets, aes(x=V.Mean.Sum,y=A.Mean.Sum,color=Candidate)) +
  geom_text(aes(label=Word),size=2,show.legend=FALSE) +
  lims(x=c(1,9),y=c(1,9)) +
  geom_hline(yintercept=4.5,linetype="dashed") +
  geom_vline(xintercept=4.5,linetype="dashed") +
  scale_color_manual(values=cbPalette) +
  labs(x="Valence (positive to negative)", y="Arousal (calm to excited)")
ggsave(file="valence-arousal.pdf",width=6,height=5)

targets %>% 
  filter(Candidate == "yes")
