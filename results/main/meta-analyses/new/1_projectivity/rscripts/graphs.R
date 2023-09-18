##### GRAPHS FOR PROJECTION RATINGS #####

# 1 libraries, helpers, settings
# 2 projectivity by operator
# 3 projectivity by predicate
  ## 3a projectivity by predicate w distribution of participants' means
  ## 3b projectivity by predicate w distribution of observations
# 4 projectivity by predicate and operator
  ## 4b projectivity means with distributions for observations
  ## 4c projectivity means with distributions for paritipant means
# 5 verb profiles


# 1 Libraries,  helpers, settings -------------------------------------------

  # set wd to script dir
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  
  # load data
  data <- read.csv("../../data_combined.csv", header = TRUE, sep = ",")
  
  # load helper functions
  source('../../../../helpers.R')
  
  # libraries for manipulating dataframes, and plotting
  library(dplyr)
  library(forcats)
  library(ggplot2)

  # Color blind friendly palette (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
    cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#D55E00", "#CC79A7", "#000000")
    graphcolor <- "#0072B2"


# 2 projectivity by operator ------------------------------------------------
  # projectivity means and 95% CIs by operator 
  proj_means_op = data %>% group_by(op) %>% 
    summarize(Mean = mean(projective), CILow = ci.low(projective), 
              CIHigh = ci.high(projective)) %>%
    mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
           op = fct_reorder(as.factor(op),Mean)) %>% ungroup()

  # plot
  proj_means_op %>% mutate(op = fct_reorder(op, Mean, .fun = mean, .desc = TRUE)) %>% 
    ggplot(aes(x = op, y=Mean, label = round(Mean, digits = 3))) +
    geom_point(size = .5, color = graphcolor) +
    geom_text(hjust = 0, nudge_x = 0.08) + 
    geom_errorbar(aes(ymin=YMin,ymax=YMax), width=0.1, color = graphcolor) +
    scale_x_discrete(labels=c("m" = "Modal", "n" = "Negation", "q" = "Question", "c" = "Conditional"),
                     name = "Operator") +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                       labels = c("0","0.2","0.4","0.6","0.8","1"),
                       name = "Mean certainty rating") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
    # labs(title = "Mean projectivity by operator")
  
  ggsave("../graphs/projective-op.pdf", height=7.5, width=4)


# 3 projectivity by predicate ---------------------------------------------
  
  # means and confidence intervals for projectivity rating by predicate ----
  pmeans = data %>% group_by(verb) %>%
    summarize(projective = mean(projective), CILow = ci.low(projective), 
              CIHigh = ci.high(projective)) %>%
    mutate(YMin = projective - CILow, YMax = projective + CIHigh, 
           verb = fct_reorder(as.factor(verb), projective)) %>% ungroup()
  
  ## 3a projectivity by predicate w distribution of participants' means ----
    # get the mean of participants' projectivity ratings by verb
    subjmeans = data %>%
      group_by(verb,workerid) %>%
      summarize(projective = mean(projective))
    subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(pmeans$verb)))
  
    # plot 
    ggplot(pmeans, aes(x=verb, y=projective)) +
      geom_violin(data=subjmeans,scale="width",color="gray80") +
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
      geom_point(size=0.5,color="black") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Certainty rating") +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
      ylab("Mean certainty rating") +
      xlab("Predicate")
    ggsave(f="../graphs/projectivity-verb-participant.pdf",height=6,width=8)
  
  ## 3b projectivity by predicate w distribution of observations ----
    # plot of projectivity means, 95% CIs and distribution of observations
    ggplot(pmeans, aes(x=verb, y=projective)) +
      data %>% mutate(verb = fct_reorder(verb, projective, .fun = mean)) %>%
      geom_violin(data = ., scale="width", color = "gray80") +
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
      geom_point(size=0.5,color="black") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Certainty rating") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
      xlab("Predicate")
    ggsave(f="../graphs/projectivity-verb-obs.pdf",height=6,width=10)



# 4 projectivity by predicate and operator --------------------------------
  ## 4a means compare between operators ----
    # projectivity by verb and operator
    pomeans = data %>% group_by(verb, op) %>%
      summarize(projective = mean(projective), CILow = ci.low(projective), 
                CIHigh = ci.high(projective)) %>%
      mutate(YMin = projective - CILow, YMax = projective + CIHigh, 
             verb = fct_reorder(as.factor(verb),projective)) %>%
      mutate(verb = fct_reorder(verb, projective, .fun = mean)) %>%
      mutate(op = fct_reorder(op, projective, .fun = mean)) %>% ungroup()
  
    # plot ----
    pmeans %>% 
      ggplot(aes(x=fct_reorder(verb, Mean), y=Mean, group = op, color = op)) +
      geom_point(aes(shape = op), size = 4) + 
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
      geom_line() +
      xlab("Predicate") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Mean certainty rating") +
      scale_shape_manual(values = c("M", "N", "C", "Q")) +
      scale_colour_manual(values=cbbPalette) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
    
    ggsave("../graphs/projective-pred-op.pdf",height=4.7,width=10)
    
  ## 4b projectivity means with distributions for observations ----
    # get the mean of participants' projectivity ratings by verb and operator 
    subjomeans = data %>%
      group_by(verb,op, workerid) %>%
      summarize(projective = mean(projective)) %>% 
      mutate(verb = fct_reorder(verb, projective, .fun = mean)) %>% ungroup()
    subjomeans$verb <- factor(subjomeans$verb, levels = unique(levels(pomeans$verb)))
    
    # plot of projectivity means by verb and operator, 95% CIs and participants' ratings 
    ggplot(pomeans, aes(x=fct_reorder(verb, projective), y=projective)) +
      geom_violin(data=subjomeans,scale="width", color="gray80") +
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width = 0.5, color="black") +
      geom_point(size=0.5,color="black") +
      xlab("Predicate") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Mean certainty rating")+
      facet_wrap(~op, labeller = labeller(op = c("m" = "Modal", 
                                                 "n" = "Negation", 
                                                 "q" = "Question", 
                                                 "c" = "Conditional"))) +
      # theme(text = element_text(size=12), axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = -0.1, color=cols$Colors)) +
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
    ggsave(f="../graphs/projectivity-pred-op-participant.pdf",height=6,width=8)
    
  ## 4c projectivity means with distributions for participant means ----
    # plot
    ggplot(pomeans, aes(x=fct_reorder(verb, projective), y=projective)) +
      data %>% mutate(verb = fct_reorder(verb, projective, .fun = mean)) %>%
        geom_violin(data = ., scale="width", color = "gray80") +
      geom_point(size=0.5,color="black") +
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width = 0.5, color="black") +
      facet_wrap(~op, labeller = labeller(op = c("m" = "Modal", "n" = "Negation", "q" = "Question", "c" = "Conditional"))) +
      xlab("Predicate") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Mean certainty rating") +
      theme_bw() +
      #theme(text = element_text(size=12), axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = -0.1, color=cols$Colors)) +
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
      
    ggsave(f="../graphs/projectivity-pred-op-obs.pdf",height=6,width=10)





# 5 verb profiles ---------------------------------------------------------
    ## 5a mean projectivity by operator, for each verb (verb profiles) ----
      # other plots ----
      proj_means %>% mutate(op = fct_reorder(op, Mean, 
                                                .fun = mean)) %>% mutate(verb = fct_reorder(verb, Mean, .fun = mean)) %>% ggplot(aes(x = op, y=Mean, group = verb)) +
        geom_point(size = 1, color = "lightblue") +
        geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1, color = "lightblue") +
        geom_line(color = "lightblue") +
        coord_cartesian(ylim = c(0,1)) +
        facet_wrap(vars(verb)) +
        scale_x_discrete(labels=c("m" = "modal", "n" = "negation", "q" = "question", "c" = "conditional")) +
        labs(title = "Mean projectivity by operator, for each verb")+
        theme_bw() + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
      ggsave("../graphs/proj-by-op-for-verb.pdf",height=8,width=10)





# VERB PROFILES ---- 

proj_means %>%
  ggplot(aes(x = fct_reorder(op, Mean), y = Mean, group = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(vars(fct_reorder(verb, Mean))) +
  geom_point(size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1, color = "lightblue") +
  geom_line(color = "lightblue") +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profiles.pdf",height=6,width=8)


levels(proj_means$verb)
vgroups <- data.frame(verb = levels(proj_means$verb), profile = 
                        c("1",
                              # pretend
                          "7",
                                  # be right
                          "7",
                                  # suggest
                          "1",   
                                    # think
                          "say",    # say
                          "4",   # prove
                          "4",   # confirm
                          "4",   # establish
                          "demonstrate",   # demonstrate
                          "2",  # announce
                          "2",   # confess
                          "2",   # admit 
                          "2",   # reveal
                          "3",  # acknowledge
                          "6",  # hear
                          "5",  # inform
                          "6",  # see
                          "3",  # discover
                          "5",  # know
                          "5"  # be annoyed
                        ))

lookup_group <- function(verb_name){
  return(as.character(filter(vgroups, verb == verb_name)[2]))
}
proj_means = proj_means %>% rowwise() %>% mutate(groups = lookup_group(verb))

# all groups overview ----
proj_means %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(~groups) +
  geom_point(size = 1, color = "lightblue") +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profiles-grouped.pdf",height=6,width=8)

# individual profiles ----

# Color blind friendly palette with black (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
cbbPalette2 <- c("#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

library("directlabels")

# group 1 ----
proj_means %>% filter(groups == "1") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 2.4) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.1),
  #                       method = list(cex=0.7, "first.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
                        method = list(cex=1, rot = 50,  "last.bumpup")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile1.pdf",height=4,width=4)


# N > M, Q, C

# group 2 ----
proj_means %>% filter(groups == "2") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 2.5) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .075),
  #                       method = list(cex=0.54, "last.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
                        method = list(cex=1,  "smart.grid")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile2.pdf",height=4,width=4)
# C > N, M, Q

# group 3 ----
proj_means %>% filter(groups == "3") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 3) + 
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.1),
                        method = list(cex=0.7, "first.bumpup")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile3.pdf",height=4,width=4)
# Q, C > N, M

# group 4 ----
proj_means %>% filter(groups == "4") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 3) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
  #                       method = list(cex=0.6, "last.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = .1),
                        method = list(cex=1,"smart.grid")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile4.pdf",height=4,width=4)
# C, M > N

# group 5 ----
proj_means %>% filter(groups == "5") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     labels = c("0","0.2","0.4","0.6","0.8","1"), 
                     name = "Mean certainty rating") +
  geom_point(aes(shape = op), size = 3) + 
  # directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.08),
  #                       method = list(cex=0.63, "first.bumpup")) +
  directlabels::geom_dl(aes(label = verb), position = position_nudge(x = -.1),
                        method = list(cex=1,"smart.grid")) +
  scale_shape_manual(values = c("M", "N", "Q", "C")) +
  scale_colour_manual(values=cbbPalette2) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(f="../graphs/profile5.pdf",height=4,width=4)
# Q > N, C > M

# group 6
proj_means %>% filter(groups == "6") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile6.pdf",height=3.7,width=4.5)
# Q, C > N > M 

# group 7
proj_means %>% filter(groups == "7") %>%
  ggplot(aes(x = op, y = Mean, group = verb, color = verb)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width=0.1) +
  geom_line() +
  xlab("") + ylab("") +
  theme_bw()
ggsave(f="../graphs/profile7.pdf",height=3.7,width=4.5)
# C > Q

