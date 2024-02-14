#### BY-PREDICATE ANALYSIS + graph






#### full model output  ----

# analysis2 = print(xtable(values),
#                   #only.contents = T,
#                   include.rownames=FALSE,
#                   include.colnames=TRUE,
#                   tabular.environment="longtable",
#                   floating=FALSE,
#                   hline.after = NULL,
#                   latex.environments=NULL,
#                   booktabs=TRUE,
#                   sanitize.text.function = function(x){x},
#                   comment = F
# )




#### create data for presenting results as part of Figure 4 ----

# there are three lines in each facet (that is, for each expression)
# one line for each context (x = context)
# geom_segment(aes(x=1,xend=2,y=-.05,yend=-.05), linetype = "solid")
# x, xend, y, yend, linetype: depend on expression and contrast
# create data called "contrasts"
# with columns "expression" and "context" (for binding and faceting with nat.means)
# and with columns "x", "xend", "y", "yend", and "linetype"

contrasts = data.frame(predicate = character(), contrast = character(), linetype = numeric())
contrasts

# show in table w colors
# linetypes
# 0 blank
# 1 solid
# 2 dashed
# 3 dotted

for (p in predicates) {
  for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
    # print(i)
    cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
    lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
    upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
    value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
    l = ifelse(lower <= 0 & upper >= 0, 0,
               ifelse(lower < 0 & upper < 0 & value <= -1.5, 1,
                      ifelse(lower < 0 & upper < 0 & -1.5 < value & value <= -0.5, 6,
                             ifelse(lower < 0 & upper < 0 & -.5 < value & value <= 0, 3,
                                    ifelse(lower > 0 & upper > 0 & value >= 1.5, 1,
                                           ifelse(lower > 0 & upper > 0 & 1.5 > value & value > 0.5, 6,
                                                  ifelse(lower > 0 & upper > 0 & .5 > value & value >= 0, 3, 666)))))))
    contrasts = contrasts %>%
      add_row(predicate = p, contrast = cntrst, linetype = l)
  }
}
str(contrasts)


contrasts <- contrasts %>% mutate(
  x = case_when(
    contrast == "conditional - negation" ~ 2,
    contrast == "conditional - question" ~ 3,
    contrast == "modal - conditional" ~ 1,
    contrast == "modal - negation" ~ 1,
    contrast == "modal - question" ~ 1,
    contrast == "negation - question" ~ 2,
    TRUE ~ 666
  ),
  xend = case_when(
    contrast == "conditional - negation" ~ 4,
    contrast == "conditional - question" ~ 4,
    contrast == "modal - conditional" ~ 4,
    contrast == "modal - negation" ~ 2,
    contrast == "modal - question" ~ 3,
    contrast == "negation - question" ~ 3,
    TRUE ~ 666
  ),
  y = case_when(
    contrast == "conditional - negation" ~ .2,
    contrast == "conditional - question" ~ .05,
    contrast == "modal - conditional" ~ .25,
    contrast == "modal - negation" ~ .05,
    contrast == "modal - question" ~ .15,
    contrast == "negation - question" ~ .1,
    TRUE ~ 666
  ),
  yend = y
)

str(contrasts)

# the remainder of this code is to create Table 2 (which has been removed from the paper because
# the results of the statistical comparison have been integrated to Fig 4)

# # what is the distribution of the values?
ggplot(values, aes(x=mean)) +
  geom_histogram(binwidth = .001)

diff2 = data.frame(predicate = character(), contrast = character(), significant = character())
diff2

for (p in predicates) {
  for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
    # print(i)
    cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
    lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
    upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
    value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
    # significant = ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0
    #                      & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper > 0, "n.d.",
    #                      ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0
    #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper < 0
    #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value < -1.5, "--", "+"))
    significant = ifelse(lower <= 0 & upper >= 0, "\\cellcolor{white}",
                         ifelse(lower < 0 & upper < 0 & value <= -1.5, "\\cellcolor{blue}",
                                ifelse(lower < 0 & upper < 0 & -1.5 < value & value <= -0.5, "\\cellcolor{blue!60}",
                                       ifelse(lower < 0 & upper < 0 & -.5 < value & value <= 0, "\\cellcolor{blue!30}",
                                              ifelse(lower > 0 & upper > 0 & value >= 1.5, "\\cellcolor{red}",
                                                     ifelse(lower > 0 & upper > 0 & 1.5 > value & value > 0.5, "\\cellcolor{red!60}",
                                                            ifelse(lower > 0 & upper > 0 & .5 > value & value >= 0, "\\cellcolor{red!30}", "error")))))))
    diff2 = diff2 %>%
      add_row(predicate = p, contrast = cntrst, significant = significant)
  }
}


diff2
diff2 = diff2 %>%
  spread(key = predicate, value = significant)
table2 = print(xtable(diff2),
               # only.contents = T,
               # include.rownames=FALSE,
               include.colnames=FALSE,
               floating=FALSE,
               hline.after = NULL,
               # latex.environments=NULL,
               booktabs=TRUE,
               sanitize.text.function = function(x){x},
               comment = F
               #hline.after = c(2,2,22,42)
)






#####


# Fig X: plot of mean certainty ratings by operator ----
# for 20 clause-embedding predicates only
# with statistics output

# calculate mean naturalness rating by predicate and context
table(data$predicate)
table(data$operator)  # modal / conditional / negation / question


## comment out in pdf version, because already computed in summary above

predicate_operator_means <- data %>% group_by(predicate, operator) %>%
  summarize(Mean = mean(projection), CILow = ci.low(projection), 
            CIHigh = ci.high(projection)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
         predicate = fct_reorder(as.factor(predicate),Mean)) %>%
  mutate(predicate = fct_reorder(predicate, Mean, .fun = mean)) %>%
  mutate(operator = fct_reorder(operator, Mean, .fun = mean)) %>% ungroup()
predicate_operator_means <- predicate_operator_means %>% mutate(projection = Mean, .keep = "unused")

predicate_operator_means
table(predicate_operator_means$operator)
predicate_operator_means$predicate <- as.factor(predicate_operator_means$predicate)
levels(predicate_operator_means$predicate)


# join results of statistical analysis with predicate operator means
# contrasts = read_csv("../data/contrasts.csv")
# view(contrasts)
# contrasts = contrasts %>%
#   select(-c(contrast)) %>%
#   select(c(expression,context,x,xend,y,yend,linetype)) %>%
#   mutate(expression = recode(expression,"be.annoyed" = "be annoyed", "be.right" = "be right")) %>%
#   filter(linetype != 0)
# contrasts$context = factor(contrasts$context, levels = c("explicitIgnorance", "factL", "factH"))
# contrasts$expression = as.factor(contrasts$expression)
# contrasts$linetype <- as.integer(contrasts$linetype)
# 
# contrasts$expression = factor(contrasts$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)

contrasts <- contrasts %>% mutate(y = -y, yend = -yend)

# plot

predicate_operator_means %>%
  # mutate(predicate = fct_reorder(predicate, projection, .fun = "mean")) %>%
  # ggplot(aes(x = fct_reorder(operator, projection, .fun = "mean"), y = projection)) +
  # geom_violin(data = data, 
  #             aes(color = fct_reorder(operator, projection, .fun = "mean")), 
  #             scale="width", alpha = .4) +
  # geom_point(shape = 21, stroke = .5, size = .5, color = "black") +
  # geom_errorbar(aes(ymin = YMin, ymax = YMax), 
  #               width = 0.5, color = "black", alpha = .6) +
  # scale_y_continuous(limits = c(-.25,1),
  #                    breaks = c(0,0.2,0.4,0.6,0.8,1.0),
  #                    labels = c("0",".2",".4",".6",".8","1"),
  #                    name = "Certainty rating") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(legend.position="top") +
  guides(linetype = "none") +
  # facet_wrap2(. ~ fct_reorder(predicate, projection, .fun = "mean"), nrow = 4) +
  facet_wrap(~ predicate) +
  geom_segment(data = contrasts, aes(group = predicate, 
                                     x = x,
                                     xend = xend, y = y, 
                                     yend=yend, 
                                     linetype = as.factor(linetype))) +
  # xlab("Operator") +
  # scale_shape_manual(values = c("M", "N", "Q", "C") +
  scale_colour_manual(values = cbbPalette, name = "Operator") +
  # scale_x_discrete(breaks = NULL) +
  # theme(axis.text.x = element_blank(),
  #         # element_text(size = 8, angle = 45, hjust = 1), 
  #       legend.position = "bottom")

# color by factive / non-factive
# 
# fill.color <- ifelse(levels(nat.means$expression) %in% factives, '#D55E00', "#009E73")
# fill.color
# 
# # to color the facets differently
# library(ggh4x)

# strip <- strip_themed(background_x = elem_list_rect(fill = fill.color))

# violinplot
ggplot() +
  geom_violin(data = data,
              aes(x = fct_reorder(operator, projection, .fun = "mean"),
                  y = projection,
                  fill = fct_reorder(operator, projection, .fun = "mean")), 
              scale="width", linewidth = 0) +
  geom_point(data = predicate_operator_means,
             aes(x = fct_reorder(operator, projection, .fun = "mean"),
                 y = projection,
                 fill = fct_reorder(operator, projection, .fun = "mean")), 
             shape=21,stroke=.5,size=2, color="black") +
  geom_errorbar(data = predicate_operator_means,
                aes(x = fct_reorder(operator, projection, .fun = "mean"),
                    ymin = YMin, ymax = YMax), 
                width=0.1, color="black") +
  ##
  scale_fill_manual(values = cbbPalette,
                    name = "Operator") +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(limits = c(-.25,1), breaks = seq(0,1,.2), labels = c("0",".2",".4",".6",".8","1")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(legend.position="top") +
  guides(linetype = "none") +
  ylab("Mean certainty rating") +
  # xlab("Context") +
  facet_wrap2(. ~ fct_reorder(predicate, projection, .fun = "mean"), nrow = 4) + #, strip = strip) +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "black")) +
  geom_segment(data = contrasts, aes(x=x,xend=xend,y=y,yend=yend, linetype = as.factor(linetype)))

ggsave("../graphs/naturalness-by-context-and-predicate-with-stats.pdf",height=4.5,width=9)