library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(nnet)
library(marginaleffects)

filepath = "~/Dropbox/SurveyTests/SurveyExperiment/replication_folder"
plotpath = "~/Dropbox/SurveyTests/SurveyExperiment/replication_folder"

survey_data <- read_csv("~/Dropbox/SurveyTests/SurveyExperiment/replication_folder/survey_data.csv")

survey_data$QRenew <- relevel(factor(survey_data$QRenew, levels = c("1", "2", "3"), 
                                                 labels = c("SupportR", "OpposeR", "NeitherR")),ref="NeitherR")

survey_data$Q39 <- relevel(factor(survey_data$Q39, levels = c("1", "2", "3"), 
                                              labels = c("SupportB", "OpposeB", "NeitherB")),ref="NeitherB")

survey_data$Q38 <- relevel(factor(survey_data$Q38, levels = c("1", "2", "3"), 
                                              labels = c("Fully Believe", "Somewhat Believe", "Do Not Believe")),ref="Somewhat Believe")

# Create the intervention treatments
survey_data$treat1 <- if_else(survey_data$energy_policy_rand==1,1,0)
survey_data$treat2 <- if_else(survey_data$energy_policy_rand==2,1,0)
survey_data$treat3 <- if_else(survey_data$energy_policy_rand==3,1,0)
survey_data$treat4 <- if_else(survey_data$energy_policy_rand==4,1,0)

# Create the survey_dataframe for the percentage
treat1df = survey_data %>% filter(treat1==1) %>% select(QRenew,Q39,Q38)

# Create a table for each variable
df_QRenew <- as.data.frame(table(treat1df$QRenew)*100/nrow(treat1df))
df_Q39 <- as.data.frame(table(treat1df$Q39)*100/nrow(treat1df))
df_Q38 <- as.data.frame(table(treat1df$Q38)*100/nrow(treat1df))

# Bind these together
treat1df = rbind(df_Q38,df_QRenew,df_Q39)
treat1df = treat1df %>% mutate(term = "treat1")

# Create the survey_dataframe for the percentage
treat2df = survey_data %>% filter(treat2==1) %>% select(QRenew,Q39,Q38)

# Create a table for each variable
df_QRenew <- as.data.frame(table(treat2df$QRenew)*100/nrow(treat2df))
df_Q39 <- as.data.frame(table(treat2df$Q39)*100/nrow(treat2df))
df_Q38 <- as.data.frame(table(treat2df$Q38)*100/nrow(treat2df))

# Bind these together
treat2df = rbind(df_Q38,df_QRenew,df_Q39)
treat2df = treat2df %>% mutate(term = "treat2")

# Create the survey_dataframe for the percentage
treat3df = survey_data %>% filter(treat3==1) %>% select(QRenew,Q39,Q38)

# Create a table for each variable
df_QRenew <- as.data.frame(table(treat3df$QRenew)*100/nrow(treat3df))
df_Q39 <- as.data.frame(table(treat3df$Q39)*100/nrow(treat3df))
df_Q38 <- as.data.frame(table(treat3df$Q38)*100/nrow(treat3df))

# Bind these together
treat3df = rbind(df_Q38,df_QRenew,df_Q39)
treat3df = treat3df %>% mutate(term = "treat3")

# Create the survey_dataframe for the percentage
treat4df = survey_data %>% filter(treat4==1) %>% select(QRenew,Q39,Q38)

# Create a table for each variable
df_QRenew <- as.data.frame(table(treat4df$QRenew)*100/nrow(treat4df))
df_Q39 <- as.data.frame(table(treat4df$Q39)*100/nrow(treat4df))
df_Q38 <- as.data.frame(table(treat4df$Q38)*100/nrow(treat4df))

# Bind these together
treat4df = rbind(df_Q38,df_QRenew,df_Q39)
treat4df = treat4df %>% mutate(term = "treat4")



# Add these into a moddata dataframe

moddata = rbind(treat1df,treat2df,treat3df,treat4df)
moddata = moddata %>% mutate(intervention = case_when(term=="treat1" ~ "(2)\nContext\nControl",
                                                      term=="treat2" ~ "(3)\nIncreased\n Emissions",
                                                      term=="treat3" ~ "(4)\nChinese\n Competition",
                                                      term=="treat4" ~ "(1)\nPure\nControl",
))



# Create the facet plot for the 5 labs
maxcoef = max(moddata$Freq)
mincoef = min(moddata$Freq)

# Create the facet grid
moddata = moddata %>% mutate(experiment=case_when(Var1 %in% c("Do Not Believe","Fully Believe","Somewhat Believe") ~ "Believe\nActivists' Claims",
                                                  Var1 %in% c("SupportB", "OpposeB", "NeitherB") ~ "Build\nNuclear Plants",
                                                  Var1 %in% c("SupportR", "OpposeR", "NeitherR") ~ "Renew\nPlant Licenses",
))
moddata$experiment = factor(moddata$experiment, levels=c("Believe\nActivists' Claims","Renew\nPlant Licenses", "Build\nNuclear Plants"))

# Create the colors for the plot

moddata = moddata %>% mutate(stance=case_when(Var1 %in% c("Do Not Believe","SupportR","SupportB") ~ "Pro-Nuclear",
                                              Var1 %in% c("Fully Believe", "OpposeR", "OpposeB") ~ "Anti-Nuclear",
                                              Var1 %in% c("Somewhat Believe", "NeitherR", "NeitherB") ~ "Ambivalent",
))

moddata$Var1 = factor(moddata$Var1, levels=c("Do Not Believe", "Somewhat Believe","Fully Believe" ,"SupportR", "NeitherR", "OpposeR", "SupportB", "NeitherB", "OpposeB"))


# Do this as confidence intervals



get_prop_ci <- function(data, responsevar) {
  # Count occurrences of each level
  summary_df <- data %>%
    filter(!is.na(.data[[responsevar]])) %>%
    group_by(response = .data[[responsevar]]) %>%
    summarise(
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      total = sum(count),
      prop_test = map2(count, total, ~ broom::tidy(prop.test(.x, .y)))
    ) %>%
    unnest_wider(prop_test) %>%
    select(response, estimate, conf.low, conf.high) %>%
    rename(Var1 = response)
  
  return(summary_df)
}


# Create a table for each variable
df_QRenew <- get_prop_ci(filter(survey_data, treat1 == 1), "QRenew") %>% mutate(term = "treat1")
df_Q39 <- get_prop_ci(filter(survey_data, treat1 == 1), "Q39") %>% mutate(term = "treat1")
df_Q38 <- get_prop_ci(filter(survey_data, treat1 == 1), "Q38") %>% mutate(term = "treat1")

# Bind these together
treat1df = rbind(df_Q38,df_QRenew,df_Q39)
treat1df = treat1df %>% mutate(term = "treat1")


# Create a table for each variable
df_QRenew <- get_prop_ci(filter(survey_data, treat2 == 1), "QRenew") %>% mutate(term = "treat2")
df_Q39 <- get_prop_ci(filter(survey_data, treat2 == 1), "Q39") %>% mutate(term = "treat2")
df_Q38 <- get_prop_ci(filter(survey_data, treat2 == 1), "Q38") %>% mutate(term = "treat2")


# Bind these together
treat2df = rbind(df_Q38,df_QRenew,df_Q39)
treat2df = treat2df %>% mutate(term = "treat2")


# Create a table for each variable
df_QRenew <- get_prop_ci(filter(survey_data, treat3 == 1), "QRenew") %>% mutate(term = "treat3")
df_Q39 <- get_prop_ci(filter(survey_data, treat3 == 1), "Q39") %>% mutate(term = "treat3")
df_Q38 <- get_prop_ci(filter(survey_data, treat3 == 1), "Q38") %>% mutate(term = "treat3")

# Bind these together
treat3df = rbind(df_Q38,df_QRenew,df_Q39)
treat3df = treat3df %>% mutate(term = "treat3")


# Create a table for each variable
df_QRenew <- get_prop_ci(filter(survey_data, treat4 == 1), "QRenew") %>% mutate(term = "treat4")
df_Q39 <- get_prop_ci(filter(survey_data, treat4 == 1), "Q39") %>% mutate(term = "treat4")
df_Q38 <- get_prop_ci(filter(survey_data, treat4 == 1), "Q38") %>% mutate(term = "treat4")

# Bind these together
treat4df = rbind(df_Q38,df_QRenew,df_Q39)
treat4df = treat4df %>% mutate(term = "treat4")

# Add these into a moddata dataframe

moddata_ci = rbind(treat1df,treat2df,treat3df,treat4df)
moddata = moddata %>% left_join(moddata_ci)
moddata = moddata %>% mutate(conf.low = 100*conf.low, conf.high = 100*conf.high)


# Create the facet plot for the 5 labs
maxcoef = max(moddata$conf.high)
mincoef = min(moddata$conf.low)

# Create the facet grid

pdf(file = file.path(plotpath,"Figure1.pdf"), width = 6.5,height = 7.5)
ggplot(moddata, aes(fill=stance, y=Var1, x=Freq)) + 
  geom_bar(position="stack", stat="identity") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = "identity", height = 0.2, color = "grey40") +  
  labs(y="",x="Percentage", fill = "")+ 
  scale_fill_manual(values = c("Ambivalent" = "grey", "Pro-Nuclear" = "red", "Anti-Nuclear" = "blue")) +
  geom_vline(xintercept = 50, lty=2)+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 270))+
  facet_grid(rows = vars(experiment), 
             cols = vars(intervention),
             scales = "free_y")
dev.off()


survey_data$age4 <- relevel(factor(survey_data$age4, levels = c("1", "2", "3", "4"), 
                                               labels = c("Under 30", "30-44", "45-64", "65+")),ref="Under 30")
survey_data$gender4 <- relevel(factor(survey_data$gender4, levels = c("1", "2", "3", "4"), 
                                                  labels = c("Man", "Woman", "NonBinary", "Other")),ref="Man")
survey_data$pid3 <- relevel(factor(survey_data$pid3, levels = c("1", "2", "3", "4","5"), 
                                               labels = c("Democrat", "Republican", "Independent", "Other", "Not sure")),ref="Republican")
survey_data$educ4 <- relevel(factor(survey_data$educ4, levels = c("1", "2", "3", "4"), 
                                                labels = c("HS or less","College (2yr)", "College (4yr)","Post-grad")),ref="HS or less")
survey_data$QTV <- relevel(factor(survey_data$QTV, levels = c("1", "2"), 
                                              labels = c("Risk-averse","Risk-neutral")),ref="Risk-neutral")
# survey_data <- survey_data %>% filter(faminc<97)

# Add a coding
survey_data <- survey_data %>% mutate(income = case_when(faminc %in% c(1,2,3,4,5)~1,
                                                                                 faminc %in% c(6,7,8,9)~2,
                                                                                 faminc %in% c(10,11)~3,
                                                                                 faminc %in% c(12,13,14,15,16)~4
))
survey_data$inc4 <- relevel(factor(survey_data$income, levels = c("1", "2", "3", "4"), 
                                               labels = c("Lower","Middle", "Middle-Upper","Upper")),ref="Lower")


modQBelievet1 <- multinom(Q38 ~ treat1 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(1,4)),  trace = FALSE)
QBelievet1 <- avg_comparisons(modQBelievet1, variables = "treat1", wts = modQBelievet1$weights)

modQBelievet2 <- multinom(Q38 ~ treat2 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(2,4)),  trace = FALSE)
QBelievet2 <- avg_comparisons(modQBelievet2, variables = "treat2", wts = modQBelievet2$weights)

modQBelievet3 <- multinom(Q38 ~ treat3 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(3,4)),  trace = FALSE)
QBelievet3 <- avg_comparisons(modQBelievet3, variables = "treat3", wts = modQBelievet3$weights)


modQRenewt1 <- multinom(QRenew ~ treat1 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(1,4)), weights = weight, trace = FALSE)
QRenewt1 <- avg_comparisons(modQRenewt1, variables = "treat1", wts = modQRenewt1$weights)

modQRenewt2 <- multinom(QRenew ~ treat2 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(2,4)), weights = weight, trace = FALSE)
QRenewt2 <- avg_comparisons(modQRenewt2, variables = "treat2", wts = modQRenewt2$weights)

modQRenewt3 <- multinom(QRenew ~ treat3 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(3,4)),weights = weight,  trace = FALSE)
QRenewt3 <- avg_comparisons(modQRenewt3, variables = "treat3", wts = modQRenewt3$weights)


modQBuildt1 <- multinom(Q39 ~ treat1 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(1,4)), weights = weight, trace = FALSE)
QBuildt1 <- avg_comparisons(modQBuildt1, variables = "treat1", wts = modQBuildt1$weights)

modQBuildt2 <- multinom(Q39 ~ treat2 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(2,4)), weights = weight, trace = FALSE)
QBuildt2 <- avg_comparisons(modQBuildt2, variables = "treat2", wts = modQBuildt2$weights)

modQBuildt3 <- multinom(Q39 ~ treat3 + age4 + gender4 + pid3 + educ4, data = survey_data %>% filter(energy_policy_rand %in% c(3,4)), weights = weight, trace = FALSE)
QBuildt3 <- avg_comparisons(modQBuildt3, variables = "treat3", wts = modQBuildt3$weights)

# Bring to one data frame

moddata = rbind(QBelievet1,QBelievet2,QBelievet3,QRenewt1,QRenewt2,QRenewt3,QBuildt1,QBuildt2,QBuildt3)

moddata = moddata %>% mutate(intervention = case_when(term=="treat1" ~ "(2)\nContext\nControl",
                                                      term=="treat2" ~ "(3)\nIncreased\n Emissions",
                                                      term=="treat3" ~ "(4)\nChinese\n Competition"
))



# Create the facet plot for the 5 labs
maxcoef = max(moddata$conf.high)
mincoef = min(moddata$conf.low)

# Create the facet grid
moddata = moddata %>% mutate(experiment=case_when(group %in% c("Do Not Believe","Fully Believe","Somewhat Believe") ~ "Believe\nActivists' Claims",
                                                  group %in% c("SupportB", "OpposeB", "NeitherB") ~ "Build\nNuclear Plants",
                                                  group %in% c("SupportR", "OpposeR", "NeitherR") ~ "Renew\nPlant Licenses",
))
moddata$experiment = factor(moddata$experiment, levels=c("Believe\nActivists' Claims","Renew\nPlant Licenses", "Build\nNuclear Plants"))

moddata$group = factor(moddata$group, levels=c("Do Not Believe", "Somewhat Believe","Fully Believe" ,"SupportR", "NeitherR", "OpposeR", "SupportB", "NeitherB", "OpposeB"))

moddata = moddata %>% mutate(stance=case_when(group %in% c("Do Not Believe","SupportR","SupportB") ~ "Pro-Nuclear",
                                              group %in% c("Fully Believe", "OpposeR", "OpposeB") ~ "Anti-Nuclear",
                                              group %in% c("Somewhat Believe", "NeitherR", "NeitherB") ~ "Ambivalent",
))

moddata = moddata %>%
  mutate(signif = case_when(conf.high<0 ~ "neg",
                            conf.low>0 ~ "pos",
                            .default = "nosig"))

moddata = moddata %>% mutate(stance= if_else(signif=="nosig","Ambivalent",stance))
moddata = moddata %>% mutate(stance= if_else(signif=="neg" & stance=="Pro-Nuclear","Anti-Nuclear",stance))
# moddata = moddata %>% mutate(stance= if_else(signif=="neg" & stance=="Anti-Nuclear","Pro-Nuclear",stance))

pdf(file = file.path(plotpath,"Figure2.pdf"), width = 6.5,height = 7.5)
ggplot(data=moddata, aes(x=estimate,y=group, colour=stance)) +
  geom_point()+
  geom_vline(xintercept = 0, lty=2)+
  xlim(mincoef,maxcoef) +
  labs(x = "Average marginal effect", y="")+
  scale_color_manual(values = c("Ambivalent" = "grey40", "Pro-Nuclear" = "red", "Anti-Nuclear" = "blue")) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_errorbar(width=.1, aes(xmin=conf.low, xmax=conf.high)) +
  facet_grid(rows = vars(experiment), 
             cols = vars(intervention),
             scales = "free_y")
dev.off()


modQBelieve <- multinom(Q38 ~ age4 + gender4 + pid3 + educ4 +  QTV, data = survey_data %>% filter(energy_policy_rand %in% c(4)), weights=weight, trace = FALSE)
QBelieve <- as.data.frame(avg_comparisons(modQBelieve, wts = modQBelieve$weights))

modQRenew <- multinom(QRenew ~ age4 + gender4 + pid3 + educ4 +  QTV, data = survey_data %>% filter(energy_policy_rand %in% c(4)), weights=weight, trace = FALSE)
QRenew <- as.data.frame(avg_comparisons(modQRenew, wts = modQRenew$weights))

modQBuild <- multinom(Q39 ~  age4 + gender4 + pid3 + educ4 +  QTV, data = survey_data %>% filter(energy_policy_rand %in% c(4)), weights=weight, trace = FALSE)
QBuild <- as.data.frame(avg_comparisons(modQBuild, wts = modQBuild$weights))

moddata = rbind(QBelieve,QBuild,QRenew)

moddata = moddata %>% separate_wider_delim(contrast, " - ", names = c("comp", "ref"))

# Trim names
moddata = moddata %>% mutate(comp = substr(comp,6,nchar(comp)-1))
moddata = moddata %>% mutate(ref = substr(ref,6,nchar(ref)-1))

# Drop categories with names
moddata = moddata %>% filter(!(comp %in% c("NA", "Other", "Not sure", "NonBinary")))

# # Create an indicator for significance
moddata = moddata %>%
  mutate(signif = case_when(conf.high<0 ~ "neg",
                            conf.low>0 ~ "pos",
                            .default = "nosig"))


moddata = moddata %>% mutate(groupref = case_when(ref=="Under 30" ~ "Age \n (Under 30)",
                                                  ref=="HS or less" ~ "Education \n (HS or less)",
                                                  ref=="Man" ~ "Gender \n(Male)",
                                                  ref=="Republican" ~ "Party \n (Republican)",
                                                  ref=="Risk-neutral" ~ "Risk \n(Seeking)"
))


# Create the facet plot for the 5 labs
maxcoef = max(moddata$conf.high)
mincoef = min(moddata$conf.low)

# 
datain = moddata %>% filter(group %in% c("Do Not Believe","SupportR","SupportB"))
datain$group = factor(datain$group, levels=c("Do Not Believe","SupportR","SupportB"))


pdf(file = file.path(plotpath,"Figure3.pdf"), width = 6,height = 7)
ggplot(data=datain, aes(x=estimate,y=comp, colour=signif)) +
  geom_point()+
  geom_vline(xintercept = 0, lty=2)+
  xlim(mincoef,maxcoef) +
  labs(x = "Average marginal effect", y="")+
  scale_color_manual(values=c("blue","grey60", "red"))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_errorbar(width=.1, aes(xmin=conf.low, xmax=conf.high)) +
  facet_grid(rows = vars(groupref), 
             cols = vars(group),
             scales = "free_y")
dev.off()



modQBelievet1 <- multinom(Q38 ~ treat1*age4 + treat1*gender4 +  treat1*pid3 + treat1*educ4 +  treat1*QTV , data = survey_data %>% filter(energy_policy_rand %in% c(1,4)), weights=weight, trace = FALSE)
modQBelievet2 <- multinom(Q38 ~ treat2*age4 + treat2*gender4 +  treat2*pid3 + treat2*educ4 +  treat2*QTV , data = survey_data %>% filter(energy_policy_rand %in% c(2,4)), weights=weight, trace = FALSE)
modQBelievet3 <- multinom(Q38 ~ treat3*age4 + treat3*gender4 +  treat3*pid3 + treat3*educ4 +  treat3*QTV , data = survey_data %>% filter(energy_policy_rand %in% c(3,4)), weights=weight, trace = FALSE)

modQRenewt1 <- multinom(QRenew ~ treat1*age4 + treat1*gender4 +  treat1*pid3 + treat1*educ4 +  treat1*QTV, data = survey_data %>% filter(energy_policy_rand %in% c(1,4)), weights=weight, trace = FALSE)
modQRenewt2 <- multinom(QRenew ~ treat2*age4 + treat2*gender4 +  treat2*pid3 + treat2*educ4 +  treat2*QTV, data = survey_data %>% filter(energy_policy_rand %in% c(2,4)), weights=weight, trace = FALSE)
modQRenewt3 <- multinom(QRenew ~ treat3*age4 + treat3*gender4 +  treat3*pid3 + treat3*educ4 +  treat3*QTV, data = survey_data %>% filter(energy_policy_rand %in% c(3,4)), weights=weight, trace = FALSE)

modQBuildt1 <- multinom(Q39 ~ treat1*age4 + treat1*gender4 +  treat1*pid3 + treat1*educ4 +  treat1*QTV, data = survey_data %>% filter(energy_policy_rand %in% c(1,4)), weights=weight, trace = FALSE)
modQBuildt2 <- multinom(Q39 ~ treat2*age4 + treat2*gender4 +  treat2*pid3 + treat2*educ4 +  treat2*QTV, data = survey_data %>% filter(energy_policy_rand %in% c(2,4)), weights=weight, trace = FALSE)
modQBuildt3 <- multinom(Q39 ~ treat3*age4 + treat3*gender4 +  treat3*pid3 + treat3*educ4 +  treat3*QTV, data = survey_data %>% filter(energy_policy_rand %in% c(3,4)), weights=weight, trace = FALSE)


# Taking average contrasts for building
QBuildt1 = list()
for (s in c("age4", "gender4", "pid3", "educ4", "QTV")){
  for (e in levels(survey_data %>% filter(treat1==1) %>% pull(s))){
    dataplug = survey_data %>% filter(energy_policy_rand %in% c(1,4)) %>% filter(!!as.symbol(s)==e)
    test1 = avg_comparisons(modQBuildt1, 
                            variables = "treat1",
                            newdata = dataplug)
    QBuildt1[[paste0(s,e)]] = test1 %>% mutate(term = s, subgroup = e)
  }
}
QBuildt1 = bind_rows(QBuildt1)


QBuildt2 = list()
for (s in c("age4", "gender4", "pid3", "educ4", "QTV")){
  for (e in levels(survey_data %>% filter(treat1==2) %>% pull(s))){
    dataplug = survey_data %>% filter(energy_policy_rand %in% c(2,4)) %>% filter(!!as.symbol(s)==e)
    test1 = avg_comparisons(modQBuildt2, 
                            variables = "treat2",
                            newdata = dataplug)
    QBuildt2[[paste0(s,e)]] = test1 %>% mutate(term = s, subgroup = e)
  }
}
QBuildt2 = bind_rows(QBuildt2)


QBuildt3 = list()
for (s in c("age4", "gender4", "pid3", "educ4", "QTV")){
  for (e in levels(survey_data %>% filter(treat1==3) %>% pull(s))){
    dataplug = survey_data %>% filter(energy_policy_rand %in% c(3,4)) %>% filter(!!as.symbol(s)==e)
    test1 = avg_comparisons(modQBuildt3, 
                            variables = "treat3",
                            newdata = dataplug)
    QBuildt3[[paste0(s,e)]] = test1 %>% mutate(term = s, subgroup = e)
  }
}
QBuildt3 = bind_rows(QBuildt3)


# Chain these together
moddata = bind_rows(QBuildt1 %>% mutate(tgp="treat1"),QBuildt2 %>% mutate(tgp="treat2"),QBuildt3 %>% mutate(tgp="treat3"))

# Drop categories with names
moddata = moddata %>% filter(!(subgroup %in% c("NA", "Other", "Not sure", "NonBinary")))

# Relabel the interventions
moddata = moddata %>% mutate(intervention = case_when(tgp=="treat1" ~ "(2)\nContext\nControl",
                                                      tgp=="treat2" ~ "(3)\nIncreased\n Emissions",
                                                      tgp=="treat3" ~ "(4)\nChinese\n Competition"
))

moddata = moddata %>% mutate(stance=case_when(group %in% c("Do Not Believe","SupportR","SupportB") ~ "Pro-Nuclear",
                                              group %in% c("Fully Believe", "OpposeR", "OpposeB") ~ "Anti-Nuclear",
                                              group %in% c("Somewhat Believe", "NeitherR", "NeitherB") ~ "Ambivalent",
))


# # Create an indicator for significance
moddata = moddata %>%
  mutate(signif = case_when(conf.high<0 ~ "neg",
                            conf.low>0 ~ "pos",
                            .default = "nosig"))

moddata = moddata %>% mutate(stance= if_else(signif=="nosig","Ambivalent",stance))
moddata = moddata %>% mutate(stancechange = if_else(signif=="neg" & stance=="Pro-Nuclear",1,0))
moddata = moddata %>% mutate(stance= if_else(signif=="neg" & stance=="Pro-Nuclear","Anti-Nuclear",stance))
moddata = moddata %>% mutate(stance= if_else(signif=="neg" & stance=="Anti-Nuclear","Pro-Nuclear",stance))


moddata = moddata %>% mutate(termlabel = case_when(term=="age4" ~ "Age",
                                                   term=="educ4" ~ "Education",
                                                   term=="gender4" ~ "Gender",
                                                   term=="pid3" ~ "Party",
                                                   term=="QTV" ~ "Risk \nTolerance"
))

moddataBuild = moddata
moddataBuild$subgroup = factor(moddataBuild$subgroup, levels=c("65+", "45-64", "30-44", "Under 30","Post-grad", "College (4yr)", "College (2yr)", "HS or less", "Woman", "Man", "Republican", "Independent", "Democrat", "Risk-neutral", "Risk-averse"))
moddataBuild$subgroup = if_else(moddataBuild$subgroup=="Risk-neutral","Risk-seeking",moddataBuild$subgroup)

# Create the facet plot for the 5 labs
maxcoef = max(moddataBuild$conf.high)
mincoef = min(moddataBuild$conf.low)



# The Support Build data
datain = moddataBuild %>% filter(group=="SupportB")


pdf(file = file.path(plotpath,"Figure4.pdf"), width = 6.5,height = 7.5)
ggplot(data=datain, aes(x=estimate,y=subgroup, colour=stance)) +
  geom_point()+
  geom_vline(xintercept = 0, lty=2)+
  xlim(mincoef,maxcoef) +
  labs(x = "Average marginal effect", y="")+
  scale_color_manual(values = c("Ambivalent" = "grey70", "Pro-Nuclear" = "red", "Anti-Nuclear" = "blue")) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_errorbar(width=.1, aes(xmin=conf.low, xmax=conf.high)) +
  facet_grid(rows = vars(termlabel), 
             cols = vars(intervention),
             scales = "free_y")
dev.off()
