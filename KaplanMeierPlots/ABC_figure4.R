
# libraries

library(survival)
library(ggfortify)
library(ggplot2)
library(dplyr)
library(broom)
library(finalfit)

# ggplot theme

theme_fig<- function () {
  theme_minimal() %+replace%
    theme(text = element_text(size = 10),
          panel.grid.minor = element_line(colour="grey95"),
          panel.grid.major = element_line(colour="grey90"),
          panel.background = element_rect(colour="grey90", fill="grey98"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold")
    )
}

# Read in raw data

data <-  read.csv("ABC_data.csv")


# Create treatment variable

data <- data %>%
  mutate(treatment= ifelse(group=="C","Untreated (Grp C)", "Treated (Grp A & B)"))



# Individual symptom Kaplan-Meier plots: Persistent Cough
# Find the day of first visit without persistent cough - time to clearance

data_clearance <- data[data$persistent_cough==0, c("id", "days", "persistent_cough")]
data_clearance <- data_clearance %>%
  group_by(id) %>%
  summarise(days=first(days),
            persistent_cough=first(persistent_cough))


# Identifies patients who continue to have persistent cough throughout

data_censored <- data %>%
  group_by(id) %>%
  summarise(days=last(days),
            persistent_cough=last(persistent_cough))
data_censored <- data_censored[data_censored$persistent_cough==1,]


# Removes patients who cleared and then rebounded by the final visit

data_censored <- data_censored[ !data_censored$id %in% data_clearance$id, ]


# Combines cleared and censored patients

data_survival <- rbind(data_clearance, data_censored)


# Combines with treatment variable

vars <- data %>%
  distinct(id, treatment)
data_survival <- full_join(data_survival, vars, by="id")


# Recode for loss of symptom i.e. event = 1, censoring = 0

data_survival <- 
  data_survival %>% 
  mutate(status = recode(persistent_cough, `0` = 1, `1` = 0))


# Create survfit object 

surv4A = survfit(Surv(days, status) ~ treatment, data = data_survival)


# log-rank p-value

p_value_4A <- glance(survdiff(Surv(days, status) ~ treatment, data = data_survival))$p.value


# Plot survival curves

fig4A <- autoplot(surv4A, conf.int.alpha = 0.2) + 
  scale_fill_manual(values=c("#440154FF", "#74D055FF"))+
  scale_colour_manual(values=c("#440154FF", "#74D055FF"))+
  labs(x = "\nTime (Days)", y = "Proportion with persistent cough")+
  annotate(geom="text",label=paste("p = ", round_tidy(p_value_4A, 3)),
           x=230,y=0.95,size=4, fontface="bold")+
  theme_fig()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face="bold", size=8))





# Individual symptom Kaplan-Meier plots: Fever
# Find the day of first visit without fever - time to clearance

data_clearance <- data[data$fever==0, c("id", "days", "fever")]
data_clearance <- data_clearance %>%
  group_by(id) %>%
  summarise(days=first(days),
            fever=first(fever))


# Identifies patients who continue to have persistent cough throughout

data_censored <- data %>%
  group_by(id) %>%
  summarise(days=last(days),
            fever=last(fever))
data_censored <- data_censored[data_censored$fever==1,]


# Removes patients who cleared and then rebounded by the final visit

data_censored <- data_censored[ !data_censored$id %in% data_clearance$id, ]


# Combines cleared and censored patients

data_survival <- rbind(data_clearance, data_censored)


# Combines with treatment variable

vars <- data %>%
  distinct(id, treatment)
data_survival <- full_join(data_survival, vars, by="id")


# Recode for loss of symptom i.e. event = 1, censoring = 0

data_survival <- 
  data_survival %>% 
  mutate(status = recode(fever, `0` = 1, `1` = 0))


# Create survfit object 

surv4B = survfit(Surv(days, status) ~ treatment, data = data_survival)


# log-rank p-value

p_value_4B <- glance(survdiff(Surv(days, status) ~ treatment, data = data_survival))$p.value


# Plot survival curves

fig4B <- autoplot(surv4A, conf.int.alpha = 0.2) + 
  scale_fill_manual(values=c("#440154FF", "#74D055FF"))+
  scale_colour_manual(values=c("#440154FF", "#74D055FF"))+
  labs(x = "\nTime (Days)", y = "Proportion with fever")+
  annotate(geom="text",label=paste("p = ", round_tidy(p_value_4B, 3)),
           x=230,y=0.95,size=4, fontface="bold")+
  theme_fig()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face="bold", size=8))





# Individual symptom Kaplan-Meier plots: Weight Loss
# Find the day of first visit without subjective weight loss - time to clearance

data_clearance <- data[data$weight_loss==0, c("id", "days", "weight_loss")]
data_clearance <- data_clearance %>%
  group_by(id) %>%
  summarise(days=first(days),
            weight_loss=first(weight_loss))


# Identifies patients who continue to have persistent cough throughout

data_censored <- data %>%
  group_by(id) %>%
  summarise(days=last(days),
            weight_loss=last(weight_loss))
data_censored <- data_censored[data_censored$weight_loss==1,]


# Removes patients who cleared and then rebounded by the final visit

data_censored <- data_censored[ !data_censored$id %in% data_clearance$id, ]


# Combines cleared and censored patients

data_survival <- rbind(data_clearance, data_censored)


# Combines with treatment variable

vars <- data %>%
  distinct(id, treatment)
data_survival <- full_join(data_survival, vars, by="id")


# Recode for loss of symptom i.e. event = 1, censoring = 0

data_survival <- 
  data_survival %>% 
  mutate(status = recode(weight_loss, `0` = 1, `1` = 0))


# Create survfit object 

surv4C = survfit(Surv(days, status) ~ treatment, data = data_survival)


# log-rank p-value

p_value_4C <- glance(survdiff(Surv(days, status) ~ treatment, data = data_survival))$p.value


# Plot survival curves

fig4C <- autoplot(surv4C, conf.int.alpha = 0.2) + 
  scale_fill_manual(values=c("#440154FF", "#74D055FF"))+
  scale_colour_manual(values=c("#440154FF", "#74D055FF"))+
  labs(x = "\nTime (Days)", y = "Proportion with subjective weight loss")+
  annotate(geom="text",label=paste("p = ", round_tidy(p_value_4C, 3)),
           x=230,y=0.95,size=4, fontface="bold")+
  theme_fig()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face="bold", size=8))





# Individual symptom Kaplan-Meier plots: Night Sweats
# Find the day of first visit without night sweats - time to clearance

data_clearance <- data[data$night_sweats==0, c("id", "days", "night_sweats")]
data_clearance <- data_clearance %>%
  group_by(id) %>%
  summarise(days=first(days),
            night_sweats=first(night_sweats))


# Identifies patients who continue to have persistent cough throughout

data_censored <- data %>%
  group_by(id) %>%
  summarise(days=last(days),
            night_sweats=last(night_sweats))
data_censored <- data_censored[data_censored$night_sweats==1,]


# Removes patients who cleared and then rebounded by the final visit

data_censored <- data_censored[ !data_censored$id %in% data_clearance$id, ]


# Combines cleared and censored patients

data_survival <- rbind(data_clearance, data_censored)


# Combines with treatment variable

vars <- data %>%
  distinct(id, treatment)
data_survival <- full_join(data_survival, vars, by="id")


# Recode for loss of symptom i.e. event = 1, censoring = 0

data_survival <- 
  data_survival %>% 
  mutate(status = recode(night_sweats, `0` = 1, `1` = 0))


# Create survfit object 

surv4D = survfit(Surv(days, status) ~ treatment, data = data_survival)


# log-rank p-value

p_value_4D <- glance(survdiff(Surv(days, status) ~ treatment, data = data_survival))$p.value


# Plot survival curves

fig4D <- autoplot(surv4D, conf.int.alpha = 0.2) + 
  scale_fill_manual(values=c("#440154FF", "#74D055FF"))+
  scale_colour_manual(values=c("#440154FF", "#74D055FF"))+
  labs(x = "\nTime (Days)", y = "Proportion with night sweats")+
  annotate(geom="text",label=paste("p = ", round_tidy(p_value_4D, 3)),
           x=200,y=0.95,size=4, fontface="bold")+
  theme_fig()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face="bold", size=8))

