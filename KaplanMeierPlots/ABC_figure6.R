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

## Read in raw data

data <- read.csv("ABC_data.csv")

data <- data %>%
  group_by(id, visit) %>%
  mutate(symptoms = ifelse((persistent_cough+weight_loss+night_sweats+fever)>0, 1, 0)) %>%
  ungroup()

data <- data %>%
  mutate(treatment= ifelse(group=="C","Untreated (Grp C)", "Treated (Grp A & B)"),
         hiv_status = ifelse(hiv_status=="Positive", "PLHIV","HIV Negative"),
         previous_history_of_tb = ifelse(previous_history_of_tb=="Yes","Previous TB", "No Previous TB"))



### Day of first visit at which Mtb is not detected - considered the time to clearance
data_clearance <- data[which(data$symptoms==0), c("id", "days", "symptoms")]
data_clearance <- data_clearance %>%
  group_by(id) %>%
  summarise(days=first(days),
            symptoms=first(symptoms))

### Identifies patients without clearance - i.e. censored
data_censored <- data %>%
  group_by(id) %>%
  summarise(days=last(days),
            symptoms=last(symptoms))
data_censored <- data_censored[which(data_censored$symptoms==1),]

### Removes patients who cleared and then rebounded by the final visit
data_censored <- data_censored[ !data_censored$id %in% data_clearance$id, ]

### Combines cleared and censored patients
data_survival <- rbind(data_clearance, data_censored)

### Combine with variables for KM plot
vars <- data %>%
  #drop_na(previous_history_of_tb) %>%
  distinct(id, treatment, hiv_status, previous_history_of_tb)

vars <- vars %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data_survival <- full_join(data_survival, vars, by="id")

### Recode symptoms for event (loss of symptoms) = 1, censoring = 0
data_survival <- data_survival %>% 
  mutate(status = recode(symptoms, `0` = 1, `1` = 0))

### Create survfit objects 
surv6A = survfit(Surv(days, status) ~ 1, data = data_survival)
surv6B = survfit(Surv(days, status) ~ treatment, data = data_survival)
surv6C = survfit(Surv(days, status) ~ previous_history_of_tb, data = data_survival)
surv6D = survfit(Surv(days, status) ~ hiv_status, data = data_survival)

### log-rank p-values
p_value_6B <- glance(survdiff(Surv(days, status) ~ treatment, data = data_survival))$p.value
p_value_6C <- glance(survdiff(Surv(days, status) ~ previous_history_of_tb, data = data_survival))$p.value
p_value_6D <- glance(survdiff(Surv(days, status) ~ hiv_status, data = data_survival))$p.value

### Plot survival curves

fig6A <- autoplot(surv6A, 
                  conf.int.colour = "#440154FF", 
                  conf.int.fill = "#440154FF", 
                  conf.int.alpha = 0.2) + 
  labs(x = "\nTime (Days)", y = "Proportion with any TB symptom") + 
  theme_fig()

ggsave("Figure6A.jpeg", plot = fig6A, width = 4, height = 2.7)


fig6B <- autoplot(surv6B, conf.int.alpha = 0.2) + 
  scale_fill_manual(values=c("#440154FF", "#74D055FF"))+
  scale_colour_manual(values=c("#440154FF", "#74D055FF"))+
  labs(x = "\nTime (Days)", y = "Proportion with any TB symptom")+
  annotate(geom="text",label=paste("p = ", round_tidy(p_value_6B, 3)),
           x=230,y=0.95,size=4, fontface="bold")+
  theme_fig()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face="bold", size=8))

ggsave("Figure6B.jpeg", plot = fig6B, width = 4, height = 3)


fig6C <- autoplot(surv6C, conf.int.alpha = 0.2) + 
  scale_fill_manual(values=c("#440154FF", "#74D055FF"))+
  scale_colour_manual(values=c("#440154FF", "#74D055FF"))+
  labs(x = "\nTime (Days)", y = "Proportion with any TB symptom")+
  annotate(geom="text",label=paste("p = ", round_tidy(p_value_6C,3)),x=230,y=0.95,size=4, fontface="bold")+
  theme_fig()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face="bold", size=8))

ggsave("Figure6C.jpeg", plot = fig6C, width = 4, height = 3)


fig6D <- autoplot(surv6D, conf.int.alpha = 0.2) + 
  scale_fill_manual(values=c("#440154FF", "#74D055FF"))+
  scale_colour_manual(values=c("#440154FF", "#74D055FF"))+
  labs(x = "\nTime (Days)", y = "Proportion with any TB symptom")+
  annotate(geom="text",label=paste("p = ", round_tidy(p_value_6D,3)),x=230,y=0.95,size=4, fontface="bold")+
  theme_fig()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face="bold", size=8))

ggsave("Figure6D.jpeg", plot = fig6D, width = 4, height = 3)


