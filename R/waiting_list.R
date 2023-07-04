library(ggplot2)
library(gridExtra)
lung_sheet <- readxl::read_xlsx(path="WaitingList_Outcomes (2015-2021).xlsx", sheet=1)
heart_sheet <- readxl::read_xlsx(path="WaitingList_Outcomes (2015-2021).xlsx", sheet=2)
identical(names(lung_sheet), names(heart_sheet))
df <- rbind(lung_sheet, heart_sheet)
df <- df[grepl("heart", df$Organ, ignore.case=TRUE), ] # remove transplants not involving hearts
head(df)
table(df$Outcome)
table(df$Urgency)

df$`Days from W/List to death` <- df$`Date of Death` - df$`Accepted to W/List`
df$Outcome.bin <- df$Outcome
df$Outcome.bin[df$Outcome.bin!="Transplanted"] <- "Not transplanted"

#### Super Urgent ####
super <- df[df$Urgency=="Super Urgent", ]
table(super$Outcome)
table(super$`Clinical Status`)

urgency.outcome.bar <- ggplot(df, aes(x=Urgency, fill=Outcome)) +
  geom_bar(position="stack")+
  labs(title="Outcome for all patients")+
  scale_x_discrete(labels=c("Super Urgent", "Urgent", "Other"))+
  theme(legend.position="bottom")


urgency.death.bar <- ggplot(df, aes(x=Urgency, fill=`Clinical Status`)) +
  geom_bar(position="stack")+
    labs(title="Clinical status for all patients")+
      scale_x_discrete(labels=c("Super Urgent", "Urgent", "Other"))+
  theme(legend.position="bottom")

urgency.outcome.dead.bar <- ggplot(df[df$`Clinical Status`=="Dead", ], aes(x=Urgency, fill=Outcome)) +
  geom_bar(position="stack")+
    labs(title="Outcome for dead patients")+
    scale_x_discrete(labels=c("Super Urgent", "Urgent", "Other"))+
  theme(legend.position="bottom")

# Death data for (dead) patients who did / did not receive a transplant
urgency.outcome.bin.dead.noTx.bar <- ggplot(df[df$`Clinical Status`=="Dead" & df$Outcome!="Transplanted", ], aes(x=Urgency, fill=Outcome)) +
  geom_bar(position="stack")+
    labs(title="Outcome for dead patients w/o transplant")+
    scale_x_discrete(labels=c("Super Urgent", "Urgent", "Other"))+
  theme(legend.position="bottom")

combined.plots <- grid.arrange(urgency.outcome.bar,
                               urgency.death.bar,
                               urgency.outcome.dead.bar, 
                               urgency.outcome.bin.dead.noTx.bar,
                               ncol=2)
#ggsave(filename="morgan_urgency.png", plot=combined.plots, device="png", path="figs", width=10, height=8)

surv.time.box <- ggplot(df[df$`Clinical Status`=="Dead", ], aes(x=Outcome.bin, y=`Days from W/List to death`))+
  geom_boxplot(aes(fill=Outcome.bin))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3)+
  theme(legend.position="none")+
  labs(x="Outcome (binary)",
       title="Number of days from waiting list acceptance to death")
#ggsave(filename="survival_time.png", plot=surv.time.box, device="png", path="figs", width=5, height=6)


tapply(df$`Days from W/List to death`, df$Outcome.bin, median, na.rm=T)

sum(df$`Days from W/List to death`[df$Outcome!="Transplanted"] < 30, na.rm=TRUE)
length(df$`Days from W/List to death`[df$Outcome!="Transplanted"])

sum(df$`Days from W/List to death`[df$Outcome=="Transplanted"] < 30, na.rm=TRUE)
length(df$`Days from W/List to death`[df$Outcome=="Transplanted"])

# Subset: removed or died within 30 days
table(df$`W/List Status`)
df$`Days from W/List to death`[df$`W/List Status`=="Removed - Died waiting for transplant"]
df$`Days from W/List to death`[df$`W/List Status` %in% c("Removed - Died waiting for transplant", "Removed - Health deteriorated", "Removed - Health improved", "Removed - At patient's request")]

# Look only at transplant patients:
tx <- df[df$Outcome=="Transplanted", ]
summary(tx)

# Proportion of outcomes:
prop.table(table(df$Outcome))

# Look only at patients who died or were removed from w/l:
neg.outc <- df[df$Outcome %in% c("Died on W/List", "Died on W/List"), ]
summary(neg.outc$`Days on W/List`)
tapply(neg.outc$`Days on W/List`, neg.outc$Urgency, summary)
table(neg.outc$Urgency) # only 1 urgent, the rest NA (routine?)

