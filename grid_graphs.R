setwd("/media/newhd/joao-souza/projects/GEMINI/GRID")

data <- read.csv("discrepancies.csv")

library(ggplot2); library(tidyverse)


# data %>% filter(Field == "patient registration") 

data %>% pivot_longer(cols=c(Brazil, Egypt, Romania, Tanzania),
                      names_to="Country", values_to="Agree") -> data_long

data_long %>% 
  filter(Field == "patient registration", Required == "Yes") %>% 
  mutate(agree = if_else(Agree == 1, 1, 0), disagree = if_else(Agree == 0, 1, 0),
         partially = if_else(Agree == 2, 1, 0)) %>% 
  ggplot(aes(y=Country,x=Variable, fill=factor(Agree))) + 
  # geom_raster(, alpha=0.4) + 
  scale_fill_manual(values=c("red","green"),
                     labels=c(0, 1)) +
  geom_tile(aes(fill=factor(Agree)), size=0.5, colour="black") +
  # labs(x="letters", y="LETTERS", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


data_long %>% 
  filter(Field == "clinical conditions") %>% 
  mutate(agree = if_else(Agree == 1, 1, 0), disagree = if_else(Agree == 0, 1, 0),
         partially = if_else(Agree == 2, 1, 0)) %>% 
  ggplot(aes(y=Country,x=Variable, fill=factor(Agree))) + 
  # geom_raster(, alpha=0.4) + 
  scale_fill_manual(values=c("red","green", "yellow"),
                    labels=c("Absent", "Present", "Different"),
                    name="") +
  geom_tile(aes(fill=factor(Agree)), size=0.5, colour="black") +
  # labs(x="letters", y="LETTERS", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


data_long %>% 
  filter(Required == "Yes") %>%
  mutate(Variable=substr(Variable, 1,10),
         Agree=factor(Agree, levels=c(0,1,2), 
                      labels=c("Absent", "Present", "Different"))) %>% 
  ggplot(aes(y=Country, x=Variable, fill=Agree))  +
  facet_wrap(~Field, scale="free") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#D43232", "#2FD72F", "#EEEE20"),
                    labels=c("Absent", "Present", "Different")) +
  geom_tile(aes(fill=Agree), size=0.5, colour="black")
ggsave("only_core_variables.png",dpi=300, width=12, height = 12)


data_long %>% 
  mutate(Variable=substr(Variable, 1,10)) %>% 
  ggplot(aes(y=Country,x=Variable, fill=factor(Agree))) + 
  scale_fill_manual(values=c("#2FD72F","#D43232", "#EEEE20"),
                    labels=c("Absent", "Present", "Different")) +
  geom_tile(aes(fill=factor(Agree)), size=0.5, colour="black") +
  facet_wrap(~Field, scale="free") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("all_variables.svg",dpi=300)


# Common, uncommon --------------------------------------------------------

data_long %>% mutate(Agree=recode(Agree, "2"=10, "0"=0, "1"=1)) %>% 
  group_by(Field, Variable) %>% 
  summarize(value=sum(Agree)) %>% 
  group_by(value, Field) %>% 
  summarise(y = str_c(Variable, collapse=",")) %>% 
  arrange(Field) %>% distinct(value)
  mutate(value=recode(value, "0"="All wrong", "1"="3 wrong", "2" = "2 wrong/right",
                      "4"="All good"))

