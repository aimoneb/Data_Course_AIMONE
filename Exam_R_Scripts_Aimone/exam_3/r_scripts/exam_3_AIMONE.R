library(tidyverse)
library(janitor)
library(stringr)
library(ggpubr)
library(broom)
library(AICcmodavg)

# 1. Load and clean FacultySalaries_1995.csv file and Re-create the graph below…
  # this data was pretyy gross, so I tried to make it as tidy as possible without losing the integrity of the data 
df<-read_csv("./data/FacultySalaries_1995.csv") %>% clean_names() %>% 
  pivot_longer(c(ends_with("salary")), names_to = "rank", values_to = "salary",names_prefix = "avg_") %>% 
  pivot_longer(c(ends_with("comp")), names_to = "comp_type", values_to = "comp_amt") %>% 
  pivot_longer(c(num_full_profs,num_assoc_profs,num_assist_profs), names_to = "faculty_type", values_to = "faculty_count") %>% view
  # I am not sure how to get rid of the third rank that is not present in your image.
  # I tried to use gsub to get rid of the suffixes on my ranks, but I haven't figured out how to quite make it work yet 
df %>% 
  ggplot(aes(x=rank, y=salary,fill=rank))+
  geom_boxplot()+
  facet_wrap(~tier)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

# 2. Build an ANOVA model and display the summary output in your report.
  # The ANOVA model should test the influence of “State”, “Tier”, and “Rank” on “Salary” 
  # but should NOT include any interactions between those predictors.
twoway<-aov(salary~state+rank+tier, data = df)
summary(twoway)
