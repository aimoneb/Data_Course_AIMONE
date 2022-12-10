library(tidyverse)
library(janitor)

# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)
  # read this bitch in 
df<-read_csv("./cleaned_covid_data.csv") %>% clean_names()

# II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)
  # we gotta filter this hoe for the states that start with A 
Astat<-df %>% 
  filter(grepl("^A",province_state))


# III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)
  #Create a scatterplot
  #Add loess curves WITHOUT standard error shading
  #Keep scales “free” in each facet
Astat %>%
  ggplot(aes(x=last_update,y=deaths))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~province_state, scales = "free")+
  theme_bw()

# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)
  # groupby -> summarise (lol I can learn new things)
state_max_fatality_rate<-df %>% 
  group_by(province_state) %>% 
  summarise(maximum_fatality_ratio = max(case_fatality_ratio,na.rm = TRUE)) %>% 
  arrange(desc(maximum_fatality_ratio))

# V. Use that new data frame from task IV to create another plot. (20 pts)
  #X-axis is Province_State
  #Y-axis is Maximum_Fatality_Ratio
  #bar plot
  #x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
  #X-axis labels turned to 90 deg to be readable
state_max_fatality_rate %>% 
  mutate(province_state = factor(province_state,levels = province_state)) %>% 
  ggplot(aes(x=province_state, y=maximum_fatality_ratio))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time
df %>% 
  group_by(last_update) %>% 
  summarise(totaldeaths = sum(deaths, na.rm = TRUE)) %>% 
  ggplot(aes(x=last_update, y =totaldeaths)) + 
  geom_point()+
  theme_bw()
