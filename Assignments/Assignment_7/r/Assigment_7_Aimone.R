library(tidyverse)
library(dplyr)

#this opened the data and got it into my r 
dat<-read_csv("./data/Utah_Religions_by_County.csv")
#this let me look at the data and see what was going on 
view(dat)

#this cleaned my data so that my variables were not spread out in multiple columns. I am assuming that the original vaues for each other these religions was a % of the religious population so I called it accordingly 
dat<-dat %>% 
  pivot_longer(c("Assemblies of God","Episcopal Church","Pentecostal Church of God","Greek Orthodox","LDS","Southern Baptist Convention","United Methodist Church","Buddhism-Mahayana","Catholic","Evangelical","Muslim","Non Denominational","Orthodox"),
                     names_to = "Religion",values_to = "Percent_by_religion") 

#This shows the amount of religious people by religion in each county 
dat %>% 
  ggplot(aes(x=Religion,y=Percent_by_religion))+
  geom_col()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~County)

#This shows the amount of each religion present in each county 
dat %>% 
  ggplot(aes(x=County,y=Percent_by_religion,color=Religion))+
  geom_point()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#This is the same as the plot above, but with a column instead of point 
dat %>% 
  ggplot(aes(x=County,y=Percent_by_religion,fill=Religion))+
  geom_col()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#This is a plot of the counties by most religious to least 
dat %>% 
  ggplot(aes(x=reorder(County,`Non-Religious`),y=`Non-Religious`))+
  geom_point()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#this a plot of the counties populations and colored by religion, except it just lists the religions it. 
#The color isn't representative of abundance or anything like that, I haven't figured out how to do that 
dat %>% 
  ggplot(aes(x=County,y=Pop_2010,fill=Religion))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

#this is a plot of counties by decreasing religiousness. I multiplied them by 100 becuase they were a % of the counties total population  
dat %>% mutate("pop_religious"=Religious*100) %>% mutate("pop_non_religious"=`Non-Religious`*100) %>% 
  ggplot(aes(x=pop_religious,y=pop_non_religious,color=County))+
  geom_point()+
  theme_minimal()

# this is a plot of the Greek Orthodox religion by county arranges from most to least 
dat %>% 
  filter(Religion=="Greek Orthodox") %>% 
  ggplot(aes(x=reorder(County,-Percent_by_religion),y=Percent_by_religion))+
  geom_point()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


dat %>% 
  filter(Religion=="LDS") %>% 
  ggplot(aes(x=reorder(County,-Percent_by_religion),y=Percent_by_religion))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

# Question 1 “Does population of a county correlate with the proportion of any specific religious group in that county?”
dat %>% 
  ggplot(aes(x=Pop_2010,y=Percent_by_religion,color=County))+
  geom_point()+
  facet_wrap(~Religion)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

# Question 2 “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
dat %>% 
  ggplot(aes(x=`Non-Religious`,y=Percent_by_religion,color=County))+
  geom_point()+
  facet_wrap(~Religion)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
  

  
