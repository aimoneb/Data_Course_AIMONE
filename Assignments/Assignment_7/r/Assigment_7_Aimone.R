library(tidyverse)
library(dplyr)

#this opened the data and got it into my r 
dat<-read_csv("./data/Utah_Religions_by_County.csv")
#this let me look at the data and see what was going on 
view(dat)

#this cleaned my data so that my variables were not spread out in multilple columns 
dat<-dat %>% 
  pivot_longer(c("Assemblies of God","Episcopal Church","Pentecostal Church of God","Greek Orthodox","LDS","Southern Baptist Convention","United Methodist Church","Buddhism-Mahayana","Catholic","Evangelical","Muslim","Non Denominational","Orthodox"),
                     names_to = "Religion",values_to = "Percent_by_religion") 

#This shows the amount of religious people by religion in each county 
dat %>% 
  ggplot(aes(x=Religion,y=Percent_by_religion))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~County)

#This shows the amount of each religion present in each county 
dat %>% 
  ggplot(aes(x=County,y=Percent_by_religion,color=Religion))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

#This is the same as the plot above, but with a column instead of point 
dat %>% 
  ggplot(aes(x=County,y=Percent_by_religion,fill=Religion))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))

#This is a plot of the counties by most religious to least 
dat %>% 
  ggplot(aes(x=reorder(County,`Non-Religious`),y=`Non-Religious`))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))


  
  
