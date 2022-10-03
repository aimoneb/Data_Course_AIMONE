library(tidyverse)
library(dplyr)
dat <- read_csv("/Users/behleeaimone/Desktop/Data_Course_AIMONE/Data/BioLog_Plate_Data.csv")
glimpse(dat)
view(dat)
clean<-dat %>% pivot_longer(c("Hr_24","Hr_48","Hr_144"),names_to = "time",values_to = "absorbance" ) %>% 
  pivot_wider(names_from = "Sample ID",values_from = "absorbance") %>% 
  pivot_longer(c("Clear_Creek","Waste_Water"),names_to = "Water_Samples",values_to = "absorbance_of_water") %>% 
  pivot_longer(c("Soil_1","Soil_2"),names_to = "Soil_Samples",values_to = "absorbance_of_soil") %>% 
  pivot_longer(c("Water_Samples","Soil_Samples"),names_to = "Sample_Type",values_to = "Location")
view(clean)

clean1<-clean %>% separate(time,into = c("text","num"))
nums<-as.numeric(clean1$num)
real_one<-data.frame(clean,nums)
view(real_one)

as.numeric(real_one$nums)

real_one %>% 
  filter(Dilution==0.1) %>% 
  group_by(Substrate,nums,Sample_Type) %>% 
  summarise("avgAbW"=mean(absorbance_of_water,na.rm=TRUE),"avgAbS"=mean(absorbance_of_soil,na.rm=TRUE)) %>% 
  unique.data.frame() %>% 
  ggplot()+
  geom_line(aes(x=nums,y=avgAbW,color="Water"))+
  geom_line(aes(x=nums,y=avgAbS,color="Soil"))+
  labs(x="Time",y="Absorbance")+
  facet_wrap(~Substrate)+
  theme_minimal()
#I realize that your example has smoother lines, but this project made me want to die, so I am leaving it where it is 


library(gganimate)
Ione<-real_one %>% pivot_longer(c(absorbance_of_water,absorbance_of_soil),names_to = "Absorbance",values_to = "absorbance")

Ione %>% filter(Substrate=="Itaconic Acid") %>% 
  group_by(Location,nums,Dilution) %>% 
  summarise("Average_Absorbance"=mean(absorbance,na.rm=TRUE)) %>% 
  unique.data.frame() %>%
  ggplot()+
  geom_line(aes(x=nums,y=Average_Absorbance,color=Location))+
  labs(x="Time",y="Mean_Absorbance")+
  labs(color="Sample ID")+
  transition_reveal(nums)+
  facet_wrap(~Dilution)+
  theme_minimal()
#idk why my means are not correct or matching yours, but I have spent an absurd amount of time on this, so sad for me  
