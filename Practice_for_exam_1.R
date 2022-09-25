library(tidyverse)
library(patchwork) 
library(grid)
library(GGally)
library(palmerpenguins)
library(ggeasy)
library(stringr)

GGally::ggpairs(iris)
?ggpairs

ggplot(iris)


#contrl-shift-m gives the %>% symbol. itis %>% (called pipe)  is the exact same as ggplot(iris)
iris %>% 
  ggplot()
  
sum(1:10)

1:10 %>% sum()

iris %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
geom_point()

iris %>% 
  filter(Species !="setosa") %>% 
  filter(Sepal.Length<7) %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
  geom_point()+
  facet_wrap(~Species) #this gives you separate subplots, one for each species (categprical)

iris %>% 
  ggplot(aes(x=Sepal.Length,y=Petal.Length))+
  geom_point()+
  geom_smooth(method = "lm",aes(color=Species))+
  facet_wrap(~Species)+
  theme_bw() + 
  theme(strip.text = element_text(face = "italic"),legend.position = "none")

iris %>% 
  ggplot(aes(x=Species,y=Petal.Length))+
  geom_violin()
data("iris")

str(iris)
iris %>% str()

# levels(iris$Species)<-c("virginica","versicolor","setosa") #this just renamed them, not what we wanted 

iris %>% 
  mutate(Sepal.Area = Sepal.Length*Sepal.Width,
         Species=factor(Species,levels = c("virginica","versicolor","setosa"))) %>% ggplot(aes(x=Species,y=Petal.Length))+
  geom_violin() #this is how we reorder things to look the way we want them to 

iris %>% 
  group_by(Species) %>% 
  summarise(max_sep_len=max(Sepal.Length),
            min_sep_len=min(Sepal.Length),
            mean_sep_len=mean(Sepal.Length),
            sd_sep_len=sd(Sepal.Length))
#we will use the group by and summarieze functions on the test 
summar

data("mtcars")



fat_flowers<-
iris %>% 
  mutate(c1=Sepal.Length>5.8,
         c2=Sepal.Width>3,
         c3=Petal.Length>3.7,
         c4=Petal.Width>1.2,
         conditions=c1+c2+c3+c4)%>%
  filter(conditions>=2) %>%
  select(-starts_with("c"))

fat_flowers %>% 
  ggplot(aes(x=Sepal.Width,y=Petal.Width,color=Species))+
  geom_point()

iris %>% 
  ggplot(aes(x=Sepal.Width,y=Petal.Width,color=Species))+
  geom_blank()+
  geom_point(data=fat_flowers)
#mutate makes new columns 
#filter is for picking rows 
#select is for picking columns
#the select lets me pick which columns I want to see
#the geom blank set a scale 



penguins
penguins %>% GGally::ggpairs()

penguins %>% names()

penguins %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=sex,y=body_mass_g,fill=sex))+
  geom_boxplot()+
  facet_wrap(~island)+
  scale_fill_manual(values = c("Blue","Green"))+
  theme_bw()+
  theme(strip.background = element_rect(fill=NA))


library(gganimate)
  