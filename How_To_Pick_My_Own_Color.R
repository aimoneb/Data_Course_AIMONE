library(tidyverse)
#this loaded my downloaded package 
library(patchwork) 

data("iris")
#this loaded some data for me to look at

df<-iris
#this called it iris, so I don't have to type it all the time 
glimpse(iris) #this kind of showed me what was in there 

#this will plot my stuff. 
ggplot(df,
       mapping = aes(x=Sepal.Length,
                     y=Sepal.Width,
                     color=Species,

                    ))+
geom_point() + geom_smooth(method = "lm",se=FALSE)

#aesthetics have to be columns on the graph, the geoms are what is actually drawn 
#the "geom_" things let me pick how I want my data to be displayed 


df<-read_delim("./Data/DatasaurusDozen.tsv")


data("mtcars")
mc<-mtcars
view(mc)
ggplot(mc,mapping = aes(x=mpg,y=qsec, color=cyl))+
  geom_smooth()+geom_smooth(method = "lm")

p1<-ggplot(df,
       mapping = aes(x=Sepal.Length,
                     y=Sepal.Width,))+
  geom_point() + geom_smooth(method = "lm", aes(color=Species),se=FALSE)
#se is standard error which is the grey thing that shows up with the line, se=FALSE is what will get rid of it 

p2<-ggplot(df,
       mapping = aes(x=Sepal.Length,
                     y=Sepal.Width,
                     color=Species))+
  geom_point() + geom_smooth(method = "lm",se=FALSE)
#the "p2" and "p1" are saving the plots as an object so that we can use the "patchwork" function

p1+p2
#this is using the patchqork function to combine multiple plots 

dir.create("figures")
#this made a directory to store my figures in 

ggsave("./Figures/myfirstplot.png",plot=p2,width = 6,height = 6,dpi = 300)

ggsave("./Figures/mysecondplot.png",plot=p1,width = 6,height = 6,dpi = 300)

p4<-p2+
  theme_minimal()+
  labs(y="Sepal Width",
       title = "big f flowers",
       subtitle = "they long",
       caption = "your mom")
ggsave("./Figures/mythirdplot.png",p4,width=4,height = 4,dpi = 300)

p5<-p4+
  theme(axis.text.x = element_text(face = "italic",size = 17,color="orange"),
        plot.background = element_rect(fill="yellow"))

p6<-
  ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species))+
  geom_boxplot()

pal<-c("#eb34e1","#0afada","#eb348f") ##eb34e1
p6+
  scale_fill_manual(values =pal )
#this is how I pick my own colors. i first combined my custom hex code as pal, then I did the scale fill manual and told it which values to use 

ggplot(iris,aes(x=Sepal.Length,
                y=Sepal.Width,
                color=Sepal.Width))+
geom_point()+
  scale_color_viridis_c(option="inferno")
viridis::rocket(100)


remotes::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
remotes::install_github("clauswilke/colorblindr")
Yes
