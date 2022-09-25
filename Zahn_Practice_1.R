library(tidyverse)
ggplot(iris,
       aes(x=Sepal.Width,
           y=Sepal.Length,
           color=Species)) +
  geom_point() +
  geom_smooth(data = iris[iris$Species != "setosa",],
              method="lm",
              se=FALSE) +
  labs(x="Sepal width",
       y="Sepal length") +
  theme_bw() +
  theme(legend.text = element_text(face="italic"))
#data = iris[iris$Species != "setosa",], is what is used to turn the line off on setosa, I am not sure why
#when you get rid of the "[iris$Species != "setosa",]" it gives setosa a line 
#the ! in r means the opposite command so we told it to do the line for everything that is not setosa 




ggplot(mtcars,
       aes(x=disp,
           y=mpg,
           color=factor(cyl))) +
  geom_smooth(color="black",
              se=FALSE,
              alpha=.5,
              linetype=2) +
  geom_point() +
  labs(color="Cylinders",
       x="Miles per gallon",
       y="Displacement (cu.in.)") +
  theme_bw() +
  scale_color_manual(values = c("darkred","dodgerblue","darkgreen"))
#if we change cyl to "cyl" it gives us a plot, but I don't think that was what you were going for 


