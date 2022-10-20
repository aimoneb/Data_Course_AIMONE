library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)


data("mtcars")
glimpse(mtcars)

mod1 = lm(mpg ~ disp, data = mtcars)
summary(mod1)

ggplot(mtcars, aes(x=disp,y=mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()


mod2 = lm(mpg ~ qsec, data = mtcars)
ggplot(mtcars, aes(x=disp,y=qsec)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mean(mod1$residuals^2)
mean(mod2$residuals^2)

df <- mtcars %>% 
  add_predictions(mod1) 
df %>% dplyr::select("mpg","pred")


df <- mtcars %>% 
  add_predictions(mod1) 
df %>% dplyr::select("mpg","pred")

newdf = data.frame(disp = c(500,600,700,800,900))
pred = predict(mod1, newdata = newdf)
hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"
fullpreds <- full_join(df,hyp_preds)

ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg),color="Black") +
  theme_minimal()

mod3 <- glm(data=mtcars,
            formula = mpg ~ hp + disp + factor(am) + qsec)
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
map(mods,performance) %>% reduce(full_join)

mtcars %>% 
  gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()
mtcars %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label=mod1$call) +
  annotate("text",x=250,y=30,label=mod2$call) +
  annotate("text",x=250,y=28,label=mod3$call)

report(mod3)


#Now the actual assignment 
# 1. loads the “/Data/mushroom_growth.csv” data set
#first I have to find and read my data 
df<-read_csv("data/mushroom_growth.csv") %>% glimpse

# 2. create several plots exploring relationships between the response and predictors
 
# this looks at how nitrogen levels impact growth rate in each species looking at humidity as well 
df %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate, color=Species))+
  geom_smooth(se=FALSE)+
  facet_wrap(~Humidity)+
  theme_minimal()

# this uses the same variables, but uses a linear method to make the plot and looks MUCH different 
df %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate, color=Species))+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(~Humidity)+
  theme_minimal()

# this compares light and growth rate while considering species 
df %>% 
  ggplot(aes(x=Light,y=GrowthRate,color=Species))+
  geom_smooth(se=FALSE)+
  theme_classic()

# this compares temperate and growth rate for each species 
df %>% 
  ggplot(aes(x=Temperature,y=GrowthRate,color=Species))+
  geom_smooth(se=FALSE)+
  theme_classic()

# This compares species and growth rate at each humidity 
df %>% 
  ggplot(aes(x=Species,y=GrowthRate,fill=Species))+
  geom_col()+
  theme_classic()+
  facet_wrap(~Humidity)

# This compares species and growth rate alone 
df %>% 
  ggplot(aes(x=Species,y=GrowthRate,fill=Species))+
  geom_col()+
  theme_classic()

# 3. defines at least 4 models that explain the dependent variable “GrowthRate”
# 4. calculates the mean sq. error of each model

mod1<-glm(data = df,formula = GrowthRate~Species+Humidity+Temperature)
summary(mod1)
mean(mod1$residuals^2)

mod2<-lm(data = df,GrowthRate~Species)
summary(mod2)
mean(mod2$residuals^2)

mod3<-lm(data=df,GrowthRate~Species+Temperature+Humidity)
summary(mod3)
mean(mod3$residuals^2)

mod4<-lm(data=df,GrowthRate~Species*Temperature+Humidity)
summary(mod4)
mean(mod4$residuals^2)

mod5<-glm(data = df,formula = GrowthRate~Species*Temperature+Humidity)
summary(mod1)
mean(mod5$residuals^2)

mod6<-lm(data = df,GrowthRate~Temperature)
summary(mod6)
mean(mod6$residuals^2)

mod7<-lm(data = df,GrowthRate~Humidity)
summary(mod7)
mean(mod7$residuals^2)

mod8<-lm(data = df,GrowthRate~Nitrogen)
summary(mod8)
mean(mod8$residuals^2)

mod9<-lm(data = df,GrowthRate~Light)
summary(mod9)
mean(mod9$residuals^2)

mod10<-glm(data = df,formula = GrowthRate~Species*Temperature+Humidity+Light)
summary(mod10)
mean(mod10$residuals^2)

mod11<-glm(data = df,formula = GrowthRate~Species+Humidity+Light)
summary(mod11)
mean(mod11$residuals^2)

mod12<-glm(data=df,formula = GrowthRate~.^2)
mean(mod12$residuals^2)

step<-MASS::stepAIC(mod12)
step$formula

mod13<-glm(data=df,formula = step$formula)
mean(mod13$residuals^2)


# 5. selects the best model you tried
compare_performance(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,rank = TRUE)
# mod13 is my best model 

# 6, adds predictions based on new hypothetical values for the independent variables used in your model
add_predictions(df,mod13)

# 7. plots these predictions alongside the real data
#this one looks like it is just two lines, but it is because the predicted are laying directly on top of the actual values 
add_predictions(df,mod13) %>% 
  ggplot(aes(y=pred,x=Temperature,color=Species))+
  geom_smooth()+
  geom_smooth(aes(y=GrowthRate),color="black",alpha=.9)+
  facet_wrap(~Species)+
  theme_minimal()

# this one is a little easier to tell what is the predictions and what is the actual data, but it is not the easiest to read graph 
add_predictions(df,mod13) %>% 
  ggplot(aes(y=pred,x=Temperature,color=Species))+
  geom_point()+
  geom_point(aes(y=GrowthRate),color="black",alpha=.2)+
  facet_wrap(~Species)+
  theme_minimal()

add_predictions(df,mod13) %>% 
  ggplot(aes(y=pred,x=Temperature,color=Species))+
  geom_point()+
  geom_point(aes(y=GrowthRate),color="black",alpha=.2)+
  facet_wrap(~Species)+
  theme_minimal()

add_predictions(df,mod13) %>% 
  ggplot(aes(y=pred,x=Light,color=Species))+
  geom_point()+
  geom_point(aes(y=GrowthRate),color="black",alpha=.2)+
  facet_wrap(~Species)+
  theme_minimal()

add_predictions(df,mod13) %>% 
  ggplot(aes(y=pred,x=Temperature,color=Humidity))+
  geom_smooth()+
  geom_smooth(aes(y=GrowthRate),color="black",alpha=.2)+
  facet_wrap(~Species)+
  theme_minimal()

