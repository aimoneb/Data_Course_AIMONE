library(tidyverse)
library(modelr)
library(easystats)
library(broom)
library(janitor)
library(stringr)

# 1. Read in the unicef data (10 pts)
df<-read_csv("./data/unicef-u5mr.csv")

# 2. Get it into tidy format (10 pts)
  # to get rid of weird capitals and spaces 
df<-df %>% clean_names() 
  # to get my data tidy and get rid of the prefix on the year 
df_clean<-df %>% pivot_longer(c(starts_with("u5mr")),names_to = "year",values_to = "u5mr",names_prefix = "u5mr_") 

# 3. Plot each country’s U5MR over time (20 points)
  # I need to make my year column into a numeric value so my plots will work
  # this mutate is not the best way to do this, I just can't remember how to do it better 
p1<-df_clean %>% 
  mutate(as.numeric(year)) %>%
  group_by(country_name) %>% 
  ggplot(aes(x=`as.numeric`(year),y=u5mr))+
  geom_path()+
  theme_bw()+
  labs(x="Year",y="U5MR")+
  facet_wrap(~continent)

# 4. Save this plot as LASTNAME_Plot_1.png (5 pts
ggsave("AIMONE_plot_1.png",plot=p1,path="./output",device = png)  

# 5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)
  # this sounds like a major pain in the ass, so I am going to try to write a function to make it less painful 
  # I did this to get rid of the na's in my data that make it harder to work with 
df_clean<-df_clean %>% na.omit()
  # I made this filter to select continent and then year to get the mean 
f1<-function(x,var1,var2){
  x %>% filter(continent %in% var1) %>% 
    na.omit() %>% 
    filter(`as.numeric`(year) %in% var2) %>% 
    summarise(mean_mort_rate=mean(u5mr))
    
}
  # I then made a list for Africa with the mean of each year 
Africa<-c(f1(df_clean,"Africa","1950"),f1(df_clean,"Africa","1951"),f1(df_clean,"Africa","1952"),f1(df_clean,"Africa","1953"),f1(df_clean,"Africa","1954"),f1(df_clean,"Africa","1955"),
  f1(df_clean,"Africa","1956"),f1(df_clean,"Africa","1957"),f1(df_clean,"Africa","1958"),f1(df_clean,"Africa","1959"),f1(df_clean,"Africa","1960"),
  f1(df_clean,"Africa","1961"),f1(df_clean,"Africa","1962"),f1(df_clean,"Africa","1963"),f1(df_clean,"Africa","1964"),f1(df_clean,"Africa","1965"),
  f1(df_clean,"Africa","1966"),f1(df_clean,"Africa","1967"),f1(df_clean,"Africa","1968"),f1(df_clean,"Africa","1969"),f1(df_clean,"Africa","1970"),
  f1(df_clean,"Africa","1971"),f1(df_clean,"Africa","1972"),f1(df_clean,"Africa","1973"),f1(df_clean,"Africa","1974"),f1(df_clean,"Africa","1975"),
  f1(df_clean,"Africa","1976"),f1(df_clean,"Africa","1977"),f1(df_clean,"Africa","1978"),f1(df_clean,"Africa","1979"),f1(df_clean,"Africa","1980"),
  f1(df_clean,"Africa","1981"),f1(df_clean,"Africa","1982"),f1(df_clean,"Africa","1983"),f1(df_clean,"Africa","1984"),f1(df_clean,"Africa","1985"),
  f1(df_clean,"Africa","1986"),f1(df_clean,"Africa","1987"),f1(df_clean,"Africa","1988"),f1(df_clean,"Africa","1989"),f1(df_clean,"Africa","1990"),
  f1(df_clean,"Africa","1991"),f1(df_clean,"Africa","1992"),f1(df_clean,"Africa","1993"),f1(df_clean,"Africa","1994"),f1(df_clean,"Africa","1995"),
  f1(df_clean,"Africa","1996"),f1(df_clean,"Africa","1997"),f1(df_clean,"Africa","1998"),f1(df_clean,"Africa","1999"),f1(df_clean,"Africa","2000"),
  f1(df_clean,"Africa","2001"),f1(df_clean,"Africa","2002"),f1(df_clean,"Africa","2003"),f1(df_clean,"Africa","2004"),f1(df_clean,"Africa","2005"),
  f1(df_clean,"Africa","2006"),f1(df_clean,"Africa","2007"),f1(df_clean,"Africa","2008"),f1(df_clean,"Africa","2009"),f1(df_clean,"Africa","2010"),
  f1(df_clean,"Africa","2011"),f1(df_clean,"Africa","2012"),f1(df_clean,"Africa","2013"),f1(df_clean,"Africa","2014"),f1(df_clean,"Africa","2015")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = starts_with("mean"),names_to = "useless",values_to = "mean_mortality")
  #this is the beginning of my data frame, to make it so it actually tells me stuff, I need to add back in some things I stripped away 
africa_word<-rep("Africa",66)
year<-c(1950:2015)
  # I will add these to my aftica data frame to make it more intuitive and make my life easier down the line 
Africa$Continent<-africa_word
Africa$year<-year

# then I did this for each continent 
Asia<-c(f1(df_clean,"Asia","1950"),f1(df_clean,"Asia","1951"),f1(df_clean,"Asia","1952"),f1(df_clean,"Asia","1953"),f1(df_clean,"Asia","1954"),f1(df_clean,"Asia","1955"),
          f1(df_clean,"Asia","1956"),f1(df_clean,"Asia","1957"),f1(df_clean,"Asia","1958"),f1(df_clean,"Asia","1959"),f1(df_clean,"Asia","1960"),
          f1(df_clean,"Asia","1961"),f1(df_clean,"Asia","1962"),f1(df_clean,"Asia","1963"),f1(df_clean,"Asia","1964"),f1(df_clean,"Asia","1965"),
          f1(df_clean,"Asia","1966"),f1(df_clean,"Asia","1967"),f1(df_clean,"Asia","1968"),f1(df_clean,"Asia","1969"),f1(df_clean,"Asia","1970"),
          f1(df_clean,"Asia","1971"),f1(df_clean,"Asia","1972"),f1(df_clean,"Asia","1973"),f1(df_clean,"Asia","1974"),f1(df_clean,"Asia","1975"),
          f1(df_clean,"Asia","1976"),f1(df_clean,"Asia","1977"),f1(df_clean,"Asia","1978"),f1(df_clean,"Asia","1979"),f1(df_clean,"Asia","1980"),
          f1(df_clean,"Asia","1981"),f1(df_clean,"Asia","1982"),f1(df_clean,"Asia","1983"),f1(df_clean,"Asia","1984"),f1(df_clean,"Asia","1985"),
          f1(df_clean,"Asia","1986"),f1(df_clean,"Asia","1987"),f1(df_clean,"Asia","1988"),f1(df_clean,"Asia","1989"),f1(df_clean,"Asia","1990"),
          f1(df_clean,"Asia","1991"),f1(df_clean,"Asia","1992"),f1(df_clean,"Asia","1993"),f1(df_clean,"Asia","1994"),f1(df_clean,"Asia","1995"),
          f1(df_clean,"Asia","1996"),f1(df_clean,"Asia","1997"),f1(df_clean,"Asia","1998"),f1(df_clean,"Asia","1999"),f1(df_clean,"Asia","2000"),
          f1(df_clean,"Asia","2001"),f1(df_clean,"Asia","2002"),f1(df_clean,"Asia","2003"),f1(df_clean,"Asia","2004"),f1(df_clean,"Asia","2005"),
          f1(df_clean,"Asia","2006"),f1(df_clean,"Asia","2007"),f1(df_clean,"Asia","2008"),f1(df_clean,"Asia","2009"),f1(df_clean,"Asia","2010"),
          f1(df_clean,"Asia","2011"),f1(df_clean,"Asia","2012"),f1(df_clean,"Asia","2013"),f1(df_clean,"Asia","2014"),f1(df_clean,"Asia","2015")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = starts_with("mean"),names_to = "useless",values_to = "mean_mortality")
asia_word<-rep("Asia",66)
year<-c(1950:2015)
Asia$Continent<-asia_word
Asia$year<-year

Europe<-c(f1(df_clean,"Europe","1950"),f1(df_clean,"Europe","1951"),f1(df_clean,"Europe","1952"),f1(df_clean,"Europe","1953"),f1(df_clean,"Europe","1954"),f1(df_clean,"Europe","1955"),
        f1(df_clean,"Europe","1956"),f1(df_clean,"Europe","1957"),f1(df_clean,"Europe","1958"),f1(df_clean,"Europe","1959"),f1(df_clean,"Europe","1960"),
        f1(df_clean,"Europe","1961"),f1(df_clean,"Europe","1962"),f1(df_clean,"Europe","1963"),f1(df_clean,"Europe","1964"),f1(df_clean,"Europe","1965"),
        f1(df_clean,"Europe","1966"),f1(df_clean,"Europe","1967"),f1(df_clean,"Europe","1968"),f1(df_clean,"Europe","1969"),f1(df_clean,"Europe","1970"),
        f1(df_clean,"Europe","1971"),f1(df_clean,"Europe","1972"),f1(df_clean,"Europe","1973"),f1(df_clean,"Europe","1974"),f1(df_clean,"Europe","1975"),
        f1(df_clean,"Europe","1976"),f1(df_clean,"Europe","1977"),f1(df_clean,"Europe","1978"),f1(df_clean,"Europe","1979"),f1(df_clean,"Europe","1980"),
        f1(df_clean,"Europe","1981"),f1(df_clean,"Europe","1982"),f1(df_clean,"Europe","1983"),f1(df_clean,"Europe","1984"),f1(df_clean,"Europe","1985"),
        f1(df_clean,"Europe","1986"),f1(df_clean,"Europe","1987"),f1(df_clean,"Europe","1988"),f1(df_clean,"Europe","1989"),f1(df_clean,"Europe","1990"),
        f1(df_clean,"Europe","1991"),f1(df_clean,"Europe","1992"),f1(df_clean,"Europe","1993"),f1(df_clean,"Europe","1994"),f1(df_clean,"Europe","1995"),
        f1(df_clean,"Europe","1996"),f1(df_clean,"Europe","1997"),f1(df_clean,"Europe","1998"),f1(df_clean,"Europe","1999"),f1(df_clean,"Europe","2000"),
        f1(df_clean,"Europe","2001"),f1(df_clean,"Europe","2002"),f1(df_clean,"Europe","2003"),f1(df_clean,"Europe","2004"),f1(df_clean,"Europe","2005"),
        f1(df_clean,"Europe","2006"),f1(df_clean,"Europe","2007"),f1(df_clean,"Europe","2008"),f1(df_clean,"Europe","2009"),f1(df_clean,"Europe","2010"),
        f1(df_clean,"Europe","2011"),f1(df_clean,"Europe","2012"),f1(df_clean,"Europe","2013"),f1(df_clean,"Europe","2014"),f1(df_clean,"Europe","2015")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = starts_with("mean"),names_to = "useless",values_to = "mean_mortality")
europe_word<-rep("Europe",66)
year<-c(1950:2015)
Europe$Continent<-europe_word
Europe$year<-year


Americas<-c(f1(df_clean,"Americas","1950"),f1(df_clean,"Americas","1951"),f1(df_clean,"Americas","1952"),f1(df_clean,"Americas","1953"),f1(df_clean,"Americas","1954"),f1(df_clean,"Americas","1955"),
          f1(df_clean,"Americas","1956"),f1(df_clean,"Americas","1957"),f1(df_clean,"Americas","1958"),f1(df_clean,"Americas","1959"),f1(df_clean,"Americas","1960"),
          f1(df_clean,"Americas","1961"),f1(df_clean,"Americas","1962"),f1(df_clean,"Americas","1963"),f1(df_clean,"Americas","1964"),f1(df_clean,"Americas","1965"),
          f1(df_clean,"Americas","1966"),f1(df_clean,"Americas","1967"),f1(df_clean,"Americas","1968"),f1(df_clean,"Americas","1969"),f1(df_clean,"Americas","1970"),
          f1(df_clean,"Americas","1971"),f1(df_clean,"Americas","1972"),f1(df_clean,"Americas","1973"),f1(df_clean,"Americas","1974"),f1(df_clean,"Americas","1975"),
          f1(df_clean,"Americas","1976"),f1(df_clean,"Americas","1977"),f1(df_clean,"Americas","1978"),f1(df_clean,"Americas","1979"),f1(df_clean,"Americas","1980"),
          f1(df_clean,"Americas","1981"),f1(df_clean,"Americas","1982"),f1(df_clean,"Americas","1983"),f1(df_clean,"Americas","1984"),f1(df_clean,"Americas","1985"),
          f1(df_clean,"Americas","1986"),f1(df_clean,"Americas","1987"),f1(df_clean,"Americas","1988"),f1(df_clean,"Americas","1989"),f1(df_clean,"Americas","1990"),
          f1(df_clean,"Americas","1991"),f1(df_clean,"Americas","1992"),f1(df_clean,"Americas","1993"),f1(df_clean,"Americas","1994"),f1(df_clean,"Americas","1995"),
          f1(df_clean,"Americas","1996"),f1(df_clean,"Americas","1997"),f1(df_clean,"Americas","1998"),f1(df_clean,"Americas","1999"),f1(df_clean,"Americas","2000"),
          f1(df_clean,"Americas","2001"),f1(df_clean,"Americas","2002"),f1(df_clean,"Americas","2003"),f1(df_clean,"Americas","2004"),f1(df_clean,"Americas","2005"),
          f1(df_clean,"Americas","2006"),f1(df_clean,"Americas","2007"),f1(df_clean,"Americas","2008"),f1(df_clean,"Americas","2009"),f1(df_clean,"Americas","2010"),
          f1(df_clean,"Americas","2011"),f1(df_clean,"Americas","2012"),f1(df_clean,"Americas","2013"),f1(df_clean,"Americas","2014"),f1(df_clean,"Americas","2015")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = starts_with("mean"),names_to = "useless",values_to = "mean_mortality")
americas_word<-rep("Americas",66)
year<-c(1950:2015)
Americas$Continent<-americas_word
Americas$year<-year


Oceania<-c(f1(df_clean,"Oceania","1950"),f1(df_clean,"Oceania","1951"),f1(df_clean,"Oceania","1952"),f1(df_clean,"Oceania","1953"),f1(df_clean,"Oceania","1954"),f1(df_clean,"Oceania","1955"),
          f1(df_clean,"Oceania","1956"),f1(df_clean,"Oceania","1957"),f1(df_clean,"Oceania","1958"),f1(df_clean,"Oceania","1959"),f1(df_clean,"Oceania","1960"),
          f1(df_clean,"Oceania","1961"),f1(df_clean,"Oceania","1962"),f1(df_clean,"Oceania","1963"),f1(df_clean,"Oceania","1964"),f1(df_clean,"Oceania","1965"),
          f1(df_clean,"Oceania","1966"),f1(df_clean,"Oceania","1967"),f1(df_clean,"Oceania","1968"),f1(df_clean,"Oceania","1969"),f1(df_clean,"Oceania","1970"),
          f1(df_clean,"Oceania","1971"),f1(df_clean,"Oceania","1972"),f1(df_clean,"Oceania","1973"),f1(df_clean,"Oceania","1974"),f1(df_clean,"Oceania","1975"),
          f1(df_clean,"Oceania","1976"),f1(df_clean,"Oceania","1977"),f1(df_clean,"Oceania","1978"),f1(df_clean,"Oceania","1979"),f1(df_clean,"Oceania","1980"),
          f1(df_clean,"Oceania","1981"),f1(df_clean,"Oceania","1982"),f1(df_clean,"Oceania","1983"),f1(df_clean,"Oceania","1984"),f1(df_clean,"Oceania","1985"),
          f1(df_clean,"Oceania","1986"),f1(df_clean,"Oceania","1987"),f1(df_clean,"Oceania","1988"),f1(df_clean,"Oceania","1989"),f1(df_clean,"Oceania","1990"),
          f1(df_clean,"Oceania","1991"),f1(df_clean,"Oceania","1992"),f1(df_clean,"Oceania","1993"),f1(df_clean,"Oceania","1994"),f1(df_clean,"Oceania","1995"),
          f1(df_clean,"Oceania","1996"),f1(df_clean,"Oceania","1997"),f1(df_clean,"Oceania","1998"),f1(df_clean,"Oceania","1999"),f1(df_clean,"Oceania","2000"),
          f1(df_clean,"Oceania","2001"),f1(df_clean,"Oceania","2002"),f1(df_clean,"Oceania","2003"),f1(df_clean,"Oceania","2004"),f1(df_clean,"Oceania","2005"),
          f1(df_clean,"Oceania","2006"),f1(df_clean,"Oceania","2007"),f1(df_clean,"Oceania","2008"),f1(df_clean,"Oceania","2009"),f1(df_clean,"Oceania","2010"),
          f1(df_clean,"Oceania","2011"),f1(df_clean,"Oceania","2012"),f1(df_clean,"Oceania","2013"),f1(df_clean,"Oceania","2014"),f1(df_clean,"Oceania","2015")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = starts_with("mean"),names_to = "useless",values_to = "mean_mortality")
oceania_word<-rep("Oceania",66)
year<-c(1950:2015)
Oceania$Continent<-oceania_word
Oceania$year<-year


  #now I need to join these continent dataframes together so I can make a plot 
joint<-full_join(Oceania,Africa)
joint<-full_join(joint,Europe)
joint<-full_join(joint,Asia)
joint<-full_join(joint,Americas)
joint_df<-as.data.frame(joint)
  # here is the code for the actual plot 
p2<-joint_df %>% 
  ggplot(aes(x=year,y=mean_mortality,color=Continent))+
  geom_line()+
  theme_bw()
# 6. Save that plot as LASTNAME_Plot_2.png (5 pts) 
ggsave("AIMONE_plot_2.png",plot=p2,path="./output",device = png)

# 7. Create three models of U5MR (20 pts)
  #- mod1 should account for only Year
mod1<-glm(data=df_clean,
          formula = u5mr ~ year)
  #- mod2 should account for Year and Continent
mod2<-glm(data=df_clean,
          formula = u5mr ~ year + continent)
  #- mod3 should account for Year, Continent, and their interaction term
mod3<-glm(data = df_clean,
          formula = u5mr ~ year * continent)

# 8. Compare the three models with respect to their performance
compare_performance(mod1,mod2,mod3,rank = TRUE)
  # Model 3 is the best model. According to the compare function, it has an 80% performance score.
  # It is the most complicated model, so it has the highest AIC. Regardless, I still think mod3 is the one to use. 

# 9. Plot the 3 models’ predictions like so: (10 pts)
pred_dat<-gather_predictions(df_clean,mod1,mod2,mod3) 
p3<-pred_dat %>% 
  ggplot(aes(x=as.numeric(year), y=pred,color=continent))+ 
  geom_smooth(method = "lm")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~model)
ggsave("AIMONE_plot_3.png",plot=p3,path="./output",device = png)

# 10. BONUS - Using your preferred model, predict what the U5MR would be for Ecuador in the year 2020. 
  # The real value for Ecuador for 2020 was 13 under-5 deaths per 1000 live births. 
  # How far off was your model prediction???
  

  # I am trying to retrain my model to use a numeric year instead of a character one to get my predict function to work. 
  # it is not working this far 
df_clean<-df_clean %>% 
  mutate(as.numeric(year))
mod4<-glm(data = df_clean,
          formula = u5mr ~ as.numeric(year) * continent)
df_clean %>% view
performance(mod4)
this<-data.frame(`as.numeric(year)`="2020", continent="Americas")
this %>% view
predict(mod4,newdata = this)

  # That wasn't working, so I will go back to mod3 which is the same but without the year as numeric 
this<-data.frame(year="2020", continent="Americas")
predict(mod3,newdata = this)
    # this still doesn't work for 2020, but I don't know how to fix it 
    # It is not liking how I am putting in my year. 
    # lets try to use it to predict a year that is in the data set 
this<-data.frame(year="1997", continent="Americas")
predict(mod3,newdata = this)
    # it is  giving me a prediction for a year that lies in the data set, but not a year that lies outside of it 
    # lets see if my other prediction model that has year as numeric will be happy with a year inside the data set 
this<-data.frame("as.numeric(year)" = 2020, continent="Americas")
predict(mod4,newdata = this)
    # now it is saying the size is wrong 
    # these hate me, so I will try some more bullshit code attempts  
this<-data.frame(year="2020", continent="Americas")
predict(mod3,newdata = this)
    # this is where I give up :(
    # jk 
#predict(mod2,newdata = this)

#mod5<-glm(data = df_clean,
          #formula = u5mr ~ as.numeric(year) + continent)
    # don't even ask 
#this<-data.frame("`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas","`as.numeric(year)`"=2020, continent="Americas",
                 #"`as.numeric(year)`"=2020, continent="Americas") %>% pivot_longer(c(starts_with(c),starts_with(X)),names_to = continent_names,values_to = year)


