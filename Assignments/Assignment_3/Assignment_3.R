###########################
#                         #
#    Assignment Week 3    #
#                         # 
###########################

# Instructions ####
# Fill in this script with stuff that we do in class.
# It might be a good idea to include comments/notes as well so you remember things we talk about
# At the end of this script are some comments with blank space after them
# They are plain-text instructions for what you need to accomplish.
# Your task is to write the code that accomplished those tasks.

# Then, make sure to upload this to both Canvas and your GitHub repository




# Vector operations! ####

# Vectors are 1-dimensional series of values in some order
1:10 # ':' only works for integers
letters # built-in pre-made vector of a - z
LETTERS #built-in vector of A-Z
head(LETTERS,n=5)#first 5 letters
tail(LETTERS,n=5)#last 5 letters
LETTERS[15]#just letter #15
length(letters) #asks how many items are in the vector 

vector1 <- c(1,2,3,4,5,6,7,8,9,10) #combined these integers into a vector 
vector1<-(1:10) #same as line 30 because they are integers
vector2 <- c(5,6,7,8,4,3,2,1,3,10) #combined these integers into a vector 
vector3 <- letters # letters and LETTERS are built-in vectors (also named it vector3)

letters[c(1,5)]<-c("a","e") #this can be used to assign a new value to points in a vector 

vector1 + 5 #this will add 5 to every value in vector 1
vector2 / 2 #this will divide every value in vector 2 by 2 
vector1*vector2 

vector3 + 1 # can't add 1 to "a" because vector 3 is letters 
class(vector3) #the class will help you figure out what kind of data is in a vector 


# Logical expressions (pay attention to these...they are used ALL THE TIME)
vector1 > 3 #this is a true false statement for all of the numbers in vector 1 compared to 3. The vectors will be the same length (10#'s are put in, 10 T/F are returned)
vector1 >= 3 
class(vector1 < 5)#logical=boolean=true/false
vector1 <= 5
vector1 == 7 #the == is necessary because one = will change it to 7, two are required to ask if something is equivalent to is 
letters == "a"
letters != "c" #the ! means is not, so all the letters that are not "c" are true and the c is false 
letters %in% c("a","b","c","z") # %in% are they any one of these values, so it will find you what you are looking for 
vector1 %in% 1:6 #this is the same thing as before but with numbers 


# Data Frames ####
# R has quite a few built-in data sets
data("iris") # load it like this

# For built-in data, there's often a 'help file'
?iris

# "Iris" is a 'data frame.' 
# Data frames are 2-dimensional (think Excel spreadsheet)
# Rows and columns
# Each row or column is a vector
letters
1:26
#vectors have to be the same length to be a data frame 
dat <- iris # can rename the object to be easier to type if you want

# ways to get a peek at our data set
names(dat) #this is the names of the columns of the vectors 
dim(dat) #this gives you the dimensions in rows then columns 
head(dat)# gives us the first 6 rows 
tail(dat,n=1) #gives me the last row 

dat$Species #gives us all 150 of the things in the species vector (only gives you the things that meet the levels you have assigned)
class(dat$Species)
dat$Sepal.Length 


# You can access specific columns of a "data frame" by name using '$'
dat$Species
dat$Sepal.Length

# You can also use square brackets to get specific 1-D or 2-D subsets of a data frame (rows and/or columns)
dat[1,1] # [Rows, Columns] gives us the factors in the locations we told it 
dat[1:3,5] 

vector2[1]
letters[1:7]
letters[c(1,3,5,7)]


# Plotting ####

# Can make a quick plot....just give vectors for x and y axes
plot(x=dat$Petal.Length, y=dat$Sepal.Length)
plot(x=dat$Species, y=dat$Sepal.Length)
#r looks at the classes of all the variables and anticipates what sort of plots you want 

# Object "Classes" ####

#check the classes of these vectors
class(dat$Petal.Length)
class(dat$Species) 

# plot() function behaves differently depending on classes of objects given to it!

# Check all classes (for each column in dat)
str(dat) #str means structure 

# "Classes" of vectors can be changed if needed (you'll need to, for sure, at some point!)

# Let's try
nums <- c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric

# convert to a factor
as.factor(nums) # show in console, now the numbers will be treated as a factor and assign levels 
nums_factor <- as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it
str(vector1)

# convert numeric to character
as.character(vector1) #changes vector 1 to characters 
as.character(vector1) + 5 #doesn't work anymore because vector 1 is not numeric anymore and r can't add 5 to a character 

# convert character to numeric
as.numeric(vector3) #it tried really hard, but r does not know how to turn letters into numbers 




#check it out
plot(nums) 
plot(nums_factor)
# take note of how numeric vectors and factors behave differently in plot()




# Simple numeric functions
# R is a language built for data analysis and statistics so there is a lot of functionality built-in

max(vector1)
min(vector1)
median(vector1)
mean(vector1)
range(vector1)
summary(vector1)

# cumulative functions
cumsum(vector1) #added all of them together 
cumprod(vector1) #multiplied all of them 
cummin(vector1) #gives the min 
cummax(vector1) #gives the max



# even has built-in statistical distributions (we will see more of these later)
dbinom(50,100,.5) # probability of getting exactly 50 heads out of 100 coin flips




# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150

dat[seq(2,150,2),] #this gave me all the even rows for all 150 rows in iris


# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

data("iris") #this found iris for me
iris_chr<-dat #this changed "dat" (what we previously called iris) to iris_chr
as.character(iris_chr$Sepal.Length)#this changed the class of the numbers in this data set to characters
as.character(iris_chr$Sepal.Width)
as.character(iris_chr$Petal.Length)
as.character(iris_chr$Petal.Width)
as.character(iris_chr$Species)

# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width
Sepal.area<-iris_chr$Sepal.Length*iris_chr$Sepal.Width #this created a new vector that contained the calculations of area 


# 4.  Add Sepal.Area to the iris data frame as a new column
iris_chr$Sepal.area<-iris_chr$Sepal.Length*iris_chr$Sepal.Width #this added it to the data set and called it Sepal.area

View(iris_chr) #this viewed it to check and make sure 
# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
      # (name it big_area_iris)

big_area_iris<-data.frame(Sepal.area[Sepal.area>20])#this made a dataframe that looked for sepal areas >20 

View(big_area_iris)#this showed me what I wanted to see 
# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
      # to canvas
      # I should be able to run your R script and get all the right objects generated
beepr::beep(3)
#I added this to give me a sense of accomplishment after not crying while completing this :)