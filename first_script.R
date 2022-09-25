# finding files 

?list.files
#this showed us what all the "short cuts" mean like dirs and full.names 
x<-list.files(recursive=TRUE,include.dirs=TRUE,
              full.names=TRUE)
# this saves them as "x"
x<-list.files(pattern=".csv",recursive=TRUE,full.names=TRUE)
# this made it so the full names are listed 
x[1]
# this brought up the first one in the list 
y<-x[1]
#this saved the first file in x as y
readLines(y)
#this tells us what is in the file we assigned as y 
readLines(y)[1]
#this shows me the first line 
readLines(y)[1:3]
#this shows lines 1-3 
read.csv(y)
#this reads the .csv file itself 
z<-read.csv(y)
#this assigned the data set to y 
View(z)
#this pulls it up in a new tab next to the readme.md
z$IATA_CODE
#this just shoes me a specific variable 
myvec<-c(1,3,5,7,9)
#the c means combine to make a vector of stuff, the myvec gives it a name 
z$IATA_CODE[myvec]




list.files(recursive = TRUE, pattern = "grade", ignore.case = TRUE, full.names = TRUE)
#did this to find the file we wanted by giving parameters

grades<-read.csv("./Data/Fake_grade_data.csv")
#this saved it as "grades" 
class(grades)
View(grades)
#this shows us the data set in a table in R 
grades[2,]
#this showed us student 2. If we had just done "2" it would have only showed us the 2nd column 
grades[2,2]
#this showed us student 2's second assignment in [row,column] format, like coordinates
grades[3,c(1,3,5)]
#this showed us students 3, rows 1,3,5. The c was necessary to combine the commands

grades$Assignment_1>15
#this gives us a list of weather or not each row is >15, 
#so we can insert this to selectively receive selective data 

#list of students who have >15 on assignment 1 
grades$Student[grades$Assignment_1>15]


#practice
list.files(recursive=TRUE,include.dirs=TRUE,
           full.names=TRUE, pattern = ".csv")

c<-list.files(recursive=TRUE,include.dirs=TRUE,
           full.names=TRUE, pattern = ".csv")
c[3]
c[5]
b<-c[5]
readLines(b)
church<-read.csv("./Assignments/Assignment_7/Utah_Religions_by_County.csv")
class(church)
View(church)
church[5,]
church[5,9]
church[5,c(1,7,9,15)]

list.files(recursive=TRUE,include.dirs=TRUE,
           full.names=TRUE)
