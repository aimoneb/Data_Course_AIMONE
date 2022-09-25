x<-read.csv("./data/lw.csv") #this opened it up 


x$Lenth*x$Width #this gave me the area of everyone 

x$Area<-x$Lenth*x$Widt #this greated a new column in x called area, the $ is necisary becuase that is how we actually access the vectors 

x$LastName<-"Smith" #gave all of them the last name of Smith 

x$FullName<-paste0(x$Name," ",x$LastName) #added a row with each person's full name 

plot(x$Width,x$Area)


cor(x$Lenth,x$Area) #this is kind of like the slope, gives us the correlarion coefficient 

cor(x$Width,x$Area) 

cor(x$Lenth*x$Width,x$Area)

x$LastName<-NULL #gets rid of it 
