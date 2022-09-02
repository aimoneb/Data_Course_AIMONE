list.files(recursive=TRUE,include.dirs=TRUE,
           full.names=TRUE, pattern = ".csv")
#this lists all the .csv files in data 
"csv_files"<-list.files(path="Data",recursive=TRUE,include.dirs=TRUE,
                        full.names=TRUE, pattern = ".csv")
#this stored the list 
length(csv_files)
#this showed me the number of files that where there with a .csv
wing<-list.files(path = "Data",recursive = TRUE, full.names = TRUE, pattern="wingspan_vs_mass.csv")
#this found the file that I was looking for and stored it as "wing" 
read.csv(file=wing)
#this showed me what was in the file 
df<- read.csv(file=wing)
#this showed me how many observations were in there with how many variables(basically the anatomy of the file
head(df,n=5) 
#this showed me the first 5 lines of the data set
list.files(path="Data", full.names=TRUE, recursive = TRUE, pattern = "^b")
#this found all the files in "data" that start with the letter b (^b=begins with, $b=ends with, *b=anywhere)
#list.files(path="Data", full.names=TRUE, recursive = TRUE, pattern = "^b.*b$")
#this lists the file that begins and ends with b. The "." means any character, the "*" means that there can be anything inbetween. Then end with a b
readLines("Data/data-shell/creatures/basilisk.dat", n=1)
readLines("Data/data-shell/data/pdb/benzaldehyde.pdb", n=1)
readLines("Data/Messy_Take2/b_df.csv" , n=1)
#this is a crappy way to list the first 3 lines 
x<-list.files(path="Data", full.names=TRUE, recursive = TRUE, pattern = "^b")\
#This called the path to the 3 b files "x"
readLines(x[1],n=1)
#this just read the lines of the first file in x 
readLines(x[2],n=1)
#first line of file 2 in b  
readLines(x[3],n=1)
#first line of file 3 in b 
#this is a dumb way to do it, so we use a "for-loop"
for(eachfile in x){print(readLines(eachfile,n=1))}
#this is the for-loop (without "print" it won't show it to you)
#make sure the cursor is at the first line of the for-loop to run it, or highlight the whole loop
for(eachfile in csv_files){print(readLines(eachfile,n=1))}
#this is a for-loop for all the .csv files that we have 
#
