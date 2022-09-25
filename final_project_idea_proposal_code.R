fileConn<-file("final_project_idea.txt")
writeLines(c("I wants to do data analysis for my mycology group. I want to do several analysis with the zones of inhibition based on things like location found, type of culture, elevation, and anything else we can come up with.","The type of data will be areas, coordinates, elevations, and characteristic data like 'liquid', 'e. coli plate', or things like that"),fileConn)
close(fileConn)

getwd()
