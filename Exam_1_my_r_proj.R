library(tidyverse)
library(patchwork)
library(dplyr)



list.files(path = "./BIOL3100_Exams",recursive = TRUE, full.names = TRUE, pattern="cleaned_covid_data.csv")
cleaned_covid_data <- read_csv("BIOL3100_Exams/Exam_1/cleaned_covid_data.csv")


A_states<-cleaned_covid_data[grepl("A",cleaned_covid_data$Province_State),]
A_states %>% 
  ggplot(aes(x=Last_Update,y=Deaths))+
  geom_point()+
  geom_smooth(method = lm,se=FALSE)+
  facet_wrap(~A_states$Province_State)



Province_State_New<-c(s1<-cleaned_covid_data %>% filter(Province_State=="Alaska"),
s2<-cleaned_covid_data %>% filter(Province_State=="Alabama"),
s3<-cleaned_covid_data %>% filter(Province_State=="Arizona"),
s4<-cleaned_covid_data %>% filter(Province_State=="Arkansas"),
s5<-cleaned_covid_data %>% filter(Province_State=="California"),
s6<-cleaned_covid_data %>% filter(Province_State=="Colorado"),
s7<-cleaned_covid_data %>% filter(Province_State=="Connecticut"),
s8<-cleaned_covid_data %>% filter(Province_State=="Delaware"),
s51<-cleaned_covid_data %>% filter(Province_State=="District of Columbia"),
s9<-cleaned_covid_data %>% filter(Province_State=="Florida"),
s10<-cleaned_covid_data %>% filter(Province_State=="Georgia"),
s11<-cleaned_covid_data %>% filter(Province_State=="Hawaii"),
s12<-cleaned_covid_data %>% filter(Province_State=="Idaho"),
s13<-cleaned_covid_data %>% filter(Province_State=="Illinois"),
s14<-cleaned_covid_data %>% filter(Province_State=="Indiana"),
s15<-cleaned_covid_data %>% filter(Province_State=="Iowa"),
s16<-cleaned_covid_data %>% filter(Province_State=="Kansas"),
s17<-cleaned_covid_data %>% filter(Province_State=="Kentucky"),
s18<-cleaned_covid_data %>% filter(Province_State=="Louisiana"),
s19<-cleaned_covid_data %>% filter(Province_State=="Maine"),
s20<-cleaned_covid_data %>% filter(Province_State=="Maryland"),
s21<-cleaned_covid_data %>% filter(Province_State=="Massachusetts"),
s22<-cleaned_covid_data %>% filter(Province_State=="Michigan"),
s23<-cleaned_covid_data %>% filter(Province_State=="Minnesota"),
s24<-cleaned_covid_data %>% filter(Province_State=="Mississippi"),
s25<-cleaned_covid_data %>% filter(Province_State=="Missouri"),
s26<-cleaned_covid_data %>% filter(Province_State=="Montana"),
s27<-cleaned_covid_data %>% filter(Province_State=="Nebraska"),
s28<-cleaned_covid_data %>% filter(Province_State=="Nevada"),
s29<-cleaned_covid_data %>% filter(Province_State=="New Hampshire"),
s30<-cleaned_covid_data %>% filter(Province_State=="New Jersey"),
s31<-cleaned_covid_data %>% filter(Province_State=="New Mexico"),
s32<-cleaned_covid_data %>% filter(Province_State=="New York"),
s33<-cleaned_covid_data %>% filter(Province_State=="North Carolina"),
s34<-cleaned_covid_data %>% filter(Province_State=="North Dakota"),
s35<-cleaned_covid_data %>% filter(Province_State=="Ohio"),
s36<-cleaned_covid_data %>% filter(Province_State=="Oklahoma"),
s37<-cleaned_covid_data %>% filter(Province_State=="Oregon"),
s38<-cleaned_covid_data %>% filter(Province_State=="Pennsylvania"),
s39<-cleaned_covid_data %>% filter(Province_State=="Rhode Island"),
s40<-cleaned_covid_data %>% filter(Province_State=="South Carolina"),
s41<-cleaned_covid_data %>% filter(Province_State=="South Dakota"),
s42<-cleaned_covid_data %>% filter(Province_State=="Tennessee"),
s43<-cleaned_covid_data %>% filter(Province_State=="Texas"),
s44<-cleaned_covid_data %>% filter(Province_State=="Utah"),
s45<-cleaned_covid_data %>% filter(Province_State=="Vermont"),
s46<-cleaned_covid_data %>% filter(Province_State=="Virginia"),
s47<-cleaned_covid_data %>% filter(Province_State=="Washington"),
s48<-cleaned_covid_data %>% filter(Province_State=="West Virginia"),
s49<-cleaned_covid_data %>% filter(Province_State=="Wisconsin"),
s50<-cleaned_covid_data %>% filter(Province_State=="Wyoming"))


stat_df<-list(s1,s2,s3,s4,s5,s6,s7,s8,s51,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50)


Maximum_Fatality_Ratio<-c(max(stat_df[[1]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[2]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[3]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[4]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[5]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[6]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[7]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[8]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[51]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[9]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[10]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[11]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[12]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[13]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[14]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[15]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[16]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[17]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[18]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[19]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[20]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[21]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[22]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[23]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[24]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[25]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[26]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[27]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[28]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[29]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[30]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[31]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[32]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[33]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[34]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[35]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[36]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[37]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[38]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[39]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[40]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[41]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[42]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[43]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[44]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[45]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[46]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[47]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[48]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[49]]$Case_Fatality_Ratio,na.rm=TRUE),
max(stat_df[[50]]$Case_Fatality_Ratio,na.rm=TRUE))

State_Name<-c("Alaska","Alabama","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")



mfrbs<-data.frame(Maximum_Fatality_Ratio,State_Name) #this put them into a data frame so that I could plot them 



mfrbs %>% 
  ggplot(aes(x=reorder(State_Name,- Maximum_Fatality_Ratio),y=Maximum_Fatality_Ratio))+
  geom_col()+
  ggeasy::easy_rotate_x_labels(angle = 90)






