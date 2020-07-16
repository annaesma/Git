# intake  data all over. change the names first then take food/water then individual animals

intake$pm <- rowSums(intake[16:27])
intake_d <- intake[,c(1,2,3,30)]
write_xlsx(intake_d,"C:/Users/dzakmice/Desktop/intake.xlsx")
intake_data <- read.delim("C:/Users/dzakmice/Desktop/intakeData.txt")
food <- sqldf("select * from intake_data where ActivatorNum=1")
water <- sqldf("select * from intake_data where ActivatorNum=7")


# we need to plot time series with the current data that you have
#and also make tidy data and plot 0 -1 day
#in total you need to make 2 (4) lists with 58 data frames

#make the time series data frame list first

f_807452 <- sqldf("select * from water where CowId=6277")
f_880369 <- sqldf("select * from water where CowId=6278")
f_880376 <- sqldf("select * from `water` where CowId=6279")
f_922376 <-sqldf("select * from `water` where CowId=6280")
f_922382<-sqldf("select * from `water` where CowId=6281")
f_922395<-sqldf("select * from `water` where CowId=6282")

f_922341<-sqldf("select * from `water` where CowId=6283")
f_922352<-sqldf("select * from `water` where CowId=6284")
f_922379<-sqldf("select * from `water` where CowId=6285")
f_922393<-sqldf("select * from `water` where CowId=6286")
f_922386<-sqldf("select * from `water` where CowId=6287")
f_922406<-sqldf("select * from `water` where CowId=6288")

f_880358<-sqldf("select * from `water` where CowId=6289")
f_922312<-sqldf("select * from `water` where CowId=6290")
f_922359<-sqldf("select * from `water` where CowId=6291")
f_922362<-sqldf("select * from `water` where CowId=6292")
f_922390<-sqldf("select * from `water` where CowId=6293")
f_922400<-sqldf("select * from `water` where CowId=6294")



f_807458<-sqldf("select * from `water` where CowId=6295")
f_922338<-sqldf("select * from `water` where CowId=6296") 
f_922340<-sqldf("select * from `water` where CowId=6297")
f_922366<-sqldf("select * from `water` where CowId=6298")
f_922387<-sqldf("select * from `water` where CowId=6299")
f_922397<-sqldf("select * from `water` where CowId=6300")

f_880383<-sqldf("select * from `water` where CowId=6301")
f_880388<-sqldf("select * from `water` where CowId=6302")
f_922335<-sqldf("select * from `water` where CowId=6303")
f_922364<-sqldf("select * from `water` where CowId=6304")
f_922375<-sqldf("select * from `water` where CowId=6305")
f_922404<-sqldf("select * from `water` where CowId=6306")

f_807459<-sqldf("select * from `water` where CowId=6307")
f_880351<-sqldf("select * from `water` where CowId=6308")
f_880355<-sqldf("select * from `water` where CowId=6309")
f_922360<-sqldf("select * from `water` where CowId=6310")
f_922369<-sqldf("select * from `water` where CowId=6311")
f_922380<-sqldf("select * from `water` where CowId=6312")

f_880375<-sqldf("select * from `water` where CowId=6313")
f_880380<-sqldf("select * from `water` where CowId=6314")
f_880385<-sqldf("select * from `water` where CowId=6315")
f_922365<-sqldf("select * from `water` where CowId=6316")
f_922398<-sqldf("select * from `water` where CowId=6317")
f_922405<-sqldf("select * from `water` where CowId=6318")




f_807439<-sqldf("select * from `water` where CowId=6319")
f_880352<-sqldf("select * from `water` where CowId=6320")
f_922320<-sqldf("select * from `water` where CowId=6321")
f_922334<-sqldf("select * from `water` where CowId=6322")
f_922337<-sqldf("select * from `water` where CowId=6323")
f_922378<-sqldf("select * from `water` where CowId=6324")

f_807446<-sqldf("select * from `water` where CowId=6325")
f_807451<-sqldf("select * from `water` where CowId=6326")
f_880350<-sqldf("select * from `water` where CowId=6327")
f_880390<-sqldf("select * from `water` where CowId=6328")
f_922313<-sqldf("select * from `water` where CowId=6329")
f_922317<-sqldf("select * from `water` where CowId=6330")
f_922322<-sqldf("select * from `water` where CowId=6331")
f_922332<-sqldf("select * from `water` where CowId=6332")
f_922358<-sqldf("select * from `water` where CowId=6333")
f_922367<-sqldf("select * from `water` where CowId=6334")
f_list <- list(f_807452,f_880369,f_880376,f_922376,f_922382,f_922395,f_922341,f_922352,f_922379,f_922393,f_922386,f_922406,f_880358,
               f_922312,f_922359,f_922362,f_922390,f_922400 ,f_807458,f_922338,f_922340,f_922366,f_922387,f_922397,
               f_880383,f_880388,f_922335,f_922364,f_922375,f_922404,f_807459,f_880351,f_880355,f_880355,f_922360,f_922369,f_922380,f_880375,f_880380,f_880385,
               f_922365, f_922398,f_922405,f_807439,f_880352,f_922320,f_922334,f_922337,f_922378,f_807446,f_807451,f_880350,f_880390,f_922313,f_922317,f_922322,f_922332,
               f_922358,f_922367)# make a function to plot all in the list

plot_f <- function(d){
  p <- ggplot()  
  p <- p+ geom_point(data=d,aes(x=as.Date(Date),y=pm,col=factor(Immunization)))+geom_line(data=d,aes(x=as.Date(Date),y=pm))+ggtitle(d$CowId)+ylab("Watervisits")+xlab("Time")
}
f_pl <- lapply(f_list, plot_f)
pdf("Water visits over time2.pdf",width=15,height=8,paper='special')
f_pl
dev.off()


#now try to make a function that...I forgot what I wanted
#now make tidy data frames and put them into list and plot for each animal
# first make a lag column to take records for a dayy before immunization for each imm day 1
# then filter to take only days of iimmunization into data frame 2
# then gather 3
#then take food and water individually
# then make individual dta frames 5


condition <- intake_data$Immunization==1


funkcija <- function(col) {
  
  ifelse(condition==T,lag(col),0)
}

intake_data$`-1` <- sapply(intake_data["pm"],FUN=funkcija) #1

intake_filtered <- filter(intake_data,Immunization!=0) # 2
colnames(intake_filtered)[5] <-"0"
intake_tidy <- gather(intake_filtered,day,intake,5,8) #3
food_tidy <- sqldf("select * from intake_tidy where ActivatorNum=1")
water_tidy$Date <- as.Date(water_tidy$Date)
water_tidy <- sqldf("select * from intake_tidy where ActivatorNum=7") # 4
food_tidy$Date <- as.Date(food_tidy$Date)


f_807452 <- sqldf("select * from food_tidy where CowId=6277")
f_880369 <- sqldf("select * from food_tidy where CowId=6278")
f_880376 <- sqldf("select * from `food_tidy` where CowId=6279")
f_922376 <-sqldf("select * from `food_tidy` where CowId=6280")
f_922382<-sqldf("select * from `food_tidy` where CowId=6281")
f_922395<-sqldf("select * from `food_tidy` where CowId=6282")

f_922341<-sqldf("select * from `food_tidy` where CowId=6283")
f_922352<-sqldf("select * from `food_tidy` where CowId=6284")
f_922379<-sqldf("select * from `food_tidy` where CowId=6285")
f_922393<-sqldf("select * from `food_tidy` where CowId=6286")
f_922386<-sqldf("select * from `food_tidy` where CowId=6287")
f_922406<-sqldf("select * from `food_tidy` where CowId=6288")

f_880358<-sqldf("select * from `food_tidy` where CowId=6289")
f_922312<-sqldf("select * from `food_tidy` where CowId=6290")
f_922359<-sqldf("select * from `food_tidy` where CowId=6291")
f_922362<-sqldf("select * from `food_tidy` where CowId=6292")
f_922390<-sqldf("select * from `food_tidy` where CowId=6293")
f_922400<-sqldf("select * from `food_tidy` where CowId=6294")



f_807458<-sqldf("select * from `food_tidy` where CowId=6295")
f_922338<-sqldf("select * from `food_tidy` where CowId=6296") 
f_922340<-sqldf("select * from `food_tidy` where CowId=6297")
f_922366<-sqldf("select * from `food_tidy` where CowId=6298")
f_922387<-sqldf("select * from `food_tidy` where CowId=6299")
f_922397<-sqldf("select * from `food_tidy` where CowId=6300")

f_880383<-sqldf("select * from `food_tidy` where CowId=6301")
f_880388<-sqldf("select * from `food_tidy` where CowId=6302")
f_922335<-sqldf("select * from `food_tidy` where CowId=6303")
f_922364<-sqldf("select * from `food_tidy` where CowId=6304")
f_922375<-sqldf("select * from `food_tidy` where CowId=6305")
f_922404<-sqldf("select * from `food_tidy` where CowId=6306")

f_807459<-sqldf("select * from `food_tidy` where CowId=6307")
f_880351<-sqldf("select * from `food_tidy` where CowId=6308")
f_880355<-sqldf("select * from `food_tidy` where CowId=6309")
f_922360<-sqldf("select * from `food_tidy` where CowId=6310")
f_922369<-sqldf("select * from `food_tidy` where CowId=6311")
f_922380<-sqldf("select * from `food_tidy` where CowId=6312")

f_880375<-sqldf("select * from `food_tidy` where CowId=6313")
f_880380<-sqldf("select * from `food_tidy` where CowId=6314")
f_880385<-sqldf("select * from `food_tidy` where CowId=6315")
f_922365<-sqldf("select * from `food_tidy` where CowId=6316")
f_922398<-sqldf("select * from `food_tidy` where CowId=6317")
f_922405<-sqldf("select * from `food_tidy` where CowId=6318")




f_807439<-sqldf("select * from `food_tidy` where CowId=6319")
f_880352<-sqldf("select * from `food_tidy` where CowId=6320")
f_922320<-sqldf("select * from `food_tidy` where CowId=6321")
f_922334<-sqldf("select * from `food_tidy` where CowId=6322")
f_922337<-sqldf("select * from `food_tidy` where CowId=6323")
f_922378<-sqldf("select * from `food_tidy` where CowId=6324")

f_807446<-sqldf("select * from `food_tidy` where CowId=6325")
f_807451<-sqldf("select * from `food_tidy` where CowId=6326")
f_880350<-sqldf("select * from `food_tidy` where CowId=6327")
f_880390<-sqldf("select * from `food_tidy` where CowId=6328")
f_922313<-sqldf("select * from `food_tidy` where CowId=6329")
f_922317<-sqldf("select * from `food_tidy` where CowId=6330")
f_922322<-sqldf("select * from `food_tidy` where CowId=6331")
f_922332<-sqldf("select * from `food_tidy` where CowId=6332")
f_922358<-sqldf("select * from `food_tidy` where CowId=6333")
f_922367<-sqldf("select * from `food_tidy` where CowId=6334")
f_list <- list(f_807452,f_880369,f_880376,f_922376,f_922382,f_922395,f_922341,f_922352,f_922379,f_922393,f_922386,f_922406,f_880358,
               f_922312,f_922359,f_922362,f_922390,f_922400 ,f_807458,f_922338,f_922340,f_922366,f_922387,f_922397,
               f_880383,f_880388,f_922335,f_922364,f_922375,f_922404,f_807459,f_880351,f_880355,f_880355,f_922360,f_922369,f_922380,f_880375,f_880380,f_880385,
               f_922365, f_922398,f_922405,f_807439,f_880352,f_922320,f_922334,f_922337,f_922378,f_807446,f_807451,f_880350,f_880390,f_922313,f_922317,f_922322,f_922332,
               f_922358,f_922367)# make a function to plot all in the list


plot_f <- function(d){
  p <- ggplot()  
  p <- p+ geom_point(data=d,aes(x=as.Date(Date),y=intake,shape=day,col=day),size=3.7)+scale_x_date(breaks = d$Date,labels=date_format("%m-%d"))+ggtitle(d$CowId)+ylab("Food Visits")+xlab("Date")
}
f_pl <- lapply(f_list, plot_f)
pdf("food difference.pdf",width=15,height=8,paper='special')
f_pl
dev.off()