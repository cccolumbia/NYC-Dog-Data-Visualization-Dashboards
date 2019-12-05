 
dog.bites <- read.csv("DOHMH_Dog_Bite_Data.csv")
dog.licenses <- read.csv("NYC_Dog_Licensing_Dataset.csv")
dog.licenses$Borough <- as.character(dog.licenses$Borough)
 
 
borough <- data.frame(table(dog.licenses$Borough))
bronx <- borough$Var1[10:13]
brooklyn <- borough$Var1[15:17]
manhattan <- borough$Var1[40:42]
queens <- borough$Var1[60:63]
statenlsland <- borough$Var1[71:74]
others <- borough$Var1[- c(bronx,brooklyn,manhattan,queens,statenlsland)]
others <- data.frame(others)
dog.licenses$Borough <- ifelse(dog.licenses$Borough %in% others$others,"others",dog.licenses$Borough)
bronx <- data.frame(bronx)
brooklyn <- data.frame(brooklyn)
manhattan <- data.frame(manhattan)
queens <- data.frame(queens)
statenlsland <- data.frame(statenlsland)
  

dog.licenses$Borough <- ifelse(dog.licenses$Borough %in% bronx[,1],"Bronx",dog.licenses$Borough)
dog.licenses$Borough <- ifelse(dog.licenses$Borough %in% brooklyn[,1],"Brooklyn",dog.licenses$Borough)
dog.licenses$Borough <- ifelse(dog.licenses$Borough %in% manhattan[,1],"Manhattan",dog.licenses$Borough)
dog.licenses$Borough <- ifelse(dog.licenses$Borough %in% queens[,1],"Queens",dog.licenses$Borough)
dog.licenses$Borough <- ifelse(dog.licenses$Borough %in% statenlsland[,1],"Staten Island",dog.licenses$Borough)
table(dog.licenses$Borough)



library(stringr)
dog.licenses$AnimalName <- str_replace_all(dog.licenses$AnimalName,fixed(" "),"")
dog.bites <- dog.bites[,-1]
dog.bites <- dog.bites[-seq(1,nrow(dog.bites))[dog.bites$Borough == "Other"],]
dog.licenses <- dog.licenses[,-1]
dog.licenses <- dog.licenses[-seq(1,nrow(dog.licenses))[dog.licenses$Borough == "others"],]



write.csv(dog.bites,file = "dog.bites.csv")
write.csv(dog.licenses,file="dog.licenses.csv")



dog.bites <- read.csv("dog.bites.csv")
dog.licenses <- read.csv("dog.licenses.csv")



#dogname <- data.frame(table(dog.licenses$AnimalName))
noname.index <- seq(1,nrow(dog.licenses.date))[dog.licenses.date$AnimalName %in% c("NAMENOTPROVIDED","UNKNOWN","NON","NOT","NA","N/A","NONE","NAME","NONAME","N.A.")]
dog.licenses.name <- dog.licenses.date[-noname.index,]
# dogname <- dogname[-noname.index,]
# dogname <- dogname[order(dogname[,2],decreasing = T),]
# dog.licenses.name.all <- dogname[1:10,]
# colnames(dog.licenses.name.all)[1] <- "Name" 
# dog.licenses.name.all$Borough = "New York City"
# #dogname <- dogname[dogname[,2]>=50,]
# dog.licenses.name <- dog.licenses[dog.licenses$AnimalName %in% dogname[,1],]
# dog.licenses.name <- subset(dog.licenses.name,select=c(AnimalName,Borough))
# 
# dog.licenses.name2 <- data.frame()
# for (i in unique(dog.licenses.name$Borough)){
#   borough.data <- dog.licenses.name[dog.licenses.name$Borough==i,]
#   borough.data <- data.frame(table(borough.data$AnimalName))
#   borough.data <- borough.data[order(borough.data[,2],decreasing = T),]
#   borough.data <- borough.data[1:10,]
#   borough.data$Borough <- i
#   dog.licenses.name2 <- rbind(dog.licenses.name2,borough.data)
# }

colnames(dog.licenses.name)[2] <- "Name"
# 
# dog.licenses.name2 <- rbind(dog.licenses.name.all,dog.licenses.name2)
#write.csv(dog.licenses.name.all,file="dog.licenses.name.all.csv")
write.csv(dog.licenses.name,file="dog.licenses.name.csv")


data <- read.csv("dog.licenses.name2.csv")
data <- data[,-1]
data <- data[data$Borough== "Brooklyn",]
# p <- ggplot(data=data,aes(x=AnimalName,y=Freq))+
#         geom_col()
# ggplotly(p)
# library(extrafont)
# plot_ly(data2,x= ~AnimalName, y= ~Freq, type= "bar",
#             marker = list(color = 'rgb(158,202,225)',
#             line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
#       layout(title = paste("Dog Names within","Brooklyn"),
#              xaxis = list(title = "Dog Name"),
#              yaxis = list(title = ""))



plot_ly(data,x= ~AnimalName, y= ~Freq, type= "bar",
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = paste("Dog Names within", "Brooklyn"),
         xaxis = list(title = "Dog Name"),
         yaxis = list(title = ""))


##Q4

na.index <- seq(1,nrow(dog.bites.date))[dog.bites.date$Breed %in% c("","UNKNOWN","MIXED","Mixed/Other")]

dog.bites.breed <- dog.bites.date[-na.index,]
# dog.bites.breed.all <- data.frame(table(dog.bites.breed$Breed))
# dog.bites.breed.all <- dog.bites.breed.all[order(dog.bites.breed.all[,2],decreasing = T),]
# dog.bites.breed.all <- dog.bites.breed.all[1:10,]
# colnames(dog.bites.breed.all)[1] <- "Breed"
# dog.bites.breed.all$Borough <- "New York City"
# 
# dog.bites.breed2 <- data.frame()
# for (i in unique(dog.bites.breed$Borough)){
#   borough.data <- dog.bites.breed[dog.bites.breed$Borough==i,]
#   borough.data <- data.frame(table(borough.data$Breed))
#   borough.data <- borough.data[order(borough.data[,2],decreasing = T),]
#   borough.data <- borough.data[1:10,]
#   borough.data$Borough <- i
#   dog.bites.breed2 <- rbind(dog.bites.breed2,borough.data)
# }
# colnames(dog.bites.breed2)[1] <- "Breed"
# dog.bites.breed2 <- rbind(dog.bites.breed.all,dog.bites.breed2)

#write.csv(dog.bites.breed.all, file="dog.bites.breeds.all.csv")
write.csv(dog.bites.breed,file="dog.bites.breed.csv")



na.index <- seq(1,nrow(dog.licenses.date))[dog.licenses.date$BreedName %in% c("","Unknown","MIXED","Mixed/Other")]
dog.licenses.breed <- dog.licenses.date[-na.index,]
# dog.licenses.breed.all <- data.frame(table(dog.licenses.breed$BreedName))
# dog.licenses.breed.all <- dog.licenses.breed.all[order(dog.licenses.breed.all[,2],decreasing = T),]
# dog.licenses.breed.all <- dog.licenses.breed.all[1:10,]
# colnames(dog.licenses.breed.all)[1] <- "Breed"
# dog.licenses.breed.all$Borough <- "New York City"
# 
# dog.licenses.breed2 <- data.frame()
# for (i in unique(dog.licenses.breed$Borough)){
#   borough.data <- dog.licenses.breed[dog.licenses.breed$Borough==i,]
#   borough.data <- data.frame(table(borough.data$Breed))
#   borough.data <- borough.data[order(borough.data[,2],decreasing = T),]
#   borough.data <- borough.data[1:10,]
#   borough.data$Borough <- i
#   dog.licenses.breed2 <- rbind(dog.licenses.breed2,borough.data)
# }
# colnames(dog.licenses.breed2)[1] <- "Breed"
#dog.licenses.breed2 <- rbind(dog.licenses.breed.all,dog.licenses.breed2)

#write.csv(dog.licenses.breed.all, file="dog.licenses.breed.all.csv")
colnames(dog.licenses.breed)[5] <- "Breed"
write.csv(dog.licenses.breed,file="dog.licenses.breed.csv")


##Which bites more?

unique(dog.bites.age$Age)



library(stringr)
dog.bites.date$Age <- str_replace_all(dog.bites.date$Age,fixed(" "),"")
dog.bites.date$Age <- toupper(dog.bites.date$Age)
dog.bites.date$Age <- ifelse(nchar(dog.bites.date$Age)>5,"",dog.bites.date$Age)

dog.bites.age <- dog.bites.date[dog.bites.date$Age!="",]
dog.bites.age <- dog.bites.age[!grepl("&",dog.bites.age$Age),]
dog.bites.age$Age[dog.bites.age$Age=="2-3YR"] <- "2.5"
dog.bites.age$Age[dog.bites.age$Age=="2-3M"] <- "2.5M"

dog.bites.age$Age <- ifelse(grepl("W",dog.bites.age$Age),0,dog.bites.age$Age)
dog.bites.age$Age <- ifelse(grepl("M",dog.bites.age$Age),1,dog.bites.age$Age)
dog.bites.age$Age <- ifelse(grepl("Y",dog.bites.age$Age),regmatches(dog.bites.age$Age,gregexpr('[0-9]+',dog.bites.age$Age)),dog.bites.age$Age)

dog.bites.age$Age <- as.numeric(dog.bites.age$Age)

write.csv(dog.bites.age,file = "dog.bites.age.csv")


dog.bites.gender <- dog.bites.date[dog.bites.date$Gender != "U",]
dog.bites.gender$Gender <- ifelse(dog.bites.gender$Gender == "F","Female","Male")
write.csv(dog.bites.gender,file="dog.bites.gender.csv")


unique(dog.licenses$AnimalGender)
dog.licenses.gender <- dog.licenses.date
colnames(dog.licenses.gender)[3] <- "Gender"
dog.licenses.gender$Gender <- ifelse(dog.licenses.gender$Gender == "F","Female","Male")
write.csv(dog.licenses.gender,file="dog.licenses.gender.csv")


library(ggplot2)
library(plotly)


dog.bites.spay <- dog.bites.date
dog.bites.spay$SpayNeuter <- ifelse(dog.bites.spay$SpayNeuter == "true","spayed/neutered","non-spayed/non-neutered")
write.csv(dog.bites.spay,file="dog.bites.spay.csv")


dog.licenses.date <- dog.licenses
dog.licenses.date$BirthYear <- substr(dog.licenses.date$AnimalBirthMonth,7,10)
dog.licenses.date$BirthMonth <-substr(dog.licenses.date$AnimalBirthMonth,1,2)
dog.licenses.date$LicenseIssuedYear<-substr(dog.licenses.date$LicenseIssuedDate,7,10)
dog.licenses.date$LicenseIssuedMonth<-substr(dog.licenses.date$LicenseIssuedDate,1,2)
dog.licenses.date$LicenseExpiredYear<-substr(dog.licenses.date$LicenseExpiredDate,7,10)
dog.licenses.date$LicenseExpiredMonth<-substr(dog.licenses.date$LicenseExpiredDate,1,2)


dog.name.year <- dog.licenses.date
noname.index <- seq(1,nrow(dog.name.year))[dog.name.year$AnimalName %in% c("NAMENOTPROVIDED","UNKNOWN","NON","NOT","NA","N/A","NONE","NAME","NONAME","N.A.")]
dog.name.year <- dog.name.year[-noname.index,]
#unique(as.numeric(dog.name.year$BirthYear))
dog.name.year <- dog.name.year[,-1]
#set.seed(0)
#dog.name.year <- dog.name.year[sample(seq(1,nrow(dog.name.year)),10000),]
colnames(dog.name.year)[1] <- "Name"
write.csv(dog.name.year,file="dog.name.year.csv")


data <- read.csv("dog.name.year.csv")
data <- data[data$BirthYear==2017,]
data <- data.frame(table(data[,"Name"]))
data <- data[order(data[,2],decreasing = T),]
data <- data[1:10,]


library(zipcode)
library(maps)
library(mapproj)

dog.licenses.map <- dog.licenses.date
dog.licenses.map <- dog.licenses.map[dog.licenses.map$ZipCode!="",]
data(zipcode)
dog.licenses.map$ZipCode <- clean.zipcodes(dog.licenses.map$ZipCode)
zip <- data.frame(table(dog.licenses.map$ZipCode))
colnames(zip) <- c("ZipCode","Freq")
zip <- merge(zip, zipcode, by.x = "ZipCode", by.y="zip")
zip <- zip[zip$state == "NY",]
#dog.licenses.map <- merge(dog.licenses.map,zip,by="ZipCode")
#dog.licenses.map <- unique(subset(dog.licenses.map,select = c(ZipCode,Borough,Freq,latitude,longitude)))
write.csv(dog.licenses.map,file="dog.licenses.map.csv")


library(nycmaps)
library(maps)
nyc <- map_data("nyc")
gg  <- ggplot()
gg  <- gg + 
  geom_map(
    data=nyc, 
    map=nyc,
    aes(x=long, y=lat, map_id=region),color="black",fill=NA)
gg


library(sf)
library(ggplot2)
library(plotly)
aoi_boundary_HARV <- st_read(
  "ZIP_CODE_040114/ZIP_CODE_040114.shp")
p <- ggplot(data=map) + 
  geom_sf(aes(fill=Freq,text = ZIPCODE), size = 0.5, color = "black") 
p %>%
  ggplotly(tooltip = "text")



map <- aoi_boundary_HARV

index <- data.frame("index"=as.numeric(rownames(map)),"ZIPCODE"=map$ZIPCODE)
map1 <- merge(index,zip,by.x="ZIPCODE",by.y="ZipCode",all.x=T)
map1$Freq[is.na(map1$Freq)] <- 0
map1 <- map1[order(map1$index,decreasing = F),]
rownames(map1) <- map1$index
map$Freq <- map1$Freq


library(stringr)
dog.bites.date <- dog.bites
dog.bites.date$Month <- substr(dog.bites.date$DateOfBite,1,3)
#dog.bites.date$Month <- match(dog.bites.date$Month,month.abb)
dog.bites.date$Month <- factor(dog.bites.date$Month,levels=month.abb)
dog.bites.date$Year <- str_sub(dog.bites.date$DateOfBite,-4,-1)
dog.bites.date$DateOfBite <- as.character(as.Date(dog.bites.date$DateOfBite, "%B %d %Y"))
dog.bites.date$Weekday <- weekdays(as.POSIXct(dog.bites.date$DateOfBite), abbreviate = T)
write.csv(dog.bites.date,file="dog.bites.date.csv")


dog.bites.map <- dog.bites.date[dog.bites.date$ZipCode!="",]
data(zipcode)
dog.bites.map$ZipCode <- clean.zipcodes(dog.bites.map$ZipCode)
write.csv(dog.bites.map,file="dog.bites.map.csv")


national.bites.month <- data.frame("Lab"=month.abb,"Freq"=c(210000,190000,245000,275000,300000,295000,325000,280000,250000,240000,220000,225000),"Group"="Month")
national.bites.weekday <- data.frame("Lab"=weekday.abb,"Freq"=c(500000,440000,400000,380000,390000,440000,501000),"Group"="Weekday")
national.bites <- rbind(national.bites.month,national.bites.weekday)
national.bites$Borough <- "the United States"
write.csv(national.bites,file="national.bites.csv")



dog.licenses.date$AnimalBirthMonth <- as.character(dog.licenses.date$AnimalBirthMonth)
dog.licenses.date$BirthMonth <-substr(dog.licenses.date$AnimalBirthMonth,1,2)
#dog.licenses.date$BirthMonth <-as.numeric(dog.licenses.date$AnimalBirthMonth)
#dog.licenses.date$BirthMonth <- match(dog.licenses.date$BirthMonth,month.abb)
write.csv(dog.licenses.date,file = "dog.licenses.date.csv")


dog.licenses.date2 <- dog.licenses.date
dog.licenses.date2$AnimalBirthMonth <- as.character(substr(dog.licenses.date2$AnimalBirthMonth,1,10))
dog.licenses.date2$LicenseIssuedDate <- as.character(dog.licenses.date2$LicenseIssuedDate)

dog.licenses.date2$BirthYear <- substr(dog.licenses.date2$AnimalBirthMonth,7,10)

dog.licenses.date2$BirthMonth <-substr(dog.licenses.date2$AnimalBirthMonth,1,2)

dog.licenses.date2$LicenseIssuedYear<-substr(dog.licenses.date2$LicenseIssuedDate,7,10)

dog.licenses.date2$LicenseIssuedMonth<-substr(dog.licenses.date2$LicenseIssuedDate,1,2)

dog.licenses.date2$LicenseExpiredYear<-substr(dog.licenses.date2$LicenseExpiredDate,7,10)

dog.licenses.date2$LicenseExpiredMonth<-substr(dog.licenses.date2$LicenseExpiredDate,1,2)



dog.licenses.date2 <- dog.licenses.date2[dog.licenses.date2$BirthYear>=2014,]
dog.licenses.date2 <- dog.licenses.date2[dog.licenses.date2$LicenseIssuedYear>=2014,]

set.seed(0)
dog.licenses.date2 <- dog.licenses.date2[sample(1:nrow(dog.licenses.date2),1000),]

dog.licenses.date2$LicenseIssuedDate <- str_replace_all(dog.licenses.date2$LicenseIssuedDate,substr(dog.licenses.date2$LicenseIssuedDate,4,5),"01")

dog.licenses.date2$AnimalBirthMonth <- as.character(dog.licenses.date2$AnimalBirthMonth)
dog.licenses.date2$AnimalBirthMonth <-as.Date(dog.licenses.date2$AnimalBirthMonth, "%m/%d/%Y")

dog.licenses.date2 <- dog.licenses.date2[order(dog.licenses.date2$AnimalBirthMonth,decreasing = F),]


dog.licenses.date2$LicenseIssuedDate <-as.Date(as.character( dog.licenses.date2$LicenseIssuedDate), "%m/%d/%Y")
dog.licenses.date2 <- dog.licenses.date2[order(dog.licenses.date2$LicenseIssuedDate,decreasing = F),]

rownames(dog.licenses.date2) <- 1:nrow(dog.licenses.date2)
dog.licenses.date2 <- dog.licenses.date2[-seq(1,79),]

dog.licenses.date2$LicenseIssuedDate <- as.character(dog.licenses.date2$LicenseIssuedDate)

dog.licenses.date2$index <- 1:nrow(dog.licenses.date2)

ggplot(data=dog.licenses.date2)+
  geom_line(mapping = aes(x=index,y=as.Date(AnimalBirthMonth, "%Y-%m-%d")))+   
  geom_line(mapping = aes(x=index,y=as.Date(LicenseIssuedDate,"%Y-%m-%d")))

write.csv(dog.licenses.date2,file="dog.licenses.date2.csv")


file <- "dog.licenses.date2.csv"
Objective <- "AnimalBirthMonth"
Objective2 <- "LicenseIssuedDate"

data <- read.csv(file)
data <- data[,c("Borough","BirthYear",Objective,Objective2)]

data[,Objective] <- as.Date(data[,Objective],"%Y-%m-%d")
data[,Objective2] <- as.Date(data[,Objective2],"%Y-%m-%d")
data <- data[order(data[,Objective],decreasing = F),]


