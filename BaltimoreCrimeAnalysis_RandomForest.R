install.packages("dplyr")
install.packages("ggplot2")
install.packages("FactoMineR")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("caret")
install.packages("rpart")
install.packages("MASS")
install.packages("caTools")
install.packages("fpc")
install.packages("cluster")
install.packages("ggmap")
install.packages("class")
library(tidyverse)
library(caret)
library(rpart)
library(MASS)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(randomForest)
library(caTools)
library(fpc)
library(cluster)
library(ggmap)
library(class)
#Importing Dataset
crime_dat<-read.csv("BPD_Crime.csv",header = TRUE, sep = ",")

# Plots 

#Extracting and making new columns for year and month
crime_dat$year<-substring(crime_dat$CrimeDate,7,11)
crime_dat$month<-substring(crime_dat$CrimeDate,1,2)
temporary_dataset<-crime_dat[,c("year","CrimeCode","Description")]

#Count of crimes by type of crime
crime_count<-temporary_dataset %>% group_by(Description) %>% tally() %>% arrange(desc(n))

#unique(crime_dat$year)
#Count of crimes by type of crime, yearwise
crime_year <- temporary_dataset %>% group_by(year,Description) %>% filter(year > "2013") %>% tally() 
ggplot(crime_year,aes(year,n,colour = Description, group=1))+geom_point() +xlab("Year")+ylab("Count")

#Count of crimes by year
crime_count_year <- temporary_dataset %>% group_by(year) %>% filter(year > "2013") %>% tally() %>% arrange(desc(n))
ggplot(crime_count_year,aes(year,n,fill = year)) + geom_bar(stat="identity")


unique(crime_dat$District)
#Bar Plot of description and count per year 
ggplot(crime_count,aes(Description,n)) + geom_bar(stat="identity")

#Analyzing by district
crime_district<-crime_dat[,c("District","year","CrimeCode","Description")] %>% group_by(District)

#Crime count by district
crime_count_district<-crime_district %>%  group_by(District) %>% tally() %>% arrange(desc(n))
ggplot(crime_count_district,aes(District,n,fill=District)) + geom_bar(stat="identity")

#Crime count by month
temporary_dataset<-crime_dat[,c("month","CrimeCode","Description")]
crime_month <- temporary_dataset %>% group_by(month) %>% tally() 
ggplot(crime_month,aes(month,n,fill=month)) + geom_bar(stat="identity")

#Count of crimes by hour of day
temporary_dataset_2<-crime_dat_miss[,c("hour","CrimeCode")]
temporary_dataset_2<-crime_dat_miss[,c("hour","Crime")]
crime_count_hourofday <- temporary_dataset_2 %>% group_by(hour,Crime) %>% tally() %>% arrange(desc(n))
crime_count_hourofday_2 <- temporary_dataset_2 %>% group_by(hour) %>% tally() %>% arrange(desc(n))

#ggplot(crime_count_hourofday,aes(hour,n,fill=Crime)) + geom_bar(stat="identity",aes(group=CrimeCode))
ggplot(crime_count_hourofday, aes(fill=Crime, y=n, x=hour)) + 
  geom_bar(position="stack", stat="identity")

ggplot(crime_count_hourofday, aes(x = hour, y = n,
                      group = Crime,
                      colour = Crime)) +
  geom_line()

#Inside vs Outside
ggplot(data =crime_dat[!is.na(crime_dat$Inside.Outside),] , aes(x = Inside.Outside, fill= Description))+
  geom_bar(position = 'dodge') +
  labs(title = "Number of incidents by Crime Group\n", x = "Inside or Outside", y = "Number", color = "Crime Group\n")

#Crime by weapon
ggplot(data=crime_dat , aes(Weapon)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill='Blue', color='Black') + 
  labs(x= "Weapon Type", y="Number of Incident") 

#Getting crime by hour of day
crime_count_hourofday_1 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="1")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_2 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="2")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_3 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="3")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_4 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="4")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_5 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="5")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_6 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="6")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_7 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="7")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_8 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="8")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_9 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="9")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_10 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="10")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_11 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="11")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_12 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="12")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_13 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="13")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_14 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="14")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_15 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="15")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_16 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="16")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_17 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="17")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_18 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="18")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_19 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="19")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_20 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="20")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_21 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="21")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_22 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="22")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_23 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="23")%>% tally() %>% arrange(desc(n))
crime_count_hourofday_24 <- temporary_dataset_2 %>% group_by(Crime) %>% filter(hour=="24")%>% tally() %>% arrange(desc(n))


#DATA ANALYSIS

#Data Cleaning
#Subsetting rows contatining missing values
crime_dat_miss <- subset(crime_dat, select = c(CrimeDate,CrimeTime,Location,Description,District,Longitude,Latitude,year,month))
crime_dat_miss <- crime_dat_miss[complete.cases(crime_dat_miss), ]

crime_dat_miss$hour<-substring(crime_dat_miss$CrimeTime,1,2)
#crime_dat_miss$TimeOfDay <- with(crime_dat_miss,  ifelse(hour >= 5 & hour<12, "Morning",
#                                                         ifelse(hour>=12 & hour<=17, "Afternoon", ifelse(hour>=17 & hour<=22, "Evening", "Night"))))
crime_dat_miss$TimeOfDay <- with(crime_dat_miss,  ifelse(hour >= 5 & hour<18, "Day","Night"))

crime_dat_miss$hour<- as.factor(crime_dat_miss$hour)

crime_dat_miss$CrimeDate <- as.Date(crime_dat_miss$CrimeDate,format="%m/%d/%Y")
crime_dat_miss$day <- weekdays(as.Date(crime_dat_miss$CrimeDate))

  
crime_dat_miss$DayOfWeek <- with(crime_dat_miss,  ifelse(day=="Monday", "1",
                                                         ifelse(day=="Tuesday", "2",
                                                                ifelse(day=="Wednesday", "3",
                                                                       ifelse(day=="Thursday", "4",
                                                                              ifelse(day=="Friday", "5",
                                                                                     ifelse(day=="Saturday", "6","7")))))))
                                                         
#crime_dat_miss$Location <- as.character(crime_dat_miss$Location)
crime_dat_miss$TimeOfDay <- as.factor(crime_dat_miss$TimeOfDay)
crime_dat_miss <- subset(crime_dat_miss, select = c(Location,Description,District,Longitude,Latitude,year,month,TimeOfDay,hour,DayOfWeek))

crime_dat_miss$Crime <- ifelse(crime_dat_miss$Description =="ROBBERY - CARJACKING", "ROBBERY", ifelse(crime_dat_miss$Description =="ROBBERY - COMMERCIAL", "ROBBERY", ifelse(crime_dat_miss$Description =="ROBBERY - RESIDENCE", "ROBBERY", ifelse(crime_dat_miss$Description =="ROBBERY - STREET", "ROBBERY", ifelse(crime_dat_miss$Description =="LARCENY FROM AUTO", "LARCENY",ifelse(crime_dat_miss$Description =="AGG. ASSAULT", "ASSAULT",ifelse(crime_dat_miss$Description =="ARSON", "ARSON",ifelse(crime_dat_miss$Description =="AUTO THEFT", "AUTO THEFT",ifelse(crime_dat_miss$Description =="BURGLARY", "BURGLARY",ifelse(crime_dat_miss$Description =="COMMON ASSAULT", "ASSAULT",ifelse(crime_dat_miss$Description =="HOMICIDE", "HOMICIDE",ifelse(crime_dat_miss$Description =="RAPE", "RAPE",ifelse(crime_dat_miss$Description =="SHOOTING", "SHOOTING",ifelse(crime_dat_miss$Description =="LARCENY", "LARCENY",NA ) ) ) ) ) ) ) ) ) )))))
crime_dat_miss$Crime <- as.factor(crime_dat_miss$Crime)
crime_dat_miss$DayOfWeek <- as.factor(crime_dat_miss$DayOfWeek)
crime_dat_miss$CrimeCode <- ifelse(crime_dat_miss$Crime =="ARSON",1,ifelse(crime_dat_miss$Crime =="ASSAULT",2,ifelse(crime_dat_miss$Crime =="AUTO THEFT",3,ifelse(crime_dat_miss$Crime =="BURGLARY",4,ifelse(crime_dat_miss$Crime =="HOMICIDE",5,ifelse(crime_dat_miss$Crime =="LARCENY",6,ifelse(crime_dat_miss$Crime =="RAPE",7,ifelse(crime_dat_miss$Crime =="ROBBERY",8,9))))))))

#Decision Forst Classifier
str(crime_dat_miss)
set.seed(42)
#data.imputed <- rfImpute( District ~ ., data = crime_dat_miss, iter=6)
model <- randomForest(DayOfWeek ~ District + Description, data=crime_dat_miss, ntree=200)
#model <- randomForest(CrimeCode ~ Latitude + Longitude, data=crime_dat_miss)

#K Means Clustering


set.seed(20)
CrimeCluster <- kmeans(crime_dat_miss[, 4:5], 9, nstart = 20)
crime_dat_miss$loc <- as.factor(CrimeCluster$cluster)
str(CrimeCluster)



#Decision Tree
set.seed(1000)
#Creating training and testing datasets

crime_dat_miss <- crime_dat_miss %>%  filter(year > "2013") 
crime_dat_miss$hour <- as.numeric(crime_dat_miss$hour)
crime_dat_miss$TimeOfDay <- with(crime_dat_miss,  ifelse(hour >= 5 & hour<12, "Morning",
                                                         ifelse(hour>=12 & hour<=17, "Afternoon", ifelse(hour>=17 & hour<=20, "Evening", "Night"))))
crime_dat_miss$TimeOfDay <- as.factor(crime_dat_miss$TimeOfDay)
crime_dat_miss$DayOfWeek <- as.factor(crime_dat_miss$DayOfWeek)
ind <- sample.split(Y=crime_dat_miss$CrimeCode, SplitRatio = 0.75)
crime_dat_miss$CrimeCode <- as.numeric(crime_dat_miss$CrimeCode)
train_set <- crime_dat_miss[ind,]
test_set <- crime_dat_miss[!ind,]

CrimeTree <- rpart(District ~ TimeOfDay + DayOfWeek + hour + Description, data = train_set , method = "class")
par(xpd = NA) # Avoid clipping the text in some device
plot(CrimeTree)
text(CrimeTree)
predicted.classes <- CrimeTree %>% predict(test_set, type ="class")
mean(predicted.classes == test_set$CrimeCode)

#KNN Classification
#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x))) }
#prc_n <- as.data.frame(lapply(crime_dat_miss[4:5], normalize))
#train_set <- crime_dat_miss[ind,]
#test_set <- crime_dat_miss[!ind,]
#prc_test_pred <- knn(train = train_set, test = test_set,cl = train_set$Crime, k=10)

#Classifying Data into violent and non-violent crimes
crime_dat_miss$CrimeViolent <- ifelse(crime_dat_miss$Description =="ROBBERY - CARJACKING", "0", ifelse(crime_dat_miss$Description =="ROBBERY - COMMERCIAL", "0", ifelse(crime_dat_miss$Description =="ROBBERY - RESIDENCE", "0", ifelse(crime_dat_miss$Description =="ROBBERY - STREET", "0", ifelse(crime_dat_miss$Description =="LARCENY FROM AUTO", "0",ifelse(crime_dat_miss$Description =="AGG. ASSAULT", "1",ifelse(crime_dat_miss$Description =="ARSON", "0",ifelse(crime_dat_miss$Description =="AUTO THEFT", "0",ifelse(crime_dat_miss$Description =="BURGLARY", "0",ifelse(crime_dat_miss$Description =="COMMON ASSAULT", "1",ifelse(crime_dat_miss$Description =="HOMICIDE", "1",ifelse(crime_dat_miss$Description =="RAPE", "1",ifelse(crime_dat_miss$Description =="SHOOTING", "1",ifelse(crime_dat_miss$Description =="LARCENY", "0",NA ) ) ) ) ) ) ) ) ) )))))
crime_dat_miss$DayOfWeek <- as.character(crime_dat_miss$DayOfWeek)
crime_dat_miss$DayOfWeek <- with(crime_dat_miss,  ifelse(day=="Monday", "1",
                                                         ifelse(day=="Tuesday", "2",
                                                                ifelse(day=="Wednesday", "3",
                                                                       ifelse(day=="Thursday", "4",
                                                                              ifelse(day=="Friday", "5",
                                                                                     ifelse(day=="Saturday", "6","7")))))))


crime_dat_miss$month <- as.numeric(crime_dat_miss$month)
crime_dat_miss$Season <- ifelse(crime_dat_miss$month <="3", "S1", ifelse(crime_dat_miss$month <= "6", "S2", ifelse(crime_dat_miss$month <="9", "S3", "S4")))
crime_dat_miss$Season <- as.factor(crime_dat_miss$Season)
crime_dat_miss$WeekdayCheck <- ifelse(crime_dat_miss$DayOfWeek <"6", "0", "1")
crime_dat_miss$WeekdayCheck <- as.factor(crime_dat_miss$WeekdayCheck)

crime_dat_miss$CrimeViolent <- as.factor(crime_dat_miss$CrimeViolent)
crime_dat_miss$TimeOfDay <- as.factor(crime_dat_miss$TimeOfDay)
count(crime_dat_miss[crime_dat_miss$CrimeViolent=="1",])

###### Random Forest
model <- randomForest(CrimeViolent ~ DayOfWeek + Season + Latitude + Longitude + hour + District, data=crime_dat_miss, ntree=200)


# Ggmap plotting crime distribution 
#ggmap

BaltimoreMap <- get_map(location = c(min(-76.625), max(39.3)),zoom=15)
ggmap(BaltimoreMap) + geom_point(aes(x = Longitude, y = Latitude, colour = as.factor(crime_dat_miss$Description)),data = crime_dat_miss,size=1) +
  ggtitle("Crime Distribution")+labs(color="Crime") 

######################
ggplot(data = subset(states, region == c("maryland")), mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  geom_point(data=crime_dat_miss,aes(x = crime_dat_miss$Longitude, y = crime_dat_miss$Latitude,group=crime_dat_miss$Description),pch=21, size=1, alpha=I(0.005)) + 
  coord_fixed(xlim = c(-76.71162, -76.5285),  ylim = c(39.20041, 39.37293) ) 

register_google(key = "AIzaSyDjLgqH4IJJdmQyBIVHKlB7eaacFYS9fV4", write = TRUE)


clusplot(CrimeCluster, CrimeCluster$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
plotcluster(CrimeCluster, CrimeCluster$cluster)

BaltimoreMap <- get_map(location = c(min(-76.625), max(39.3)),zoom=12)
ggmap(BaltimoreMap) + geom_point(aes(x = Longitude, y = Latitude, colour = as.factor(crime_dat_miss$Crime)),data = crime_dat_miss,size=1) +
  ggtitle("Crime Distribution")+labs(color="Crime")

