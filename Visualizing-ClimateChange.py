#Data From NOAA’s website

getwd()
library(gtools)
setwd(“/Users/jheeldoshi/Downloads/Storm”)
#Removed all lagging variables post the year in the csv names
df <- read.csv(“StormEvents_details-ftp_v1.0_d1950.csv”,stringsAsFactors = FALSE,header = TRUE,numerals = “no.loss”,colClasses=c(rep(“character”,5)),na.strings=c(“”,” “,”NA”))
#x
#View(x)

years <- as.matrix(seq(1951,2016,1))

for(i in 1:length(years))
{
var <- as.character(as.matrix(years[i,1]))
var1 <- paste0(“StormEvents_details-ftp_v1.0_d”,var,”.csv”)
y <- read.csv(var1,stringsAsFactors = FALSE,header = TRUE,numerals = “no.loss”,colClasses=c(rep(“character”,5)),na.strings=c(“”,” “,”NA”))
df <- smartbind(df,y)
}
rm(years)
rm(var)
rm(var1)
rm(y)
rm(i)

x <- df

# Add citation for this function
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
#propmiss(x)
#The following columns are completely missing

x$TOR_OTHER_WFO <- NULL
x$TOR_OTHER_CZ_STATE <- NULL
x$TOR_OTHER_CZ_FIPS <- NULL
x$TOR_OTHER_CZ_NAME <- NULL
x$EPISODE_NARRATIVE <- NULL
#View(x[toupper(x$EVENT_TYPE)==’OTHER’,])
#View(x[x$EPISODE_ID == ‘1119823’ & x$EVENT_ID==’5258604′,])

x[x$EPISODE_ID == ‘1119823’ & x$EVENT_ID==’5258604′,”EVENT_TYPE”] <- “Dust Devil”
#NOT REQUIRED
x$DATA_SOURCE <- NULL
x$EVENT_NARRATIVE <- NULL
x$BEGIN_DAY <- NULL
x$END_DAY <- NULL
x$END_TIME <- NULL

x$BEGIN_YEARMONTH <- NULL
x$END_YEARMONTH <- NULL
x$CZ_TIMEZONE <- NULL
x$BEGIN_TIME <- NULL
x$WFO <- NULL
x$SOURCE <- NULL
#1.4 MILLION RECORDS

#EXPLORING DATA
library(sqldf)
#q1 <- sqldf(‘Select COUNT(*) from x where EPISODE_NARRATIVE is NULL and EVENT_NARRATIVE is NULL LIMIT 100;’)
options(digits=2)
#m <- as.data.frame((table(x$STATE)*100)/nrow(x))
#View(m)
DescriptiveProps <- function(x)
{
print(head(x))
print(str(x))
print(summary(x))
}
most_common <- function(x){ tail(sort(table(x)),n=50L) }

# BELOW questions from https://www.epa.gov/climate-change-science/understanding-link-between-climate-change-and-extreme-weather
#Frequency: Are events occurring more often than they did in the past?
#Intensity: Are events getting more severe, with the potential for more damaging effects?
#Duration: Are events lasting longer than “the norm”?
#Timing: Are events occurring earlier or later in the season or the year than they used to?
#DescriptiveProps(x)
#apply(x,2,most_common)
#1. Divide year and month
#2. Is time important
#3. Create a variable that tells you the number of days event lasted
#4. Remove Data_Source(CSV, pdf etc), remove event_Descriptive and event_narrative as text mining can be done if time left
#5. Check how many beg_lat & beg_long missing (IMPUTE FROM STATE or end lat-long)
#6. Event specific data like azimuth , wind etc, earthquak, flood_type
#7 Source of data, can be used at later source to add weight to certain types of sources such as park rangers
#8 Damage Property and life
library(dplyr)
x <- select(x, EVENT_ID, everything())

#apply(x[(is.na(x$BEGIN_LAT)),],2,most_common)
#x[duplicated(x$EVENT_ID),]
substrRight <- function(x, n){
substr(x, nchar(x)-n+1, nchar(x))
}

#unique(substrRight(trimws(x$DAMAGE_PROPERTY), 1))

ind <- grepl(pattern = “K$”, x$DAMAGE_PROPERTY)
x[ind == TRUE ,]$DAMAGE_PROPERTY <- as.character((as.numeric(sub(‘K$’,”,x[ind == TRUE ,]$DAMAGE_PROPERTY)) * 1000))
ind <- grepl(pattern = “M$”, x$DAMAGE_PROPERTY)
x[ind == TRUE ,]$DAMAGE_PROPERTY <- as.character((as.numeric(sub(‘M$’,”,x[ind == TRUE ,]$DAMAGE_PROPERTY)) * 1000000))
ind <- grepl(pattern = “B$”, x$DAMAGE_PROPERTY)
x[ind == TRUE ,]$DAMAGE_PROPERTY <- as.character((as.numeric(sub(‘B$’,”,x[ind == TRUE ,]$DAMAGE_PROPERTY)) * 1000000000))
# Donot know if H is an error but for now I am replacing it with Hundreds
ind <- grepl(pattern = “h$”, x$DAMAGE_PROPERTY)
x[ind == TRUE ,]$DAMAGE_PROPERTY <- as.character((as.numeric(sub(‘h$’,”,x[ind == TRUE ,]$DAMAGE_PROPERTY)) * 100))
ind <- grepl(pattern = “H$”, x$DAMAGE_PROPERTY)
x[ind == TRUE ,]$DAMAGE_PROPERTY <- as.character((as.numeric(sub(‘H$’,”,x[ind == TRUE ,]$DAMAGE_PROPERTY)) * 100))
x$DAMAGE_PROPERTY <- as.numeric(x$DAMAGE_PROPERTY)

#unique(substrRight(trimws(x$DAMAGE_CROPS), 1))
ind <- grepl(pattern = “K$”, x$DAMAGE_CROPS)
x[ind == TRUE ,]$DAMAGE_CROPS <- as.character((as.numeric(sub(‘K$’,”,x[ind == TRUE ,]$DAMAGE_CROPS)) * 1000))
ind <- grepl(pattern = “k$”, x$DAMAGE_CROPS)
x[ind == TRUE ,]$DAMAGE_CROPS <- as.character((as.numeric(sub(‘k$’,”,x[ind == TRUE ,]$DAMAGE_CROPS)) * 1000))
ind <- grepl(pattern = “M$”, x$DAMAGE_CROPS)
x[ind == TRUE ,]$DAMAGE_CROPS <- as.character((as.numeric(sub(‘M$’,”,x[ind == TRUE ,]$DAMAGE_CROPS)) * 1000000))
ind <- grepl(pattern = “B$”, x$DAMAGE_CROPS)
x[ind == TRUE ,]$DAMAGE_CROPS <- as.character((as.numeric(sub(‘B$’,”,x[ind == TRUE ,]$DAMAGE_CROPS)) * 1000000000))
ind <- grepl(pattern = “\\?$”, x$DAMAGE_CROPS)
x[ind == TRUE ,]$DAMAGE_CROPS <- as.character((as.numeric(sub(‘\\?$’,”,x[ind == TRUE ,]$DAMAGE_CROPS)) * 0))
ind <- grepl(pattern = “T$”, x$DAMAGE_CROPS)
#Cannot discern what T stands for as Cant be TRILLIONS maybe (for one of them!- Looked it up, web said damage wasn’t as extensive)
#x$BEGIN_DATE_TIME<- df$BEGIN_DATE_TIME
#View(x[as.numeric(x$YEAR) < 2000,”BEGIN_DATE_TIME”])
#head(df$BEGIN_DATE_TIME)
#29-AUG-50 16:00:00
rm(ind)
x$BEGIN_DATE_TIME[as.numeric(x$YEAR) < 2000] <- gsub(‘^(.{7})(.*)$’, ‘\\119\\2’,x[as.numeric(x$YEAR) < 2000,]$BEGIN_DATE_TIME)
x$BEGIN_DATE_TIME[as.numeric(x$YEAR) >= 2000] <- gsub(‘^(.{7})(.*)$’, ‘\\120\\2’,x[as.numeric(x$YEAR) >= 2000,]$BEGIN_DATE_TIME)
x$END_DATE_TIME[as.numeric(x$YEAR) < 2000] <- gsub(‘^(.{7})(.*)$’, ‘\\119\\2’,x[as.numeric(x$YEAR) < 2000,]$END_DATE_TIME)
x$END_DATE_TIME[as.numeric(x$YEAR) >= 2000] <- gsub(‘^(.{7})(.*)$’, ‘\\120\\2′,x[as.numeric(x$YEAR) >= 2000,]$END_DATE_TIME)

x$BEGIN_DATE_TIME <- strptime(x$BEGIN_DATE_TIME, format=”%d-%B-%Y %H:%M:%S”)
x$END_DATE_TIME <- strptime(x$END_DATE_TIME, format=”%d-%B-%Y %H:%M:%S”)
x$DURATION <- as.numeric((x$END_DATE_TIME)-(x$BEGIN_DATE_TIME),units=”secs”)

#nrow(x[is.na(x$BEGIN_LAT),])

library(XML)
library(httr)
url<- “https://en.wikipedia.org/wiki/User:Michael_J/County_table&#8221;
r <- GET(url)

doc <- readHTMLTable(
doc=content(r, “text”))
mm <- as.data.frame(doc[[1]])
#class(mm$FIPS)
mm$`Sort [1]`<- NULL
mm$`County Seat(s) [3]`<- NULL
mm$`Population(2010)`<- NULL
mm <- mm[,c(1,2,3,10,11)]
mm$`County [2]`<- gsub(“[[:punct:]]”, “”, mm$`County [2]`)
mm$`County [2]`<- gsub(“[[:digit:]]”, “”, mm$`County [2]`)
mm$State<- toupper(trimws(mm$State))
mm$`County [2]` <- toupper(trimws(mm$`County [2]`))

rm(doc)
rm(r)
rm(url)

m<- sprintf(“%03s”, x$CZ_FIPS)
x$CZ_FIPS<- m
m <- sprintf(“%02s”, x$STATE_FIPS)
x$STATE_FIPS<- m
rm(m)
x$FIPS <- paste0(x$STATE_FIPS,x$CZ_FIPS)
x$CZ_NAME <- gsub(“[[:punct:]]”, “”, x$CZ_NAME)
x$CZ_NAME <- gsub(“[[:digit:]]”, “”, x$CZ_NAME)

#x$STATE_FIPS<- NULL
#x$CZ_FIPS<- NULL
xx <- x[is.na(x$BEGIN_LAT),]
x <- x[!(is.na(x$BEGIN_LAT)),]
mm <- mm[,c(2,4,5)]
xx<-merge(xx,mm,all.x = TRUE,all.y = FALSE)
xxx <- xx[is.na(xx$Latitude),]
xx <- xx[!(is.na(xx$Latitude)),]
state_csv<- read.csv(“/Users/jheeldoshi/Downloads/state.csv”, stringsAsFactors = FALSE, header = FALSE,colClasses=’character’)
state_csv$FIPS <- paste0(state_csv$V2,state_csv$V3)
state_csv <- state_csv[,c(1,4,5)]
colnames(state_csv) <- c(“STATE”,”CZ_NAME”,”FIPS”)
state <- read.csv(“/Users/jheeldoshi/Downloads/states.csv”, stringsAsFactors = FALSE, header = FALSE,colClasses=’character’)
state$V1<- toupper(trimws(state$V1))
state_csv<- merge(state_csv,state,all.x = TRUE,by.x = c(“STATE”),by.y = c(“V2”))
state_csv$STATE<- NULL
names(state_csv)[names(state_csv) == ‘V1’] <- ‘STATE’
state_csv$STATE<- toupper(trimws(state_csv$STATE))
state_csv$CZ_NAME<- toupper(trimws(state_csv$CZ_NAME))
state_csv$CZ_NAME <- gsub(“[[:punct:]]”, “”, state_csv$CZ_NAME)
state_csv$CZ_NAME <- gsub(“[[:digit:]]”, “”, state_csv$CZ_NAME)

xxx$Latitude<- NULL
xxx$Longitude <- NULL
names(xxx)[names(xxx) == ‘FIPS’] <- ‘WrongFips’
xxx$CZ_NAME <- toupper(trimws(xxx$CZ_NAME))
xxx$STATE <- toupper(trimws(xxx$STATE))

xxx <- merge(xxx,state_csv,all.x = TRUE,all.y = FALSE,by = c(“STATE”,”CZ_NAME”))

xxx$WrongFips <- NULL
xxx<- xxx[!(xxx$EVENT_ID==10314668),]
natural <- xxx[is.na(xxx$FIPS),]
xxx <- xxx[!is.na(xxx$FIPS),]
xxx<-merge(xxx,mm,all.x = TRUE,all.y = FALSE)
##query3 <- sqldf(“Select DISTINCT(FIPS) from xx where FIPS not in (SELECT DISTINCT(FIPS) from mm);”)
xx <- rbind(xx,xxx)

rm(xxx)

colnames(xx)
xx$BEGIN_LAT<- NULL
xx$BEGIN_LON<-NULL
names(xx)[names(xx) == ‘Latitude’] <- ‘BEGIN_LAT’
names(xx)[names(xx) == ‘Longitude’] <- ‘BEGIN_LON’
class(xx$BEGIN_LAT)
xx$BEGIN_LAT<- trimws(as.character(xx$BEGIN_LAT))
xx$BEGIN_LAT<- substring(xx$BEGIN_LAT, 1, 6)
xx$BEGIN_LON<- trimws(as.character(xx$BEGIN_LON))
xx$BEGIN_LON<- substring(xx$BEGIN_LON, 1, 6)
library(leaflet)

m <- leaflet() %>%
addTiles() %>% # Add default OpenStreetMap map tiles
addMarkers(lng=72.86061259999997, lat=19.1133586, popup=”Jheel’s Heart”)
m
x<- rbind(x,xx)
rm(xx)
x$CZ_NAME<- NULL

x$INJURIES_DIRECT <- as.numeric(x$INJURIES_DIRECT)
x$INJURIES_INDIRECT <- as.numeric(x$INJURIES_INDIRECT)
x$DEATHS_DIRECT <- as.numeric(x$DEATHS_DIRECT)
x$DEATHS_INDIRECT <- as.numeric(x$DEATHS_INDIRECT)
