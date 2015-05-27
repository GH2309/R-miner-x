require(tm)
require(RJSONIO)
require(plyr)
require(dplyr)

##FDAurltype generates a url that can be used to download a JSON file from the FDA Adverse Events database. It takes FDA Product Code, date range, and event type
##The number of files returned is limited by the FDA to 100 by default (at least for now), so specifying the event type and using a short date range is advised.
##The event type is defaulted to "death"

FDAurltype <- function(prodcode,from,to,type="death"){
  ##prodcode=FDA Product Code
  ##from=start of the date range of interest. Input is numeric yyyymmdd
  ##to=End of the date range of interest. Input is numeric yyyymmdd
  ##type= If specified, the type of injury. Valid inputs include "injury", "death", or "malfunction"
  paste("https://api.fda.gov/device/event.json?search=date_received:[",from,"+TO+",to,"]+AND+device.device_report_product_code:",prodcode,"+AND+event_type:",type,"&limit=100",sep="")
}

##FDAurl generates a url that can be used to gather FDA data from the web. As inputs, it takes the FDA product code
##and the date range (from and to). The FDA automatically limits the returned file to 100 cases, so using "FDAurltype"
##is the preferred method. Not specifying your event type will truncate the results to the most recent 100 events.
FDAurl <- function(prodcode,from,to){
  ##prodcode=FDA Product Code
  ##from=start of the date range of interest. Input is numeric yyyymmdd
  ##to=End of the date range of interest. Input is numeric yyyymmdd
  ##type= If specified, the type of injury. Valid inputs include "injury", "death", or "malfunction"
  paste("https://api.fda.gov/device/event.json?search=date_received:[",from,"+TO+",to,"]+AND+device.device_report_product_code:",prodcode,"&limit=100",sep="")
}

# FDAdata <- function(url){
# event <- readLines(url,warn="F")##Untested
# close(url)

##This takes an existing FDA JSON file and converts it to a data file that can be more easily manipulated in R.
##Most fields will be enetered as "Factors", and in fact, this is actually desirable for most analysis. It may be prudent
##change the date fields using "as.Date"
FDAJSON <- function(file){
json2 <- fromJSON(file)##Untested, but works for a "file.json" that is in the working directory.
list <- (lapply(json2$results,unlist))
mylist2 <- lapply(lapply(list, unlist), function(x) {
  names(x)[names(x) == "results"] <- "results1"
  data.frame(t(x))
})
rbind.fill(mylist2)
}

##This will change the date fields from "Factor" to "Date"
data$date_received <- as.Date(data$date_received,"%Y%m%d")
data$date_of_event <- as.Date(data$date_of_event,"%Y%m%d")
data$date_manufacturer_received <- as.Date(data$date_manufacturer_received,"%Y%m%d")
data$date_report <- as.Date(data$date_report,"%Y%m%d")
##This gives the average time between when the event was received and when it was reported, by event type.
mavg <- data%>%group_by(event_type)%>%summarise(avg=mean(date_received-date_report))

##A lot of the data fields are empty or "NA", so this removes columns that are more empty than not. The threshold needs
##to be entered as a percent complete. 
naRemove <- function(df,pct){
  d <- apply(df,2,FUN = function(x) NROW(df[!(x == "" | (is.na(x))),])/NROW(x))
  nap <- which (d>pct)
  df[,nap]
}



## This will take the adverse effects text field and make a Corpus for further analysis
FDACorpus <- function(data){
t <- Corpus(VectorSource(data[,19]))
t <- tm_map(t,removeWords,stopwords("SMART"))
t <- tm_map(t,removeWords,c("THE","AND","WAS","THAT","THIS","HAVE", "DID", "NOT"))
t <- tm_map(t,removePunctuation)
t <- tm_map(t,removeNumbers)
t <- tm_map(t,tolower)
}
# # t <- tm_map(t,stemDocument) "Requires SnowballC
# }
# dtm <- DocumentTermMatrix(t)
# freq <- 
#   findFreqTerms(dtm,lowfreq=10)
# (findAssocs(dtm,terms=freq,corlimit=0.7))
# actual <- findAssocs(dtm,"actual",corlimit=0.9)
