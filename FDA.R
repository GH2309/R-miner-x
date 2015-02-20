


##require(plyr)

#dataframe <- function(name){
#file <- paste(name,".json",sep="")
#https://api.fda.gov/device/event.json?search=date_received:[20120101+TO+20150101]+AND+device.device_report_product_code:bsk+AND+event_type:injury&limit=100
FDAurl <- function(prodcode,from,to,type=NULL){
  ##prodcode=FDA Product Code
  ##from=start of the date range of interest. Input is numeric yyyymmdd
  ##to=End of the date range of interest. Input is numeric yyyymmdd
  ##type= If specified, the type of injury. Valid inputs include "injury", "death", or "malfunction"
  paste("https://api.fda.gov/device/event.json?search=date_received:[",from,"+TO+",to,"]+AND+device.device_report_product_code:",prodcode,"+AND+event_type:",type,"&limit=100",sep="")
}
FDAdata <- function(url){
require(plyr)
require(RJSONIO)
event <- readLines(url,warn="F")##Untested
close(url)
json2 <- fromJSON(event)##Untested, but works for a "file.json" that is in the working directory.
list <- (lapply(json2$results,unlist))
mylist2 <- lapply(lapply(list, unlist), function(x) {
  names(x)[names(x) == "results"] <- "results1"
  data.frame(t(x))
data <- rbind.fill(mylist2)
data$date_received <- as.Date(data$date_received,"%Y%m%d")
})
}
# t <- Corpus(VectorSource(data$mdr_text.text))
# t <- tm_map(t,removeWords,stopwords("SMART"))
# t <- tm_map(t,removeWords,c("THE","AND"))
# t <- tm_map(t,removePunctuation)
# t <- tm_map(t,removeNumbers)
# t <- tm_map(t,tolower)
# t <- tm_map(t,stemDocument)
# dtm <- DocumentTermMatrix(t)
# freq <- findFreqTerms(dtm,lowfreq=15)
# ass <- (findAssocs(dtm,terms=freq,corlimit=0.7))
# actual <- findAssocs(dtm,"actual",corlimit=0.9)
