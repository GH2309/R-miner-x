
# read in the data

# I only want to keep certain columns so I ran this code on a previous load of the data frame to isolate which columns I needed to keep in subsequent iterations. It doesn't need to be run again.

#         Class <- t(as.data.frame(lapply(Data,class)))
#         Class <- as.data.frame(Class)
#         Class$V1 <- as.character(Class$V1)
#         Class[c(1:3,9:22,29,30,34:35,37),1] <- "NULL"
#          write.csv(Class,"Classes.csv")

Class <- as.data.frame(t(read.csv("Classes.csv")))
Class<- as.data.frame(Class[2:38,1])
Class[,1] <- as.character(Class[,1])
Class[36,1] <- "character"

Data <- read.csv("repdata_data_StormData.csv.bz2",stringsAsFactors=F,colClasses=Class[,1])
#Get rid of data without injuries, fatalities, or costs
DataInj <- Data[which(Data$FATALITIES!=0|Data$INJURIES!=0),]
DataCost <- Data[which(Data$PROPDMG!=0|Data$CROPDMG!=0),]
# recombine
Data <- as.data.frame(rbind(DataCost,DataInj))
# cleanup
rm(DataInj,DataCost,Class)
# Convert characters "k","m", and "b" to real numbers
Data[which(Data$PROPDMGEXP=="K"|Data$PROPDMGEXP=="k"),9] <- as.numeric(10^3)
Data[which(Data$PROPDMGEXP=="M"|Data$PROPDMGEXP=="m"),9] <- as.numeric(10^6)
Data[which(Data$PROPDMGEXP=="B"),9] <- as.numeric(10^9)
Data[which(Data$CROPDMGEXP=="K"|Data$CROPDMGEXP=="k"),11] <- as.numeric(10^3)
Data[which(Data$CROPDMGEXP=="M"|Data$CROPDMGEXP=="m"),11] <- as.numeric(10^6)
Data[which(Data$CROPDMGEXP=="B"),11] <- as.numeric(10^9)

Data <- as.data.frame(lapply(Data,tolower))
TypeTerms <- unique(Data$EVTYPE)
library(tm)
library(wordcloud)
t <- Corpus(VectorSource(TypeTerms))
tdm <- TermDocumentMatrix(t)
freq <- findFreqTerms(tdm,5)
findAssocs(tdm,freq,corlimit = 0.3)

wordcloud(t)

# td <- dist(tdm)
# h <- hclust(td)
# hd <- as.dendrogram(h)
# plot(hd,)
# This is to see how many of each group. There are still a lot of unknown factors.
table(Data$FATALITIES,Data$EVTYPE)
table(Data$CROPDMGEXP)
table(Data$PROPDMGEXP)
Data$PROPDMGEXP <- as.numeric(Data$PROPDMGEXP)
Data$CROPDMGEXP <- as.numeric(Data$CROPDMGEXP)
Data$TotProp <- Data$PROPDMG*Data$PROPDMGEXP
Data$TotCrop <- Data$CROPDMG*Data$CROPDMGEXP
Data$AllCost <- Data$TotProp+Data$TotCrop

# Initial analysis
summary(Data[,16:18]) # There are a lot of zeroes, maybe we should get rid of them...
Data2 <- Data[which(Data$AllCost!=0),]
hist(Data2$AllCost,breaks=5,plot=F)
which(Data2$AllCost==max(Data2$AllCost,na.rm=T)) # This appears to be an outlier...
# Let's remove it?
Data2 <- Data2[-14841,]
hist(Data2$AllCost,breaks=5,plot = F)
plot(Data2$AllCost)
Data$TIME_ZONE <- toupper(Data$TIME_ZONE)
unique(Data$TIME_ZONE)
plot(Data2$AllCost~as.factor(Data2$TIME_ZONE),main="Total Storm Damage Cost by Time Zone",xlab="Time Zone",ylab="Total Damage Cost, $")

table(Data$TIME_ZONE)
# class(Data$PROPDMGEXP)
# names(Data)

