pubmed<- function(search.term){ ## add year
##search.term <- "science[journal]+AND+breast+cancer+AND+2008[pdat]"
db <- "pubmed"
  library(XML)
url <- paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=",db,"&term=",search.term,"&retmax=1000")

##Download unique identifiers
suppressWarnings(download.file(url=url,destfile="./uid.XML"))
##grab each UID and put it into a dataframe  
doc = xmlTreeParse("uid.xml", useInternal = TRUE)
top=xmlRoot(doc)
uid <-   xmlSApply(top[[4]],xmlValue)
uidlist <- list(as.numeric(uid))
uidframe <- data.frame(as.numeric(uid))

##Ensure that the number of UIDs is small enough. If it's not, stop and prompt user to refine search terms
count <- nrow(uidframe)
if (count>100)stop("Too many cases. Try to refine search terms")else
 ## Download all abstracts
look <- paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=",db,"&id=",uidlist,"&rettype=abstract&retmode=text")
suppressWarnings(download.file(url=look,destfile=paste0(search.term,".txt")))

