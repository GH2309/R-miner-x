# requires a PDF to text rendering engine such as poppler
# which is available for GNU/Linux and Microsoft Windows
# and possibly Apple Mac OS X.

library(tm)
library(stringr)
# pdf <- readPDF(control = list(c(text = "-layout")))
# pdf <- pdf(elem=list(uri="repdata_peer2_doc_pd01016005curr.pdf"),language="en")
# c(pdf$content[seq(397, 420)], pdf$content[seq(425, 448)])
Text <- readLines("repdata_peer2_doc_pd01016005curr.txt")
Table <- Text[492:515]
Table <- as.list(str_split_fixed(Table," [A-Z] ",n = 4))
Table <- Table[which(Table[]!="")]
