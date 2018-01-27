#------------------------------------------------------------------------------------------------------------------
# Created On :5Aug2016
# Created For: Script for Preprocessing the Wikipedia XML DUMP
#-------------------------------------------------------------------------------------------------------------------


#install.packages("XML")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("gridExtra")

require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")
# Load the package required to read XML files.
library("XML")

# Also load the other required package.
library("methods")

# Give the folder path here.
#setwd("~/Desktop/Shilpita/COEN242/Project/DataCleaning/Xml_to_CSV") #you will need to change the filepath on your machine

#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history5.xml-p000564697p000565313.xml",useInternalNodes = TRUE)
#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history9.xml-p002330208p002336422.xml",useInternalNodes = TRUE)
#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history12.xml-p005036053p005040436.xml",useInternalNodes = TRUE) #Give the input file name here
#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history23.xml-p030490097p030503449.xml",useInternalNodes = TRUE) #Give the input file name here
xmlfile <- xmlParse(file="dataFile.xml-p030490097p030503449",useInternalNodes = TRUE) #Give the input file name here


class(xmlfile) #"XMLInternalDocument" "XMLAbstractDocument"

xmltop = xmlRoot(xmlfile) #gives content of root

ns <- c(ns="http://www.mediawiki.org/xml/export-0.10/")

xpathSApply(xmltop,"/ns:mediawiki//ns:page",function(node){
  
  page.title <- xmlValue(node[["title"]])
  # page.ns1 <- xmlValue(node[["ns"]])
  page.id <- xmlValue(node[["id"]])
  dd <- data.frame(page.title,page.id)
  
  # print(dd)
  xpathSApply(node,"./ns:revision",function(innernode){
    
    revision.id <- xmlValue(innernode[["id"]])
    # revision.parentid <- xmlValue(innernode[["parentid"]])
    if("parentid" %in% names(innernode))
      revision.parentid <- xmlValue(innernode[["parentid"]])
    else 
      revision.parentid <- 0
    
    revision.timestamp <- xmlValue(innernode[["timestamp"]])
    
    if("username" %in% names(innernode[["contributor"]]))
      revision.contributor <- xmlValue(innernode[["contributor"]][["username"]])
    else 
      revision.contributor <- "DefaultContributor"
    
  #  revision.comment <- gsub(",", "  ", xmlValue(innernode[["comment"]]))
 comment <- gsub(",", "  ", xmlValue(innernode[["comment"]]))
	revision.comment <- gsub("[^[:alpha:] ]", "",comment)


    revision.sha1 <- xmlValue(innernode[["sha1"]])
    # revision.text1 <- gsub(",", "  ", gsub("\r?\n|\r", " ",xmlValue(innernode[["text"]])))
    # revision.text <- substr(revision.text1,1,22000)
    
    revdd <- data.frame(revision.id,revision.parentid,revision.timestamp,revision.contributor,revision.sha1,revision.comment)
    
   # revdd <- data.frame(revision.id,revision.parentid,revision.timestamp,revision.contributor,revision.comment,revision.sha1,revision.text)
  
   #   revdd$revision.text <- sub("^$", "No text", revdd$revision.text)
    
    new <- cbind(dd,revdd)
    
    new <- na.omit(new)
    
    
    # print(new)
    
    write.table(new, "fileNoText5.csv", append=TRUE,row.names=FALSE, col.names=!file.exists("fileNoText5.csv"),sep=",")
    write.table(new, "fileNoText5.txt", append=TRUE,row.names=FALSE, col.names=!file.exists("fileNoText5.txt"),sep="\t")
  },namespaces=ns)
},namespaces=ns)

