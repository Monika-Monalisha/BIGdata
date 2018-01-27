#------------------------------------------------------------------------------------------------------------------
# Created On :5Aug2016
# Created For: Script for Preprocessing the Wikipedia XML DUMP
#-------------------------------------------------------------------------------------------------------------------

# 
# install.packages("XML")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("gridExtra")

require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")
# Load the package required to read XML files.
library("XML")

# Also load the other required package.
library("methods")

# Give the folder path here.
 # setwd("give your folder path") 

#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history10.xml-p002530814p002535910",useInternalNodes = TRUE)
xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history1.xml-p000028382p000030303",useInternalNodes = TRUE)
#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history12.xml-p005036053p005040436.xml",useInternalNodes = TRUE) #Give the input file name here
#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history23.xml-p030490097p030503449.xml",useInternalNodes = TRUE) #Give the input file name here
#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history14.xml-p007727623p007744799.xml",useInternalNodes = TRUE) #Give the input file name here
#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history16.xml-p011519896p011539266.xml",useInternalNodes = TRUE) #Give the input file name here


#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history25.xml-p037971877p038067202.xml",useInternalNodes = TRUE) #Give the input file name here

#xmlfile <- xmlParse(file="enwiki-20160601-pages-meta-history8.xml-p001728079p001791079.xml",useInternalNodes = TRUE) #Give the input file name here



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
      revision.contributor <- paste("ip", xmlValue(innernode[["contributor"]][["ip"]]), sep = "")      #   #"DefaultContributor"
    
   
    if("id" %in% names(innernode[["contributor"]]))
      revision.contributor.id <- xmlValue(innernode[["contributor"]][["id"]])
    else
     # revision.contributor.id <- xmlValue(innernode[["contributor"]][["ip"]])
       revision.contributor.id <- gsub("[^[:alnum:] ]", "",xmlValue(innernode[["contributor"]][["ip"]]))
    
    tempcomment <- gsub(",", "  ", xmlValue(innernode[["comment"]]))
    revision.comment <- gsub("[^[:alpha:] ]", "",tempcomment)
    
    revision.sha1 <- xmlValue(innernode[["sha1"]])

	revision.text1 <- substr(xmlValue(innernode[["text"]]),1,15000)
    temptext <- gsub(",", "  ", gsub("\r?\n|\r", " ",revision.text1))
	 revision.text <- gsub("[^[:alpha:] ]", "",temptext )
    
   
      
      revdd <- data.frame(revision.id,revision.parentid,revision.timestamp,revision.contributor,revision.contributor.id,revision.comment,revision.sha1,revision.text)
   # revdd <- data.frame(revision.id,revision.parentid,revision.timestamp,revision.contributor,revision.comment,revision.sha1,revision.text)  
      revdd$revision.text <- sub("^$", "No text", revdd$revision.text)
      revdd$revision.comment <- sub("^$", "No Comment", revdd$revision.comment)
  # revdd$revision.contributor.id <- sub(".", "", revdd$revision.contributor.id)
      
      new <- cbind(dd,revdd)
      
      new <- na.omit(new)
        
      
     # print(new)
      
      write.table(new, "E://cleanedFiles//csv108.csv", append=TRUE,row.names=FALSE, col.names=!file.exists("E://cleanedFiles//csv108.csv"),sep=",")
   #   write.table(new, "txt1.txt", append=TRUE,row.names=FALSE, col.names=!file.exists("txt1.txt"),sep="\t")
    },namespaces=ns)
  },namespaces=ns)

