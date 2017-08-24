library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
library(reshape2)


link <- "http://everestmarathon.com/"
home <- html_session(link) %>% read_html()
menu <- home %>% html_node(xpath = "//li[@id='mod_menu_no_5']") %>% html_nodes(xpath = ".//li")
leta <- sapply(menu, html_text) %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer()
strani <- menu %>% html_nodes(xpath = ".//a") %>% html_attr("href")

uvozi.rez <- function(leto, stran) {
  tabela <- paste0(link, stran) %>% html_session() %>% read_html() %>%
    html_nodes(xpath = "//table") %>% .[[1]] %>% html_table(fill = TRUE)
  if (leto != 2011 && leto != 2012) {
    while (! tabela[1,1] %in% c("1", "01")) {
      tabela <- tabela[-1, ]
    }
  } else {
    if (leto == 2011){
      tabela <- tabela[-c(1, 2), ]
    }
    else{
      tabela <- tabela[-c(1), ]
    }
  }
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  return(tabela)
}


vsi.rez <- lapply(1:length(leta), . %>% { uvozi.rez(leta[.], strani[.]) })

rez.2017 <- data.frame(vsi.rez[[1]])
rez.2016 <- data.frame(vsi.rez[[2]]) 
rez.2015 <- data.frame(vsi.rez[[3]]) 
rez.2014  <- data.frame(vsi.rez[[4]]) 
rez.2013 <- data.frame(vsi.rez[[5]]) 
rez.2012 <- data.frame(vsi.rez[[6]]) 
rez.2011  <- data.frame(vsi.rez[[7]]) 
rez.2010  <- data.frame(vsi.rez[[8]]) 
rez.2009 <- data.frame(vsi.rez[[9]]) 
rez.2008  <- data.frame(vsi.rez[[10]]) 
rez.2007  <- data.frame(vsi.rez[[11]]) 
rez.2006  <- data.frame(vsi.rez[[12]]) 
rez.2005  <- data.frame(vsi.rez[[13]]) 
rez.2004  <- data.frame(vsi.rez[[14]])
rez.2003  <- data.frame(vsi.rez[[15]]) 


rez.2003 <- data.frame(rez.2003$X3,rez.2003$X4)
rez.2004 <- data.frame(rez.2004$X3,rez.2004$X4, rez.2004$X5)
rez.2005 <- data.frame(rez.2005$X3,rez.2005$X4, rez.2005$X5, rez.2005$X6)
rez.2006 <- data.frame(rez.2006$X3,rez.2006$X4, rez.2006$X5, rez.2006$X6)
rez.2007 <- data.frame(rez.2007$X3,rez.2007$X4, rez.2007$X5, rez.2007$X6)
rez.2008 <- data.frame(rez.2008$X3,rez.2008$X4, rez.2008$X5, rez.2008$X6)
rez.2009 <- data.frame(rez.2009$X3,rez.2009$X4, rez.2009$X5, rez.2009$X6)
rez.2010 <- data.frame(rez.2010$X3,rez.2010$X4, rez.2010$X5, rez.2010$X6)
rez.2011 <- data.frame(rez.2011$X1,rez.2011$X2, rez.2011$X3, rez.2011$X4)
rez.2012 <- data.frame(rez.2012$X2,rez.2012$X3, rez.2012$X4, rez.2012$X5)
rez.2013 <- data.frame(rez.2013$X3,rez.2013$X4, rez.2013$X5)
rez.2014 <- data.frame(rez.2014$X3,rez.2014$X4, rez.2014$X5)
rez.2015 <- data.frame(rez.2015$X2,rez.2015$X3, rez.2015$X4, rez.2015$X6, rez.2015$X7, rez.2015$X8)
rez.2016 <- data.frame(rez.2016$X2,rez.2016$X3, rez.2016$X4, rez.2016$X6, rez.2016$X7, rez.2016$X8)
rez.2017 <- data.frame(rez.2017$X2,rez.2017$X3, rez.2017$X4, rez.2017$X6, rez.2017$X7, rez.2017$X8)

#Urejanje razpredelnice za leto 2003
m2003 <- strsplit(as.character(rez.2003$rez.2003.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df03 <- data.frame(rez.2003, m2003)

rezultat03 <- data.frame(df03$rez.2003.X3, df03$X1, df03$X2, df03$X3)
stevilo03 <- nrow(rezultat03)
m03 <- data.frame(matrix(data = NA, nrow = stevilo03, ncol = 1))
colnames(m03) <- c("")
rezultat03$spol <- m03
rezultat03$drzava <- m03
rez03 <- rezultat03[c(1, 5, 6, 2,3,4)]

names(rez03) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

leto03 <- data.frame(matrix(data = "2003", nrow = stevilo03, ncol=1))
colnames(leto03) <- c("")
rez03$Year <- leto03
  
  
#Urejanje razpredelnice za leto 2004
m2004 <- strsplit(as.character(rez.2004$rez.2004.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df04 <- data.frame(rez.2004, m2004)

rezultat04 <- data.frame(df04$rez.2004.X3, df04$X1, df04$X2, df04$X3)
stevilo04 <- nrow(rezultat04)
m04 <- data.frame(matrix(data = NA, nrow = stevilo04, ncol = 1))
colnames(m04) <- c("")
rezultat04$spol <- m04
rezultat04$drzava <- m04
rez04 <- rezultat04[c(1, 5, 6, 2,3,4)]

names(rez04) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

leto04 <- data.frame(matrix(data = "2004", nrow = stevilo04, ncol=1))
colnames(leto04) <- c("")
rez04$Year <- leto04

#Urejanje razpredelnice za leto 2005
m2005 <- strsplit(as.character(rez.2005$rez.2005.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
rez05 <- data.frame(rez.2005, m2005)

rez05[4] <- NULL

names(rez05) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

leto05 <- data.frame(matrix(data = "2005", nrow = nrow(rez05), ncol=1))
colnames(leto05) <- c("")
rez05$Year <- leto05

#Urejanje razpredelnice za leto 2006
m2006 <- strsplit(as.character(rez.2006$rez.2006.X6), ":") %>% 
  sapply(function(x) {
    if (length(x)  != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df06 <- data.frame(rez.2006, m2006)

df06[4] <- NULL

df06$rez.2006.X4 <- as.character(df06$rez.2006.X4)
df06$rez.2006.X4[df06$rez.2006.X4 != c("M","F")] <- NA
df06$rez.2006.X4 <- as.factor(df06$rez.2006.X4)

df06$rez.2006.X5 <- as.character(df06$rez.2006.X5)
df06$rez.2006.X5[df06$rez.2006.X5 == "\""] <- NA
df06$rez.2006.X5 <- as.factor(df06$rez.2006.X5)

u <- df06$rez.2006.X5
u <- as.character(u)
u[u == "-"] <- NA
u <- as.factor(u)

df06[3] <- u

rez06 <- df06
names(rez06) <- c("Name","Gender","Country","Hours","Minutes","Seconds")


leto06 <- data.frame(matrix(data = "2006", nrow = nrow(rez06), ncol=1))
colnames(leto06) <- c("")
rez06$Year <- leto06

#Urejanje razpredelnice za leto 2007
m2007 <- strsplit(as.character(rez.2007$rez.2007.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df07 <- data.frame(rez.2007, m2007)

df07[4] <- NULL

df07[df07==""] <- NA
rez07 <- df07
names(rez07) <- c("Name","Gender","Country","Hours","Minutes","Seconds")


leto07 <- data.frame(matrix(data = "2007", nrow = nrow(rez07), ncol=1))
colnames(leto07) <- c("")
rez07$Year <- leto07

#Urejanje razpredelnice za leto 2008
m2008 <- strsplit(as.character(rez.2008$rez.2008.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df08 <- data.frame(rez.2008, m2008)

df08[4] <- NULL

rez08 <- df08
names(rez08) <- c("Name","Gender","Country","Hours","Minutes","Seconds")


leto08 <- data.frame(matrix(data = "2008", nrow = nrow(rez08), ncol=1))
colnames(leto08) <- c("")
rez08$Year <- leto08

#Urejanje razpredelnice za leto 2009
m2009 <- strsplit(as.character(rez.2009$rez.2009.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df09 <- data.frame(rez.2009, m2009)


df09[4] <- NULL
rez09 <- df09
names(rez09) <- c("Name","Gender","Country","Hours","Minutes","Seconds")


leto09 <- data.frame(matrix(data = "2009", nrow = nrow(rez09), ncol=1))
colnames(leto09) <- c("")
rez09$Year <- leto09

#Urejanje razpredelnice za leto 2010
m2010 <- strsplit(as.character(rez.2010$rez.2010.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df10 <- data.frame(rez.2010, m2010)


df10[4] <- NULL
rez10 <- df10
rez10[rez10==""] <- NA

names(rez10) <- c("Name","Gender","Country","Hours","Minutes","Seconds")


leto10 <- data.frame(matrix(data = "2010", nrow = nrow(rez10), ncol=1))
colnames(leto10) <- c("")
rez10$Year <- leto10



#Urejanje razpredelnice za leto 2011

m2011 <- strsplit(as.character(rez.2011$rez.2011.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df11 <- data.frame(rez.2011, m2011)


df11[4] <- NULL
rez11 <- df11
rez11[rez11==""] <- NA

names(rez11) <- c("Name","Gender","Country","Hours","Minutes","Seconds")
rez11 <- rez11[-c(93,107,108,109,110,111,112,113,114), ]


leto11 <- data.frame(matrix(data = "2011", nrow = nrow(rez11), ncol=1))
colnames(leto11) <- c("")
rez11$Year <- leto11


#Urejanje razpredelnice za leto 2012

m2012 <- strsplit(as.character(rez.2012$rez.2012.X5), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df12 <- data.frame(rez.2012, m2012)


df12[4] <- NULL
rez12 <- df12
rez12[rez12==""] <- NA

names(rez12) <- c("Name","Gender","Country","Hours","Minutes","Seconds")
rez12 <- rez12[-c(93), ]


leto12 <- data.frame(matrix(data = "2012", nrow = nrow(rez12), ncol=1))
colnames(leto12) <- c("")
rez12$Year <- leto12

#Urejanje razpredelnice za leto 2013

m2013 <- strsplit(as.character(rez.2013$rez.2013.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df13 <- data.frame(rez.2013, m2013)

stevilo13 <- nrow(df13)
m13 <- data.frame(matrix(data = NA, nrow = stevilo13, ncol = 1))
colnames(m13) <- c("")
df13$spol <- m13

df13[2] <- NULL
df13[df13==""] <- NA
rez13 <- df13[c(1,6,2,3,4,5)]

names(rez13) <- c("Name","Gender","Country","Hours","Minutes","Seconds")
rez13 <- rez13[-c(46),]


rez13$Name <- gsub("Mr. ","",rez13$Name)
rez13$Name <- gsub("Miss ", "", rez13$Name)
rez13$Name <- gsub("Ms.", "", rez13$Name)
rez13$Name <- gsub("Mrs.", "", rez13$Name)
rez13$Name <- gsub("Mr.","",rez13$Name)


leto13 <- data.frame(matrix(data = "2013", nrow = nrow(rez13), ncol=1))
colnames(leto13) <- c("")
rez13$Year <- leto13

#Urejanje razpredelnice za leto 2014
rez.2014 <- rez.2014[-c(65,66), ]


spol <- c("r" = "M", "s" = "F")
a <- rez.2014$rez.2014.X3 %>% as.character() %>% strapplyc("^.(.)") %>% unlist() %>% spol[.] 
aa <- matrix(a, nrow = length(a), ncol = 1)
rez.2014$spol <- aa

m2014 <- strsplit(as.character(rez.2014$rez.2014.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df14 <- data.frame(rez.2014, m2014)

df14[2] <- NULL

rez14 <- df14[c(1, 3,2, 4,5,6)]

names(rez14) <- c("Name","Gender","Country","Hours","Minutes","Seconds")


rez14$Name <- gsub("Mr. ","",rez14$Name)
rez14$Name <- gsub("Miss ", "", rez14$Name)
rez14$Name <- gsub("Ms.", "", rez14$Name)
rez14$Name <- gsub("Mrs.", "", rez14$Name)
rez14$Name <- gsub("Mr.","",rez14$Name)

leto14 <- data.frame(matrix(data = "2014", nrow = nrow(rez14), ncol=1))
colnames(leto14) <- c("")
rez14$Year <- leto14


#Urejanje razpredelnice za leto 2015

rez.2015[rez.2015==""] <- NA
rez15 <- rez.2015
names(rez15) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

rez15$Name <- gsub("Mr. ","",rez15$Name)
rez15$Name <- gsub("Miss ", "", rez15$Name)
rez15$Name <- gsub("Ms.", "", rez15$Name)
rez15$Name <- gsub("Mrs.", "", rez15$Name)
rez15$Name <- gsub("Mr.","",rez15$Name)

leto15 <- data.frame(matrix(data = "2015", nrow = nrow(rez15), ncol=1))
colnames(leto15) <- c("")
rez15$Year <- leto15



#Urejanje razpredelnice za leto 2016
rez.2016[rez.2016==""] <- NA
rez16 <- rez.2016
names(rez16) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

rez16$Name <- gsub("Mr. ","",rez16$Name)
rez16$Name <- gsub("Miss.", "", rez16$Name)
rez16$Name <- gsub("Ms.", "", rez16$Name)
rez16$Name <- gsub("Mrs.", "", rez16$Name)
rez16$Name <- gsub("Mr.","",rez16$Name)


rez16 <- rez16[-c(29),]

leto16 <- data.frame(matrix(data = "2016", nrow = nrow(rez16), ncol=1))
colnames(leto16) <- c("")
rez16$Year <- leto16

#Urejanje razpredelnice za leto 2017
rez.2017[rez.2017==""] <- NA
rez17 <- rez.2017
names(rez17) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

rez17 <- rez17[-c(108,109,110,111,112,113,114), ]

rez17$Name <- gsub("Mr. ","",rez17$Name)
rez17$Name <- gsub("Miss.", "", rez17$Name)
rez17$Name <- gsub("Ms.", "", rez17$Name)
rez17$Name <- gsub("Mrs.", "", rez17$Name)

rez17 <- rez17[-c(47),]
leto17 <- data.frame(matrix(data = "2017", nrow = nrow(rez17), ncol=1))
colnames(leto17) <- c("")
rez17$Year <- leto17








