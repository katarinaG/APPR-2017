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
rezultat03$spol <- NA
rezultat03$drzava <- NA
rezultat03$leto <- 2003
rez03 <- rezultat03[c(1, 5, 6, 2,3,4,7)]

names(rez03) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds", "Year")


  
#Urejanje razpredelnice za leto 2004
m2004 <- strsplit(as.character(rez.2004$rez.2004.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df04 <- data.frame(rez.2004, m2004)

rezultat04 <- data.frame(df04$rez.2004.X3, df04$X1, df04$X2, df04$X3)

rezultat04$spol <- NA
rezultat04$drzava <- NA
rezultat04$Year <- 2004


rez04 <- rezultat04[c(1, 5, 6, 2,3,4,7)]

names(rez04) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds", "Year")


#Urejanje razpredelnice za leto 2005
m2005 <- strsplit(as.character(rez.2005$rez.2005.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
rez05 <- data.frame(rez.2005, m2005)

rez05[4] <- NULL

names(rez05) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez05$Year <- 2005


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
names(rez06) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez06$Year <- 2006


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
names(rez07) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez07$Year <- 2007

#Urejanje razpredelnice za leto 2008
m2008 <- strsplit(as.character(rez.2008$rez.2008.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df08 <- data.frame(rez.2008, m2008)

df08[4] <- NULL

rez08 <- df08
names(rez08) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")


rez08$Year <- 2008

#Urejanje razpredelnice za leto 2009
m2009 <- strsplit(as.character(rez.2009$rez.2009.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df09 <- data.frame(rez.2009, m2009)


df09[4] <- NULL
rez09 <- df09
names(rez09) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez09$Year <- 2009

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

names(rez10) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez10$Year <- 2010



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

names(rez11) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")
rez11 <- rez11[-c(93,107,108,109,110,111,112,113,114), ]

rez11$Year <- 2011


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

names(rez12) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")
rez12 <- rez12[-c(93), ]

rez12$Year <- 2012


#Urejanje razpredelnice za leto 2013

m2013 <- strsplit(as.character(rez.2013$rez.2013.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df13 <- data.frame(rez.2013, m2013)

df13$spol <- NA


df13[2] <- NULL
df13[df13==""] <- NA
rez13 <- df13[c(1,6,2,3,4,5)]

names(rez13) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")
rez13 <- rez13[-c(46),]

rez13$Year <- 2013



#Urejanje razpredelnice za leto 2014
rez.2014 <- rez.2014[-c(65,66), ]


spol <- c("r" = "M", "s" = "F")
a <- rez.2014$rez.2014.X3 %>% as.character() %>% strapplyc("^.(.)") %>% unlist() %>% spol[.] 
rez.2014$spol <- a


m2014 <- strsplit(as.character(rez.2014$rez.2014.X4), ":") %>% 
  sapply(function(x) {
    if (length(x) != 3) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
df14 <- data.frame(rez.2014, m2014)

df14[2] <- NULL

rez14 <- df14[c(1, 3,2, 4,5,6)]

names(rez14) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")


rez14$Name <- gsub("Mr. ","",rez14$Name)
rez14$Name <- gsub("Miss ", "", rez14$Name)
rez14$Name <- gsub("Ms.", "", rez14$Name)
rez14$Name <- gsub("Mrs.", "", rez14$Name)
rez14$Name <- gsub("Mr.","",rez14$Name)


rez14$Year <- 2014


#Urejanje razpredelnice za leto 2015

rez.2015[rez.2015==""] <- NA
rez15 <- rez.2015
names(rez15) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez15$Name <- gsub("Mr. ","",rez15$Name)
rez15$Name <- gsub("Miss ", "", rez15$Name)
rez15$Name <- gsub("Ms.", "", rez15$Name)
rez15$Name <- gsub("Mrs.", "", rez15$Name)
rez15$Name <- gsub("Mr.","",rez15$Name)

rez15$Year <- 2015



#Urejanje razpredelnice za leto 2016
rez.2016[rez.2016==""] <- NA
rez16 <- rez.2016
names(rez16) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez16$Name <- gsub("Mr. ","",rez16$Name)
rez16$Name <- gsub("Miss.", "", rez16$Name)
rez16$Name <- gsub("Ms.", "", rez16$Name)
rez16$Name <- gsub("Mrs.", "", rez16$Name)
rez16$Name <- gsub("Mr.","",rez16$Name)


rez16 <- rez16[-c(29),]

rez16$Year <- 2016

#Urejanje razpredelnice za leto 2017
rez.2017[rez.2017==""] <- NA
rez17 <- rez.2017
names(rez17) <- c("Name","Gender","Nationality","Hours","Minutes","Seconds")

rez17 <- rez17[-c(108,109,110,111,112,113,114), ]

rez17$Name <- gsub("Mr. ","",rez17$Name)
rez17$Name <- gsub("Miss.", "", rez17$Name)
rez17$Name <- gsub("Ms.", "", rez17$Name)
rez17$Name <- gsub("Mrs.", "", rez17$Name)

rez17 <- rez17[-c(47),]

rez17$Year <- 2017

#Zdruzitev vseh rezultatov

rez <- rbind(rez03, rez04, rez05, rez06, rez07, rez08, rez09,
             rez10, rez11, rez12, rez13, rez14, rez15, rez16, rez17) %>%
  mutate(Time = as.difftime(paste(Hours, Minutes, Seconds, sep = ":"))) %>%
  select(-Hours, -Minutes, -Seconds)

#Drzavljanstvo zelimo spremeniti v drzave 

rez$Nationality[rez$Nationality == "Nepali"] <- "Nepal"
rez$Nationality[rez$Nationality == "Polish"] <- "Poland"
rez$Nationality[rez$Nationality == "Spanish"] <- "Spain"
rez$Nationality[rez$Nationality == "American"] <- "United States"
rez$Nationality[rez$Nationality == "German"] <- "Germany"
rez$Nationality[rez$Nationality == "New Zealander"] <- "New Zealand"
rez$Nationality[rez$Nationality == "U.K."] <- "United Kingdom"
rez$Nationality[rez$Nationality == "U.S.A."] <- "United States"
rez$Nationality[rez$Nationality == "Scottish"] <- "United Kingdom"
rez$Nationality[rez$Nationality == "French"] <- "France"
rez$Nationality[rez$Nationality == "UK"] <- "United Kingdom"
rez$Nationality[rez$Nationality == "USA"] <- "United States"
rez$Nationality[rez$Nationality == "Swiss"] <- "Switzerland"
rez$Nationality[rez$Nationality == "British"] <- "United Kingdom"
rez$Nationality[rez$Nationality == "Newzealand"] <- "New Zealand"
rez$Nationality[rez$Nationality == "Swiss"] <- "Switzerland"
rez$Nationality[rez$Nationality == "New    Zealand"] <- "New Zealand"
rez$Nationality[rez$Nationality == "South    Africa"] <- "South Africa"
rez$Nationality[rez$Nationality == "Hong    Kong"] <- "China"
rez$Nationality[rez$Nationality == "Swiss"] <- "Switzerland"
rez$Nationality[rez$Nationality == "Dutch"] <- "Netherlands"
rez$Nationality[rez$Nationality == "Swedish"] <- "Sweden"
rez$Nationality[rez$Nationality == "Netherlandase"] <- "Netherlands"
rez$Nationality[rez$Nationality == "Hungarian"] <- "Hungary"
rez$Nationality[rez$Nationality == "Catalonia"] <- "Spain"
rez$Nationality[rez$Nationality == "Uk"] <- "United Kingdom"
rez$Nationality[rez$Nationality == "Canadan"] <- "Canada"
rez$Nationality[rez$Nationality == "Brazilian"] <- "Brazil"
rez$Nationality[rez$Nationality == "Singaporean"] <- "Singapore"
rez$Nationality[rez$Nationality == "Indian"] <- "India"
rez$Nationality[rez$Nationality == "South Afirca"] <- "South Africa"
rez$Nationality[rez$Nationality == "South Korean"] <- "South Korea"
rez$Nationality[rez$Nationality == "Israeli"] <- "Israel"
rez$Nationality[rez$Nationality == "Russian"] <- "Russia"
rez$Nationality[rez$Nationality == "Hong Kong"] <- "China"
rez$Nationality[rez$Nationality == "Irish"] <- "Ireland"
rez$Nationality[rez$Nationality == "South African"] <- "South Africa"
rez$Nationality[rez$Nationality == "Chinese"] <- "China"
rez$Nationality[rez$Nationality == "Japanese"] <- "Japan"
rez$Nationality[rez$Nationality == "Chileans"] <- "Chile"
rez$Nationality[rez$Nationality == "Netherland"] <- "Netherlands"
rez$Nationality[rez$Nationality == "Nepalese"] <- "Nepal"
rez$Nationality[rez$Nationality == "Czech"] <- "Czech Republic"
rez$Nationality[rez$Nationality == "Taiwanese"] <- "Taiwan"
rez$Nationality[rez$Nationality == "Lebanese"] <- "Lebanon"
rez$Nationality[rez$Nationality == "Australian"] <- "Australia"
rez$Nationality[rez$Nationality == "Italian"] <- "Italy"
rez$Nationality[rez$Nationality == "Canadian"] <- "Canada"
rez$Nationality[rez$Nationality == "Austrian"] <- "Austria"
rez$Nationality[rez$Nationality == "Malaysian"] <- "Malaysia"
rez$Nationality[rez$Nationality == "Danish"] <- "Denmark"


names(rez)[names(rez) == "Nationality"] <- "Country"







