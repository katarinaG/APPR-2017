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
rezultat03$spol <- m03
rezultat03$drzava <- m03
rez03 <- rezultat03[c(1, 5, 6, 2,3,4)]

names(rez03) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

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
rezultat04$spol <- m04
rezultat04$drzava <- m04
rez04 <- rezultat04[c(1, 5, 6, 2,3,4)]

names(rez04) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

#Urejanje razpredelnice za leto 2005
m2005 <- strsplit(as.character(rez.2005$rez.2005.X6), ":") %>% 
  sapply(function(x) {
    if (length(x) == 0) return(c(NA, NA, NA))
    else return(as.numeric(x))
  }) %>% t()
rez05 <- data.frame(rez.2005, m2005)

rez05[4] <- NULL

names(rez05) <- c("Name","Gender","Country","Hours","Minutes","Seconds")

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

#Urejanje razpredelnice za leto 2009

#Urejanje razpredelnice za leto 2010

#Urejanje razpredelnice za leto 2011

#Urejanje razpredelnice za leto 2012

#Urejanje razpredelnice za leto 2013

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
















