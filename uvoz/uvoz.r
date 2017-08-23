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
  tabela <- tabela[-c(1, 2), ]
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




# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi občine iz Wikipedije
# uvozi.obcine <- function() {
#   link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
#   stran <- html_session(link) %>% read_html()
#   tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
#     .[[1]] %>% html_table(dec = ",")
#   colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
#                         "ustanovitev", "pokrajina", "regija", "odcepitev")
#   tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
#   tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
#   tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
#   for (col in colnames(tabela)) {
#     tabela[tabela[[col]] == "-", col] <- NA
#   }
#   for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
#     if (is.numeric(tabela[[col]])) {
#       next()
#     }
#     tabela[[col]] <- gsub("[.*]", "", tabela[[col]]) %>% as.numeric()
#   }
#   for (col in c("obcina", "pokrajina", "regija")) {
#     tabela[[col]] <- factor(tabela[[col]])
#   }
#   return(tabela)
# }
# 
# # Funkcija, ki uvozi podatke iz datoteke druzine.csv
# uvozi.druzine <- function(obcine) {
#   data <- read_csv2("podatki/druzine.csv", col_names = c("obcina", 1:4),
#                     locale = locale(encoding = "Windows-1250"))
#   data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
#     strapplyc("([^ ]+)") %>% sapply(paste, collapse = " ") %>% unlist()
#   data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
#   data <- data %>% melt(id.vars = "obcina", variable.name = "velikost.druzine",
#                         value.name = "stevilo.druzin")
#   data$velikost.druzine <- as.numeric(data$velikost.druzine)
#   data$obcina <- factor(data$obcina, levels = obcine)
#   return(data)
# }

# Zapišimo podatke v razpredelnico obcine
#obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
#druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
