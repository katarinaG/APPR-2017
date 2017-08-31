# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(sp)
library(maptools)
library(digest)
gpclibPermit()
library(rvest)
library(gsubfn)
library(readr)
library(tibble)
library(formattable)


#Naredim tabelo, v kateri glede na spol in leto prikazem stevilo prijavljenih in povprecen cas


stevilo_sodelujocih <- rez %>% group_by(Year, Gender) %>% summarise(
  Stevilo_prijav = length(Name),
  Odstop = sum(is.na(Time)),
  Odstotek_uspesnih = round((1 - sum(is.na(Time))/length(Name))*100, 2),
  Povprecen_cas = mean(Time, na.rm = TRUE)) %>% group_by(Year) %>%
  arrange(Year, !is.na(Gender), desc(Gender)) %>%
  mutate(Prijave_kumulativno = cumsum(Stevilo_prijav))



#Se tabela z isto analizo podatkov, grupirana samo po letih
stevilo_sodelujocih_vsi <- rez %>% group_by(Year) %>% summarise(
  Stevilo_prijav_vsi = length(Name),
  Odstop_vsi = sum(is.na(Time)),
  Odstotek_uspesnih_vsi = round((1 - sum(is.na(Time))/length(Name))*100, 2),
  Povprecen_cas_vsi = mean(Time, na.rm = TRUE),
  Koncali_vsi = length(Name)- sum(is.na(Time)))

#Naredim histogram za prikaz prijavljenih v odvisnosti od casa


graf1 <- ggplot(stevilo_sodelujocih, aes(x = Year, y = Stevilo_prijav, fill = Gender)) + geom_bar(stat = "identity") + 
  xlab("Leto") + ylab("Število tekmovalcev") + ggtitle("Rast prijav skozi čas") + theme(legend.position = "right") +
  scale_fill_discrete(name="Spol", labels = c("Ženske", "Moški", "Ni podatka")) + theme_minimal() +
  geom_text(aes(y= Prijave_kumulativno, label= Stevilo_prijav), vjust=0.1, color="black", size=3.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2003, 2017, by =1)) 
  

#Graf, ki prikaze stevilo odstopov

razmerje_uspeh <- data.frame(stevilo_sodelujocih_vsi$Stevilo_prijav_vsi, stevilo_sodelujocih_vsi$Koncali_vsi, stevilo_sodelujocih_vsi$Year)
names(razmerje_uspeh) <- c( "Vsi", "Koncali", "Leto")
seen <- gather(razmerje_uspeh, key, value, Vsi, Koncali)
razmerje_uspeh$odstotek <- percent(razmerje_uspeh$Koncali / razmerje_uspeh$Vsi)

graf2 <- ggplot(NULL) +
  geom_line(data= seen, aes(x=Leto, y=value, colour = key), size = 1) +
  geom_line(size =1) + ggtitle("Število odstopov skozi čas") + theme(legend.position = "right") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2003, 2017, by =1)) + ylab("Število tekmovalcev") +
  scale_y_continuous(breaks = seq(0, 200, by =25)) +
  scale_color_discrete(name = "Tekači", labels = c("Končali", "Vsi")) +
  geom_point(data = razmerje_uspeh, aes(x = Leto, y= Koncali, label = odstotek)) +
  geom_text(data = razmerje_uspeh, aes(x= Leto, y = Koncali, label=odstotek),hjust=0.5, vjust=-0.5, size = 3)


#Graf, ki prikazuje povprecno hitrost glede na spol in leto

stev_sod <- stevilo_sodelujocih[-c(18),]
ind <- which(is.na(stev_sod$Gender))
stev_sod[ind, "Gender"] <- 'Ni podatka'
stevilo_sodelujocih_vsi$Cifre <- strftime(stevilo_sodelujocih_vsi$Povprecen_cas_vsi + as.POSIXct(0, origin="1970-01-01"), format = "%X", tz = "UTC")

graf3 <- ggplot(NULL, aes(x=Year)) + 
  geom_point(data= stev_sod, aes(y = Povprecen_cas, color=Gender), size=6, alpha=0.6) + 
  xlab("Leto") + ylab("Ure") + ggtitle("Povprečen čas")  + theme_minimal() +
  scale_x_continuous(breaks = seq(2003, 2017, by =1)) +
  scale_color_manual(values = c("F" = 'red','M' = 'blue', 'Ni podatka' = 'black'), name = "Spol", labels = c("Ženske", "Moški","Ni podatka")) +
  geom_line(data = stevilo_sodelujocih_vsi, aes(y=Povprecen_cas_vsi), size= 1) + 
  geom_text(data = stevilo_sodelujocih_vsi, aes(y= Povprecen_cas_vsi, label = Cifre), size=3, vjust = -1.1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data= stevilo_sodelujocih_vsi, aes(y = Povprecen_cas_vsi), size=2, alpha=0.6)

#Spreminjanje rekorda skozi čas

#Naredim funkcijo, ki bo izpisala zmagovalca in njegove podatke glede na posamezno leto

najhitrejsi <- function(x){
  nov <- rez %>% filter(Year == x)
  zmagovalec <- nov[which.min(nov$Time), ]
  zmagovalec$Best <- strftime(zmagovalec$Time + as.POSIXct(0, origin="1970-01-01"), format = "%X", tz = "UTC")
  zmagovalec
}


#Naredim tabelo zmagovalcev
zmagovalci <- rbind(najhitrejsi(2003), najhitrejsi(2004), najhitrejsi(2005), najhitrejsi(2006),
                    najhitrejsi(2007), najhitrejsi(2008), najhitrejsi(2009), najhitrejsi(2010),
                    najhitrejsi(2011), najhitrejsi(2012), najhitrejsi(2013), najhitrejsi(2014),
                    najhitrejsi(2015), najhitrejsi(2016), najhitrejsi(2017))

#Narišimo graf, ki prikazuje čase zmagovalcev po posameznih letih 

graf4 <- ggplot(zmagovalci, aes(x=Year, y=Time)) + 
  geom_point(size=6, alpha=0.6, color = "blue") + 
  xlab("Leto") + ylab("Ure") + ggtitle("Časi zmagovalcev")  + theme_minimal() +
  scale_x_continuous(breaks = seq(2003, 2017, by =1)) +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = Best), size = 3.5, vjust = -1.5) +
  ylim(3.4, 4.4)

#Naredimo tabelo, ki prikaze stevilo tekmovalcev glede na stevilo njihovih udejstvovanj

posamezniki <- rez %>% group_by(Name) %>% summarise(
  st_maratonov = length(Time)) %>% group_by(st_maratonov) %>% summarise(
    st_ljudi=length(Name))
vsi <- sum(posamezniki$st_ljudi)
posamezniki$procent <- (posamezniki$st_ljudi / vsi)


# Narisimo tortni graf

graf5 <- ggplot(posamezniki, aes(x = factor(1), y=procent,fill=factor(st_maratonov)) ) + 
  geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") +
  ggtitle("Število sodelovanj posameznikov") +
  theme_void() + 
  scale_fill_discrete(labels = sprintf("%dx - %.4f %%", posamezniki$st_maratonov, posamezniki$procent))+
  theme(legend.position="right", legend.title=element_blank(), plot.title = element_text(lineheight=2, color="black", size=14))

#Naredim se zemljevid, ki bo prikazoval stevilo prijavljenih v 2017

source("lib/uvozi.zemljevid.r")

# Uvozimo zemljevid.

zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", encoding = "Windows-1252") %>%
  pretvori.zemljevid()

msp <- rez%>% group_by(Country) %>% summarise(st_drz = length(Name))

#Preverim katera imena se ne ujemajo in jih popravim
ne_ujema <- msp$Country[! msp$Country %in% zemljevid$sovereignt]

msp$Country[msp$Country == "Lithunia"] <- "Lithuania"
msp$Country[msp$Country == "Phillipines"] <- "Philippines"
msp$Country[msp$Country == "United States"] <- "United States of America"

zem <- msp[-c(45),]
colnames(zem)<- c("Country", "Število")
graf6 <- ggplot() + geom_polygon(data = left_join(zemljevid, zem, by = c("sovereignt" = "Country")), 
                        aes(x = long, y = lat, group = group, fill = Število))  + xlab("") + ylab("")


