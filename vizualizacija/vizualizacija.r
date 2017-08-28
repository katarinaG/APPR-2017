# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)

#Naredim tabelo, v kateri glede na spol in leto prikazem stevilo prijavljenih in povprecen cas

stevilo_sodelujocih <- rez %>% group_by(Year, Gender) %>% summarise(
  Stevilo_prijav = length(Name),
  Odstop = sum(is.na(Time)),
  Odstotek_uspesnih = round((1 - sum(is.na(Time))/length(Name))*100, 2),
  Povprecen_cas = mean(Time, na.rm = TRUE))


#Se tabela z isto analizo podatkov, grupirana samo po letih
# stevilo_sodelujocih_vsi <- rez %>% group_by(Year) %>% summarise(
#   Stevilo_prijav_vsi = length(Name),
#   Odstop_vsi = sum(is.na(Time)),
#   Odstotek_uspesnih_vsi = round((1 - sum(is.na(Time))/length(Name))*100, 2),
#   Povprecen_cas_vsi = mean(Time, na.rm = TRUE))

#Naredim histogram za prikaz prijavljenih v odvisnosti od casa


ggplot(stevilo_sodelujocih, aes(x = Year, y = Stevilo_prijav, fill = Gender)) + geom_bar(stat = "identity") + 
  xlab("Leto") + ylab("Število tekmovalcev") + ggtitle("Rast prijav skozi čas") + theme(legend.position = "right") +
  scale_fill_discrete(name="Spol", labels = c("Ženske", "Moški", "Ni podatka")) + theme_minimal() +
  geom_text(aes(y= Stevilo_prijav, label= Stevilo_prijav), vjust=0.1, color="black", size=3.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2003, 2017, by =1)) 
  

#Graf, ki prikaze stevilo odstopov

razmerje_uspeh <- data.frame(stevilo_sodelujocih_vsi$Stevilo_prijav_vsi, stevilo_sodelujocih_vsi$Koncali, stevilo_sodelujocih_vsi$Year)
names(razmerje_uspeh) <- c( "Vsi", "Koncali", "Leto")
gather(razmerje_uspeh, key, value, Vsi, Koncali) %>%
  ggplot(aes(x=Leto, y=value, colour=key)) +
  geom_line(size =1) + ggtitle("Število odstopov skozi čas") + theme(legend.position = "right") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2003, 2017, by =1)) + ylab("Število tekmovalcev") +
  scale_y_continuous(breaks = seq(0, 200, by =25)) +
  scale_color_discrete(name = "Tekači", labels = c("Končali", "Vsi"))

#Graf, ki prikazuje povprecno hitrost glede na spol in leto

stev_sod <- stevilo_sodelujocih[-c(20),]
ind <- which(is.na(stev_sod$Gender))
stev_sod[ind, "Gender"] <- 'Ni podatka'


ggplot(NULL, aes(x=Year)) + 
  geom_point(data= stev_sod, aes(y = Povprecen_cas, fill = Gender, color=Gender), size=6, alpha=0.6) + 
  xlab("Leto") + ylab("Ure") + ggtitle("Povprečen čas")  + theme_minimal() +
  scale_x_continuous(breaks = seq(2003, 2017, by =1)) +
  scale_color_manual(values = c("F" = 'red','M' = 'blue', 'Ni podatka' = 'black')) +
  geom_line(data = stevilo_sodelujocih_vsi, aes(y=Povprecen_cas_vsi), size= 1) + 
  theme(plot.title = element_text(hjust = 0.5)) 
