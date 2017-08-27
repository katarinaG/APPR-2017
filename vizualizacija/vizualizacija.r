# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)
library(reshape2)

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











