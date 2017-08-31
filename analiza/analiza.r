# 4. faza: Analiza podatkov

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
library(prediction)
library(mosaic)
library(ggpubr)

#Napoved rasti prijav

#Pogledam razlicne regresijske metode - linearno in eksponentno
sodelujoci <- data.frame(stevilo_sodelujocih_vsi$Year, stevilo_sodelujocih_vsi$Stevilo_prijav_vsi)
names(sodelujoci) <- c("leta", "stevilo")
#Linearna regresija

fit1 = lm(stevilo ~ leta, data = sodelujoci)
# Napoved novih podatkov
newdata <- data.frame('eta'= 2018:2032)
newdata$mi <- predict(fit1, newdata)


# narišemo stare in nove podatke

napoved1 <- ggplot(NULL) +
  geom_point(data = sodelujoci, aes(x=leta, y= stevilo), size=3, color = "blue") +
  xlab("Leto") + ylab("Število tekmovalcev") + theme_minimal()+ geom_line(data = sodelujoci, aes(x=leta, y= stevilo), size=1) +
  geom_point(data = newdata, aes(x= leta, y=mi), size=3, color = "green") +
  scale_x_continuous(breaks = seq(2003, 2033, by =1)) + ggtitle("Napoved z uporabo linearne regresije") + 
  theme(plot.title = element_text(hjust = 0.5))


#Polinomska regresija
fit2 <- lm(stevilo ~ poly(leta,3), data = sodelujoci)
newdata2 <- data.frame('eta'= 2018:2032)
newdata2$mi <- predict(fit2, newdata2)

napoved2 <- ggplot(NULL) +
  geom_point(data = sodelujoci, aes(x=leta, y= stevilo), size=3, color = "blue") +
  xlab("Leto") + ylab("Število tekmovalcev") + theme_minimal()+ geom_line(data = sodelujoci, aes(x=leta, y= stevilo), size=1) +
  geom_point(data = newdata2, aes(x=leta, y=mi), size=3, color = "green") +
  scale_x_continuous(breaks = seq(2003, 2033, by =1)) + ggtitle("Napoved z uporabo polinomske regresije") + 
  theme(plot.title = element_text(hjust = 0.5))


#Eksponentni model
fit3 <- lm(stevilo ~ exp(leta - 2000), data = sodelujoci)
newdata3 <- data.frame('eta'= 2018:2032)
newdata3$mi <- predict(fit3, newdata3)

napoved3 <- ggplot(NULL) +
  geom_point(data = sodelujoci, aes(x=leta, y= stevilo), size=3, color = "blue") +
  xlab("Leto") + ylab("Število tekmovalcev") + theme_minimal()+ geom_line(data = sodelujoci, aes(x=leta, y= stevilo), size=1) +
  geom_point(data = newdata3, aes(x=leta, y=mi), size=3, color = "green") +
  scale_x_continuous(breaks = seq(2003, 2033, by =5)) + ggtitle("Napoved z uporabo eksponentne regresije") + 
  theme(plot.title = element_text(hjust = 0.5))


#Pogledamo ostanke pri modelih. Tisti, ki ima manjši ostanek je bolj natančen
vsota.kvadratov <- sapply(list(fit1, fit2, fit3), function(x) sum(x$residuals^2))

napovedi <- ggarrange(napoved1, napoved2 + rremove("x.text"), ncol = 1, nrow = 2)

