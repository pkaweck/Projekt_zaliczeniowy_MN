#*****************************************************************************#
# Drzewa regresyjne - przykÅ‚ad                                                #
#*****************************************************************************#

library(rpart)
library(rpart.plot)
library(dplyr)

install.packages("rpart.plot")

### Gospodarstwa domowe ####
# ZaÅ‚adowaÄ‡ dane gd.RData


load(file = "C:/Users/Pawe³/Desktop/Informatyka i ekonometria materia³y/Semestr 3/Modele nieparametryczne/Drzewo regresyjne/gd.RData")

# DokonaÄ‡ podstawowej eksploracji zmiennej WYD

cbind(colSums(is.na(gd)))

summary(gd$WYD)

#Usuniêcie rekordów z brakami dla WYD (dla zmiennych niezale¿nych mog¹ zostaæ braki)

gd <- filter(gd, !is.na(WYD))

sort(gd$WYD, decreasing = T)[1:10]

hist(gd$WYD[gd$WYD <= 10000])


#Usuniêcie rekordów z WYD > 10 000

gd = filter(gd, WYD <= 1e4)

hist(gd$WYD)

gd$WYD = gd$WYD/1000 #Wydatki w tysi¹cach z³


var(gd$WYD)

VAR(gd$WYD) * (nrow(gd)-1) #suma kwadratowreszt od œredniej 

sum((gd$WYD - mean(gd$WYD))^2) 


#funckja pomocnicza licz¹ca sumê kwadratów 

ss = function(y){
  sum((y-mean(y))^2)
}

ss(gd$WYD)


# ZbudowaÄ‡ model drzewa regresyjnego (zmienna celu = WYD)

drzewo = rpart(
  formula = WYD ~.,
  data = gd
)


# NarysowaÄ‡ wykres drzewa

rpart.plot(drzewo,
           branch.type = 5)


rpart.rules(drzewo, style = "tallw")

# PrzestudiowaÄ‡ proces budowy drzewa

summary(drzewo)



# OdczytaÄ‡ waÅ¼noÅ›Ä‡ zmiennych


cbind(drzewo$variable.importance)
cbind(drzewo$variable.importance*100/ ss(gd$WYD))

# UstaliÄ‡ optymalnÄ… wielkoÅ›Ä‡ drzewa na podstawie sprawdzania krzyÅ¼owego

drzewo_przeuczone = rpart(
  formula = WYD ~.,
  data = gd,
  control = rpart.control(cp = 0)
)


dim(drzewo_przeuczone$cptable)
tail(drzewo_przeuczone$cptabel)

drzewo_przeuczone$cptable

# OceniÄ‡ dokÅ‚adnoÅ›Ä‡ modelu


przycinanie <- function(drzewo) {
  bledy <- drzewo$cptable
  tmp1 <- which.min(bledy[, "xerror"])  # min b³¹d w sprawdzaniu krzy¿owym
  tmp2 <- sum(bledy[tmp1, c("xerror", "xstd")]) # min b³¹d + odchylenie standardowe
  optymalny <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa
  
  prune(drzewo, cp = bledy[optymalny, "CP"]) # przyciêcie drzewa
}

drzewo_przyciete = przycinanie(drzewo_przeuczone)


#Sprzawdzenie predykcji
predykcje= predict(drzewo_przyciete)

#Œrednia kwadratowa b³êdu
mean((gd$WYD - predykcje)^2)

#Œredni b³¹d

mean((gd$WYD - predykcje)^2)^0.5
#B³¹d to 1400 z³

plot(x  =gd$WYD, y = predykcje)


table(predykcje)




