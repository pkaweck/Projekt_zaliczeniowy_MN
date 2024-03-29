#*****************************************************************************#
# Drzewa regresyjne - przykład                                                #
#*****************************************************************************#

library(rpart)
library(rpart.plot)
library(dplyr)

install.packages("rpart.plot")

### Gospodarstwa domowe ####
# Załadować dane gd.RData


load(file = "C:/Users/Pawe�/Desktop/Informatyka i ekonometria materia�y/Semestr 3/Modele nieparametryczne/Drzewo regresyjne/gd.RData")

# Dokonać podstawowej eksploracji zmiennej WYD

cbind(colSums(is.na(gd)))

summary(gd$WYD)

#Usuni�cie rekord�w z brakami dla WYD (dla zmiennych niezale�nych mog� zosta� braki)

gd <- filter(gd, !is.na(WYD))

sort(gd$WYD, decreasing = T)[1:10]

hist(gd$WYD[gd$WYD <= 10000])


#Usuni�cie rekord�w z WYD > 10 000

gd = filter(gd, WYD <= 1e4)

hist(gd$WYD)

gd$WYD = gd$WYD/1000 #Wydatki w tysi�cach z�


var(gd$WYD)

VAR(gd$WYD) * (nrow(gd)-1) #suma kwadratowreszt od �redniej 

sum((gd$WYD - mean(gd$WYD))^2) 


#funckja pomocnicza licz�ca sum� kwadrat�w 

ss = function(y){
  sum((y-mean(y))^2)
}

ss(gd$WYD)


# Zbudować model drzewa regresyjnego (zmienna celu = WYD)

drzewo = rpart(
  formula = WYD ~.,
  data = gd
)


# Narysować wykres drzewa

rpart.plot(drzewo,
           branch.type = 5)


rpart.rules(drzewo, style = "tallw")

# Przestudiować proces budowy drzewa

summary(drzewo)



# Odczytać ważność zmiennych


cbind(drzewo$variable.importance)
cbind(drzewo$variable.importance*100/ ss(gd$WYD))

# Ustalić optymalną wielkość drzewa na podstawie sprawdzania krzyżowego

drzewo_przeuczone = rpart(
  formula = WYD ~.,
  data = gd,
  control = rpart.control(cp = 0)
)


dim(drzewo_przeuczone$cptable)
tail(drzewo_przeuczone$cptabel)

drzewo_przeuczone$cptable

# Ocenić dokładność modelu


przycinanie <- function(drzewo) {
  bledy <- drzewo$cptable
  tmp1 <- which.min(bledy[, "xerror"])  # min b��d w sprawdzaniu krzy�owym
  tmp2 <- sum(bledy[tmp1, c("xerror", "xstd")]) # min b��d + odchylenie standardowe
  optymalny <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa
  
  prune(drzewo, cp = bledy[optymalny, "CP"]) # przyci�cie drzewa
}

drzewo_przyciete = przycinanie(drzewo_przeuczone)


#Sprzawdzenie predykcji
predykcje= predict(drzewo_przyciete)

#�rednia kwadratowa b��du
mean((gd$WYD - predykcje)^2)

#�redni b��d

mean((gd$WYD - predykcje)^2)^0.5
#B��d to 1400 z�

plot(x  =gd$WYD, y = predykcje)


table(predykcje)




