#*****************************************************************************#
# Drzewa klasyfikacyjne - podejście wielomodelowe                             #
#*****************************************************************************#

library(adabag)  # (boosting i bagging)
library(randomForestSRC)  # (random forest)



#### Spam ####
### Załadować dane spam.RData

load(file = "C:/Users/Pawe�/Desktop/Informatyka i ekonometria materia�y/Semestr 3/Modele nieparametryczne/spam.RData")

### Podzielić dane losowo na zbiór uczący, walidacyjny i testowy

grupy = sample(rep(c("ucz�cy", "walidacyjny", "testowy"),
            round(c(0.6, 0.2, 0.2)*nrow(spam))))

spam.ucz�cy = spam[grupy == "ucz�ce",]
spam.walidacyjny = spam[grupy == "walidacyjny"]
spam.testowy = spam[grupy == "testowy"]



### Zbudować model BAGGING

spam.bagging = bagging(
  formula = Spam~.,
  data = spam.ucz�cy,
  mfinal = 20
)

# Narysować kilka pierwszych pojedynczych drzew

spam.bagging$trees[[1]]

rpart.plot(spam.bagging$trees[[1]],
           roundint = F)

# Przeanalizować rozkłady głosów

head(spam.bagging$votes)

table(spam.bagging$votes[,2])

barplot(table(spam.ucz�cy$Spam, spam.bagging$votes[,2]), 
        legend.text = T,
        args.legend = list(x = "top",
        title = "Rzeczywista klasa"),
        xlab = "Liczba g�os�w na tak")

#dla jakich wiadomo�ci liczba g�os�w na tak = 20, a nie jest to spam

which(spam.bagging$votes[,2]==20 & 
        spam.ucz�cy$Spam == "Nie")


# Odczytać ważność zmiennych

cbind(sort(spam.bagging$importance))

# Narysować wykresy błędów (na zbiorach uczącym i walidacyjnym) w zależności od liczby drzew

bledy.ucz�cy = errorevol(spam.bagging, spam.uczacy)
bledy.walidacyjny = errorevol(spam.bagging, spam.walidacyjny)

plot.errorevol(x = bledy.walidacyjny,
               y = bledy.uczacy)

grid()

#zmiana argumentu cp dla pojedynczych drzew

spam.bagging = bagging(
  formula = Spam~.,
  data = spam.ucz�cy,
  mfinal = 20,
  control = rpart.control(cp = 0)
)


# Ustalić optymalną liczbę modeli bazowych

p = min(bledy.walidacyjny$error)

se = (p*(1-p)/nrow(spam.walidacyjny))^0.5

#Pr�g to:
optymalne.m = which(bledy.walidacyjny$error < p + se)


spam.bagging = bagging(
  formula = Spam~.,
  data = spam.ucz�cy,
  mfinal = optymalny.m,
  control = rpart.control(cp = 0)
)


# Sprawdzić dokładność modelu na zbiorze testowym

predykcje  = predict(spam.bagging, newdata = spam.testowy)
predykcje$confusion
predykcja$error

### Zbudować model BOOSTING

#jako zadanie dodmowe zrobi� to 

# Przeanalizować rozkłady głosów

# Odczytać ważność zmiennych

# Narysować wykresy błędów (na zbiorach uczącym i walidacyjnym) w zależności od liczby drzew

# Ustalić optymalną liczbę modeli bazowych

# Sprawdzić dokładność modelu na zbiorze testowym


### Zbudować model RANDOM FOREST

# Odczytać ważność zmiennych

# Ustalić optymalną liczbę modeli bazowych

# Sprawdzić dokładność modelu na zbiorze testowym


#### Pojazdy (dane:Vehicle) ####
library(mlbench)
data("Vehicle")

# Wykonać polecenia jw. (zmienna celu = Class)


#### Kredytobiorcy ####
### Załadować dane credit2.RData
# Wykonać polecenia jw. (zmienna celu = AKCEPTACJA)


