load(file ="C:/Users/Marcel/Desktop/Studia/Semestr III/modele nieparametryczne/projekt/dane_zaliczenie.RData")

load(file = "C:/Users/Paweł/Desktop/Pulpit/Informatyka i ekonometria materiały/Semestr 3/Modele nieparametryczne/Projekt_zaliczeniowy_MN/dane_zaliczenie.RData")




# Realizacja zadań powinna obejmować:

# Wstępną eksplorację danych 
# Wzbogacanie danych (utworzenie dodatkowych zmiennych, np. dzień tygodnia na podstawie createtime, przypisanie cech transakcji inicjalizującej do transakcji rekurencyjnej)
# Budowanie modelu
# Prezentację i interpretację wyników modelu
# Krytyczną ocenę uzyskanych modeli
# Predykcję modelowanych zmiennych na danych testowych (predykcje należy zapisać w odpowiednich kolumnach obiektu predykcje_testowa)

install.packages("rpart.plot")
install.packages("ElemStatLearn")
install.packages('e1071')
install.packages("earth")
library(earth)
library(e1071)
library(rpart)
install.packages('adabag')
library(adabag)
library(rpart.plot)
library(dplyr)
library(ElemStatLearn)
library(ggplot2)
library(stringr)
library("data.table")
library(vip)

#Względna liczba braków rekordów danych
cbind(colSums((is.na(proba_testowa)/15185)*100))
cbind(colSums((is.na(proba_uczaca)/nrow(proba_uczaca))*100))

head(proba_uczaca)
head(proba_testowa)
#Wykres dla status unikatowa liczba dla kolumn
ggplot(proba_uczaca, aes(x = status)) +
  geom_bar()

#Liczba unikatowych wyborów w status 
table(proba_uczaca$status)

#recurring action = MANUAL wystepuje tylko w probie testowej w liczebnosci 3. Nie mozna przewidziec ze wzgledu na za mala liczbe
#przypadkow oraz niedostepnosc kategorii w probie uczacej w celu przyuczenia modelu do takiej transakcji. JEst to wyjątek w kategorii
# wystepujacy tylko w przypadku jednej transakcji initial 125778
head(proba_testowa %>%
    filter(proba_testowa$recurringaction == 'MANUAL'),n=3)

table(proba_uczaca$recurringaction)
#Bardzo rzadka liczba transakcji kartą MAESTRO. Głównie transkacje wykonywane za pomocą kart
#MasterCard oraz VISA

ggplot(proba_uczaca, aes(x = issuer)) +
  geom_bar()

#Przewaga liczby transkacji kartą debetową.

ggplot(proba_uczaca, aes(x = type)) +
  geom_bar()


#Ujednolicenie danych ze względu na różną pisownie w opisie transakcji
proba_uczaca["description"][proba_uczaca["description"] == "PLAY - FAKTURA"] <- "PLAY - faktura"
proba_uczaca["description"][proba_uczaca["description"] == "RedBull Mobile - FAKTURA"] = 'RedBull Mobile - faktura'
proba_uczaca["description"][proba_uczaca["description"] == "Szybkie sprawdzanie firmy"] = 'Szybkie sprawdzanie'

#Zmiana wartosci na other dla rzadkich typów klas karty
card_level_names <- c(names(table(proba_uczaca$level))[as.vector(table(proba_uczaca$level))>100])
proba_uczaca$level <- ifelse(proba_uczaca$level %in% card_level_names,proba_uczaca$level, "OTHER")
table(proba_uczaca$level)

#Zmiana wartosci na other dla niewielu obserwacji country code
country_names <- c(names(table(proba_uczaca$countrycode))[as.vector(table(proba_uczaca$countrycode))>99])
proba_uczaca$countrycode <- ifelse(proba_uczaca$countrycode %in% country_names,proba_uczaca$countrycode, "OT")
table(proba_uczaca$countrycode)

#Podział tablicy danych na tablice z transakcjami inicującycmi i rekurencyjnymi.
#Następnie do rzędów transakcji rekurencyjnych dodane są dane o transakcji inicjujących 
table(proba_uczaca$recurringaction)
proba_uczaca_recurring <- subset(proba_uczaca, recurringaction =="AUTO")
proba_uczaca_initial <- subset(proba_uczaca, recurringaction !="AUTO")

#Zmiana nazw kolumn w celu identyfikacji column initial po dodaniu do tablicy danych recurring
names(proba_uczaca_initial)<- paste0('init', "_",names(proba_uczaca_initial))


proba_uczaca_merged <- merge(y = proba_uczaca_initial, x = proba_uczaca_recurring, by.x = 'initialtransaction_id', all.x = TRUE, by.y = 'init_id')
proba_uczaca_merged["mccname"][proba_uczaca_merged["mccname"] == "Sklepy kosmetyczne"] = 'Usługi biznesowe gdzie indziej nie sklasyfikowane'


ggplot(proba_uczaca_merged, aes(x = acquirerconnectionmethod)) +
  geom_bar()


#Do wyciągnięcia: pełna data, godzina, dzień tygodnia ze względu na możliwość różnicy w liczbie transkacji wykonywanych w danym momencie, co może miec wpływ na pomyślne wykonanie
#Wyciagnięcie daty z createtime

proba_uczaca_merged$recurring_createdate <- as.Date(proba_uczaca_merged$createtime,format="%Y-%m-%d")
proba_uczaca_merged$init_createdate <- as.Date(proba_uczaca_merged$init_createtime,format="%Y-%m-%d")


#Wyciąganie dnia tygodnia

proba_uczaca_merged$recurring_weekday <- weekdays(as.Date(proba_uczaca_merged$createtime))
proba_uczaca_merged$init_weekday <- weekdays(as.Date(proba_uczaca_merged$init_createtime))

#Wyciagnięcie godziny z createtime

proba_uczaca_merged$recurring_hour <- format(proba_uczaca_merged$createtime,format="%H")
proba_uczaca_merged$init_hour <- format(proba_uczaca_merged$init_createtime,format="%H")

head(proba_uczaca_merged)

#Wyciagnięcie zmiennej expired_card
proba_uczaca_merged$expiry_date =  format(as.Date(paste(proba_uczaca_merged$expiryyear, proba_uczaca_merged$expirymonth,"01", sep = '-')), format = "%Y-%m-01")
proba_uczaca_merged$compare_to_expiry = format(proba_uczaca_merged$createtime, format="%Y-%m-01")
head(proba_uczaca_merged$expiry_date==proba_uczaca_merged$compare_to_expiry)
proba_uczaca_merged$expired_card = as.integer(as.logical(proba_uczaca_merged$expiry_date<proba_uczaca_merged$compare_to_expiry))

#Zamiana jednostek wysokości i szerekości do komputera bądź telefonu

plot(proba_uczaca_merged$init_screenheight,proba_uczaca_merged$init_screenwidth)
proba_uczaca_merged["device"] <- NA
proba_uczaca_merged["device"][proba_uczaca_merged["init_screenwidth"]<proba_uczaca_merged["init_screenheight"]] <- "mobilephone"
proba_uczaca_merged["device"][proba_uczaca_merged["init_screenwidth"]>=proba_uczaca_merged["init_screenheight"]] <- "computer"

#Barplot dla device

  ggplot((proba_uczaca_merged), aes(x = device)) +
  geom_bar()


proba_uczaca_merged[c('browser', 'other_software_info')] <- str_split_fixed(proba_uczaca_merged$init_browseragent, ' ', 2)

proba_uczaca_merged[c('device_type', 'other_software_info')] <- substring(str_split_fixed(proba_uczaca_merged$other_software_info, ';', 2),2)

#Podmiana wartości

proba_uczaca_merged$device_type[grepl('Windows NT 6.1',proba_uczaca_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 6.1'

proba_uczaca_merged$device_type[grepl('Playstation',proba_uczaca_merged$device_type,ignore.case = TRUE)] <- 'Playstation 4'

proba_uczaca_merged$device_type[grepl('Windows NT 10.0',proba_uczaca_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 10.0'

proba_uczaca_merged$device_type[grepl('Windows NT 10.0',proba_uczaca_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 10.0'
#Zmiana wartosci na other dla rzadkich typów klas karty
device_type_names <- c(names(table(proba_uczaca_merged$device_type))[as.vector(table(proba_uczaca_merged$device_type))>500])
proba_uczaca_merged$device_type <- ifelse(proba_uczaca_merged$device_type %in% device_type_names,proba_uczaca_merged$device_type, "OTHER")

##### USUWANIE ZMIENNYCH KLASYFIKACJI

###Postanowić jakie zmienne są przydatne wgl do modelu. Ustalenie jakie mają sens dla transakcji.

proba_uczaca_merged_nodel <- proba_uczaca_merged
proba_uczaca_merged <- subset(proba_uczaca_merged, select = -c(initialtransaction_id,id,description,init_createdate, init_payclickedtime, payclickedtime,init_payclickedtime, init_listtype,init_mccname,init_status,init_initialtransaction_id, createtime, init_createtime,browseragent,
init_browseragent,init_screenheight,init_screenwidth,screenwidth,screenheight,other_software_info,expirymonth,expiryyear,mccname, listtype,init_expirymonth,init_expiryyear,compare_to_expiry,expiry_date))


cbind(colSums((is.na(proba_uczaca_merged)/nrow(proba_uczaca_merged))*100))

## DATASET DO MODELOWANIA:
proba_uczaca_model <- proba_uczaca_merged
head(proba_uczaca_model)
# Przygotowanie zbioru testowego

#Ujednolicenie danych ze względu na różną pisownie w opisie transakcji
proba_testowa["description"][proba_testowa["description"] == "PLAY - FAKTURA"] <- "PLAY - faktura"
proba_testowa["description"][proba_testowa["description"] == "RedBull Mobile - FAKTURA"] = 'RedBull Mobile - faktura'
proba_testowa["description"][proba_testowa["description"] == "Szybkie sprawdzanie firmy"] = 'Szybkie sprawdzanie'


#Zmiana wartosci na other dla rzadkich typów klas karty
card_level_names <- c(names(table(proba_testowa$level))[as.vector(table(proba_testowa$level))>100])
proba_testowa$level <- ifelse(proba_testowa$level %in% card_level_names,proba_testowa$level, "OTHER")



#Zmiana wartosci na other dla niewielu obserwacji country code
country_names <- c(names(table(proba_testowa$countrycode))[as.vector(table(proba_testowa$countrycode))>99])
proba_testowa$countrycode <- ifelse(proba_testowa$countrycode %in% country_names,proba_testowa$countrycode, "OT")

#Podział tablicy danych na tablice z transakcjami inicującycmi i rekurencyjnymi.
#Następnie do rzędów transakcji rekurencyjnych dodane są dane o transakcji inicjujących 
#Podmiana wartosci manual na AUTO w celu ujednolicenia danych
proba_testowa_recurring <- subset(proba_testowa, recurringaction == "AUTO")
proba_testowa_initial <- subset(proba_testowa, recurringaction !="AUTO")
proba_testowa_recurring["recurringaction"][proba_testowa_recurring["recurringaction"] == "MANUAL"] = 'AUTO'
#Zmiana nazw kolumn w celu identyfikacji column initial po dodaniu do tablicy danych recurring
names(proba_testowa_initial)<- paste0('init', "_",names(proba_testowa_initial))


proba_testowa_merged <- merge(y = proba_testowa_initial, x = proba_testowa_recurring, by.x = 'initialtransaction_id',
all.x = TRUE, by.y = 'init_id')
proba_testowa_merged["mccname"][proba_testowa_merged["mccname"] == "Sklepy kosmetyczne"] = 'Usługi biznesowe gdzie indziej nie sklasyfikowane'


#Do wyciągnięcia: pełna data, godzina, dzień tygodnia ze względu na możliwość różnicy w liczbie transkacji wykonywanych w danym momencie, co może miec wpływ na pomyślne wykonanie
#Wyciagnięcie daty z createtime

proba_testowa_merged$recurring_createdate <- as.Date(proba_testowa_merged$createtime,format="%Y-%m-%d")
proba_testowa_merged$init_createdate <- as.Date(proba_testowa_merged$init_createtime,format="%Y-%m-%d")


#Wyciąganie dnia tygodnia

proba_testowa_merged$recurring_weekday <- weekdays(as.Date(proba_testowa_merged$createtime))
proba_testowa_merged$init_weekday <- weekdays(as.Date(proba_testowa_merged$init_createtime))

#Wyciagnięcie godziny z createtime

proba_testowa_merged$recurring_hour <- format(proba_testowa_merged$createtime,format="%H")
proba_testowa_merged$init_hour <- format(proba_testowa_merged$init_createtime,format="%H")

#Wyciagnięcie zmiennej expired_card
proba_testowa_merged$expiry_date =  format(as.Date(paste(proba_testowa_merged$expiryyear, proba_testowa_merged$expirymonth,"01", sep = '-')), format = "%Y-%m-01")
proba_testowa_merged$compare_to_expiry = format(proba_testowa_merged$createtime, format="%Y-%m-01")

proba_testowa_merged$expired_card = as.integer(as.logical(proba_testowa_merged$expiry_date<proba_testowa_merged$compare_to_expiry))

#Zamiana jednostek wysokości i szerekości do komputera bądź telefonu

proba_testowa_merged["device"] <- NA
proba_testowa_merged["device"][proba_testowa_merged["init_screenwidth"]<proba_testowa_merged["init_screenheight"]] <- "mobilephone"
proba_testowa_merged["device"][proba_testowa_merged["init_screenwidth"]>=proba_testowa_merged["init_screenheight"]] <- "computer"

proba_testowa_merged[c('browser', 'other_software_info')] <- str_split_fixed(proba_testowa_merged$init_browseragent, ' ', 2)

proba_testowa_merged[c('device_type', 'other_software_info')] <- substring(str_split_fixed(proba_testowa_merged$other_software_info, ';', 2),2)

#Podmiana wartości

proba_testowa_merged$device_type[grepl('Windows NT 6.1',proba_testowa_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 6.1'

proba_testowa_merged$device_type[grepl('Playstation',proba_testowa_merged$device_type,ignore.case = TRUE)] <- 'Playstation 4'

proba_testowa_merged$device_type[grepl('Windows NT 10.0',proba_testowa_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 10.0'

proba_testowa_merged$device_type[grepl('Windows NT 10.0',proba_testowa_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 10.0'


#Zmiana wartosci na other dla rzadkich typów klas karty
device_type_names <- c(names(table(proba_testowa_merged$device_type))[as.vector(table(proba_testowa_merged$device_type))>500])
proba_testowa_merged$device_type <- ifelse(proba_testowa_merged$device_type %in% device_type_names,proba_testowa_merged$device_type, "OTHER")

##### USUWANIE ZMIENNYCH KLASYFIKACJI

###Postanowić jakie zmienne są przydatne wgl do modelu. Ustalenie jakie mają sens dla transakcji.

proba_testowa_nodel<- proba_testowa_merged
proba_testowa_merged <- subset(proba_testowa_merged, select = -c(initialtransaction_id,description,init_createdate, init_payclickedtime, payclickedtime,init_payclickedtime, init_listtype,init_mccname,init_initialtransaction_id, createtime, init_createtime,browseragent,
init_browseragent,init_screenheight,init_screenwidth,screenwidth,screenheight,other_software_info,expirymonth,expiryyear,mccname, listtype,init_expirymonth,init_expiryyear,compare_to_expiry,expiry_date))

head(proba_testowa_merged)

####### ZADANIE 1 PREDYKCJA STATUSU

#Zmiana status na wartość dychotomiczną 
proba_uczaca_model$status = as.character(proba_uczaca_model$status)
proba_uczaca_model$status = as.factor(proba_uczaca_model$status)
proba_uczaca_model$status  = factor(x = proba_uczaca_model$status, levels=c("completed successfully","card limit exceeded","bank declined","do not honor"),labels = c(1,0,0,0))


### Zbudować drzewo klasyfikacyjne (zmienna celu: status)
head(proba_uczaca_model)

big.drzewo = rpart(
  formula = status~.,
  data = proba_uczaca_model,
  control= rpart.control(cp=0.005)
  )

big.drzewo

rpart.plot(
  x = big.drzewo,
  box.palette = "Orange",
)

#reguły decyzyjne
big.drzewo_reg_decyzyjne = rpart.rules(x = big.drzewo)
big.drzewo_reg_decyzyjne



# Ustalić optymalne parametry modelu (wielkość drzewa)

bledy = big.drzewo$cptable

nr.min.cp <- which.min(bledy[, "xerror"])  # numer min cp błąd w sprawdzaniu krzyżowym
prog.odciecia <- sum(bledy[nr.min.cp, c("xerror", "xstd")]) # prog odciecia (min błąd + odchylenie standardowe)
nr.optymalny <- which(bledy[, "xerror"] < prog.odciecia)[1] # nr optymalnego drzewa
cp.optymalny = bledy[nr.optymalny, "CP"]
prog.odciecia

przyciete <- prune(tree = big.drzewo, 
                       cp = cp.optymalny) # przycięcie drzewa
rpart.plot(przyciete)



# Odczytać ważność zmiennych

przyciete$variable.importance

dotchart(rev(przyciete$variable.importance))


# Sprawdzić dokładność modelu na zbiorze uczacym

przyciete <- prune(tree = big.drzewo, 
                       cp = cp.optymalny) # przycięcie drzewa

predykcja.uczaca = predict(
  object = przyciete,
  newdata = proba_uczaca_model,
  type = "class"
)


table(Rzeczywiste = proba_uczaca_model$status,
      Przewidywane = predykcja.uczaca )

#Błąd klasyfikacji.
mean(proba_uczaca_model$status != predykcja.uczaca)

cbind(przyciete$variable.importance[1:5])


# Wykonanie predykcji dla zbioru testowego

predykcja.statusu = predict(
  object = przyciete,
  newdata = proba_testowa_merged,
  type = "class"
)

length(predykcja.statusu)


### model BAGGING

#podział grupy uczacej na uczacą i walidacje
grupy <- sample(rep(c("uczacy", "walidacja"),
                    round(c(0.8, 0.2)*nrow(proba_uczaca_model))))

df.bagging.uczacy <- proba_uczaca_model[grupy == "uczacy", ]
df.bagging.walidacyjny <- proba_uczaca_model[grupy == "walidacja", ]


drzewo.bagging.check <- bagging(
  formula = status ~.,
  data = df.bagging.uczacy,
  mfinal = 20,
  control = rpart.control(cp = 0)
)
table(df.bagging.uczacy$status, drzewo.bagging.check$votes[, 2])


barplot(table(df.bagging.uczacy$status, drzewo.bagging.check$votes[,2]), 
        legend.text = T,
        args.legend = list(x = "top",
        title = "Rzeczywista klasa"),
        xlab = "Liczba głosów na tak")
# Sprawdzić dokładność modelu na zbiorze testowym

predykcje_valid <- predict(drzewo.bagging.check, newdata = df.bagging.walidacyjny)

bledy.uczacy <-errorevol(drzewo.bagging.check, df.bagging.uczacy)
bledy.walidacyjny <-errorevol(drzewo.bagging.check, df.bagging.walidacyjny)

plot.errorevol(x = bledy.walidacyjny,
               y = bledy.uczacy)


# Ustalić optymalną liczbę modeli bazowych
p <- min(bledy.walidacyjny$error)
se <- (p*(1-p)/nrow(df.bagging.walidacyjny))^0.5

optymalne.m <- which(bledy.walidacyjny$error < p +se)[1]
optymalne.m
#error na baggingu jest mniejszy o 0.03 od błedu na przycietym drzewie losowym, 
#zatem wybrano model baggingu do predykcji
predykcje_valid$error

drzewo.bagging.final <- bagging(
  formula = status ~.,
  data = proba_uczaca_model,
  mfinal = optymalne.m,
  control = rpart.control(cp = 0)
)

# Przeanalizować rozkłady głosów
head(drzewo.bagging.final$votes,15)

barplot(table(proba_uczaca_model$status, drzewo.bagging.final$votes[, 1]),
        legend.text = T,
        args.legend = list(x = "top",
                           title = "Rzeczywista klasa"),
        xlab = "Liczba głosów na TAK")

table(proba_uczaca_model$status, drzewo.bagging.final$votes[, 1])
predykcje <- predict(drzewo.bagging.final, newdata = proba_testowa_merged)
length(predykcje$class)


to.merge <- data.frame(cbind(id = as.numeric(proba_testowa_merged$id),pred = ifelse(predykcje$class=="1","sukces","porażka")))
head(to.merge)
head(proba_testowa_merged)
predykcje_testowa.m <- merge(predykcje_testowa, to.merge, by.x = 'id', by.y = 'id', all.x = T)
predykcje_testowa.m <- predykcje_testowa.m %>% dplyr::select(-status) %>% mutate(status = pred) %>% dplyr::select(-pred)

predykcje_testowa <- predykcje_testowa.m


###ZADANIE 2 - REGRESJA ZMIENNEJ AMOUNT
proba_uczaca_merged_nodel <- subset(proba_uczaca_merged_nodel, select = -c(initialtransaction_id,id,init_createdate, init_payclickedtime, payclickedtime,init_initialtransaction_id, createtime, init_createtime,browseragent,init_browseragent,init_screenheight,
init_screenwidth,screenwidth,screenheight,other_software_info,expirymonth,expiryyear,expiry_date,init_expirymonth,init_expiryyear,compare_to_expiry,expiry_date,init_status, init_mccname,init_listtype,expired_card,recurringaction,status))
proba_uczaca_model_amount <- proba_uczaca_merged_nodel 
colnames(proba_uczaca_model_amount)
proba_testowa_nodel <- subset(proba_testowa_nodel, select = -c(init_createdate, init_payclickedtime, payclickedtime,init_initialtransaction_id, createtime, init_createtime,browseragent,init_browseragent,init_screenheight,
init_screenwidth,screenwidth,screenheight,other_software_info,expirymonth,expiryyear,expiry_date,init_expirymonth,init_expiryyear,compare_to_expiry,expiry_date, init_mccname,init_listtype,expired_card,recurringaction,amount))
proba_testowa_model_amount <- proba_testowa_nodel 


proba_uczaca_model_amount_1500 = filter(proba_uczaca_model_amount, amount <= 1500)
proba_uczaca_model_amount_1000 = filter(proba_uczaca_model_amount, amount <= 1000)
# Zbudować model drzewa regresyjnego
head(proba_uczaca_model_amount_1500)
drzewo_amount = rpart(
  formula = amount ~.,
  data = proba_uczaca_model_amount
)



# Narysować wykres drzewa

rpart.plot(drzewo_amount,
           branch.type = 5)


rpart.rules(drzewo_amount, style = "tallw")

# Ustalić optymalną wielkość drzewa na podstawie sprawdzania krzyżowego

drzewo_amount_przeuczone = rpart(
  formula = amount ~.,
  data = proba_uczaca_model_amount,
  control = rpart.control(cp = 0)
)

drzewo_amount_przeuczone_1500 = rpart(
  formula = amount ~.,
  data = proba_uczaca_model_amount_1500,
  control = rpart.control(cp = 0)
)

drzewo_amount_przeuczone_1000 = rpart(
  formula = amount ~.,
  data = proba_uczaca_model_amount_1000,
  control = rpart.control(cp = 0)
)




drzewo_amount_przeuczone$cptable
drzewo_przeuczone$cptable
big.drzewo_reg_decyzyjne = rpart.rules(x = big.drzewo)
big.drzewo_reg_decyzyjne



# Ocenić dokładność modelu


przycinanie <- function(drzewo) {
  bledy <- drzewo$cptable
  tmp1 <- which.min(bledy[, "xerror"])  # min b��d w sprawdzaniu krzy�owym
  tmp2 <- sum(bledy[tmp1, c("xerror", "xstd")]) # min b��d + odchylenie standardowe
  optymalny <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa
  
  prune(drzewo, cp = bledy[optymalny, "CP"]) # przyci�cie drzewa
}

drzewo_przyciete <- przycinanie(drzewo_amount_przeuczone)
drzewo_przyciete_1500 <- przycinanie(drzewo_amount_przeuczone_1500)
drzewo_przyciete_1000 <- przycinanie(drzewo_amount_przeuczone_1000)

#Sprzawdzenie predykcji
predykcje <- predict(drzewo_przyciete)
predykcje_1500 <- predict(drzewo_przyciete_1500)
predykcje_1000 <- predict(drzewo_przyciete_1000)
#�rednia kwadratowa b��du
mean((proba_uczaca_model_amount$amount - predykcje)^2)
mean((proba_uczaca_model_amount_1500$amount - predykcje_1500)^2)
mean((proba_uczaca_model_amount_1000$amount - predykcje_1000)^2)
#�redni b��d

mean((proba_uczaca_model_amount$amount - predykcje)^2)^0.5
mean((proba_uczaca_model_amount_1500$amount - predykcje_1500)^2)^0.5
mean((proba_uczaca_model_amount_1000$amount - predykcje_1000)^2)^0.5
#B��d to 1400 z�

plot(x  =proba_uczaca_model_amount$amount, y = predykcje)
plot(x  =proba_uczaca_model_amount_1500$amount, y = predykcje_1500)
plot(x  =proba_uczaca_model_amount_1000$amount, y = predykcje_1000)



rpart.plot(drzewo_przyciete_1500,
           branch.type = 5)


rpart.rules(drzewo_przyciete_1500, style = "tallw")

#Decyzja - pozostajmy na poziomie 1500 wartości
# Ocenić dokładność modelu



###MARS MODEL

# MARS                                                                        #
#*****************************************************************************#

#### Przykład wprowadzający ####
# Wygenerować wartości dla dwóch zależnych od siebie zmiennych losowych
#proba_testowa_model_amount
#proba_uczaca_model_amount

#Sprawdzenie braków danych
cbind(colSums((is.na(proba_uczaca_model_amount)/length(proba_uczaca_model_amount))*100))
cbind(colSums((is.na(proba_testowa_model_amount)/length(proba_testowa_model_amount)))*100)

#Usunięcie zmiennych description ze względu na braki danych które nie mogą występować w przypadku modeli MARS
proba_uczaca_model_MARS <- subset(proba_uczaca_model_amount, select = -c(init_description, description))
proba_testowa_model_MARS <- subset(proba_testowa_model_amount, select = -c(init_description, description))

head(proba_testowa_model_amount)

#szacowanie modelu MARS
mars1 <- earth(amount ~.,
               data = proba_uczaca_model_MARS)
summary(mars1)

# Dokonać przeglądu wyników modelu 
X <- mars1$bx # macierz faktycznych predyktorów (w finalnym modelu)
X
punkty <- mars1$cuts
punkty

mars1$selected.terms
przycinanie <- mars1$prune.terms
przycinanie
plot(mars1$rss.per.subset)
plot(mars1$gcv.per.subset)

# Zbudować model MARS pozwalając na interakcje
mars2 <- earth(amount ~ .,
               data = proba_uczaca_model_MARS,
               degree = 3)
summary(mars2)

earth(formula = amount ~ .,
      data = proba_uczaca_model_MARS,
      degree = 3,
      trace = 3) # drukuje szczegóły przebiegu fazy I i II

# Ustalić optymalne wartości parametrów ('nk', 'minspan', 'thresh')
        # 'nk' - max liczba funkcji bazowych w pierwszej fazie
        # 'minspan' - min liczba obserwacji pomiędzy węzłami (dla minspan=1 węzły dla każdej obserwacji)
        # 'thresh' - min zwiększenie R2

gcv.nk <- sapply(seq(70, 190, 30), function(x)
        earth(amount ~ .,
              data = proba_uczaca_model_MARS,
              degree = 3,
              nk = x)$gcv) 
gcv.nk
 # nk = 130
gcv.thresh <- sapply(0.1^(1:3), function(x)
        earth(formula = amount ~ .,
              data = proba_uczaca_model_MARS,
              degree = 3,
              nk = 130,
              thresh = x)$gcv)  

# thresh = 0.001
gcv.minspan <- sapply(seq(1,8,2), function(x)
        earth(formula = amount ~ .,
              data = proba_uczaca_model_MARS,
              degree = 3,
              nk = 130,
              thresh = 0.001,
              minspan = x)$gcv)
gcv.minspan
 # minspan = 4
mars3 <- earth(formula = amount ~ .,
               data = proba_uczaca_model_MARS,
               degree = 3,
               nk = 100,
               thresh = 0.001,
               minspan = 4)

summary(mars3)

# Sporządzić wykres diagnostyczny modelu
plot(mars3, info = T)

# Sprawdzić jaka byłaby optymalna liczba składników (terms) na podstawie sprawdzania krzyżowego
mars4 <- earth(formula = amount ~ .,
               data = proba_uczaca_model_MARS,
               degree = 3,
               nk = 130,
               thresh = 0.001,
               minspan = 4,
               pmethod = "cv", # sprawdzanie krzyżowe
               nfold = 10,  # liczba części w sprawdzaniu krzyżowym
               ncross = 1, # ile razy wykonać cv (większa wartość = stabilniejsze wyniki)
               keepxy = T) # zachowanie danych x i y, żeby na wykresach diagnostycznych było cv

plot(mars4, which = 1)

summary(mars4)

# Sporządzić wykres przedstawiający wpływ poszczególnych zmiennych niezależnych na zmienną zależną
plotmo(mars3)
plotmo(mars4)
plotmo(mars1)

plotmo(mars1, 
       degree1 = "metraz", 
       pt.col = "grey",  # kolor punktu
       jitter = 0,       # bez "potrząsania" danymi
       ylim = c(5, 25),
       xlim = c(0, 200),
       grid.col = T,     # linie siatki
       npoints = T)      # wszystkie punkty

plotmo(mars4,
       degree2 = c("metraz", "l_pieter"),
       degree1 = F)

# Oszacować ważność zmiennych objaśniających
evimp(mars4)  # jest to ważność w modelu (nie ważność w ogóle dla tego problemu regresji)
