load(file = "C:/Users/Marcel/Desktop/Studia/Semestr III/modele nieparametryczne/projekt/dane_zaliczenie.RData")


# Realizacja zadań powinna obejmować:

# Wstępną eksplorację danych 
# Wzbogacanie danych (utworzenie dodatkowych zmiennych, np. dzień tygodnia na podstawie createtime, przypisanie cech transakcji inicjalizującej do transakcji rekurencyjnej)
# Budowanie modelu
# Prezentację i interpretację wyników modelu
# Krytyczną ocenę uzyskanych modeli
# Predykcję modelowanych zmiennych na danych testowych (predykcje należy zapisać w odpowiednich kolumnach obiektu predykcje_testowa)
library(rpart)
library(rpart.plot)
library(dplyr)
install.packages("rpart.plot")
library(ggplot2)
library(stringr)
library(like)
install.packages("data.table")
library("data.table")


#Względna liczba braków rekordów danych
cbind(colSums((is.na(proba_testowa)/15185)*100))
cbind(colSums((is.na(proba_uczaca)/29412)*100))


View(proba_uczaca)
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
table(proba_uczaca$issuer)
ggplot(proba_uczaca, aes(x = issuer)) +
  geom_bar()

#Przewaga liczby transkacji kartą debetową.
table(proba_uczaca$type)
ggplot(proba_uczaca, aes(x = type)) +
  geom_bar()


#Ujednolicenie danych ze względu na różną pisownie w opisie transakcji
proba_uczaca["description"][proba_uczaca["description"] == "PLAY - FAKTURA"] <- "PLAY - faktura"
proba_uczaca["description"][proba_uczaca["description"] == "RedBull Mobile - FAKTURA"] = 'RedBull Mobile - faktura'
proba_uczaca["description"][proba_uczaca["description"] == "Szybkie sprawdzanie firmy"] = 'Szybkie sprawdzanie'
proba_uczaca_merged["mccname"][proba_uczaca_merged["mccname"] == "Sklepy kosmetyczne"] = 'Usługi biznesowe gdzie indziej nie sklasyfikowane'


#Podział tablicy danych na tablice z transakcjami inicującycmi i rekurencyjnymi.
#Następnie do rzędów transakcji rekurencyjnych dodane są dane o transakcji inicjujących 
proba_uczaca_recurring <- subset(proba_uczaca, recurringaction =="AUTO")
proba_uczaca_initial <- subset(proba_uczaca, recurringaction !="AUTO")

#Zmiana nazw kolumn w celu identyfikacji column initial po dodaniu do tablicy danych recurring
names(proba_uczaca_initial)<- paste0('init', "_",names(proba_uczaca_initial))
colnames(proba_uczaca_initial)


proba_uczaca_merged <- merge(y = proba_uczaca_initial, x = proba_uczaca_recurring, by.x = 'initialtransaction_id',
all.x = TRUE, by.y = 'init_id')

View(proba_uczaca_merged)
ggplot(proba_uczaca_merged, aes(x = acquirerconnectionmethod)) +
  geom_bar()

#Nie ma potrzeba usuwania braków danych (NA) ze względu na umiejętność omijania tych danych przez modele nieparametryczne (drzewa decyzyjne, lasy losowe)

table(proba_uczaca_merged$mccname)

#Do wyciągnięcia: pełna data, godzina, dzień tygodnia ze względu na możliwość różnicy w liczbie transkacji wykonywanych w danym momencie, co może miec wpływ na pomyślne wykonanie
#Wyciagnięcie daty z createtime

proba_uczaca_merged$recurring_createdate <- as.Date(proba_uczaca_merged$createtime,format="%Y-%m-%d")
proba_uczaca_merged$init_createdate <- as.Date(proba_uczaca_merged$createtime,format="%Y-%m-%d")


#Wyciąganie dnia tygodnia

proba_uczaca_merged$recurring_weekday <- weekdays(as.Date(proba_uczaca_merged$createtime))
proba_uczaca_merged$init_weekday <- weekdays(as.Date(proba_uczaca_merged$init_createtime))

#Wyciagnięcie godziny z createtime

proba_uczaca_merged$recurring_hour <- format(proba_uczaca_merged$createtime,format="%H:00")
proba_uczaca_merged$init_hour <- format(proba_uczaca_merged$init_createtime,format="%H:00")




head(proba_uczaca_merged)
table(proba_uczaca_merged$init_screenheight)
table(proba_uczaca_merged$init_screenwidth)
table(proba_uczaca_merged$init_browseragent)

ggplot(proba_uczaca_merged, aes(x =init_screenwidth,y=init_screenheight)) +
  geom_dotplot()

#Zamiana jednostek wysokości i szerekości do komputera bądź telefonu

plot(proba_uczaca_merged$init_screenheight,proba_uczaca_merged$init_screenwidth)
proba_uczaca_merged[proba_uczaca_merged$init_screenwidth == NAN] <- NULL
proba_uczaca_merged["device"] <- NA
proba_uczaca_merged["device"][proba_uczaca_merged["init_screenwidth"]<proba_uczaca_merged["init_screenheight"]] <- "mobilephone"
proba_uczaca_merged["device"][proba_uczaca_merged["init_screenwidth"]>proba_uczaca_merged["init_screenheight"]] <- "computer"

#Barplot dla device

ggplot((proba_uczaca_merged), aes(x = device)) +
  geom_bar()

string <- "list"
string
string
substring(string,2)

proba_uczaca_merged[c('browser', 'other_software_info')] <- str_split_fixed(proba_uczaca_merged$init_browseragent, ' ', 2)
proba_uczaca_merged %>% mutate_if(is.character, list(~na_if(init_screenheight,"")))
head(proba_uczaca_merged)

proba_uczaca_merged[c('device_type', 'other_software_info')] <- substring(str_split_fixed(proba_uczaca_merged$other_software_info, ';', 2),2)

if (like(proba, pattern, ignore.case = FALSE, fixed = TRUE)) {
   selected
}

#Podmiana wartości

proba_uczaca_merged$device_type[grepl('Windows NT 6.1',proba_uczaca_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 6.1'

proba_uczaca_merged$device_type[grepl('Playstation',proba_uczaca_merged$device_type,ignore.case = TRUE)] <- 'Playstation 4'

proba_uczaca_merged$device_type[grepl('Windows NT 10.0',proba_uczaca_merged$device_type,ignore.case = TRUE)] <- 'Windows NT 10.0'

table(proba_uczaca_merged$device_type)




##### USUWANIE ZMIENNYCH
#proba_uczaca_merged <- subset(proba_uczaca_merged, select = -c(createtime, init_createtime,browser_agent,init_browseragent,init_screenheight,init_screenwidth,screenwidth,screenheight,other_software_info))