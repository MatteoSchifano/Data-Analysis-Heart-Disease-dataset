### Esame 
## Schifano, Ke, Cortinovis
#---------------------------------------------------------------------
## 1.Caricare il dataset heart.csv e analizzarne dettagliatamente la struttura.

# carico il dataset nella variabile df

f <- file.choose()
df <- read.csv(f, header = TRUE, stringsAsFactors = TRUE)

## controllo la struttura di df

#visualizzo il dataframe
View(df)

# visualizziamo i primi e gli ultimi 7 records
head(df, n = 7)
tail(df, n = 7)

# restituisce il numero di righe e di colonne
dim(df)

# restituisce il nome delle colonne, le nostre features 
names(df)

# visualizziamo la struttura di df
str(df)

summary(df)
#---------------------------------------------------------------------
## 2.Trasformare i dati in modo che siano TECNICAMENTE CORRETTI:

# 2.1. controllo i dati NA, in caso ci fossero li elimino
library(tidyverse)


# conto il numero di NA per ogni colonna
apply(is.na(df), 2, sum)

# salvo in df_pulito i record senza valori NA per operare su un df consistente
df_pulito <- df %>%
  drop_na()

# ri-conto il numero di NA per ogni colonna
apply(is.na(df_pulito), 2, sum)

# 2.2. le colonne dovranno essere del tipo corretto

df_pulito$x <- as.numeric(df_pulito$x)                  # id paziente
df_pulito$age <- as.numeric(df_pulito$age)              # anni del paziente
# sesso paziente 
# -- Value 1: maschio 
# -- Value 2: femmina
df_pulito$cp <- as.factor(df_pulito$cp)                 # tipologia di dolore al petto: 
# -- Value 1: typical angina == infarto 
# -- Value 2: atypical angina == rischi infarto
# -- Value 3: non-anginal pain == dolore generico
# -- Value 4: asymptomatic == asintomatico             
df_pulito$trestbps <- as.numeric(df_pulito$trestbps)    # pressione sanquigna a riposo in mm Hg
df_pulito$fbs <- as.factor(df_pulito$fbs)              # (insulina > 120 mg/dl) (1 = true; 0 = false)
df_pulito$restecg <- as.factor(df_pulito$restecg)       # ecg a riposo (elettrocardiogramma)
# -- Value 0: normal == normale
# -- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) == anormale
# -- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria == ipertrofia ventricolare sx
df_pulito$thalach <- as.numeric(df_pulito$thalach)      # massima frequenza cardiaca archiviata
df_pulito$exang <- as.factor(df_pulito$exang)           # esercizi per il dolore al petto (1 = yes; 0 = no)
# oldpeak     # Depressione ST indotta dall'esercizio rispetto al riposo
df_pulito$slope <- as.factor(df_pulito$slope)           # la pendenza del segmento ST di picco di esercizio
# -- Value 1: upsloping == salita
# -- Value 2: flat == piatto
# -- Value 3: downsloping == discesa
df_pulito$ca <- as.numeric(df_pulito$ca)                # numero di vasi principali (0-3) colorati da fluoroscopia
df_pulito$chol <- as.numeric(df_pulito$chol)            # tasso di colesterolo in mg/dl
df_pulito$thal <- as.factor(df_pulito$thal)             # 3 = normale; 6 = difetto fisso; 7 = difetto reversibile
df_pulito$target <- as.factor(df_pulito$target)        # obiettivo da predirre


# 2.3. Rinomino le colonne in modo che siano esplicative
names(df_pulito)[names(df_pulito) == "x"] <- "ID"                        # nominale
names(df_pulito)[names(df_pulito) == "age"] <- "eta"                     # intervallo
names(df_pulito)[names(df_pulito) == "cp"] <- "dolore_petto"             # nominale o ordinale
names(df_pulito)[names(df_pulito) == "trestbps"] <- "pres_sangue_riposo" # intervallo                  
names(df_pulito)[names(df_pulito) == "sex"] <- "sesso"                   # nominale
names(df_pulito)[names(df_pulito) == "fbs"] <- "insulina"                # 
names(df_pulito)[names(df_pulito) == "restecg"] <- "ECG_riposo"          # nominale
names(df_pulito)[names(df_pulito) == "thalach"] <- "freq_cardiaca_max"   # intervallo
names(df_pulito)[names(df_pulito) == "chol"] <- "colesterolo"            # intervallo
names(df_pulito)[names(df_pulito) == "exang"] <- "esercizi"              # nominale
names(df_pulito)[names(df_pulito) == "oldpeak"] <- "vecchi_picchi"       # raporto ???
names(df_pulito)[names(df_pulito) == "slope"] <- "inclinazione_grafico"  # nominale
names(df_pulito)[names(df_pulito) == "ca"] <- "vasi_sang_colorati"       # ordinale
names(df_pulito)[names(df_pulito) == "thal"] <- "difetto"                # 
names(df_pulito)[names(df_pulito) == "target"] <- "obiettivo"            # nominale

# 2.4. Rinominare e ordinare i livelli dei fattori in maniera appropriata

levels(df_pulito$sesso)
levels(df_pulito$sesso)[3] <- NA
apply(is.na(df_pulito), 2, sum)
df_pulito <- df_pulito %>%
  drop_na()
levels(df_pulito$sesso) <- c('femmina', 'maschio')
levels(df_pulito$sesso)
# il terzo livello della variabile sesso si decide di eliminarlo poiche 
# nella descrizione del dataset non è presente il sesso non specificato e 
# per quanto riguarda un'ospedale vieni classificato o maschio o femmina 
# per i vari trattamenti

levels(df_pulito$esercizi)
levels(df_pulito$esercizi) <- c('no', 'si')

levels(df_pulito$insulina)
levels(df_pulito$insulina) <- c('falso', 'vero')

levels(df_pulito$dolore_petto)
levels(df_pulito$dolore_petto) <- c('infarto', 'rischio infarto', 'dolore generico', 'asintomatico')

levels(df_pulito$ECG_riposo)
levels(df_pulito$ECG_riposo) <- c('normale', 'anormale', 'ipertrofia ventr. sx')

levels(df_pulito$inclinazione_grafico)
levels(df_pulito$inclinazione_grafico) <- c('salita', 'piatto', 'discesa')

levels(df_pulito$obiettivo)
levels(df_pulito$obiettivo) <- c('0', '1')

summary(df_pulito)

#---------------------------------------------------------------------
## Trasformare i dati in modo che siano CONSISTENTI:

# 1) la frequenza cardiaca superiore a 222 e inferiore e inferiore a 0 verra' 
# rimpiazzata dalla media
library(ggplot2)

#---------------------------------------------------------------------
# CREAZIONE FUINZIONI 
#---------------------------------------------------------------------


vaI <- function(x){
  val <- quantile(x)[2] - 1.5 * (quantile(x)[4] - quantile(x)[2])
  return(val)
}

vaS <- function(x){
  val <- quantile(x)[2] + 1.5 * (quantile(x)[4] - quantile(x)[2])
  return(val)
}

df_senza_outlier <- function(df_pulito, x){
  df <-
    df_pulito[x >= vaI(x) &
                x <= vaS(x), ]
  return(df)
}

boxplotX <- function(df, y){
  
  grafico <- ggplot(df %>% drop_na(), aes(x = '', y = y)) +
    geom_boxplot(
      color="blue",
      fill="slateblue",
      alpha=0.2,
      
      # custom outliers
      outlier.colour="red",
      outlier.fill="red",
      outlier.size=3
    )
  
  return (grafico)
}


#---------------------------------------------------------------------
# ANALISI DEI VALORI DI ETA
#---------------------------------------------------------------------

print(boxplotX(df_pulito, df_pulito$eta) +
        ggtitle('Distribuzione eta') +
        ylab('eta'))


eta_valido <- replace(df_pulito$eta, 
                      df_pulito$eta <= 1 | df_pulito$eta > 122, 
                      mean(df_pulito$freq_cardiaca > 1 & df_pulito$freq_cardiaca <= 122))

print(boxplotX(df_pulito, eta_valido) +
        ggtitle('Distribuzione eta') +
        ylab('eta'))

# viene rimosso il dato insonsistente negativo, l'eta non puo essere 
# negativa


#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA PRESSIONE SANGUIGNA
#---------------------------------------------------------------------


print(boxplotX(df_pulito, df_pulito$pres_sangue_riposo) +
        ggtitle('Distribuzione pressione sanguigna a riposo') +
        ylab('pressione sanguigna'))

n_valido <- replace(df_pulito$pres_sangue_riposo, 
                    df_pulito$pres_sangue_riposo <= 0 | df_pulito$pres_sangue_riposo > 230, 
                    mean(df_pulito$pres_sangue_riposo > 0 & df_pulito$pres_sangue_riposo <= 230))


print(boxplotX(df_pulito, n_valido)+
        ggtitle('Distribuzione pressione sanguigna a riposo') +
        ylab('pressione sanguigna'))

# non erano presenti valori inconsistenti, pertanto i due grafici risultano 
# uguali


#---------------------------------------------------------------------
# ANALISI DEI VALORI DI FREQUENZA CARDIACA MAX
#---------------------------------------------------------------------

print(boxplotX(df_pulito, df_pulito$freq_cardiaca_max) +
        ggtitle('Distribuzione frequenza cardiaca massima registrata') +
        ylab('frequenza cardiaca max'))


n_valido2 <- replace(
  df_pulito$freq_cardiaca_max, 
  df_pulito$freq_cardiaca_max <= 1 | df_pulito$freq_cardiaca_max > 222, 
  mean(df_pulito$freq_cardiaca_max[df_pulito$freq_cardiaca_max > 1 &
                                     df_pulito$freq_cardiaca_max < 222])
)

print(boxplotX(df_pulito, n_valido2) +
        ggtitle('Distribuzione frequenza cardiaca massima registrata') +
        ylab('frequenza cardiaca max')) # da aggiustare o chiedere al prof

# i dati non vengono visulizzati se sono inconsistenti, come valori superiori 
# a 222 


#---------------------------------------------------------------------
# ANALISI DEI VALORI DEL COLESTEROLO
#---------------------------------------------------------------------

print(boxplotX(df_pulito, df_pulito$colesterolo) +
        ggtitle('Distribuzione tasso di colesterolo') +
        ylab('colesterolo in mg/dl'))

# non riporta valori di outlier o valori inconsistenti

#---------------------------------------------------------------------
# RIMOZIONE DEGLI OUTLIER
#---------------------------------------------------------------------

print(boxplotX(df_senza_outlier(df_pulito, df_pulito$pres_sangue_riposo),
               df_senza_outlier(df_pulito, df_pulito$pres_sangue_riposo)$pres_sangue_riposo) +
        ggtitle('Distribuzione pressione sanguigna a riposo senza outlier') +
        ylab('pressione sanguigna a riposo'))

dim(df_pulito)
df_pulito_so <- df_senza_outlier(df_pulito, df_pulito$pres_sangue_riposo)
dim(df_pulito_so)

print(boxplotX(df_senza_outlier(df_pulito_so, df_pulito_so$freq_cardiaca_max),
               df_senza_outlier(df_pulito_so, df_pulito_so$freq_cardiaca_max)$freq_cardiaca_max) +
        ggtitle('Distribuzione frequenza cardiaca massima registrata senza outlier') +
        ylab('frequenza cardiaca max'))

df_pulito_so <- df_senza_outlier(df_pulito_so, df_pulito_so$freq_cardiaca_max)
dim(df_pulito_so)

print(boxplotX(df_senza_outlier(df_pulito_so, df_pulito_so$eta), 
               df_senza_outlier(df_pulito_so, df_pulito_so$eta)$eta) +
        ggtitle('Distribuzione eta senza outlier') +
        ylab('eta'))

df_pulito_so <- df_senza_outlier(df_pulito_so, df_pulito_so$eta)
dim(df_pulito_so)

# forse da togliere
print(boxplotX(df_senza_outlier(df_pulito_so, df_pulito_so$colesterolo),
               df_senza_outlier(df_pulito_so, df_pulito_so$colesterolo)$colesterolo) +
        ggtitle('Distribuzione tasso di colesterolo') +
        ylab('colesterolo in mg/dl'))

# visualizzazione dei grafici analizzati in precedenza senza outlier

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE DIFETTO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(difetto)) + 
    geom_bar()
)

levels(df_pulito_so$difetto)

# il numero di livelli di difetto e errato, da descrizione sono solamente 3,
# pertanto rimuovo il livello errato, il primo con meno elementi

df_pulito_so$difetto <- as.numeric(df_pulito_so$difetto)
df_pulito_so$difetto[df_pulito_so$difetto == 1] <- NA 
df_pulito_so <- df_pulito_so %>% 
  drop_na()
df_pulito_so$difetto <- as.factor(df_pulito_so$difetto)
levels(df_pulito_so$difetto) <- c('normale', 'difetto fisso', 'difetto reversibile')


print(
  ggplot(df_pulito_so, aes(difetto)) + 
    geom_bar()
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE DOLORE PETTO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(dolore_petto)) + 
    geom_bar()
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE INSULINA
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(insulina)) + 
    geom_bar()
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE ESERCIZI
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(esercizi)) + 
    geom_bar()
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE ECG RIPOSO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(ECG_riposo)) + 
    geom_bar()
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE INCLINAZIONE GRAFICO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(inclinazione_grafico)) + 
    geom_bar()
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE SESSO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(sesso)) + 
    geom_bar()
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE OBIETTIVO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(obiettivo)) + 
    geom_bar()
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE VASI COLORATI
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(vasi_sang_colorati)) + 
    geom_bar()
)

# dato che i vasi sanguigni colorati possono essere solamente 3 i valori superiori
# vengono eliminati

df_pulito_so$vasi_sang_colorati[df_pulito_so$vasi_sang_colorati == 4] <- NA
df_pulito_so <- df_pulito_so %>% 
  drop_na()

print(
  ggplot(df_pulito_so, aes(vasi_sang_colorati)) + 
    geom_bar()
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE VECCHI PICCHI
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(vecchi_picchi)) + 
    geom_bar()
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI ID
#---------------------------------------------------------------------
# controllo se ci sono duplicati 
table(df_pulito_so$ID)[table(df_pulito_so$ID) > 1]
# sostituiamo i valori in modo tale da avere tutti casi distinti e ordinati
df_pulito_so$ID <- 1:dim(df_pulito_so)[1]
df_pulito_so$ID <- as.numeric(df_pulito_so$ID)

#---------------------------------------------------------------------
# ANALISI DESCRITTIVA
#---------------------------------------------------------------------

## Distribuzione del sesso tramite un diagramma a torta
sex <- ggplot(df_pulito_so,  aes(x = "", y = sesso, fill = sesso)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette="Blues") +
  ggtitle('Distribuzione sesso') 
print(sex)  

# oltre il 75% delle persone esaminate sono maschi,  
#---------------------------------------------------------------------

## Distribuzione delle eta in base al sesso tramite boxplot
eta_sex <- ggplot(df_pulito_so, aes(sesso, eta)) + 
  geom_boxplot(
    color="blue",
    fill="slateblue",
    alpha=0.2
  ) +
  scale_fill_brewer(palette="Blues") +
  ggtitle('Distribuzione eta per sesso') 

print(eta_sex)

# notiamo come mediamente i pazienti maschili siano piu giovani delle donne
# notiamo come lo scarto interquartile delle donne e > di quello maschile
# VAS e VAI femminili si discostano di meno dallo scarto interquartile rispetto 
# alle registrazioni prese sui maschi
#---------------------------------------------------------------------

## distribuzione dei pazienti in base al sesso, eta e al tipo di ECG

ecg_eta_sex <- ggplot(df_pulito_so, aes(ECG_riposo, eta, col = sesso)) + 
  geom_boxplot(
    alpha=0.2
  ) +
  ggtitle('Distribuzione eta per esito ECG e sesso') +
  facet_grid(vars(sesso))

print(ecg_eta_sex)

# l'eta femminile con un ECG a riposo normale si concentra tra i 50 e i 
# 60 anni con pochi, la donna piu giovane ha un ECG anormale. lo scarto interquantile
# femminile di chi registra un ECG anormale e elevato. l'unica donna
# che soffre di ipretrofia ventricolare sx ha 55 anni circa. 
# per quanto riguarda gli uomini rileviamo scarti interquantili pittosto ristretti
# e si aggirano sempre sui 50 - 60 anni per chi ha un ECG normale con molti casi
# che rientrano al di sotto del Q2. possiamo notare che chi registra un ECG anormale 
# ha una media di eta minore di chi registra un ECG normale. l'unico caso maschile
# di ipertrofia registra circa 58 anni.
#---------------------------------------------------------------------

## distribuzione delle modalita di dolore al petto per il sesso 

sex_dolore <- ggplot(df_pulito_so, aes(dolore_petto, fill = sesso)) +
  geom_bar() +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione dolore al petto per sesso') 

print(sex_dolore)

# si nota come per le donne soffrano prevalentemente di infarto e dolore
# generico mentre il numero di uomini che registrano un infarto superano 
# di molto il numero di uomini che riportano le altre tipologie di dolore

#---------------------------------------------------------------------

## distribuzione delle modalita di esercizi per il sesso 

sex_esercizi <- ggplot(df_pulito_so, aes(esercizi, fill = sesso)) +
  geom_bar() +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione esercizi per sesso') 

print(sex_esercizi)
# dal grafico comprendiamo che le donne sono meno propense a svolgere degli 
# esercizi per il dolore al petto, gli uomini, nonostante registrino piu non
# svolge l'esercizio c'e anche una gran quantita che svolge gli esercizi

#---------------------------------------------------------------------

## distribuzione dolore petto per esercizi

dolore_esercizi <- ggplot(df_pulito_so, aes(dolore_petto, fill = dolore_petto)) +
  geom_bar() +
  facet_grid(vars(esercizi)) +
  ggtitle('Dolore petto curato con esercizi') 

print(dolore_esercizi)

# comprendiamo che chi svolge esercizi e prevalentemente chi ha registrato un
# infarto o soffre di un dolore generico. si notano anche tanti infarti e rischi
# infarto che non vengono curati con esercizi

#---------------------------------------------------------------------

##

vasi_eta_sex <- ggplot(df_pulito_so, aes(eta, fill = sesso)) +
  geom_bar() +
  facet_grid(vars(vasi_sang_colorati)) +
  # da aggiungere la legenda dei vasi sanguigni colorati
  ggtitle('Distribuzione eta per numero di vasi sanguigni colorati') 

print(vasi_eta_sex)

# si nota come le persone che hanno 3 vasi sanguigni colorati siano in netta
# minoranza, siano prevalentemente maschi e che abbiano una soglia di eta 
# minima registrata elevata. 
# si nota come chi registra 0 vasi sanguigni colorati siano in netta
# maggioranza, siano prevalentemente maschi anche perche sono in maggioranza
# e un'eta ben distribuita intorno ai valori che si presentano con frequenza
# maggiore
#---------------------------------------------------------------------

##  DA RIVEDERE

n_classi <- 4
differenza <- max(df_pulito_so$eta) - min(df_pulito_so$eta)
elementiXclasse <- differenza/n_classi

eta_col <- ggplot(df_pulito_so, aes(x = '', colesterolo)) +
  geom_boxplot(data = seq(min(df_pulito_so$eta), max(df_pulito_so$eta), elementiXclasse))
print(eta_col)
#
#
#---------------------------------------------------------------------

## distribuzione colesterolo per pressione sanguigna a riposo
pres_col <- ggplot(df_pulito_so, aes(colesterolo, pres_sangue_riposo)) +
  geom_point() +
  labs(
    x = 'colesterolo mg/dl',
    y = 'pressione sangue a riposo mmHg'
  ) +
  ggtitle('Distribuzione colesterolo per pressione sanguigna a riposo') 


print(pres_col)
# i dati si distribuiscono quasi uniformemente nello spazio, con una lieve tendenza
# a superare i 120 mmHg per i vari valori del colesterolo
#---------------------------------------------------------------------
## distribuzione colesterolo per pressione sanguigna a riposo
pres_col_sex <- ggplot(df_pulito_so, aes(colesterolo, pres_sangue_riposo, col = sesso)) +
  geom_point() +
  labs(
    x = 'colesterolo mg/dl',
    y = 'pressione sangue a riposo mmHg'
  ) +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione colesterolo per pres. sanguigna a riposo diviso per sesso') 


print(pres_col_sex)
# possiamo notare come i valori registrati per le donne potrebbero essere direttamente
# proporzionali, al crescere della x cresce anche la y,
# per quanto riguarda i maschi la distribuzione è quasi uniforme nello spazio

#---------------------------------------------------------------------
## distribuzione eta per pressione sanguigna a riposo
pres_eta <- ggplot(df_pulito_so, aes(eta, pres_sangue_riposo)) +
  geom_point() +
  labs(
    x = 'eta',
    y = 'pressione sangue a riposo mmHg'
  ) +
  ggtitle('Distribuzione eta per pres. sanguigna a riposo') 


print(pres_eta)
#
#
#
#---------------------------------------------------------------------
## distribuzione eta per frequenza cardiaca massima
fcm_eta <- ggplot(df_pulito_so, aes(eta, freq_cardiaca_max)) +
  geom_point() +
  labs(
    x = 'eta',
    y = 'frequenza cardiaca massima'
  ) +
  ggtitle('Distribuzione eta per pres. sanguigna a riposo') 


print(fcm_eta)
#
#
#
#---------------------------------------------------------------------
## distribuzione colesterolo per frequenza cardiaca massima
col_fcm <- ggplot(df_pulito_so, aes(colesterolo, freq_cardiaca_max)) +
  geom_point() +
  labs(
    x = 'colesterolo mg/dl',
    y = 'frequenza cardiaca massima'
  ) +
  ggtitle('Distribuzione eta per pres. sanguigna a riposo') 


print(col_fcm)
#
#
#
#---------------------------------------------------------------------
# ANALISI RELAZIONE TRA PRESSIONE SANGUIGNA A RIPOSO E COLESTEROLO
#---------------------------------------------------------------------

# controllo se le variabili hanno una buona correlazione

cor(df_pulito_so$colesterolo, df_pulito_so$pres_sangue_riposo)
cor(df_pulito_so$eta, df_pulito_so$freq_cardiaca_max)
cor(df_pulito_so$colesterolo, df_pulito_so$freq_cardiaca_max)
cor(df_pulito_so$eta, df_pulito_so$pres_sangue_riposo)
cor(df_pulito_so$colesterolo, df_pulito_so$eta)

# la correlazione è bassa ~10% ma ricordiamo che anche una correlazione alta 
# non implica casualita, in questo caso avremo a che fare con una relazione con 
# un alta varianza 

# y = df_pulito_so$pres_sangue_riposo
# x = df_pulito_so$colesterolo

## REGRESSIONE LINEARE
reg <- lm(df_pulito_so$pres_sangue_riposo ~ df_pulito_so$colesterolo)

# grafico della regr. lineare 
plot(df_pulito_so$pres_sangue_riposo ~ df_pulito_so$colesterolo,
     ylab = 'pressione sangue a riposo (mmHg)', xlab = 'colesterolo (mg/dl)')
title(main = "Regr.lin tra pressione sangue a riposo e colesterolo")
abline (reg, col = "red")

# si aggiungono i segmenti
segments(df_pulito_so$colesterolo, fitted(reg), df_pulito_so$colesterolo, df_pulito_so$pres_sangue_riposo,
         col = "blue", lty = 2)

# pres_sangue_riposo = 35.9921 + 0.2894 * colesterolo
summary(reg)

# stima del modello utilizzando la formula I()
reg2 <- lm(df_pulito_so$pres_sangue_riposo ~ I(df_pulito_so$colesterolo - mean(df_pulito_so$colesterolo)))

summary(reg2)
# calcolo della r e R^2
r <- cor(df_pulito_so$colesterolo, df_pulito_so$pres_sangue_riposo)
r
r^2
# analisi dei residui
plot(reg$fitted, reg$residuals, main = "Residui")
abline(0, 0)
# il grafico conferma l'ipotesi di distribuzione casuale dei residui dato che 
# dai dati presenti nel grafico non rileviamo pattern e si distribuiscono 
# uniformemente sia sopra che sotto alla linea tracciata

# distribuzione in quantili confrontabile con quella di una normale
qqnorm(reg$residuals)
qqline(reg$residuals)
# in questo grafico notiamo come vi sia una distribuzione casuale dei
# residui, poiché i valori sono equidistribuiti intorno alla retta e si distribuiscono
# sia sopra che sotto


reg <- lm(pres_sangue_riposo ~ colesterolo, data = df_pulito_so)

# previsione sui dati non presenti nel df

predict(reg,
        newdata = data.frame("colesterolo" = c(105, 112, 12, 53, 145, 19, 41, 152, 160, 162)))

predict(reg,
        newdata = data.frame("colesterolo" = c(105, 112, 12, 53, 145, 19, 41, 152, 160, 162)),
        interval = "confidence")

#---------------------------------------------------------------------
# ANALISI CON ALGORITMO DEL GRADIENTE
#---------------------------------------------------------------------
mse <- function(a, b, x = df_pulito_so$colesterolo, y = df_pulito_so$pres_sangue_riposo) {
  # previsione del modello
  prediction <- a + b*x 
  # distantza tra predizione e osservazione
  residuals <- y - prediction 
  # eleviamo al ^2 per evitare la somma pari a zero 
  squared_residuals <- residuals^2 
  
  # media delle distanze quadrate
  ssr <- mean(squared_residuals)
  ssr
}
compute_gradient <- function(a, b, x = df_pulito_so$colesterolo, y = df_pulito_so$pres_sangue_riposo) {
  n <- length(y)
  predictions <- a + (b * x)
  residuals <- y - predictions
  
  da <- (1/n) * sum(-2*residuals)
  db <- (1/n) * sum(-2*x*residuals)
  
  c(da, db)
}
gd_step <- function(a, b, 
                    learning_rate = 0.1, 
                    x = df_pulito_so$colesterolo, 
                    y = df_pulito_so$pres_sangue_riposo) {
  grad <- compute_gradient(a, b, x, y)
  step_a <- grad[1] * learning_rate
  step_b <- grad[2] * learning_rate
  
  c(a - step_a, b - step_b)
}
estimate_gradient <- function(pars_tbl, learning_rate = 0.1, x = df_pulito_so$colesterolo, y = df_pulito_so$pres_sangue_riposo) {
  
  pars <- gd_step(pars_tbl[["a"]], pars_tbl[["b"]],
                  learning_rate)
  
  tibble(a = pars[1], b = pars[2], mse = mse(a, b, x, y))
}


# reg <- lm(df_pulito_so$pres_sangue_riposo ~ df_pulito_so$colesterolo)

Sys.time()
set.seed(2022)

dati <- tibble(x = df_pulito_so$colesterolo, y = df_pulito_so$pres_sangue_riposo)
dati

# se i due risultati sono uguali possiamo affermare che funziona la funzione
mse(a = coef(reg)[1], b = coef(reg)[2])
mean(resid(reg)^2)

# candidate values
grid <- expand.grid(a = seq(min(dati$x), max(dati$x), 1), 
                    b = seq(min(dati$y), max(dati$y), 1)) %>% 
  as_tibble()
grid
# candidate values
mse_grid <- grid %>% 
  rowwise(a, b) %>% 
  summarize(mse = mse(a, b), .groups = "drop")
mse_grid

mse_grid %>% 
  arrange(mse) %>% 
  slice(1)

coef(reg)

walk <- gd_step(0, 0)
walk

for(i in 1:25) {
  walk <- gd_step(walk[1], walk[2])
}
walk
# initialize
grad <- estimate_gradient(tibble(a = 0, b = 0))

# loop through
for(i in 2:50) {
  grad[i, ] <- estimate_gradient(grad[i - 1, ])
}
grad

grad <- grad %>% 
  rowid_to_column("iteration")

ggplot(grad, aes(iteration, mse)) +
  geom_line()

ggplot(dati, aes(x, y)) +
  geom_point() +
  geom_abline(aes(intercept = a, slope = b),
              data = grad,
              color = "gray60",
              size = 0.3) +
  geom_abline(aes(intercept = a, slope = b),
              data = grad[nrow(grad), ],
              color = "magenta")

# install.packages('gganimate')
library(gganimate)
anim <- ggplot(grad) +
  geom_point(aes(x, y), dati) +
  geom_smooth(aes(x, y), dati,
              method = "lm", se = FALSE) +
  geom_abline(aes(intercept = a,
                  slope = b),
              color = "#de4f60") +
  transition_manual(frames = iteration)
animate(anim)
#---------------------------------------------------------------------
# ANALISI CON ALGORITMO FORZA BRUTA
#---------------------------------------------------------------------





#---------------------------------------------------------------------
# APPLICAZIONE DI UN ALGORITMO DI MACHINE LEARNING
#---------------------------------------------------------------------
# carico la libreria per ML
# install.packages('caret')
library(caret)

# si impostano le variabili x e y
ml <- df_pulito_so

#---------------------------------------------------------------------
## Prova 1 tutti gli attributi eccetto ID

x <-  ml[, c(2:15)]
dim(x)
y <- ml[, 15]
dim(t(y))

set.seed(2022)
# create a matrix of 80% of the rows in the original dataset that we can use for training
training_index <- createDataPartition(y, p = .80, list = FALSE)
# select 80% of data to training the models
training_set <- x[training_index, ]
nrow(training_set)
# use the remaining 20% of the data for test
test_set <- x[-training_index, ]
nrow(test_set)


seed = set.seed(2022)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"


# linear algorithms
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")

## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")

## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn))
summary(results)



#---------------------------------------------------------------------
## Prova 2 solo valori numerici

x <-  ml[, c(2, 5, 6, 9, 11, 13, 15)]
dim(x)
y <- ml[, 15]
dim(t(y))

set.seed(2022)
# create a matrix of 80% of the rows in the original dataset that we can use for training
training_index <- createDataPartition(y, p = .80, list = FALSE)
# select 80% of data to training the models
training_set <- x[training_index, ]
nrow(training_set)
# use the remaining 20% of the data for test
test_set <- x[-training_index, ]
nrow(test_set)


seed = set.seed(2022)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"


# linear algorithms
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")

## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")

## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn))
summary(results)



#---------------------------------------------------------------------
## Prova 3 

x <- ml[, c(2, 3, 4, 5, 6, 7, 8, 10, 13, 15)]
dim(x)
y <- ml[, 15]
dim(t(y))

set.seed(2022)
# create a matrix of 80% of the rows in the original dataset that we can use for training
training_index <- createDataPartition(y, p = .80, list = FALSE)
# select 80% of data to training the models
training_set <- x[training_index, ]
nrow(training_set)
# use the remaining 20% of the data for test
test_set <- x[-training_index, ]
nrow(test_set)


seed = set.seed(2022)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"


# linear algorithms
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")

## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")

## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn))
summary(results)


#---------------------------------------------------------------------
## Prova 4 

x <- ml[, c(2, 3, 4, 5, 6, 7, 8, 10, 13, 15)]
dim(x)
y <- ml[, 15]
dim(t(y))

set.seed(2022)
# create a matrix of 80% of the rows in the original dataset that we can use for training
training_index <- createDataPartition(y, p = .80, list = FALSE)
# select 80% of data to training the models
training_set <- x[training_index, ]
nrow(training_set)
# use the remaining 20% of the data for test
test_set <- x[-training_index, ]
nrow(test_set)


seed = set.seed(2022)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"


# linear algorithms
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")

## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")

## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn))
summary(results)

heatmap(cor(df_pulito_so))
cor(as.numeric(df_pulito_so))
