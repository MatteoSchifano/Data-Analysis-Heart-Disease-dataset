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
df_pulito$fbs <- as.factor(df_pulito$fbs)               # (insulina > 120 mg/dl) (1 = true; 0 = false)
df_pulito$restecg <- as.factor(df_pulito$restecg)       # ecg a riposo (elettrocardiogramma)
# -- Value 0: normale
# -- Value 1: anormale
# -- Value 2: ipertrofia ventricolare sx
df_pulito$thalach <- as.numeric(df_pulito$thalach)      # massima frequenza cardiaca archiviata
df_pulito$exang <- as.factor(df_pulito$exang)           # esercizi per il dolore al petto (1 = yes; 0 = no)
# oldpeak                                               # Depressione ST indotta dall'esercizio rispetto al riposo
df_pulito$slope <- as.factor(df_pulito$slope)           # la pendenza del segmento ST di picco di esercizio
# -- Value 1: upsloping == salita
# -- Value 2: flat == piatto
# -- Value 3: downsloping == discesa
df_pulito$ca <- as.numeric(df_pulito$ca)                # numero di vasi principali (0-3) colorati da fluoroscopia
df_pulito$chol <- as.numeric(df_pulito$chol)            # tasso di colesterolo in mg/dl
df_pulito$thal <- as.factor(df_pulito$thal)             # 3 = normale; 6 = difetto fisso; 7 = difetto reversibile
df_pulito$target <- as.factor(df_pulito$target)         # obiettivo da predirre

# 2.3. Rinomino le colonne in modo che siano esplicative

names(df_pulito)[names(df_pulito) == "x"] <- "ID"                         # NOMINALE 
names(df_pulito)[names(df_pulito) == "age"] <- "eta"                      # INTERVALLO  
names(df_pulito)[names(df_pulito) == "cp"] <- "dolore_petto"              # ORDINALE 
names(df_pulito)[names(df_pulito) == "trestbps"] <- "press_sangue_riposo" # INTERVALLO                  
names(df_pulito)[names(df_pulito) == "sex"] <- "sesso"                    # NOMINALE
names(df_pulito)[names(df_pulito) == "fbs"] <- "insulina"                 # NOMINALE
names(df_pulito)[names(df_pulito) == "restecg"] <- "ECG_riposo"           # NOMINALE 
names(df_pulito)[names(df_pulito) == "thalach"] <- "freq_cardiaca_max"    # INTERVALLO
names(df_pulito)[names(df_pulito) == "chol"] <- "colesterolo"             # INTERVALLO
names(df_pulito)[names(df_pulito) == "exang"] <- "esercizi"               # NOMINALE
names(df_pulito)[names(df_pulito) == "oldpeak"] <- "vecchi_picchi"        # RAPPORTO
names(df_pulito)[names(df_pulito) == "slope"] <- "inclinazione_grafico"   # NOMINALE
names(df_pulito)[names(df_pulito) == "ca"] <- "vasi_sang_colorati"        # ORDINALE
names(df_pulito)[names(df_pulito) == "thal"] <- "difetto"                 # NOMINALE
names(df_pulito)[names(df_pulito) == "target"] <- "obiettivo"             # NOMINALE

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
# negli un ospedale si viene classificati come maschio o femmina 
# per i vari trattamenti

levels(df_pulito$esercizi)
levels(df_pulito$esercizi) <- c('no', 'si')

levels(df_pulito$insulina)
levels(df_pulito$insulina) <- c('no', 'si')

levels(df_pulito$dolore_petto)
levels(df_pulito$dolore_petto) <- c('infarto', 'rischio infarto', 'dolore generico', 'asintomatico')

levels(df_pulito$ECG_riposo)
levels(df_pulito$ECG_riposo) <- c('normale', 'anormale', 'ipertrofia ventr. sx')

levels(df_pulito$inclinazione_grafico)
levels(df_pulito$inclinazione_grafico) <- c('salita', 'piatto', 'discesa')

levels(df_pulito$obiettivo)
levels(df_pulito$obiettivo) <- c('sano', 'problema cardiaco')

levels(df_pulito_so$difetto)
# viene gestito successivamente nell'analisi della variabile

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
                      df_pulito$eta < 1 | df_pulito$eta > 122, 
                      mean(df_pulito$eta[df_pulito$eta >= 1 &
                                           df_pulito$eta <= 122]))

print(boxplotX(df_pulito, eta_valido) +
        ggtitle('Distribuzione eta') +
        ylab('eta'))

# viene rimosso il dato insonsistente negativo, l'eta non puo essere 
# negativa


#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA PRESSIONE SANGUIGNA
#---------------------------------------------------------------------


print(boxplotX(df_pulito, df_pulito$press_sangue_riposo) +
        ggtitle('Distribuzione pressione sanguigna a riposo') +
        ylab('pressione sanguigna'))

n_valido <- replace(df_pulito$press_sangue_riposo, 
                    df_pulito$press_sangue_riposo <= 0 | df_pulito$press_sangue_riposo > 230, 
                    mean(df_pulito$press_sangue_riposo[df_pulito$press_sangue_riposo >= 1 &
                                                         df_pulito$press_sangue_riposo <= 230]))


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
                                     df_pulito$freq_cardiaca_max < 222]))

print(boxplotX(df_pulito, n_valido2) +
        ggtitle('Distribuzione frequenza cardiaca massima registrata') +
        ylab('frequenza cardiaca max')) 

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

print(boxplotX(df_senza_outlier(df_pulito, df_pulito$press_sangue_riposo),
               df_senza_outlier(df_pulito, df_pulito$press_sangue_riposo)$press_sangue_riposo) +
        ggtitle('Distribuzione pressione sanguigna a riposo senza outlier') +
        ylab('pressione sanguigna a riposo'))

dim(df_pulito)
df_pulito_so <- df_senza_outlier(df_pulito, df_pulito$press_sangue_riposo)
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
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)

levels(df_pulito_so$difetto)

# il numero di livelli di difetto e errato: da descrizione sono solamente 3,
# pertanto rimuovo il livello errato, ossia il primo, quello con meno elementi

df_pulito_so$difetto <- as.numeric(df_pulito_so$difetto)
df_pulito_so$difetto[df_pulito_so$difetto == 1] <- NA 
df_pulito_so <- df_pulito_so %>% 
  drop_na()
df_pulito_so$difetto <- as.factor(df_pulito_so$difetto)
levels(df_pulito_so$difetto) <- c('normale', 'difetto fisso', 'difetto reversibile')


print(
  ggplot(df_pulito_so, aes(difetto)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE DOLORE PETTO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(dolore_petto)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE INSULINA
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(insulina)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE ESERCIZI
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(esercizi)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE ECG RIPOSO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(ECG_riposo)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE INCLINAZIONE GRAFICO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(inclinazione_grafico)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE SESSO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(sesso)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE OBIETTIVO
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(obiettivo)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8) 
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE VASI COLORATI
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(vasi_sang_colorati)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
)

# dato che i vasi sanguigni colorati possono essere solamente 3 i valori 
# superiori a 3 vengono eliminati

df_pulito_so$vasi_sang_colorati[df_pulito_so$vasi_sang_colorati == 4] <- NA
df_pulito_so <- df_pulito_so %>% 
  drop_na()

print(
  ggplot(df_pulito_so, aes(vasi_sang_colorati)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8) 
)

#---------------------------------------------------------------------
# ANALISI DEI VALORI DELLA VARIABILE VECCHI PICCHI
#---------------------------------------------------------------------

print(
  ggplot(df_pulito_so, aes(vecchi_picchi)) + 
    geom_bar(fill = '#ff8533', 
             col = '#4d1f00',
             alpha=0.8)
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

# dal grafico si può notare che oltre il 75% delle persone esaminate sono maschi,
# mentre le donne sono una minoranza
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
# lo scarto interquartile delle donne e maggiore di quello maschile
# VAS e VAI femminili si discostano di meno dallo scarto interquartile rispetto 
# alle registrazioni prese sui maschi
#---------------------------------------------------------------------

## distribuzione dei pazienti in base al sesso, eta e al tipo di ECG

ecg_eta_sex <- ggplot(df_pulito_so, aes(ECG_riposo, eta, fill = sesso)) + 
  geom_boxplot(
    alpha=0.8
  ) +
  ggtitle('Distribuzione eta per esito ECG e sesso') +
  facet_grid(vars(sesso))

print(ecg_eta_sex)

# l'eta media femminile con un ECG a riposo normale si concentra tra i 50 e i 
# 60 anni con pochi; la donna piu giovane ha un ECG anormale. Lo scarto interquantile
# femminile di chi registra un ECG anormale e elevato. l'unica donna
# che soffre di ipretrofia ventricolare sx ha 55 anni circa. 
# Per quanto riguarda gli uomini rileviamo scarti interquantili pittosto ristretti
# e la media si aggira sempre sui 50 - 60 anni per chi ha un ECG normale, con molti casi
# che rientrano al di sotto del Q2. Possiamo notare che chi registra un ECG anormale 
# ha una media di eta minore di chi registra un ECG normale. l'unico caso maschile
# di ipertrofia registra circa 58 anni.
#---------------------------------------------------------------------

## distribuzione delle modalita di dolore al petto per il sesso 

sex_dolore <- ggplot(df_pulito_so, aes(dolore_petto, fill = sesso)) +
  geom_bar(col = '#4d1f00') +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione dolore al petto per sesso') 

print(sex_dolore)

# si nota come per le donne soffrano prevalentemente di infarto e dolore
# generico mentre il numero di uomini che registrano un infarto supera
# di molto il numero di uomini che riportano le altre tipologie di dolore

#---------------------------------------------------------------------

## distribuzione delle modalita di esercizi per il sesso 

sex_esercizi <- ggplot(df_pulito_so, aes(esercizi, fill = sesso)) +
  geom_bar(col = '#4d1f00') +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione esercizi per sesso') 

print(sex_esercizi)
# dal grafico comprendiamo che i pazienti, sia donne che uomini,
# che devono svolgere esercizi sono in minoranza rispetto a chi
# non li deve svolgere

#---------------------------------------------------------------------

## distribuzione dolore petto per esercizi

dolore_esercizi <- ggplot(df_pulito_so, aes(dolore_petto, fill = dolore_petto)) +
  geom_bar(col = '#4d1f00') +
  facet_grid(vars(esercizi)) +
  ggtitle('Dolore petto curato con esercizi') 

print(dolore_esercizi)

# comprendiamo che chi svolge esercizi e prevalentemente chi ha registrato un
# infarto o soffre di un dolore generico. Si notano anche molti infarti e rischi
# infarto che non vengono curati con esercizi

#---------------------------------------------------------------------

## Distribuzione eta per numero di vasi sanguigni colorati

vasi_eta_sex <- ggplot(df_pulito_so, aes(eta, fill = sesso)) +
  geom_bar(alpha = 0.8) +
  facet_grid(vars(vasi_sang_colorati)) +
  theme(legend.position = 'top') +
  ggtitle('Distribuzione eta per numero di vasi sanguigni colorati') 

print(vasi_eta_sex)

# si nota come le persone che hanno 3 vasi sanguigni colorati siano in netta
# minoranza, siano prevalentemente maschi e abbiano una soglia di eta 
# minima registrata elevata. 
# le persone con 0 vasi sanguigni colorati sono in netta maggioranza,
# sono prevalentemente maschi e hanno un'eta distribuita
# intorno ai valori con frequenza maggiore

#---------------------------------------------------------------------

##  DA RIVEDERE

n_classi <- 4
differenza <- max(df_pulito_so$eta) - min(df_pulito_so$eta)
elementiXclasse <- differenza/n_classi

eta_col <- ggplot(df_pulito_so, aes(x = '', colesterolo)) +
  geom_boxplot(data = seq(min(df_pulito_so$eta), max(df_pulito_so$eta), elementiXclasse))
print(eta_col)
#
#---------------------------------------------------------------------

## distribuzione sani e malati per eta

target_age <- ggplot(df_pulito_so, aes(eta)) +
  geom_density(aes(fill = obiettivo), alpha = 0.5) +
  ggtitle('Distribuzione eta per sani e malati') 


print(target_age)

# si nota come, al contrario di quanto si possa pensare, le persone piu anziane siano 
# quelle che risualtano essere piu sane in linea di massima. La distribuzione delle 
# persone malate si concentra circa intorno ai 43 e 55 anni, mentre per quanto 
# riguarda quella dei sani si concentra introno ai 59 anni

#---------------------------------------------------------------------
## distribuzione sani e malati per eta e sesso

target_age_sex <- ggplot(df_pulito_so, aes(eta)) +
  geom_density(aes(fill = obiettivo), alpha = 0.5) +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione eta per sani e malati') 


print(target_age_sex)

# dal grafico si evince che, con l'aumentare dell'età, le persone
# tendono ad avere meno patologie cardiache, come già affermato precedentemente.
# La deifferenza tra sani e malati si può notare soprattutto nel grafico femminile

#---------------------------------------------------------------------
## distribuzione colesterolo per pressione sanguigna a riposo
pres_col <- ggplot(df_pulito_so, aes(colesterolo, press_sangue_riposo)) +
  geom_point(col = '#ffa366') +
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
pres_col_sex <- ggplot(df_pulito_so, aes(colesterolo, press_sangue_riposo, col = sesso)) +
  geom_point() +
  labs(
    x = 'colesterolo mg/dl',
    y = 'pressione sangue a riposo mmHg'
  ) +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione colesterolo per pres. sanguigna a riposo diviso per sesso') 


print(pres_col_sex)
# possiamo notare come i valori registrati per le donne potrebbero essere direttamente
# proporzionali, al crescere della x cresce anche la y;
# per quanto riguarda i maschi la distribuzione è quasi uniforme nello spazio

#---------------------------------------------------------------------
## distribuzione eta per pressione sanguigna a riposo
pres_eta <- ggplot(df_pulito_so, aes(eta, press_sangue_riposo)) +
  geom_point(col = '#ffa366') +
  labs(
    x = 'eta',
    y = 'pressione sangue a riposo mmHg'
  ) +
  ggtitle('Distribuzione eta per pres. sanguigna a riposo') 


print(pres_eta)
# dal grafico si deduce che la pressione media è sicuramente superiore ai 120mmHg,
# con la maggior parte delle persone avente piu di 50 anni

#---------------------------------------------------------------------
## distribuzione eta per frequenza cardiaca massima
fcm_eta <- ggplot(df_pulito_so, aes(eta, freq_cardiaca_max)) +
  geom_point(col = '#ffa366') +
  labs(
    x = 'eta',
    y = 'frequenza cardiaca massima'
  ) +
  ggtitle('Distribuzione eta per pres. sanguigna a riposo') 


print(fcm_eta)
# si nota che, tendenzialmente, con l'aumentare dell'età
# la frequenza cardiaca massima diminuisce
#---------------------------------------------------------------------
## distribuzione colesterolo per frequenza cardiaca massima
col_fcm <- ggplot(df_pulito_so, aes(colesterolo, freq_cardiaca_max)) +
  geom_point(col = '#ffa366') +
  labs(
    x = 'colesterolo mg/dl',
    y = 'frequenza cardiaca massima'
  ) +
  ggtitle('Distribuzione colesterolo per frequenza cardiaca massima') 


print(col_fcm)
# generalmente si puo affermare che la media delle frequenze
# si trova al di sopra dei 140 battiti al minuto. Si nota anche
# che la frequenza cardiaca massima dei pazienti che hanno
# un'eta compresa tra i 40 e i 50 anni e leggermente superiore a quella
# dei pazienti piu anziani

#---------------------------------------------------------------------
# ANALISI RELAZIONE TRA PRESSIONE SANGUIGNA A RIPOSO E COLESTEROLO
#---------------------------------------------------------------------

# controllo se le variabili hanno una buona correlazione
prova <- df_pulito_so[, c(2, 5, 6, 9, 11, 13)]

# install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(round(cor(prova), 5))
# questo grafico mostra le correlazioni tra le diverse variabili numeriche 

cor(df_pulito_so$press_sangue_riposo, df_pulito_so$colesterolo)

# la correlazione è bassa, circa 10%, ma una correlazione alta 
# non implica necessariamente casuazione e casualita.   
# In questo caso si ha a che fare con una relazione con un'alta varianza 

# y = df_pulito_so$press_sangue_riposo
# x = df_pulito_so$colesterolo

## REGRESSIONE LINEARE
reg <- lm(df_pulito_so$press_sangue_riposo ~ df_pulito_so$colesterolo)

# grafico della regr. lineare 
plot(df_pulito_so$press_sangue_riposo ~ df_pulito_so$colesterolo,
     ylab = 'pressione sangue a riposo (mmHg)', xlab = 'colesterolo (mg/dl)')
title(main = "Regr.lin tra pressione sangue a riposo e colesterolo")
abline (reg, col = "red")

# si aggiungono i segmenti
segments(df_pulito_so$colesterolo, fitted(reg), df_pulito_so$colesterolo, df_pulito_so$press_sangue_riposo,
         col = "blue", lty = 2)

coef(reg)
# press_sangue_riposo = 125.60457 + 0.03191 * colesterolo
summary(reg)

# stima del modello utilizzando la formula I()
reg2 <- lm(df_pulito_so$press_sangue_riposo ~ I(df_pulito_so$colesterolo - mean(df_pulito_so$colesterolo)))

summary(reg2)
# calcolo della r e R^2
r <- cor(df_pulito_so$colesterolo, df_pulito_so$press_sangue_riposo)
r
r^2
# 0.009215276 denota che la varianza dei residui è molto alta e indica un cattivo
# modello

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


reg <- lm(press_sangue_riposo ~ colesterolo, data = df_pulito_so)

# previsione sui dati non presenti nel df

predict(reg,
        newdata = data.frame("colesterolo" = c(105, 112, 12, 53, 145, 19, 41, 152, 160, 162)),
        interval = "confidence")


#---------------------------------------------------------------------
# ANALISI RELAZIONE TRA ETA A FREQUENZA CARDIACA MAX
#---------------------------------------------------------------------

# controllo se le variabili hanno una buona correlazione
prova <- df_pulito_so[, c(2, 5, 6, 9, 11, 13)]

# install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(round(cor(prova), 5))
# questo grafico mostra le correlazioni tra le diverse variabili numeriche 

cor(df_pulito_so$eta, df_pulito_so$freq_cardiaca_max)

# la correlazione è ~ -30% quindi significa che se il valore di una variabile 
# aumenta, il valore dell'altra variabile diminuisce.

# y = df_pulito_so$freq_cardiaca_max
# x = df_pulito_so$eta

## REGRESSIONE LINEARE
reg <- lm(df_pulito_so$eta ~ df_pulito_so$freq_cardiaca_max)

# grafico della regr. lineare 
plot(df_pulito_so$eta ~ df_pulito_so$freq_cardiaca_max,
     xlab = 'frequenza cardiaca max (bpm)', ylab = 'eta')
title(main = "Regr.lin tra età e frequenza cardiaca max")
abline (reg, col = "red")

# si aggiungono i segmenti
segments(df_pulito_so$freq_cardiaca_max, fitted(reg), df_pulito_so$freq_cardiaca_max, df_pulito_so$eta,
         col = "blue", lty = 2)

coef(reg)
# freq_cardiaca_max =  69.14204 - 0.10663 * eta
summary(reg)

# stima del modello utilizzando la formula I()
reg2 <- lm(df_pulito_so$eta ~ I(df_pulito_so$freq_cardiaca_max - mean(df_pulito_so$freq_cardiaca_max)))

summary(reg2)
# calcolo della r e R^2
r <- cor(df_pulito_so$freq_cardiaca_max, df_pulito_so$eta)
r
r^2
# 0.08572463 denota che la varianza dei residui è molto alta e indica un cattivo
# modello


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


reg <- lm(eta ~ freq_cardiaca_max, data = df_pulito_so)

# previsione sui dati non presenti nel df


predict(reg, 
        newdata = data.frame("freq_cardiaca_max" = c(90, 100, 102, 119, 129, 176, 188, 190, 192, 199)), 
        interval = "confidence")


#---------------------------------------------------------------------
# ANALISI CON ALGORITMO DEL GRADIENTE
#---------------------------------------------------------------------

coef(reg)

n <- 1000
x <- rnorm(n)
# freq_cardiaca_max =  69.14204 - 0.10663 * eta
a <- as.numeric(coef(reg)[1])
b <- as.numeric(coef(reg)[2])
e <- 3

y <- a + b*x + rnorm(n, sd = e)
sim_d <- tibble(x = x, y = y)
sim_d

ggplot(sim_d, aes(x, y)) +
  geom_point()

sim_ols <- lm(y ~ x)
summary(sim_ols)

mse <- function(a, b, x = sim_d$x, y = sim_d$y) {
  # si calcola la predizione
  prediction <- a + b*x 
  # distanza tra la predizione e i risultati osservati
  residuals <- y - prediction 
  # si eleva al quadrato per evitare somme uguali a zero
  squared_residuals <- residuals^2 
  
  
  # media delle distanze elevata al quadrato
  ssr <- mean(squared_residuals)
  ssr
}

mse(a = coef(sim_ols)[1], b = coef(sim_ols)[2])

mean(resid(sim_ols)^2)

grid <- expand.grid(a = seq(55, 70, 0.1), b = seq(-5, 5, 0.1)) %>% 
  as_tibble()
grid

mse_grid <- grid %>% 
  rowwise(a, b) %>% 
  summarize(mse = mse(a, b), .groups = "drop")
mse_grid

mse_grid %>% 
  arrange(mse) %>% 
  slice(1)

coef(sim_ols)

compute_gradient <- function(a, b, x = sim_d$x, y = sim_d$y) {
  n <- length(y)
  predictions <- a + (b * x)
  residuals <- y - predictions
  
  da <- (1/n) * sum(-2*residuals)
  db <- (1/n) * sum(-2*x*residuals)
  
  c(da, db)
}

gd_step <- function(a, b, 
                    learning_rate = 0.1, 
                    x = sim_d$x, 
                    y = sim_d$y) {
  grad <- compute_gradient(a, b, x, y)
  step_a <- grad[1] * learning_rate
  step_b <- grad[2] * learning_rate
  
  c(a - step_a, b - step_b)
}

walk <- gd_step(0, 0)

# si tiene traccia del tempo che impiega l'algoritmo
Sys.time()
for(i in 1:25) {
  walk <- gd_step(walk[1], walk[2])
}
Sys.time()
walk

estimate_gradient <- function(pars_tbl, learning_rate = 0.1, x = sim_d$x, y = sim_d$y) {
  
  pars <- gd_step(pars_tbl[["a"]], pars_tbl[["b"]],
                  learning_rate)
  
  tibble(a = pars[1], b = pars[2], mse = mse(a, b, x, y))
}

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

ggplot(sim_d, aes(x, y)) +
  geom_point() +
  geom_abline(aes(intercept = a, slope = b),
              data = grad,
              color = "gray60",
              size = 0.3) +
  geom_abline(aes(intercept = a, slope = b),
              data = grad[nrow(grad), ],
              color = "magenta")

# install.packages("gganimate")
# install.packages("gifski")
# install.packages("png")
library(gganimate)
anim <- ggplot(grad) +
  geom_point(aes(x, y), sim_d) +
  geom_smooth(aes(x, y), sim_d,
              method = "lm", se = FALSE) +
  geom_abline(aes(intercept = a,
                  slope = b),
              color = "#de4f60") +
  transition_manual(frames = iteration)
animate(anim)



#---------------------------------------------------------------------
# ANALISI CON ALGORITMO FORZA BRUTA
#---------------------------------------------------------------------

# l'algoritmo di forza bruta fa riferimento ai dati e al pre processing di questi 
# ultimi dell'algoritmo del 

# if the difference between mse_old and mse_new is smaller than the epsilon value (or if the maximum number of iterations is reached), then the algorithm will halt
mse_old <- 0
mse_new <- .1
epsilon <- .0005
iteration <- 50

# initialize
grad <- estimate_gradient(tibble(a = 0, b = 0))

# loop through
i <- 2

# si tiene traccia del tempo che impiega l'algoritmo
Sys.time()
while(abs(mse_new - mse_old) > epsilon & i <= iteration) {
  grad[i, ] <- estimate_gradient(grad[i - 1, ])
  mse_old <- grad[i - 1, ncol(grad)]
  mse_new <- grad[i, ncol(grad)]
  i = i + 1
}
Sys.time()

grad

grad <- grad %>% 
  rowid_to_column("iteration")

ggplot(grad, aes(iteration, mse)) +
  geom_line()

ggplot(sim_d, aes(x, y)) +
  geom_point() +
  geom_abline(aes(intercept = a, slope = b),
              data = grad,
              color = "gray60",
              size = 0.3) +
  geom_abline(aes(intercept = a, slope = b),
              data = grad[nrow(grad), ],
              color = "magenta")

#install.packages("gganimate")
#install.packages("gifski")
#install.packages("png")
library(gganimate)
anim <- ggplot(grad) +
  geom_point(aes(x, y), sim_d) +
  geom_smooth(aes(x, y), sim_d,
              method = "lm", se = FALSE) +
  geom_abline(aes(intercept = a,
                  slope = b),
              color = "#de4f60") +
  transition_manual(frames = iteration)
animate(anim)

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
control <- trainControl(method = "cv", number = 25, seed = seed)
metric <- "Accuracy"


# linear algorithms
# si tiene traccia del tempo che impiega l'algoritmo
Sys.time()
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")
Sys.time()
## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")
## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")
## MLP
fit_mlp <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "mlp")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn, mlp = fit_mlp))
summary(results)

# misurazione dell'accuratezza sul test_set
predictions <- predict(fit_lda, test_set)
confusionMatrix(predictions, test_set$obiettivo)

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
control <- trainControl(method = "cv", number = 25, seed = seed)
metric <- "Accuracy"


# linear algorithms
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")

## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")

## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")

fit_mlp <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "mlp")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn, mlp = fit_mlp))
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
control <- trainControl(method = "cv", number = 25, seed = seed)
metric <- "Accuracy"


# linear algorithms
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")

## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")

## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")

fit_mlp <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "mlp")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn, mlp = fit_mlp))
summary(results)


#---------------------------------------------------------------------
## Prova 4 solo fattori

x <- ml[, c( 3, 4, 7, 8, 10, 12, 14, 15)]
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
control <- trainControl(method = "cv", number = 25, seed = seed)
metric <- "Accuracy"


# linear algorithms
fit_lda <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "lda")

## CART
fit_cart <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")

## KNN
fit_knn <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "knn")

fit_mlp <- train(obiettivo ~ ., data = training_set, metric = metric, trControl = control, method = "mlp")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn, mlp = fit_mlp))
summary(results)

#---------------------------------------------------------------------
## Prova 5 utilizzo del rfe

set.seed(2022)
options(warn = -1)

subsets <- c(10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
# si tiene traccia del tempo che impiega l'algoritmo
Sys.time()
lmProfile <- rfe(x = ml[, c(2:14)], y = ml$obiettivo,
                 sizes = subsets,
                 rfeControl = ctrl)
Sys.time()

lmProfile
#---------------------------------------------------------------------
## CONSIDERAZIONI FINALI
#---------------------------------------------------------------------

# il tempo per la creazione del modello preso in considerazione (LDA)
# e il tempo di esecuzione degli algoritmi di forza bruta e del gradiente 
# è pressochè 0 mentre gli algoritmi come ad esempio RFE impiega circa ~10 s
# Questi dati sono relativi a un MSI Creator p100x

