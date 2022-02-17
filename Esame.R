### Esame 
## Schifano, Ke, Cortinovis
#---------------------------------------------------------------------
## 1.Caricare il dataset heart.csv e analizzarne dettagliatamente la struttura.

# carico il dataset nella variabile df

df <- read.csv('esame/heart.csv', header = TRUE, stringsAsFactors = TRUE)
df <- read.csv('heart.csv', header = TRUE, stringsAsFactors = TRUE)

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
# -- Value 3: non specificato
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
df_pulito$target <- as.numeric(df_pulito$target)        # obiettivo da predirre


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
levels(df_pulito$sesso) <- c('femmina', 'maschio', 'non specificato')

levels(df_pulito$esercizi)
levels(df_pulito$esercizi) <- c('no', 'si')

levels(df_pulito$insulina)
levels(df_pulito$insulina) <- c('falso', 'vero')

levels(df_pulito$dolore_petto)
levels(df_pulito$dolore_petto) <- c('infarto', 'rischio infarto', 'dolore generico', 'asintomatico')

levels(df_pulito$ECG_riposo)
levels(df_pulito$ECG_riposo) <- c('normale', 'anormale', 'ipertrofia ventr. sx')

levels(df_pulito$inclinamento_grafico)
levels(df_pulito$inclinamento_grafico) <- c('salita', 'piatto', 'discesa')

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
dim(df_pulito)

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
    geom_density()
)
#---------------------------------------------------------------------
# ANALISI DEI VALORI ID
#---------------------------------------------------------------------
# controllo se ci sono duplicati 
table(df_pulito_so$ID)[table(df_pulito_so$ID) > 1]
# sostituiamo i valori in modo tale da avere tutti casi distinti e ordinati
df_pulito_so$ID <- 1:dim(df_pulito_so)[1]

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

# oltre il 75% del campione esaminato e rappresentato dalla modalita maschio,
# i pazienti che hanno infarti o soffrono di dolori al petto sono prevalentemente 
# maschi
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
# la persona di sesso NS ha circa 48 anni
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
# notiamo come la persona di sesso NS ha un ECG normale e circa 48 anni
#---------------------------------------------------------------------

## distribuzione delle modalita di dolore al petto per il sesso 

sex_dolore <- ggplot(df_pulito_so, aes(dolore_petto, fill = sesso)) +
  geom_bar() +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione dolore al petto per sesso') 

print(sex_dolore)

#
#
#
#---------------------------------------------------------------------

## distribuzione delle modalita di esercizi per il sesso 

sex_esercizi <- ggplot(df_pulito_so, aes(esercizi, fill = sesso)) +
  geom_bar() +
  facet_grid(vars(sesso)) +
  ggtitle('Distribuzione esercizi per sesso') 

print(sex_esercizi)
#
#
#
#
#---------------------------------------------------------------------

## distribuzione eta per numero di vasi sanguigni colorati dal fluido di contrasto 

vasi_eta <- ggplot(df_pulito_so, aes(eta, fill = vasi_sang_colorati)) +
  geom_bar() +
  facet_grid(vars(vasi_sang_colorati)) +
  ggtitle('Distribuzione eta per numero di vasi sanguigni colorati') 

print(vasi_eta)
#
#
#
#
#---------------------------------------------------------------------

##

vasi_eta_sex <- ggplot(df_pulito_so, aes(eta, fill = sesso)) +
  geom_bar() +
  facet_grid(vars(vasi_sang_colorati)) +
  # da aggiungere la legenda dei vasi sanguigni colorati
  ggtitle('Distribuzione eta per numero di vasi sanguigni colorati') 

print(vasi_eta_sex)
#
#
#
#---------------------------------------------------------------------

##

n_classi <- 4
differenza <- max(df_pulito_so$eta) - min(df_pulito_so$eta)
elementiXclasse <- differenza/n_classi

eta_col <- ggplot(df_pulito_so, aes(x = '', colesterolo)) +
  geom_boxplot(data = seq(min(df_pulito_so$eta), max(df_pulito_so$eta), elementiXclasse))
print(eta_col)
#
#

#---------------------------------------------------------------------

reg <- lm(df_pulito_so$eta ~ df_pulito_so$colesterolo)
grafico_reg <- ggplot(df_pulito_so, aes(eta, colesterolo)) +
  geom_point() +
  geom_smooth(method='lm')

print(grafico_reg)

grafico_reg <- ggplot(df_pulito_so, aes(eta, freq_cardiaca_max)) +
  geom_point() +
  geom_smooth(method='lm')

print(grafico_reg)

grafico_reg <- ggplot(df_pulito_so, aes(colesterolo, freq_cardiaca_max)) +
  geom_point() +
  geom_smooth(method='lm')

print(grafico_reg)

grafico_reg <- ggplot(df_pulito_so, aes(colesterolo, pres_sangue_riposo)) +
  geom_point() +
  geom_smooth(method='lm')

print(grafico_reg)
