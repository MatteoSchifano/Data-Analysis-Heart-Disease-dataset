### Esame 
## Schifano, Ke, Cortinovis

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
## trasformo i dati in modo tale che siano TECNICAMENTE CORRETTI:

# 1) controllo i dati NA, in caso ci fossero li elimino
library(tidyverse)


# conto il numero di NA per ogni colonna
apply(is.na(df), 2, sum)

# salvo in df_pulito i record senza valori NA per operare su un df consistente
df_pulito <- df %>%
  drop_na()

# ri-conto il numero di NA per ogni colonna
apply(is.na(df_pulito), 2, sum)

# 2) le colonne dovranno essere del tipo corretto

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
df_pulito$fbs <- as.numeric(df_pulito$fbs)              # (insulina > 120 mg/dl) (1 = true; 0 = false)
df_pulito$restecg <- as.factor(df_pulito$restecg)       # ecg a riposo (elettrocardiogramma)
                                          # -- Value 0: normal == normale
                                          # -- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) == anormale
                                          # -- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria == ipertrofia ventricolare sx
df_pulito$thalach <- as.numeric(df_pulito$thalach)      # massima frequenza cardiaca archiviata
df_pulito$exang <- as.numeric(df_pulito$exang)          # esercizi per il dolore al petto (1 = yes; 0 = no)
                                          # oldpeak     # Depressione ST indotta dall'esercizio rispetto al riposo
df_pulito$slope <- as.factor(df_pulito$slope)           # la pendenza del segmento ST di picco di esercizio
                                          # -- Value 1: upsloping == salita
                                          # -- Value 2: flat == piatto
                                          # -- Value 3: downsloping == discesa
df_pulito$ca <- as.numeric(df_pulito$ca)                # numero di vasi principali (0-3) colorati da fluoroscopia
df_pulito$chol <- as.numeric(df_pulito$chol)            # tasso di colesterolo in mg/dl
df_pulito$thal <- as.factor(df_pulito$thal)             # 3 = normale; 6 = difetto fisso; 7 = difetto reversibile
df_pulito$target <- as.numeric(df_pulito$target)        # obiettivo da predirre


#---------------------------------------------------------------------
# RIORDINO I LIVELLI DEI FATTORI
#---------------------------------------------------------------------

levels(df_pulito$sex) <- c('femmina', 'maschio', 'non specificato')
levels(df_pulito$sex)

levels(df_pulito) <- c('infarto', 'rischio infarto', 'dolore generico', 'asintomatico')
levels(df_pulito$cp)

levels(df_pulito$restecg) <- c('normale', 'anormale', 'ipertrofia ventr. sx')
levels(df_pulito$restecg)

levels(df_pulito$slope) <- c('salita', 'piatto', 'discesa')
levels(df_pulito$slope)

levels(df_pulito$thal) <- c('normale', 'difetto fisso', 'difetto reversibile')
levels(df_pulito$thal)

str(df_pulito)

# 2) Rinomino le colonne in modo che siano esplicative
names(df_pulito)[names(df_pulito) == "x"] <- "ID"                        # nominale
names(df_pulito)[names(df_pulito) == "age"] <- "eta"                     # intervallo
names(df_pulito)[names(df_pulito) == "cp"] <- "dolore_petto"             # nominale o ordinale
names(df_pulito)[names(df_pulito) == "trestbps"] <- "pres_sangue_riposo" # intervallo                  
names(df_pulito)[names(df_pulito) == "sex"] <- "sesso"                   # nominale
names(df_pulito)[names(df_pulito) == "fbs"] <- "insulina"                # nominale
names(df_pulito)[names(df_pulito) == "restecg"] <- "ECG_riposo"          # nominale
names(df_pulito)[names(df_pulito) == "thalach"] <- "freq_cardiaca_max"   # intervallo
names(df_pulito)[names(df_pulito) == "chol"] <- "colesterolo"            # intervallo
names(df_pulito)[names(df_pulito) == "exang"] <- "esercizi"              # nominale
names(df_pulito)[names(df_pulito) == "oldpeak"] <- "vecchi_picchi"       # raporto ???
names(df_pulito)[names(df_pulito) == "slope"] <- "inclinamento_grafico"  # nominale
names(df_pulito)[names(df_pulito) == "ca"] <- "vasi_sang_colorati"       # ordinale
names(df_pulito)[names(df_pulito) == "thal"] <- "difetto"                # 
names(df_pulito)[names(df_pulito) == "target"] <- "obiettivo"            # nominale

summary(df_pulito$difetto)

## trasformo i dati in modo tale che siano TECNICAMENTE CONSISTENTI:

# 1) la frequenza cardiaca superiore a 222 e inferiore e inferiore a 0 verrà 
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
  # NA
  mean(df_pulito$freq_cardiaca_max > 1 & df_pulito$freq_cardiaca_max < 222)
 )

print(boxplotX(df_pulito, n_valido2) +
      ggtitle('Distribuzione frequenza cardiaca massima registrata') +
        ylab('frequenza cardiaca max')) # da aggiustare o chiedere al prof

min(df_pulito$freq_cardiaca_max)
min(n_valido2)

# i dati non vengono visulizzati se sono inconsistenti, come valori superiori 
# a 222 

#---------------------------------------------------------------------
# ANALISI DEI VALORI DI ETA
#---------------------------------------------------------------------

print(boxplotX(df_pulito, df_pulito$eta) +
        ggtitle('Distribuzione età') +
        ylab('età'))


eta_valido <- replace(df_pulito$eta, 
                      df_pulito$eta <= 1 | df_pulito$eta > 122, 
                      mean(df_pulito$freq_cardiaca > 1 & df_pulito$freq_cardiaca <= 122))

print(boxplotX(df_pulito, eta_valido) +
        ggtitle('Distribuzione età') +
        ylab('età'))

# viene rimosso il dato insonsistente negativo, l'età non può essere 
# negativa

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
df_pulito <- df_senza_outlier(df_pulito, df_pulito$pres_sangue_riposo)
dim(df_pulito)

print(boxplotX(df_senza_outlier(df_pulito, df_pulito$freq_cardiaca_max),
               df_senza_outlier(df_pulito, df_pulito$freq_cardiaca_max)$freq_cardiaca_max) +
        ggtitle('Distribuzione frequenza cardiaca massima registrata senza outlier') +
        ylab('frequenza cardiaca max'))

df_pulito <- df_senza_outlier(df_pulito, df_pulito$freq_cardiaca_max)
dim(df_pulito)

print(boxplotX(df_senza_outlier(df_pulito, df_pulito$eta), 
               df_senza_outlier(df_pulito, df_pulito$eta)$eta) +
        ggtitle('Distribuzione età senza outlier') +
        ylab('età'))

df_pulito <- df_senza_outlier(df_pulito, df_pulito$eta)
dim(df_pulito)

# forse da togliere
print(boxplotX(df_senza_outlier(df_pulito, df_pulito$colesterolo),
               df_senza_outlier(df_pulito, df_pulito$colesterolo)$colesterolo) +
        ggtitle('Distribuzione tasso di colesterolo') +
        ylab('colesterolo in mg/dl'))

# visualizzazione dei grafici analizzati in precedenza senza outlier

#---------------------------------------------------------------------
# ANALISI DESCRITTIVA
#---------------------------------------------------------------------

## Distribuzione del sesso tramite un diagramma a torta
sex <- ggplot(df_pulito,  aes(x = "", y = sesso, fill = sesso)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette="Blues") +
  ggtitle('Distribuzione sesso') 
print(sex)  

# oltre il 75% del campione esaminato è rappresentato dalla modalità maschio,
# i pazienti che hanno infarti o soffrono di dolori al petto sono prevalentemente 
# maschi


## Distribuzione delle eta in base al sesso tramite boxplot
eta_sex <- ggplot(df_pulito, aes(sesso, eta)) + 
  geom_boxplot(
    color="blue",
    fill="slateblue",
    alpha=0.2
  ) +
  scale_fill_brewer(palette="Blues") +
  ggtitle('Distribuzione eta per sesso') 

print(eta_sex)

#
#
#
#
#----------------------------------------------------
###DA RIVEDERE
ecg_eta_sex <- ggplot(df_pulito, aes(ECG_riposo, eta, col = sesso)) + 
  geom_boxplot(
    alpha=0.2
  ) +
  ggtitle('Distribuzione età per esito ECG e sesso') +
  facet_grid(vars(sesso))

print(ecg_eta_sex)

col_pressione <- ggplot(df_pulito, aes(ID, colesterolo )) +
  geom_point()

print(col_pressione)
#----------------------------------------------------


