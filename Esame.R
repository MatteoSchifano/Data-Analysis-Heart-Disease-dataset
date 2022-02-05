### Esame 
## Schifano, Ke, Cortinovis

# carico il dataset nella variabile df

df <- read.csv('esame/heart.csv', header = TRUE, stringsAsFactors = TRUE))

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

df$x <- as.numeric(df$x)                  # id paziente
df$age <- as.numeric(df$age)              # anni del paziente
df$cp <- as.numeric(df$cp)                # tipologia di dolore al petto: 
                                          # -- Value 1: typical angina == infarto 
                                          # -- Value 2: atypical angina == rischi infarto
                                          # -- Value 3: non-anginal pain == dolore generico
                                          # -- Value 4: asymptomatic == asintomatico             
df$trestbps <- as.numeric(df$trestbps)    # pressione sanquigna a riposo
df$fbs <- as.numeric(df$fbs)              # (insulina > 120 mg/dl) (1 = true; 0 = false)
df$restecg <- as.numeric(df$restecg)      # ecg a riposo (elettrocardiogramma)
                                          # -- Value 0: normal == normale
                                          # -- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) == anormale
                                          # -- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria == ipertrofia ventricolare sx
df$thalach <- as.numeric(df$thalach)      # maximum heart rate achieved  ???
df$exang <- as.numeric(df$exang)          # exercise induced angina (1 = yes; 0 = no)
df$slope <- as.numeric(df$slope)          # the slope of the peak exercise ST segment
                                          # -- Value 1: upsloping 
                                          # -- Value 2: flat 
                                          # -- Value 3: downsloping 
df$ca <- as.numeric(df$ca)                # number of major vessels (0-3) colored by flourosopy
df$chol <- as.numeric(df$chol)            # serum cholestoral in mg/dl
df$thal <- as.numeric(df$thal)            # 3 = normal; 6 = fixed defect; 7 = reversable defect
df$target <- as.numeric(df$target)        ## forse da rimuovere

str(df)

# 2) Rinomino le colonne in modo che siano esplicative
names(df)[names(df) == "x"] <- "ID"                        # nominale
names(df)[names(df) == "age"] <- "eta"                     # intervallo
names(df)[names(df) == "cp"] <- "dolore_petto"             # nominale o ordinale
names(df)[names(df) == "trestbps"] <- "pres_sangue_riposo" # intervallo                  
names(df)[names(df) == "sex"] <- "sesso"                   # nominale
names(df)[names(df) == "fbs"] <- "insulina"                # nominale
names(df)[names(df) == "restecg"] <- "ECG_riposo"          # nominale
names(df)[names(df) == "trestbps"] <- "freq_cardiaca"      # intervallo
names(df)[names(df) == "thalach"] <- "freq_cardiaca_max"   # intervallo
names(df)[names(df) == "chol"] <- "colesterolo"            # intervallo



## trasformo i dati in modo tale che siano TECNICAMENTE CONSISTENTI:

# 1) la frequenza cardiaca superiore a 222 e inferiore e inferiore a 0 verrà 
# rimpiazzata dalla media

boxplot(df_pulito$freq_cardiaca)
n_valido <- replace(df_pulito$freq_cardiaca, 
                    df_pulito$freq_cardiaca <= 0 & df_pulito$freq_cardiaca > 222, 
                    mean(df_pulito$freq_cardiaca > 0 & df_pulito$freq_cardiaca <= 222))

boxplot(n_valido)


vaI <- function(x){
  val <- quantile(x)[2] - 1.5 * (quantile(x)[4] - quantile(x)[2])
  return(val)
}

vaS <- function(x){
  val <- quantile(x)[2] + 1.5 * (quantile(x)[4] - quantile(x)[2])
  return(val)
}


df_senza_outlier <- 
  df_pulito[df_pulito$freq_cardiaca >= vaI(df_pulito$freq_cardiaca) &
            df_pulito$freq_cardiaca <= vaS(df_pulito$freq_cardiaca), ]

boxplot(df_senza_outlier$freq_cardiaca)

# forse da togliere 
boxplot(df_pulito$colesterolo)

df_senza_outlier2 <- 
  df_pulito[df_pulito$colesterolo >= vaI(df_pulito$colesterolo) &
              df_pulito$colesterolo <= vaS(df_pulito$colesterolo), ]

boxplot(df_senza_outlier$colesterolo)


# grafico relativo all'età

boxplot(df_pulito$eta)

eta_valido <- replace(df_pulito$eta, 
                    df_pulito$eta <= 1 | df_pulito$eta > 122, 
                    mean(df_pulito$freq_cardiaca > 1 & df_pulito$freq_cardiaca <= 122))

boxplot(eta_valido)

# grafico relativo alla frequenza cardiaca massima

boxplot(df_pulito$freq_cardiaca_max)

fmax_valido <- replace(df_pulito$freq_cardiaca_max, 
                      df_pulito$freq_cardiaca_max <= 1 | df_pulito$freq_cardiaca_max > 222, 
                      mean(df_pulito$freq_cardiaca_max > 1 & df_pulito$freq_cardiaca_max <= 222))

boxplot(fmax_valido) # da capire il minimo 

min(fmax_valido)

summary(df_pulito)
str(df)



