
# --------------------------------------------------------------------------------------------------
# Legende betreffend Code-Generierung, jeweils hinter Überschrift des Code-Abschnittes vermerkt
# --------------------------------------------------------------------------------------------------
# / MM = Code von M. Miché
# / MU = Code von M. Uhlmann
# / CHATGPT = Mehrheit des Codes oder ganzer Code mit ChatGPT 4o generiert
# ----------------------------------------------------------------------------------------
# INSTALL PACKAGES AND LOAD DATA
# ----------------------------------------------------------------------------------------
# # Install necessary packages
# install.packages("haven")
# install.packages("ggplot2")
# install.packages("performance")
# install.packages("see") 
# install.packages("patchwork") 
# install.packages("lmtest")
# install.packages("nortest")
# install.packages("grid")
# install.packages("csranks")
# install.packages("tidyverse")
# install.packages("psych")
# install.packages("e1071")
# install.packages("gridExtra")
# install.packages("lmtest")

# Load necessary packages
library(haven)
library(ggplot2)
library(performance) # for Assumptions test
library(see) # for Assumptions test
library(patchwork) # for Assumptions test
library(lmtest) # for BP test (Assumptions test)
library(nortest) # for Shapiro-Wilk test (Assumptions test)
library(car) # for VIF (Assumptions test)
library(grid)
library(csranks) # Package for Rank Regressions
library(tidyverse) # includes ggplot2
library(psych) # für describe() Funktion
library(e1071)  # für Skewness und Kurtosis
library(gridExtra) # um die estimates und confidence intervalls beim forest plot anzuzeigen
library(lmtest)

# Maybe additionally needed for Rank Regression Code:
# install.packages("devtools") #MU
# devtools::install_github("r-lib/conflicted") #MU
# force = TRUE

# ----------------------------------------------------------------------------------------
# DATENSATZ EINLESEN 
# ----------------------------------------------------------------------------------------
dat <- data.frame(haven::read_sav("~/Desktop/MA/Data for _Effects of Childhood Trauma on Mentalization Capacities and Dissociative Experiences_.sav"))
# ----------------------------------------------------------------------------------------
# PROBANDEN ENTFERNEN, DIE KEINE QUESTIONNAIRES AUSGEFÜLLT HABEN / CHATGPT
# ----------------------------------------------------------------------------------------
# Entfernen der Probanden mit Zeilenpositionen 75 und 87
dat <- dat[-c(75, 87), ]

# Anzeigen der Anzahl verbleibender Probanden nach dem Entfernen
dim(dat)

# Show colums:
colnames(dat)

# ----------------------------------------------------------------------------------------
# ANTEILE DER GESCHLECHTER BERECHNEN / CHATGPT
# ----------------------------------------------------------------------------------------
table(dat$Sex)
# Frauen kodiert mit 2
# Männer kodiert mit 1

# Anzahl der Männer und Frauen berechnen
gender_counts <- table(dat$Sex)

# Gesamtanzahl der Einträge (nur Male und Female)
total_gender <- sum(gender_counts)

# Prozentuale Verteilung berechnen
gender_percent <- prop.table(gender_counts) * 100

# Ergebnisse anzeigen
print(gender_counts)
print(gender_percent)

# ----------------------------------------------------------------------------------------
# ANTEILE VON RACE BERECHNEN / CHATGPT
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# Anteile "race" berechnen:
race_columns <- c("Race_Arab", "Race_Asian", "Race_Black_Amer", 
                  "Race_Black_NonAmer", "Race_Hisp", "Race_NatAmer", "Race_PacIsland", 
                  "Race_White", "Race_Mixed", "Race_Other")

# Initialisierung einer leeren Liste für die Ergebnisse
results <- list()

# Schleife durch jede "race"-variable
for (race in race_columns) {
  # Berechnung der Anzahl der Probanden in jeder Kategorie (nur die mit Wert 1, falls binär)
  counts <- sum(dat[[race]] == 1, na.rm = TRUE)
  
  # Berechnung der Prozente
  percent <- (counts / nrow(dat)) * 100
  
  # Speichern der Ergebnisse in der Liste
  results[[race]] <- list(counts = counts, percent = percent)
}

# Ausgabe der Ergebnisse
for (race in names(results)) {
  cat(race, ": Anzahl =", results[[race]]$counts, ", Prozent =", round(results[[race]]$percent, 2), "%\n")
}

# Überprüfung auf Mehrfachnennungen oder fehlende Werte in den "race"-variablen
race_columns <- c("Race_Arab", "Race_Asian", "Race_Black_Amer", 
                  "Race_Black_NonAmer", "Race_Hisp", "Race_NatAmer", 
                  "Race_PacIsland", "Race_White", "Race_Mixed", "Race_Other")

# Prüfen, ob jemand mehreren "race"-kategorien angehört
multi_race_count <- rowSums(dat[race_columns], na.rm = TRUE)

# Anzahl der Personen mit Mehrfachnennungen
multi_race_probanden <- sum(multi_race_count > 1)

# Ausgabe der Anzahl von Personen mit Mehrfachnennungen
cat("Number of N with multiple race-categories:", multi_race_probanden, "\n")

# Ersetze NA-Werte in den "race"-variablen durch 0
dat[race_columns] <- lapply(dat[race_columns], function(x) ifelse(is.na(x), 0, x))

# Überprüfen, ob es fehlende Werte in den "race"-variablen gibt
missing_values <- sapply(dat[race_columns], function(x) sum(is.na(x)))

# Ausgabe der fehlenden Werte pro "race"-kategorie
cat("missing values per race-category:\n")
print(missing_values)

# ----------------------------------------------------------------------------------------
# ALTERSRANGE BERECHNEN / CHATGPT
# ----------------------------------------------------------------------------------------
# Berechnung der Altersrange (min und max), Standardabweichung und Mittelwert
age_min <- min(dat$Age, na.rm = TRUE)
age_max <- max(dat$Age, na.rm = TRUE)
age_mean <- mean(dat$Age, na.rm = TRUE)
age_sd <- sd(dat$Age, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Altersrange: ", age_min, "bis", age_max, "\n")
cat("Mittelwert des Alters: ", round(age_mean, 2), "\n")
cat("Standardabweichung des Alters: ", round(age_sd, 2), "\n")

# ----------------------------------------------------------------------------------------
# BENÖTIGTE ZEIT IN SEKUNDEN FÜR DAS AUSFÜLLEN ALLER FRAGEN BEI DATENERHEBUNG  / MU
# ----------------------------------------------------------------------------------------
# Berechnung der Zeit in Sekunden welche die Probanden benötigt haben, für das Ausfüllen aller Fragen und Fragebögen
time_min <- min(dat$Duration__in_seconds_, na.rm = TRUE)
time_max <- max(dat$Duration__in_seconds_, na.rm = TRUE)
time_mean <- mean(dat$Duration__in_seconds_, na.rm = TRUE)
time_sd <- sd(dat$Duration__in_seconds_, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Zeitdauer in Sekunden: ", time_min, "bis", time_max, "\n")
cat("Durchnittliche Zeitdauer in Sekunden: ", round(time_mean, 2), "\n")
cat("Standardabweichung Zeitdauer in Sekunden: ", round(time_sd, 2), "\n")

# Bei der Zeitdauer gibt es Unstimmigkeiten, ev. wurde die Zeit bei einigen Teilnehmern nach Beendigung der Umfrage nicht gestoppt. (Zeitdauer in Sekunden:  von 1342 bis 605994, Durchnittliche Zeitdauer in Sekunden:  36885.08 Standardabweichung der Zeitdauer in Sekunden:  111452.1). Diese Angaben gelten für die 99 inkludierten Teilnehmer, die anderen wurden nicht berücksichtigt. 
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# BERECHNUNGEN DER SUBSCORES UND SUMSCORES DER FRAGEBÖGEN ETISR UND FFMQ 
# ------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# 1. ETISR // EARLY TRAUMA INVENTORY SHORT-FORM REVISED QUESTIONNAIRE / MM
# ----------------------------------------------------------------------------------------
# Find all items that contain etis
idxetis <- grepl("etis", colnames(dat), ignore.case = TRUE)
colnames(dat)[idxetis]

# Challenge: 1 = yes, however no shall be 0, not 2, and 3 shall be NA.

# Challenge 2: There are also NAs in that dataset.

# Most efficient solution: First transform all NAs to 3. Next replace all 2s to 0, then all 3s to NA.
dat[,idxetis][is.na(dat[,idxetis])] <- 3
# All 2s be 0
dat[,idxetis][dat[,idxetis]==2] <- 0
# All 3s be NA
dat[,idxetis][dat[,idxetis]==3] <- NA

# Gibt es mindestens einen fehlenden Wert?
any(is.na(dat[,idxetis]))
# Wie viele?
length(which(is.na(dat[,idxetis])))
dim(dat[,idxetis]) # Anzahl Zeilen und Spalten
226/(101*29) # prozentuale Menge fehlender Werte
idNA <- apply(dat[,idxetis], 1, function(x) {
    if(any(is.na(x))) {
        1
    } else {
        0
    }
})
# idNA == 1 heisst: in dieser Zeile von diesem Datenobjekt (dat[,idxetis]) gibt es mindestens einen fehlenden Wert.
sum(idNA)
# Übersicht aller 101 Werte
idNA

# etisr general trauma
dat[,paste0("ETISR1_", 1:11)]
# Produce and append sum score //StNH// Margin = 1 means to apply the function (i.e. sum) to the rows //StNH// The FUN is what is applied to the row 
dat$GTSumScore <- apply(dat[,paste0("ETISR1_", 1:11)], MARGIN=1, FUN=sum)

# etisr physical punishment
dat[,paste0("ETISR2_", 1:5)]
# Produce and append sum score
dat$PPSumScore <- apply(dat[,paste0("ETISR2_", 1:5)], MARGIN=1, FUN=sum)

# etisr emotional abuse
dat[,paste0("ETISR3_", 1:5)]
# Produce and append sum score
dat$EASumScore <- apply(dat[,paste0("ETISR3_", 1:5)], MARGIN=1, FUN=sum)

# etisr sexual events
dat[,paste0("ETISR4_", 1:6)]
# Produce and append sum score
dat$SESumScore <- apply(dat[,paste0("ETISR4_", 1:6)], MARGIN=1, FUN=sum)

# List column names with indices /ChatGPT
col_info <- data.frame(Index = 1:ncol(dat), ColumnName = colnames(dat))
# Print the column information /ChatGPD
print(col_info)

# Anzeigen der Items, damit ich weiss, welche ich für den etis sumscore ausschliessen muss
which(idxetis) 

# total etisr score wenn General Trauma inkludiert wird
# idxetis1 <- idxetis
# idxetis1[c(233,234)] <- FALSE
# # Total etis score (letzte beiden Items weggelassen, da nicht eingeschlossen in total score).
# dat$etisSumScore <- apply(dat[,idxetis1], MARGIN=1, FUN=sum)

# total etisr score wenn General Trauma nicht inkulidert wird
idxetis1 <- idxetis
idxetis1[c(206,207,208,209,210,211,212,213,214,215,216,233,234)] <- FALSE
# Total etis score (erste 11, die zu General Trauma gehören, da das Alter hier AB 18 Jahren abgefragt wurde, sowie letzte beiden Items weggelassen, da nicht eingeschlossen in total score).
dat$etisSumScore <- apply(dat[,idxetis1], MARGIN=1, FUN=sum)


# ----------------------------------------------------------------------------------------
# 2. FFMQ FIVE FACET MINDFULNESS QUESTIONNAIRE / MM
# ----------------------------------------------------------------------------------------
# Find all items that contain ffmq1
idxffmq1 <- grepl("ffmq", colnames(dat), ignore.case = TRUE)
(idxffmq1Cols <- colnames(dat)[idxffmq1])

# Step 1: How does your data look like right now?
# Answer: 39 columns that contain values between 1 and 5, maybe some missing values.
# Determine the relevant columns:

idxffmq <- colnames(dat)[291:329]

# Are there any missing values in this subset of the data?
any(is.na(dat[idxffmq]))
# There is at least one missing value, therefore set na.rm to TRUE (meaning: ignore NAs)
range(dat[,idxffmq], na.rm = TRUE)
# Surprise (not surprisingly): Values of FFMQ1 items range from 1 to 6, not from 1 to 5. However, codebook says that 6 represents 'no answer'. Therefore, before continuing, set the value 6 to NA.
dat[,idxffmq][is.na(dat[,idxffmq])|dat[,idxffmq]==6] <- NA
# Now, again, check range of values.
range(dat[,idxffmq], na.rm = TRUE)

# revere of the reversed scale FFMQ1-values:
# Determine what columns are relevant
reverseColumns <- paste0("FFMQ1_", c(3, 5, 8, 10, 12:14, 16:18, 22, 23, 25, 28, 30, 34, 35, 38, 39))
# Important note: If only you (really nobody else) uses your data, then simply replace the current non-reversed values with the reversed values. Reason: Less work. If at least one other person uses that dataset, it is extremely important to follow good practice rules. 

# For subjective certainty that things work as expected, display the old values in the R console, before reversing the values (vorher nachher vergleich)
head(dat[,reverseColumns])

# Here, I will simply replace the non-reversed with the reversed values.
dat[,reverseColumns] <- abs(dat[,reverseColumns]-6)

# Check, display new values in R console:
head(dat[,reverseColumns])

# The second thing that you want is the sum scores of the 5 subscales:

# Determine the 5 subscales
observing <- paste0("FFMQ1_", c(1, 6, 11, 15, 20, 26, 31, 36))
describing <- paste0("FFMQ1_", c(2, 7, 12, 16, 22, 27, 32, 37))
actWithAwareness <- paste0("FFMQ1_", c(5, 8, 13, 18, 23, 28, 34, 38))
nonjudging <- paste0("FFMQ1_", c(3, 10, 14, 17, 25, 30, 35, 39))
nonreactive <- paste0("FFMQ1_", c(4, 9, 19, 21, 24, 29, 33))

# Using R (irrespective of being a beginner or beyond being a beginner), means check and re-check what you have done. In this case, have you entered the numbers correctly in script lines above?
# How to find out? Copy paste all the numbers into a single vector. If everything is correct, the resulting vector must contain values from 1-39 (the number of columns of the FFMQ1 questionnaire).
checkVec <- c(1, 6, 11, 15, 20, 26, 31, 36, 2, 7, 12, 16, 22, 27, 32, 37, 5, 8, 13, 18, 23, 28, 34, 38, 3, 10, 14, 17, 25, 30, 35, 39, 4, 9, 19, 21, 24, 29, 33)
# Produce linear order:
checkVecSorted <- sort(checkVec)
# Now do the checking:
all(checkVecSorted == 1:39)
# If TRUE, then continue, if FALSE, find the error, then check again, until TRUE.

# Get the sum score of the five subscales. Right away, append the sum score as new column to the dataset:
dat$observeSum <- apply(dat[,observing], MARGIN = 1, sum)
dat$describeSum <- apply(dat[,describing], MARGIN = 1, sum)
dat$actAwareSum <- apply(dat[,actWithAwareness], MARGIN = 1, sum)
dat$nonjudgeSum <- apply(dat[,nonjudging], MARGIN = 1, sum)
dat$nonreactiveSum <- apply(dat[,nonreactive], MARGIN = 1, sum)

# Prepare computation of FFMQ total score
dat$ffmqSumScore <- apply(dat[,idxffmq1Cols], MARGIN = 1, sum)
# Wie viele Personen haben mind. einen fehlenden Wert in den 39 ffmq Items?
length(which(is.na(dat$ffmqSumScore)))

# ----------------------------------------------------------------------------------------
# CRONBACHS ALPHA FÜR DIE ETIS SUBSCORES / CHATGPT
# ----------------------------------------------------------------------------------------

# Berechnung von Cronbach's Alpha für Physical Punishment (ETISR2_1 bis ETISR2_5)
etisr_physical_punishment_items <- dat[, paste0("ETISR2_", 1:5)]
alpha_physical_punishment <- psych::alpha(etisr_physical_punishment_items, na.rm = TRUE)
cat("Cronbach's Alpha für Physical Punishment (ETISR):\n")
print(alpha_physical_punishment)

# Berechnung von Cronbach's Alpha für Emotional Abuse (ETISR3_1 bis ETISR3_5)
etisr_emotional_abuse_items <- dat[, paste0("ETISR3_", 1:5)]
alpha_emotional_abuse <- psych::alpha(etisr_emotional_abuse_items, na.rm = TRUE)
cat("Cronbach's Alpha für Emotional Abuse (ETISR):\n")
print(alpha_emotional_abuse)

# Berechnung von Cronbach's Alpha für Sexual Events (ETISR4_1 bis ETISR4_6)
etisr_sexual_events_items <- dat[, paste0("ETISR4_", 1:6)]
alpha_sexual_events <- psych::alpha(etisr_sexual_events_items, na.rm = TRUE)
cat("Cronbach's Alpha für Sexual Events (ETISR):\n")
print(alpha_sexual_events)

# ----------------------------------------------------------------------------------------
# CRONBACHS ALPHA FÜR DIE FFMQ SUBSCORES / CHATGPT
# ----------------------------------------------------------------------------------------

# Berechnung von Cronbach's Alpha für Observing (FFMQ1_1, FFMQ1_6, FFMQ1_11, usw.)
ffmq_observing_items <- dat[, paste0("FFMQ1_", c(1, 6, 11, 15, 20, 26, 31, 36))]
alpha_observing <- psych::alpha(ffmq_observing_items, na.rm = TRUE)
cat("Cronbach's Alpha für Observing (FFMQ):\n")
print(alpha_observing)

# Berechnung von Cronbach's Alpha für Describing (FFMQ1_2, FFMQ1_7, FFMQ1_12, usw.)
ffmq_describing_items <- dat[, paste0("FFMQ1_", c(2, 7, 12, 16, 22, 27, 32, 37))]
alpha_describing <- psych::alpha(ffmq_describing_items, na.rm = TRUE)
cat("Cronbach's Alpha für Describing (FFMQ):\n")
print(alpha_describing)

# Berechnung von Cronbach's Alpha für Acting with Awareness (FFMQ1_5, FFMQ1_8, FFMQ1_13, usw.)
ffmq_act_aware_items <- dat[, paste0("FFMQ1_", c(5, 8, 13, 18, 23, 28, 34, 38))]
alpha_act_aware <- psych::alpha(ffmq_act_aware_items, na.rm = TRUE)
cat("Cronbach's Alpha für Acting with Awareness (FFMQ):\n")
print(alpha_act_aware)

# Berechnung von Cronbach's Alpha für Nonjudging (FFMQ1_3, FFMQ1_10, FFMQ1_14, usw.)
ffmq_nonjudging_items <- dat[, paste0("FFMQ1_", c(3, 10, 14, 17, 25, 30, 35, 39))]
alpha_nonjudging <- psych::alpha(ffmq_nonjudging_items, na.rm = TRUE)
cat("Cronbach's Alpha für Nonjudging (FFMQ):\n")
print(alpha_nonjudging)

# Berechnung von Cronbach's Alpha für Nonreactive (FFMQ1_4, FFMQ1_9, FFMQ1_19, usw.)
ffmq_nonreactive_items <- dat[, paste0("FFMQ1_", c(4, 9, 19, 21, 24, 29, 33))]
alpha_nonreactive <- psych::alpha(ffmq_nonreactive_items, na.rm = TRUE)
cat("Cronbach's Alpha für Nonreactive (FFMQ):\n")
print(alpha_nonreactive)

# ----------------------------------------------------------------------------------------
# SAVE THE DATA AS A NEW RDS FILE AND LOAD NEW FILE / MM
# ----------------------------------------------------------------------------------------
##### FRAGE// B E W A R E: Be sure that you know the directory in which you currently are, before saving the dataset as rds file.
# setwd("~/Desktop/TeachingClass/FS2023Masterprojekt/MelanieUhlmann/Today20240725")

saveRDS(object=dat, file="NEU_ETIS_Data_ChildhoodTrauma_Mindfulness_NEW_99.rds")
# # Lesen der zwischengespeicherten Daten
dat <- readRDS(file="NEU_ETIS_Data_ChildhoodTrauma_Mindfulness_NEW_99.rds")

# -----------------------------------------------------------------------
# UEBERSICHT DAT / MM / MU
# -----------------------------------------------------------------------
dim(dat)
colnames(dat)
summary(dat[,c("etisSumScore", "ffmqSumScore")])
# Uebersicht Subscores Trauma / MU
summary(dat[,c("EASumScore", "PPSumScore", "SESumScore")])

# Boxplots Trauma Subscores /MU
boxplot(dat$EASumScore)
boxplot(dat$SESumScore)
boxplot(dat$PPSumScore)

# Boxplots Mindfulness Subscores /MU
boxplot(dat$observeSum)
boxplot(dat$describeSum)
boxplot(dat$actAwareSum)
boxplot(dat$nonjudgeSum)
boxplot(dat$nonreactiveSum)

# -----------------------------------------------------------------------
# Boxplots-Grafik erstellen Trauma Subscores und dem Sumscore /ChatGPT
# -----------------------------------------------------------------------
# Uebersicht Subscores Trauma/MU
summary(dat[,c( "EASumScore", "PPSumScore", "SESumScore", "etisSumScore")])

# Öffne eine PNG-Grafikdatei
png("boxplots_sum_scores.png", width = 1700, height = 1800, res = 300)

# Erstellen eines 2x3 Plots, um alle Boxplots in einem Diagramm zu zeigen
par(mfrow = c(2, 2))  # 2 Zeilen, 3 Spalten

# Boxplot für EASumScore
boxplot(dat$EASumScore, main = "Emotional Abuse", col = "lightgreen", na.rm = TRUE)

# Boxplot für PPSumScore
boxplot(dat$PPSumScore, main = "Physical Punishment", col = "lightcoral", na.rm = TRUE)

# Boxplot für SESumScore
boxplot(dat$SESumScore, main = "Sexual Events", col = "lightyellow", na.rm = TRUE)

# Boxplot für etisSumScore
boxplot(dat$etisSumScore, main = "Trauma Sum Score", col = "lightgray", na.rm = TRUE)

# Zurücksetzen der Ploteinstellungen
par(mfrow = c(1, 1))

# Schließen der PNG-Datei und Speichern des Plots
dev.off()

# Bestätigungsnachricht
cat("Die Boxplots wurden erfolgreich als 'boxplots_sum_scores.png' gespeichert.\n")

# -----------------------------------------------------------------------
# Boxplots-Grafik erstellen Mindfulness Subscores und Sumscore /ChatGPT
# -----------------------------------------------------------------------

# Uebersicht Subscores Mindfulness/MU
summary(dat[,c("observeSum", "describeSum", "actAwareSum", "nonjudgeSum", "nonreactiveSum", "ffmqSumScore")])

# Öffne eine PNG-Grafikdatei
png("boxplots_mindfulness_subscores.png", width = 2000, height = 1600, res = 300)

# Erstellen eines 2x3 Plots, um alle Boxplots in einem Diagramm zu zeigen
par(mfrow = c(2, 3))  # 2 Zeilen, 3 Spalten

# Boxplot für observeSum
boxplot(dat$observeSum, main = "Observing", col = "skyblue", na.rm = TRUE)

# Boxplot für describeSum
boxplot(dat$describeSum, main = "Describing",  col = "turquoise", na.rm = TRUE)

# Boxplot für actAwareSum
boxplot(dat$actAwareSum, main = "Acting with Awareness",  col = "salmon", na.rm = TRUE)

# Boxplot für nonjudgeSum
boxplot(dat$nonjudgeSum, main = "Nonjudging",  col = "gold", na.rm = TRUE)

# Boxplot für nonreactiveSum
boxplot(dat$nonreactiveSum, main = "Nonreacting", col = "lightpink", na.rm = TRUE)

# Boxplot für ffmqSumScore
boxplot(dat$ffmqSumScore, main = "Mindfulness Sum Score", col = "lavender", na.rm = TRUE)

# Zurücksetzen der Ploteinstellungen
par(mfrow = c(1, 1))

# Schließen der PNG-Datei und Speichern des Plots
dev.off()

# Bestätigungsnachricht
cat("Die Boxplots wurden erfolgreich als 'boxplots_mindfulness_subscores.png' gespeichert.\n")

# -----------------------------------------------------------------------
# BERECHNUNG DER ANZAHL PROBANDEN DIE "JA" (WERT > 0) FÜR JEDE VARIABLE GEWÄHLT HABEN / MM / CHATGPT
# -----------------------------------------------------------------------
easum_yes <- sum(dat$EASumScore > 0, na.rm = TRUE)
ppsum_yes <- sum(dat$PPSumScore > 0, na.rm = TRUE)
sesum_yes <- sum(dat$SESumScore > 0, na.rm = TRUE)

# Berechnung der Anzahl der Teilnehmer, die in jeder Kategorie geantwortet haben (ja oder nein)
easum_total <- sum(!is.na(dat$EASumScore))
ppsum_total <- sum(!is.na(dat$PPSumScore))
sesum_total <- sum(!is.na(dat$SESumScore))

# Ausgabe der Ergebnisse
cat("Anzahl der Probanden, die 'Ja' für EASumScore angekreuzt haben:", easum_yes, "\n")
cat("Anzahl der Probanden, die 'Ja' für PPSumScore angekreuzt haben:", ppsum_yes, "\n")
cat("Anzahl der Probanden, die 'Ja' für SESumScore angekreuzt haben:", sesum_yes, "\n")

cat("Anzahl der Probanden, die insgesamt für EASumScore geantwortet haben:", easum_total, "\n")
cat("Anzahl der Probanden, die insgesamt für PPSumScore geantwortet haben:", ppsum_total, "\n")
cat("Anzahl der Probanden, die insgesamt für SESumScore geantwortet haben:", sesum_total, "\n")

# Zusammenführen der Ergebnisse in einem Dataframe
df <- data.frame(
  Kategorie = c("Emotional Abuse", "Physical Punishment", "Sexual Events"),
  JaAntworten = c(easum_yes, ppsum_yes, sesum_yes),
  Gesamt = c(easum_total, ppsum_total, sesum_total)
)

# Prozentuale Ja-Antworten berechnen
df$ProzentJa <- df$JaAntworten / df$Gesamt * 100
df$ProzentJaFormatiert <- paste0(round(df$ProzentJa, 1), "%")

# Plot erstellen und als hochauflösendes PNG speichern / MM / CHATGPT /MU

# Öffne eine PNG-Grafikdatei mit höherer Auflösung (300 DPI)
png("balkendiagramm_ja_antworten_mit_prozent_high_res.png", 
    width = 2400, height = 1800, res = 300)  # 2400x1800 Pixel, 300 DPI

# Plot erstellen
library(ggplot2)

ggplot(df, aes(x = Kategorie, y = JaAntworten)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  xlab("Trauma Subscales") +
  ylab("Number of Yes-Responses") +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 16, margin = margin(t = 20)),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16, margin = margin(t = 20)),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  # Hinzufügen der absoluten Zahlen oberhalb der Balken
  geom_text(aes(label = JaAntworten), vjust = -0.3, size = 6) +
  # Hinzufügen der Prozentzahlen innerhalb der Balken
  geom_text(aes(label = ProzentJaFormatiert), vjust = 1.5, size = 5, colour = "white") +
  ylim(0, max(df$JaAntworten) + 10)

# Schließen der PNG-Datei und Speichern des Plots
dev.off()

# Bestätigungsnachricht
cat("Das Balkendiagramm wurde erfolgreich als 'balkendiagramm_ja_antworten_mit_prozent_high_res.png' gespeichert.\n")

# -----------------------------------------------------------------------
# ÜBERBLICK SUMSCORES UND AUSREISSER ANSCHAUEN / CHATGPT
# -----------------------------------------------------------------------
# Auswahl der relevanten Variablen
variables <- dat[, c("ffmqSumScore", "etisSumScore")]

# Berechne deskriptive Statistiken für jede Variable und ignoriere fehlende Werte
results <- data.frame(
  Variable = colnames(variables),
  Min = apply(variables, 2, min, na.rm = TRUE),
  Max = apply(variables, 2, max, na.rm = TRUE),
  M = colMeans(variables, na.rm = TRUE),
  SD = apply(variables, 2, sd, na.rm = TRUE),
  Skewness = apply(variables, 2, skewness, na.rm = TRUE),
  Kurtosis = apply(variables, 2, kurtosis, na.rm = TRUE)
)

# Ausgabe der Resultate
print(results)

# #MM Erreiche Ziel einfacher mit Package psych und Funktion describe
# psych::describe(dat[,c("ffmqSumScore", "etisSumScore")])

# Setzt das Layout für 2x2 Grafiken
par(mfrow = c(2, 2))  

# Histogramm für ffmqSumScore
hist(dat$ffmqSumScore, breaks = 20, main = "Histogramm von ffmqSumScore",
     xlab = "ffmqSumScore", col = "lightblue", border = "black")
# Q-Q-Plot für ffmqSumScore
qqnorm(dat$ffmqSumScore, main = "Q-Q-Plot von ffmqSumScore")
qqline(dat$ffmqSumScore, col = "red")

# Histogramm für etisSumScore
hist(dat$etisSumScore, breaks = 20, main = "Histogramm von etisSumScore",
     xlab = "etisSumScore", col = "lightgreen", border = "black")
# Q-Q-Plot für etisSumScore
qqnorm(dat$etisSumScore, main = "Q-Q-Plot von etisSumScore")
qqline(dat$etisSumScore, col = "red")

# Layout zurücksetzen (falls gewünscht)
par(mfrow = c(1, 1))

# Erster sehr grober visueller Überblick
plot(dat$etisSumScore, dat$ffmqSumScore)
# gleiches mit Emotional Abuse Sumscore und Nonjudgment Sumscore /MU
plot(dat$EASumScore, dat$nonjudgeSum)

# Boxplots der Sumscores
boxplot(dat$etisSumScore)
boxplot(dat$ffmqSumScore)

# Berechne das 1. und 3. Quartil sowie den Interquartilsabstand
Q1 <- quantile(dat$ffmqSumScore, 0.25, na.rm = TRUE)
Q3 <- quantile(dat$ffmqSumScore, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Berechne die obere und untere Grenze für Ausreißer
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Finde den spezifischen Wert des Ausreißers über der oberen Grenze
outliers <- dat$ffmqSumScore[dat$ffmqSumScore > upper_bound]

# Ausgabe des spezifischen Ausreisserwerts
print(outliers)

# Initialisiere eine leere Liste, um die Positionen zu speichern
outlier_positions <- list()

# Iteriere über alle Ausreisserwerte
for (value in outliers) {
  positions <- which(dat$ffmqSumScore == value)
  outlier_positions[[as.character(value)]] <- positions
}

# Ausgabe der Zeilen mit den Ausreisserwerten und den zugehörigen Probanden
for (value in names(outlier_positions)) {
  cat("Ausreißerwert:", value, "\n")
  print(dat[outlier_positions[[value]], ])
}

# -----------------------------------------------------------------------
# ÜBERBLICK über die fehlenden Werte in den Prädiktoren und Outcomes / CHATGPT
# -----------------------------------------------------------------------
# Definiere die Namen der Prädiktoren und Outcomes
outcome_columns <- c("ffmqSumScore", "observeSum", "describeSum", "actAwareSum", "nonjudgeSum", "nonreactiveSum")
predictor_columns <- c("etisSumScore", "EASumScore", "PPSumScore", "SESumScore")

# Zusammenführen der Prädiktoren und Outcomes in einer Liste
all_columns <- c(outcome_columns, predictor_columns)

# Überprüfe, ob alle Spalten existieren
missing_columns <- setdiff(all_columns, colnames(dat))
if (length(missing_columns) > 0) {
  cat("Folgende Spalten fehlen im Datensatz:", missing_columns, "\n")
} else {
  cat("Alle Spalten sind im Datensatz vorhanden.\n")
}

# Berechne die Anzahl der fehlenden Werte pro Spalte
missing_values_per_column <- colSums(is.na(dat[, all_columns]))

# Berechne den prozentualen Anteil der fehlenden Werte pro Spalte
total_rows <- nrow(dat)  # Gesamtanzahl der Zeilen
missing_percent_per_column <- (missing_values_per_column / total_rows) * 100

# Erstelle einen Dataframe für eine bessere Übersicht
missing_values_df <- data.frame(
  Column = all_columns,
  MissingValues = missing_values_per_column,
  MissingPercent = round(missing_percent_per_column, 2)
)

# Ausgabe der Anzahl und des prozentualen Anteils der fehlenden Werte
print(missing_values_df)

# Ueberprüfung wie viele Zeilen gezählt wurden (sollten 99 sein)
nrow(dat)

# ----------------------------------------------------------------------------------------
# MULTIPLE LINEARE REGRESSIONEN 
# ----------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# Collect regression formulas in a list object / MM
# -------------------------------------------------------------------------------------
# Beispiel, wie mehrere lineare Regressionen mit so wenig Codeaufwand wie möglich ausgeführt werden können.

source("~/Desktop/makeModelFormulas20240730_NEU.R", echo=FALSE)
regressionFormulas

# Collect several things (e.g., results) in a list object:
# -------------------------------------------------------
# 1. linMod = The linear model.
# 2. bpRes = Breusch Pagan Test result of the linear model.
# 3. shapiroRes = Shapiro Test result of the linear model.
# 4. vifRes = Variance inflation factor results of the predictors in the linear model.
lmModLs <- list()
for(i in 1:length(regressionFormulas)) {
  linMod_i <- lm(formula=regressionFormulas[[i]], data=dat)
  lmModLs[[paste0("linMod_",i)]] <- linMod_i
  lmModLs[[paste0("linMod_",i, "_bpRes")]] <- bptest(linMod_i)
  lmModLs[[paste0("linMod_",i, "_shapiroRes")]] <- shapiro.test(linMod_i$residuals)
  lmModLs[[paste0("linMod_",i, "_vif")]] <- car::vif(linMod_i)
}

# Collect all results. Collect VIF extra.
results <- vifRes <- c()
for(i in 1:length(regressionFormulas)) {
  estmts <- coefficients(object=summary(lmModLs[[paste0("linMod_",i)]]))[2,]
  ci <- confint(object=lmModLs[[paste0("linMod_",i)]])[2,]
  res_i <- c(estmts[1], ci, estmts[2:length(estmts)])
  bp_i <- lmModLs[[paste0("linMod_",i, "_bpRes")]]
  sh_i <- lmModLs[[paste0("linMod_",i, "_shapiroRes")]]
  res_i <- c(res_i, bp_i$statistic, "p_"=bp_i$p.value, "Shapiro"=sh_i$statistic, "p_Shapiro.W"=sh_i$p.value)
  results <- c(results, res_i)
  vifRes <- c(vifRes, lmModLs[[paste0("linMod_",i, "_vif")]])
}

# Convert into a usable format:
colNames <- unique(names(results))
resultsDf <- data.frame(matrix(data=results, nrow=length(regressionFormulas), byrow = TRUE))
colnames(resultsDf) <- colNames

# Add the regressionFormulas as a column to resultsDf
resultsDf$regressionFormulas <- names(regressionFormulas)

# Give the results a number, so that in the end, first result has number 1, second result number 2, etc.
resultsDf$resultNumber <- 1:nrow(resultsDf)

# Assign 'Outcome Group' based on the regression formulas
resultsDf$`Outcome Group` <- sapply(regressionFormulas, function(f) {
  if (grepl("ffmqSumScore", deparse(f))) return("ffmqSumScore")
  else if (grepl("observeSum", deparse(f))) return("observeSum")
  else if (grepl("describeSum", deparse(f))) return("describeSum")
  else if (grepl("actAwareSum", deparse(f))) return("actAwareSum")
  else if (grepl("nonjudgeSum", deparse(f))) return("nonjudgeSum")
  else if (grepl("nonreactiveSum", deparse(f))) return("nonreactiveSum")
})

# Extract the Predictor variable from the regressionFormulas
resultsDf$Predictor <- sapply(regressionFormulas, function(f) {
  terms <- strsplit(deparse(f), "~")[[1]]
  gsub("\\s+", "", terms[2])  # Entfernt Leerzeichen aus dem Prädiktor-Teil
})

# Sort the DataFrame by 'Outcome Group' to ensure correct grouping in the plot
resultsDf0 <- resultsDf[order(resultsDf$`Outcome Group`), ]
idxRemoveGTSumScore <- grep(pattern="GTSumScore", resultsDf0$Predictor)
resultsDf <- resultsDf0[-idxRemoveGTSumScore,]

# Check summary of vif results:
summary(vifRes)

# Copy (= execute script lines 102-104), then paste in Excel (on Mac)
clip <- pipe("pbcopy", "w")
write.table(resultsDf, file=clip, sep = '\t', row.names = FALSE, col.names=TRUE)
close(clip)

# Want to check the formula for a certain result number, e.g., result no.3?
print(resultsDf[3, c("Outcome Group", "Predictor", "regressionFormulas")])

# -----------------------------------------------------------------
# Visualize overview of all 24 results / MM / CHATGPT 
# -----------------------------------------------------------------

# Create the Forest Plot with sorted Outcome Groups
regressionWeightPlot <- 
  ggplot(resultsDf, aes(x=Estimate, y=factor(resultNumber, levels=unique(resultNumber)), color=`Outcome Group`)) +
  geom_point(shape=124, size=4) +
  geom_errorbar(width=.85, aes(xmin=`2.5 %`, xmax=`97.5 %`), linewidth=1) +
  geom_text(aes(label=Predictor), vjust=-0.5, hjust=-0.5) + # Shows the Predictor as text on the plot
  geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=1) +
  xlab(label="Predictor regression weight") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_blank(),
    axis.text.y=element_text(size=14),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "top")

# show plot in R
print(regressionWeightPlot)

# Save the new plot
ggsave(filename="GT_NEU_regressionWeightPlotNeu.png", plot = regressionWeightPlot, path = "~/Desktop", device = "png", width=8, height=7, units="in", dpi=300)

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# LINEARE RANG REGRESSIONEN (RANK REGRESSIONS) 
# ----------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# Collect rank regression formulas in a list object / MM 
# -------------------------------------------------------------------------------------

source("~/Desktop/makeModelFormulas20240730_NEU.R", echo=FALSE)
rankRegressionFormulas

# Use the list object 'variablesInvolvedInRangRegressions' to prepare the subdataset (removal of missing values)
removedSubjects <- c()
rankRegModLs <- list()
for(i in 1:length(rankRegressionFormulas)) {
  # set the subdataset for the current rank regression.
  dat1 <- dat[,variablesInvolvedInRangRegression[[i]]]
  # Identify all missing values and save in variable idxNA.
  idxNA <- apply(dat1, 1, FUN=function(x) any(is.na(x)))
  # Use idxNA to remove all missing values; new subset is called dat2.
  dat2 <- dat1[!idxNA,]
  
  removedSubjects <- c(removedSubjects, nrow(dat)-nrow(dat2))
  
  rankRegMod_i <- csranks::lmranks(formula = rankRegressionFormulas[[i]], data=dat2)
  rankRegModLs[[paste0("rankRegMod_",i)]] <- rankRegMod_i
}

# Collect all results. 
results <- c()
for(i in 1:length(rankRegressionFormulas)) {
  # Extract the main predictor estimates
  estmts <- coefficients(object=summary(rankRegModLs[[paste0("rankRegMod_",i)]]))[2,]
  ci <- confint(object=rankRegModLs[[paste0("rankRegMod_",i)]])[2,]
  res_i <- c(estmts[1], ci, estmts[2:length(estmts)])
  results <- c(results, res_i)
}
# Keep names of the named vector, use it as column names below.
colNames <- unique(names(results))
# Convert into a usable format:
resultsDf <- data.frame(matrix(data=results, nrow=length(rankRegressionFormulas), byrow = TRUE))
colnames(resultsDf) <- colNames

# Give the results a number, so that in the end, first result has number 1, second result number 2, etc.
resultsDf$resultNumber <- 1:nrow(resultsDf)
#MM Um die korrekten Ergebnisse mit Sicherheit korrekt zuweisen zu können, hänge hier folgende Spalte an:
resultsDf$resultName <- names(rankRegressionFormulas)
# Jetzt kann über resultName jederzeit überprüft werden, welcher Outcome und welcher Prädiktor sich in der dazugehörigen Formel befinden, z.B. Resultat Nr.10 bzw. Name actAwareSum_10_2
rankRegressionFormulas$actAwareSum_10_2

# Assign 'Outcome Group' based on the regression formulas
resultsDf$`Outcome Group Full` <- sapply(rankRegressionFormulas, function(f) {
  if (grepl("ffmqSumScore", deparse(f))) return("ffmqSumScore")
  else if (grepl("observeSum", deparse(f))) return("observeSum")
  else if (grepl("describeSum", deparse(f))) return("describeSum")
  else if (grepl("actAwareSum", deparse(f))) return("actAwareSum")
  else if (grepl("nonjudgeSum", deparse(f))) return("nonjudgeSum")
  else if (grepl("nonreactiveSum", deparse(f))) return("nonreactiveSum")
})

# Extract the Predictor variable from the rankRegressionFormulas
#MM Umbenannt zu PredictorFormula
resultsDf$PredictorFormula <- sapply(rankRegressionFormulas, function(f) {
  terms <- strsplit(deparse(f), "~")[[1]]
  gsub("\\s+", "", terms[2])  # Entfernt Leerzeichen aus dem Prädiktor-Teil
})
#MM Da Aufwand extrem gering ist, hier manuell:
resultsDf$PredictorFull <- rep(c("etisSumScore",
                                 "GTSumScore",
                                 "EASumScore",
                                 "PPSumScore",
                                 "SESumScore"), each=6)

#MM Empfehlung: Wenn etwas konstant (z.B. in einem Namen) vorkommt, dann kann man es genauso gut auch weglassen, weil es keinen besonderen (unique) Informationsgehalt besitzt. Bei Spalte 'Predictor' lasse 'SumScore' weg, bei Spalte 'Outcome Group' lasse 'Sum' bzw. 'SumScore' weg. Ergebnis: Weniger Text = einfacher zu lesen.
resultsDf$Predictor <- rep(c("etis",
                             "GT",
                             "EA",
                             "PP",
                             "SE"), each=6)

resultsDf$`Outcome Group` <- resultsDf$`Outcome Group Full`
resultsDf$`Outcome Group` <- gsub("Sum", "", resultsDf$`Outcome Group`)
resultsDf$`Outcome Group` <- gsub("Score", "", resultsDf$`Outcome Group`)

# Sort the DataFrame by 'Outcome Group' to ensure correct grouping in the plot
resultsDf0 <- resultsDf[order(resultsDf$`Outcome Group`), ]
idxRemoveGTSumScore <- grep(pattern="GTSumScore", resultsDf0$PredictorFull)
resultsDf <- resultsDf0[-idxRemoveGTSumScore,]

#MM Damit ggplot2 die Outcomegruppe in der gewünschten Reihenfolge darstellt, muss ein Faktor erzeugt werden, dessen interne Reihenfolge von AnwenderIn bestimmt werden sollte. Heisst, wenn man sie selbst nicht aktiv bestimmt, dann wird sie per default automatisch bestimmt, meist in aufsteigender Reihenfolge, z.B. 1, 2, 3 oder A, B, C.
resultsDf$`Outcome Group` <- factor(resultsDf$`Outcome Group`,
                                    levels=unique(resultsDf$`Outcome Group`))
# Check levels and its order
levels(resultsDf$`Outcome Group`)

#MM Auch Spalte 'resultNumber' gleich hier als Faktor festlegen.
resultsDf$resultNumber <- factor(resultsDf$resultNumber,
                                 levels=resultsDf$resultNumber)
# Check levels and its order
levels(resultsDf$resultNumber)

# ---------------------------
# # MM -- Von weiter unten hierher gesetzt.

# Text für Regressionsgewicht und Konfidenzintervall, ohne "CI:"
resultsText <- paste(
    sprintf("Estimate: %.3f", resultsDf$Estimate),
    sprintf("[%.3f, %.3f]", resultsDf$`2.5 %`, resultsDf$`97.5 %`),
    sep = " "
)

# Text auf der gleichen Höhe wie die Prädiktorbeschriftungen
resultsDf$resultsText <- resultsText
# ---------------------------

# Check summary of results
print(resultsDf)

# Copy (= execute script lines 102-104), then paste in Excel (on Mac)
clip <- pipe("pbcopy", "w")
write.table(resultsDf, file=clip, sep = '\t', row.names = FALSE, col.names=TRUE)
close(clip)

#MM Bzgl. Reihenfolge: Wichtig, führe entweder Zeilen 114 und 120 gemeinsam aus, oder keine von beiden, andernfalls entsteht ein Durcheinander.

#MM Wenn gewünscht, Reihenfolge umdrehen mit rev(), siehe levels=rev()
resultsDf$resultNumber <- factor(resultsDf$resultNumber,
                                 levels=rev(resultsDf$resultNumber))
# Check levels and its order
levels(resultsDf$resultNumber)

# #MM Wenn Reihenfolge von resultNumber umgedreht wird, muss auch Spalte Predictor derselben Reihenfolge folgen:
# resultsDf$Predictor <- rev(resultsDf$Predictor)

# # ---------------------------------------------------------------------------
# # Visualize overview of all results / MM 
# # ---------------------------------------------------------------------------

# Plot erstellen und die Ergebnisse auf gleicher Höhe anzeigen
rankRegressionWeightPlot <- 
  ggplot(resultsDf, aes(x=Estimate, y=resultNumber, color=`Outcome Group`)) +
  geom_point(shape=124, size=4) +
  geom_errorbar(width=.85, aes(xmin=`2.5 %`, xmax=`97.5 %`), linewidth=1) +
  geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=1) +
  #MM --- Veränderung: rev(), zuvor stand nur: labels=resultsDf$Predictor.
  scale_y_discrete(labels=rev(resultsDf$Predictor)) +
  geom_text(aes(x = 1.5, label=resultsText),  # Text rechts platzieren
            hjust=0, size=4, color="black") +  # Textgröße und Ausrichtung
  xlab(label="Predictor Regression Weight") +
  xlim(c(-1.2, 2.5)) +  # X-Achse auf einen größeren Bereich erweitern
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_blank(),
    axis.text.y=element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "top")

# Den Plot anzeigen
print(rankRegressionWeightPlot)

# Speichere den neuen Plot als PNG
ggsave(filename="GT_NEU_rankRegressionWeightPlotNeu99Neu.png", plot = rankRegressionWeightPlot, path = "~/Desktop", device = "png", width=9, height=8, units="in", dpi=300)

# ----------------------------------------------------------------------------------------
# ERSTELLUNG DES FOREST PLOTS FÜR NUR DEN PRÄDIKTOR EMOTIONAL ABUSE (EA) / MM
# ----------------------------------------------------------------------------------------

resultsDf_EA <- resultsDf[resultsDf$Predictor == "EA", ]

# Erstelle den Plot nur für "EA"
rankRegressionWeightPlot_EA <- 
    ggplot(resultsDf_EA, aes(x=Estimate, y=resultNumber, color=`Outcome Group`)) +
    geom_point(shape=124, size=4) +
    geom_errorbar(width=.85, aes(xmin=`2.5 %`, xmax=`97.5 %`), linewidth=1) +
    geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=1) +
    #MM ---- Hier rev() nicht nötig, weil konstant y-labels nur EA.
    scale_y_discrete(labels=resultsDf_EA$Predictor) +
    geom_text(aes(x = 1.2, label=resultsText),  # Text bleibt rechts von den Balken
              hjust=0, size=4, color="black") +  # Textgröße und Ausrichtung
    xlab(label="Predictor regression weight") +
    xlim(c(-1.2, 2.3)) +  # X-Achse auf einen größeren Bereich erweitern (nach links und rechts)
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.y=element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        panel.border = element_rect(color="black", fill=NA),
        legend.position = "top"
    )

# Den Plot anzeigen
print(rankRegressionWeightPlot_EA)

# Speichere den neuen Plot als PNG
ggsave(filename="rankRegressionWeightPlot_EA.png", plot = rankRegressionWeightPlot_EA, 
       path = "~/Desktop", 
       device = "png", width=10, height=8, units="in", dpi=300)  # Breite des Bildes anpassen


# -----------------------------------------------------------------------------------------------------------------
# AB HIER NICHT NÖTIG FÜR MA-SCRIPT, NUR FÜR MEINE ÜBERSICHT (Ausser Erstellung eines Q-Q-Plots für den Appendix)
# -----------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------
# DIE GLEICHEN MULTIPLEN LINEAREN REGRESSIONEN WIE OBEN IM SCRIPT, ABER EINZELN AUFGEFÜHRT / CHATGPT / MU
# ----------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# Regressionen für ffmqSumScore:
# -------------------------------------------------------------------------------------
#multiple lineare Regression mit Adjustierungsvariablen
lin.model2 <-lm(ffmqSumScore ~ etisSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model2))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(ffmqSumScore ~ EASumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(ffmqSumScore ~ PPSumScore  + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(ffmqSumScore ~ SESumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# Regressionen für observeSum:
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(observeSum ~ etisSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(observeSum ~ EASumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(observeSum ~ PPSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(observeSum ~ SESumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# Regressionen für describeSum
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(describeSum ~ etisSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(describeSum ~ EASumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# GRAFIK FÜR Q-Q PLOT DER RESIDUEN ERSTELLEN / CHATGPT
# -------------------------------------------------------------------------------------
# Q-Q Plot in R anzeigen
qqnorm(lin.model$residuals, main="Q-Q Plot der Residuen", pch=16, col="blue")
qqline(lin.model$residuals, col="red")

# Datei explizit auf den Desktop speichern mit der erweiterten Pfadangabe
png(filename = file.path(path.expand("~"), "Desktop/qqplot_residuals.png"), width = 800, height = 600)

# Q-Q Plot erneut erstellen, um ihn in der PNG-Datei zu speichern
qqnorm(lin.model$residuals, main="Normality of Residuals", pch=16, col="blue")
qqline(lin.model$residuals, col="red")

# Schließen der PNG-Datei und Speicherung
dev.off()
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(describeSum ~ PPSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(describeSum ~ SESumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
#Regressionen für actAwareSum
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(actAwareSum ~ etisSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(actAwareSum ~ EASumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(actAwareSum ~ PPSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(actAwareSum ~ SESumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# Regressionen für nonjudgeSum
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonjudgeSum ~ etisSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonjudgeSum ~ EASumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonjudgeSum ~ PPSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonjudgeSum ~ SESumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# Regressionen für nonreactiveSum
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonreactiveSum ~ etisSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonreactiveSum ~ EASumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonreactiveSum ~ PPSumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# multiple lineare Regression mit Adjustierungsvariablen
lin.model <-lm(nonreactiveSum ~ SESumScore + Sex + Age, data=dat)
# Print the summary of the linear regression
print(summary(lin.model))
# Check assumptions standart diagnostic plots method 1 (check_model function from the performance package)
check_model(lin.model) 
# Check assumptions standard diagnostic plots method 2
par(mfrow=c(2,2))
plot(lin.model, pch = 16, col = "blue")
# Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lin.model)
print("Breusch-Pagan Test for Homoscedasticity:")
print(bp_test)
# Normality of residuals - Shapiro-Wilk test
shapiro_test <- shapiro.test(lin.model$residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_test)
# Multicollinearity - Variance Inflation Factor (VIF)
vif_values <- vif(lin.model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
# ----------------------------------------------------------------------------------------
