# --- Simulation: Datensatz mit signifikantem Center-Effekt auf den Change ---

set.seed(42) # Für Reproduzierbarkeit

# 1. Basis-Parameter
n_pro_gruppe <- 60
n_total <- n_pro_gruppe * 2

# 2. Soziodemographie & Gruppen
ID <- 1:n_total
Gruppe <- rep(c("TAU", "CBT_New"), each = n_pro_gruppe)
Gruppe <- factor(Gruppe, levels = c("TAU", "CBT_New")) 

# Alter & Geschlecht
Alter <- round(c(rnorm(n_pro_gruppe, 45, 10), rnorm(n_pro_gruppe, 42, 9)))
Geschlecht <- factor(sample(c("Männlich", "Weiblich"), n_total, replace = TRUE))

# Klinik-Standort
Klinik_Standort <- sample(c("Berlin", "München", "Hamburg"), n_total, replace = TRUE)
Klinik_Standort <- factor(Klinik_Standort)

# Resilienz
Resilienz <- round(rnorm(n_total, mean = 50, sd = 15))
Resilienz <- pmax(0, pmin(100, Resilienz))


# 3. Simulation der Depressionswerte (BDI-II Score)

# --- T0: Baseline ---
# Berlin startet schon gestresster (+3)
baseline_score <- 35 - (0.2 * Resilienz) 
baseline_score <- baseline_score + ifelse(Klinik_Standort == "Berlin", 3, 0)
baseline_score <- baseline_score + rnorm(n_total, mean = 0, sd = 5) 

BDI_T0 <- round(pmax(0, pmin(63, baseline_score)))


# --- T1: Post-Treatment (3 Monate) ---
# Effekt TAU: -5 / Effekt CBT: -14
effekt_tau <- -5
effekt_cbt <- -14

raw_t1 <- BDI_T0 # Startwert

# Gruppen-Effekte anwenden
raw_t1[Gruppe == "TAU"] <- raw_t1[Gruppe == "TAU"] + effekt_tau
raw_t1[Gruppe == "CBT_New"] <- raw_t1[Gruppe == "CBT_New"] + effekt_cbt

# Resilienz-Effekt
raw_t1 <- raw_t1 - (0.05 * Resilienz)

# --- HIER IST DIE ÄNDERUNG (DER CENTER EFFEKT) ---
# Berlin performt schlechter! Wir addieren 10 Punkte auf den T1-Wert.
# Das heißt: Der Wert sinkt kaum (oder steigt sogar), während er in München/Hamburg sinkt.
# Das erzeugt einen massiven Unterschied im "Change".
raw_t1[Klinik_Standort == "Berlin"] <- raw_t1[Klinik_Standort == "Berlin"] + 10

# Zufallsrauschen
raw_t1 <- raw_t1 + rnorm(n_total, mean = 0, sd = 4)
BDI_T1 <- round(pmax(0, pmin(63, raw_t1)))


# --- T2: Follow-Up (6 Monate) ---
change_t2 <- numeric(n_total)
change_t2[Gruppe == "TAU"] <- 2
change_t2[Gruppe == "CBT_New"] <- 0
BDI_T2 <- BDI_T1 + change_t2 + rnorm(n_total, mean = 0, sd = 3)
BDI_T2 <- round(pmax(0, pmin(63, BDI_T2)))


# 4. Data Frame erstellen
studie_data <- data.frame(
  ID = ID,
  Gruppe = Gruppe,
  Klinik = Klinik_Standort,
  Alter = Alter,
  Geschlecht = Geschlecht,
  Resilienz = Resilienz,
  BDI_T0 = BDI_T0,
  BDI_T1 = BDI_T1,
  BDI_T2 = BDI_T2
)



# --- Checks ---

# 1. ANOVA Testen (Das wolltest du sehen!)
mod <- lm(BDI_Change_T0_T1 ~ Klinik, data = studie_data)
print(anova(mod))

# 2. Mittelwerte pro Klinik anzeigen (damit du siehst, was passiert ist)
print(tapply(studie_data$BDI_Change_T0_T1, studie_data$Klinik, mean))
# 5. Speichern (Optional)
saveRDS(studie_data, "klinische_patientendaten.rds")

# --- Kurzer Check der Daten ---
head(studie_data)

# Schneller Check ob Signifikanz vorliegt (nur zur Info für dich)
print("Mittelwerte pro Gruppe zu den Zeitpunkten:")
aggregate(cbind(BDI_T0, BDI_T1, BDI_T2) ~ Gruppe, data = studie_data, mean)

