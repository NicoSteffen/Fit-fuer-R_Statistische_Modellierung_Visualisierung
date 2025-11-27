# --- Simulation: Erweiterter Datensatz für Fortgeschrittene Analysen ---

set.seed(42) # Für Reproduzierbarkeit

# 1. Basis-Parameter
n_pro_gruppe <- 60
n_total <- n_pro_gruppe * 2

# 2. Soziodemographie & Gruppen
ID <- 1:n_total
Gruppe <- rep(c("TAU", "CBT_New"), each = n_pro_gruppe)
Gruppe <- factor(Gruppe, levels = c("TAU", "CBT_New")) 

# Alter simulieren
Alter <- round(c(rnorm(n_pro_gruppe, 45, 10), rnorm(n_pro_gruppe, 42, 9)))

# Geschlecht (0 = Männlich, 1 = Weiblich)
Geschlecht <- factor(sample(c("Männlich", "Weiblich"), n_total, replace = TRUE))

# NEU: Klinik-Standort (3 Stufen für ANOVA Post-Hoc Tests)
# Wir verteilen sie zufällig
Klinik_Standort <- sample(c("Berlin", "München", "Hamburg"), n_total, replace = TRUE)
Klinik_Standort <- factor(Klinik_Standort)

# NEU: Resilienz (Kontinuierlich für Regression)
# Skala 0 (wenig) bis 100 (viel). Normalverteilt um 50.
Resilienz <- round(rnorm(n_total, mean = 50, sd = 15))
Resilienz <- pmax(0, pmin(100, Resilienz)) # Begrenzen auf 0-100


# 3. Simulation der Depressionswerte (BDI-II Score)

# --- T0: Baseline ---
# Basis-Wert: 35
# Einfluss Resilienz: Wer resilienter ist, startet schon gesünder (-0.2 pro Punkt)
# Einfluss Klinik: Berlin ist etwas stressiger (+3 Punkte im Schnitt)
baseline_score <- 35 - (0.2 * Resilienz) 
baseline_score <- baseline_score + ifelse(Klinik_Standort == "Berlin", 3, 0)
baseline_score <- baseline_score + rnorm(n_total, mean = 0, sd = 5) # Rauschen

# Begrenzen
BDI_T0 <- round(pmax(0, pmin(63, baseline_score)))


# --- T1: Post-Treatment (3 Monate) ---
# Effekt TAU: -5
# Effekt CBT: -14
# Einfluss Resilienz: Resiliente profitieren BESSER von Therapie (zusätzlich -0.1 pro Punkt)
effekt_tau <- -5
effekt_cbt <- -9

raw_t1 <- BDI_T0 # Startwert

# Wir wenden die Effekte an
# Gruppen-Effekt
raw_t1[Gruppe == "TAU"] <- raw_t1[Gruppe == "TAU"] + effekt_tau
raw_t1[Gruppe == "CBT_New"] <- raw_t1[Gruppe == "CBT_New"] + effekt_cbt

# Resilienz-Effekt auf Veränderung (Resilienz hilft bei der Heilung)
raw_t1 <- raw_t1 - (0.05 * Resilienz)

# Zufallsrauschen
raw_t1 <- raw_t1 + rnorm(n_total, mean = 0, sd = 4)

BDI_T1 <- round(pmax(0, pmin(63, raw_t1)))


# --- T2: Follow-Up (6 Monate) ---
# TAU: leichte Verschlechterung (+2)
# CBT: stabil (0)
change_t2 <- numeric(n_total)
change_t2[Gruppe == "TAU"] <- 2
change_t2[Gruppe == "CBT_New"] <- 0

BDI_T2 <- BDI_T1 + change_t2 + rnorm(n_total, mean = 0, sd = 3)
BDI_T2 <- round(pmax(0, pmin(63, BDI_T2)))


# 4. Data Frame erstellen (WIDE FORMAT)
studie_data <- data.frame(
  ID = ID,
  Gruppe = Gruppe,
  Klinik = Klinik_Standort, # NEU
  Alter = Alter,
  Geschlecht = Geschlecht,
  Resilienz = Resilienz,    # NEU
  BDI_T0 = BDI_T0,
  BDI_T1 = BDI_T1,
  BDI_T2 = BDI_T2
)

# 5. Speichern (Optional)
saveRDS(studie_data, "klinische_patientendaten.rds")

# --- Kurzer Check der Daten ---
head(studie_data)

# Schneller Check ob Signifikanz vorliegt (nur zur Info für dich)
print("Mittelwerte pro Gruppe zu den Zeitpunkten:")
aggregate(cbind(BDI_T0, BDI_T1, BDI_T2) ~ Gruppe, data = studie_data, mean)

