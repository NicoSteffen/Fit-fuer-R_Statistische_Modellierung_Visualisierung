# --- Simulation: Längsschnittdaten für ANOVA & Regression ---

set.seed(42) # Für Reproduzierbarkeit

# 1. Basis-Parameter
n_pro_gruppe <- 60
n_total <- n_pro_gruppe * 2

# 2. Soziodemographie & Gruppen
ID <- 1:n_total
Gruppe <- rep(c("TAU", "CBT_New"), each = n_pro_gruppe)
Gruppe <- factor(Gruppe, levels = c("TAU", "CBT_New")) # Referenz ist TAU

# Alter simulieren (Interventionsgruppe zufällig etwas jünger, um Covariate zu üben)
# Wir erzeugen eine leichte Korrelation, die man später kontrollieren könnte
Alter <- round(c(rnorm(n_pro_gruppe, 45, 10), rnorm(n_pro_gruppe, 42, 9)))

# Geschlecht (0 = Männlich, 1 = Weiblich)
Geschlecht <- factor(sample(c("Männlich", "Weiblich"), n_total, replace = TRUE))


# 3. Simulation der Depressionswerte (BDI-II Score)
# Wir bauen die Effekte mathematisch zusammen

# --- T0: Baseline ---
# Beide Gruppen starten ähnlich (Mittelwert ca. 35 = schwere Depression)
# Rauschen (Noise) ist wichtig
baseline_score <- rnorm(n_total, mean = 35, sd = 6)
# Wir korrigieren leichte Ausreißer nach oben/unten (Skala 0-63)
baseline_score <- pmax(0, pmin(63, baseline_score))


# --- T1: Post-Treatment (3 Monate) ---
# Effekt TAU: -5 Punkte (Zeit/Placebo)
# Effekt CBT: -14 Punkte (Signifikant besser)
# + Zufälliges Rauschen pro Person
effekt_tau_t1 <- -5
effekt_cbt_t1 <- -14

random_noise_t1 <- rnorm(n_total, mean = 0, sd = 4)

# Berechnung T1
BDI_T0 <- round(baseline_score)
BDI_T1 <- numeric(n_total)

# Logik: Wenn TAU, dann T0 - 5. Wenn CBT, dann T0 - 14.
BDI_T1[Gruppe == "TAU"] <- BDI_T0[Gruppe == "TAU"] + effekt_tau_t1 + random_noise_t1[Gruppe == "TAU"]
BDI_T1[Gruppe == "CBT_New"] <- BDI_T0[Gruppe == "CBT_New"] + effekt_cbt_t1 + random_noise_t1[Gruppe == "CBT_New"]


# --- T2: Follow-Up (6 Monate) ---
# TAU: Bleibt stabil oder verschlechtert sich leicht wieder (+1)
# CBT: Bleibt stabil stabil (Nachhaltiger Effekt)
effekt_tau_t2_change <- 1 
effekt_cbt_t2_change <- 0

random_noise_t2 <- rnorm(n_total, mean = 0, sd = 3)

BDI_T2 <- numeric(n_total)
BDI_T2[Gruppe == "TAU"] <- BDI_T1[Gruppe == "TAU"] + effekt_tau_t2_change + random_noise_t2[Gruppe == "TAU"]
BDI_T2[Gruppe == "CBT_New"] <- BDI_T1[Gruppe == "CBT_New"] + effekt_cbt_t2_change + random_noise_t2[Gruppe == "CBT_New"]

# Sicherstellen, dass keine Werte < 0 sind
BDI_T1 <- pmax(0, round(BDI_T1))
BDI_T2 <- pmax(0, round(BDI_T2))


# 4. Data Frame erstellen (WIDE FORMAT)
studie_data <- data.frame(
  ID = ID,
  Gruppe = Gruppe,
  Alter = Alter,
  Geschlecht = Geschlecht,
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
