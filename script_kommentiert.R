# ============================================================================
# Fit für R — Statistische Modellierung & Visualisierung
# ----------------------------------------------------------------------------
# Quelle / Bookdown: https://stephangoerigk.github.io/CFH_R_bookdown/über-dieses-skript.html
# Repo:              https://github.com/NicoSteffen/Fit-fuer-R_Statistische_Modellierung_Visualisierung
# Table:              https://apastyle.apa.org/style-grammar-guidelines/tables-figures/sample-tables
#
# LEGENDE DER KOMMENTAR-MARKER (zum schnellen Auffinden im Kurs):
#   # ERKLÄRUNG : Was passiert hier / warum diese Methode?
#   # HINWEIS   : Statistisch/methodisch wichtig 
# ============================================================================


# ============================================================================
# 0) SETUP — Pakete laden
# ----------------------------------------------------------------------------

# Einmalig installieren (auskommentiert lassen, wenn schon vorhanden):
# install.packages(c("car","afex","emmeans","ggplot2","tidyr",
#                     "lmerTest","performance","olsrr"))

library(car)          # Levene-Test, Anova(type = 3)
library(afex)         # aov_ez(): ANOVA mit Typ-III-SS + Effektstärken + RM-Korrekturen
library(emmeans)      # geschätzte Randmittel, Post-hoc-Kontraste, Trendanalysen
library(ggplot2)      # Visualisierung (Grammar of Graphics)
library(tidyr)        # pivot_longer(): wide -> long
library(lmerTest)     # lineare gemischte Modelle (lmer) inkl. p-Werten
library(performance)  # check_model(): Modellannahmen in einem Aufwasch
# olsrr wird unten nur per olsrr:: angesprochen (kein library nötig)


# ============================================================================
# 1) DATEN LADEN & INSPIZIEREN
# ----------------------------------------------------------------------------
df = readRDS(url("https://raw.githubusercontent.com/NicoSteffen/Fit-fuer-R_Statistische_Modellierung_Visualisierung/main/klinische_patientendaten.rds"))

# ERKLÄRUNG: str() ist der erste Blick in JEDEN Datensatz:
#   - Wie viele Beobachtungen (Zeilen) und Variablen (Spalten)?
#   - Sind Faktoren auch als 'factor' codiert (wichtig für ANOVA!) oder als chr/num?
str(df)

# TIPP: Ergänzende Schnell-Checks, die im Kurs gern gefragt werden:
head(df)            # erste Zeilen
summary(df)         # Verteilung / fehlende Werte (NA) je Variable
colSums(is.na(df))  # NAs gezielt zählen
names(df)           # nur die Spaltennamen

# Variablen im Datensatz:
#   ID, Gruppe (CBT_New / TAU), Klinik (Berlin / Hamburg / München),
#   Alter, Geschlecht (Männlich / Weiblich), Resilienz, BDI_T0, BDI_T1, BDI_T2


# ============================================================================
# 2) EINFAKTORIELLE ANOVA
#    Frage: Unterscheidet sich die BDI-Veränderung zwischen den Standorten?
# ----------------------------------------------------------------------------

# ERKLÄRUNG: Wir bilden einen Veränderungs-/Differenzwert (T0 minus T1).
#   Positiv = Verbesserung (Depressivität sinkt von T0 nach T1).
#   Mit so einem Change-Score reduzieren wir die zwei Messzeitpunkte auf EINE
#   Zielvariable -> dann reicht eine "normale" (einfaktorielle) ANOVA.
df$change_bdi = df$BDI_T0 - df$BDI_T1


# --- 2a) Voraussetzungen prüfen -------------------------------------------

# ERKLÄRUNG: Varianzhomogenität (gleiche Streuung in allen Gruppen).
# HINWEIS: center = "mean" => klassischer Levene-Test.
#   Default in car ist center = "median" (= Brown-Forsythe), das ist robuster
#   gegenüber Ausreißern/Schiefe. Für den Kurs gut zu wissen, dass beides
#   "Levene" genannt wird, sich aber im Zentrum unterscheidet.
car::leveneTest(change_bdi ~ Klinik, data = df, center = "mean")

# ERKLÄRUNG: Normalverteilung der Werte INNERHALB jeder Gruppe.
#   by() wendet shapiro.test() getrennt je Klinik an (split-apply).
by(df$change_bdi, df$Klinik, shapiro.test)

# ERKLÄRUNG: Optische Prüfung der Verteilung (hier exemplarisch München).
hist(df$change_bdi[df$Klinik == "München"])

# Q-Q-Plot für die NV-Prüfung (exemplarisch München)
qqnorm(df$change_bdi[df$Klinik == "München"])
qqline(df$change_bdi[df$Klinik == "München"])

# ERKLÄRUNG: Gruppengrößen anschauen (für das Robustheits-Argument unten wichtig).
table(df$Klinik)

# HINWEIS (zentrale Kursaussage):
#   Der Levene-Test KANN signifikant werden, ABER die ANOVA ist i. d. R. robust
#   gegenüber Verletzungen der Normalverteilung — besonders bei n > 30 je Gruppe
#   (zentraler Grenzwertsatz). Hier sind alle Gruppen > 30 -> entspannt bleiben.
#   Falls man wirklich unsicher ist: nonparametrische Alternative ->
#   Kruskal-Wallis (Omnibus) + paarweise Wilcoxon-Rangsummentests (Post-hoc).


# --- 2b) Das Modell schätzen ----------------------------------------------

# ERKLÄRUNG: Eine ANOVA ist nichts anderes als ein lineares Modell mit einem
#   kategorialen Prädiktor. lm() schätzt das Modell, anova() zerlegt die Varianz.
mod = lm(change_bdi ~ Klinik, data = df)
anova(mod)

# HINWEIS: anova() liefert hier Typ-I-Quadratsummen (sequenziell).
#   Bei NUR EINEM Faktor ist das egal — Typ I = Typ II = Typ III sind identisch.
#   Der Unterschied wird erst bei mehreren Faktoren relevant (siehe Abschnitt 3)!

# ERKLÄRUNG: Nonparametrische Alternative zur einfaktoriellen ANOVA.
kruskal.test(change_bdi ~ Klinik, data = df)

# ERKLÄRUNG: Derselbe Test, aber mit afex::aov_ez().
# HINWEIS: Warum aov_ez() statt anova(lm())? Drei gute Gründe:
#   1) Typ-III-SS standardmäßig (jeder Effekt kontrolliert für alle anderen).
#   2) Setzt automatisch die korrekten Kontraste (contr.sum), die für Typ III
#      überhaupt erst sinnvoll sind — ein klassischer Anfänger-Fallstrick.
#   3) Gibt Effektstärken (generalisiertes eta², "ges") gleich mit aus
#      und beherrscht Messwiederholung inkl. Sphärizitätskorrektur (s. u.).
# id = eindeutige Personenkennung; between = Zwischen-Personen-Faktor.
mod = aov_ez(dv = "change_bdi", between = "Klinik", id = "ID", data = df)
mod   # Ausgabe: ANOVA-Tabelle mit F, df, p und ges


# --- 2c) Post-hoc: Wo liegen die Unterschiede genau? ----------------------

# ERKLÄRUNG: emmeans = "estimated marginal means" (geschätzte Randmittel).
#   pairwise ~ Klinik vergleicht alle Standortpaare miteinander
#   (Berlin–Hamburg, Berlin–München, Hamburg–München) und korrigiert für
#   multiples Testen (Default: Tukey).
emmeans(mod, pairwise ~ Klinik)

# ERKLÄRUNG: Nonparametrisches Post-hoc-Pendant (passend zu Kruskal-Wallis).
#   p.adjust.method = "holm": Holm-Korrektur gegen Alpha-Fehler-Kumulierung.
pairwise.wilcox.test(df$change_bdi,
                     df$Klinik,
                     p.adjust.method = "holm")


# --- 2d) Visualisierung: Mittelwerte + Standardfehler ---------------------

# KORREKTUR: Im Original stand y = BDI_change — diese Variable existiert nicht!
#            Sie heißt change_bdi (siehe Abschnitt 2). Sonst: "object not found".
#
# ZEILE-FÜR-ZEILE:
ggplot(data = df, aes(x = Klinik, y = change_bdi)) +
  # aes(): Mapping. x = Standort (kategorial), y = Veränderungswert (metrisch).
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  # stat_summary() berechnet eine Zusammenfassung PRO x-Gruppe direkt im Plot.
  #   fun.data = mean_se -> liefert Mittelwert sowie Mittelwert ± 1 Standardfehler.
  #   geom = "errorbar"  -> zeichnet daraus die Fehlerbalken.
  #   width = 0.2        -> Breite der Querstriche (rein optisch).
  #   color = "red"      -> Farbe der Balken.

  



  library(ggplot2)
# 95%-CI über mean_cl_normal braucht Hmisc -> einmalig: install.packages("Hmisc")

p_anova <- ggplot(df, aes(x = Klinik, y = change_bdi)) +
  geom_violin(aes(fill = Klinik), alpha = 0.15, colour = NA, width = 0.9) +
  # ^ Verteilungsform je Standort als halbtransparente "Wolke" im Hintergrund
  geom_jitter(aes(colour = Klinik), width = 0.08, alpha = 0.35, size = 1.8) +
  # ^ Rohdaten leicht horizontal versetzt (jitter), damit sich Punkte nicht überdecken
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               width = 0.12, linewidth = 0.7, colour = "black") +
  # ^ Mittelwert ± 95%-Konfidenzintervall (schwarz, im Vordergrund)
  stat_summary(fun = mean, geom = "point", size = 3, colour = "black") +
  # ^ der Mittelwert als deutlicher Punkt oben drauf
  scale_fill_brewer(palette = "Set2", guide = "none") +
  scale_colour_brewer(palette = "Set2", guide = "none") +
  # ^ dezente Farbpalette; guide = "none" blendet die (hier redundante) Legende aus
  labs(title = "BDI-Veränderung (T0 - T1) nach Standort",
       subtitle = "Punkte = Rohdaten · Wolke = Verteilung · schwarz = Mittelwert ± 95%-CI",
       x = "Klinik", y = expression(Delta*"BDI (T0 - T1)")) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

p_anova

ggsave("anova_change_bdi.pdf", plot = p_anova,
       width = 16, height = 12, units = "cm", device = pdf)


# ============================================================================
# 3) MEHRFAKTORIELLE (ZWEIFAKTORIELLE) ANOVA
#    Frage: Wirken Klinik UND Gruppe (Therapie) — und interagieren sie?
# ----------------------------------------------------------------------------

# ERKLÄRUNG: Klinik * Gruppe = beide Haupteffekte PLUS deren Interaktion
#   (Kurzform für Klinik + Gruppe + Klinik:Gruppe).
mod = lm(change_bdi ~ Klinik * Gruppe, data = df)
anova(mod)

# HINWEIS — DER zentrale Punkt dieses Abschnitts (anova() vs. aov_ez()):
#   anova(lm()) rechnet Typ-I-SS = SEQUENZIELL. Jeder Effekt erklärt nur die
#   Varianz, die nach den davor stehenden Termen übrig ist. Bei UNbalancierten
#   Designs hängt das Ergebnis von der REIHENFOLGE der Faktoren ab!
#   -> Unser Design IST unbalanciert (Zellen ungleich groß, s. table() unten),
#      d. h. Typ I ist hier in der Regel NICHT das, was man berichten will.
table(df$Klinik, df$Gruppe)   # zeigt die ungleichen Zellbesetzungen

# ERKLÄRUNG: "Effekt der Reihenfolge" + balancierte Gruppen wären die
#   Voraussetzung dafür, dass Typ I unproblematisch ist. Sind sie hier nicht.

# Der sichere Weg: aov_ez() -> Typ-III-SS + korrekte Kontraste automatisch.
# between = c("Klinik", "Gruppe") => zwei Zwischen-Personen-Faktoren.
mod = aov_ez(dv = "change_bdi", between = c("Klinik", "Gruppe"), id = "ID", data = df)
mod
# TIPP: Äquivalent ginge auch car::Anova(lm(...), type = 3) — ABER nur, wenn
#       man vorher contr.sum setzt. aov_ez() nimmt einem genau das ab.


# ERKLÄRUNG: Post-hoc-Logik bei zwei Faktoren:
emmeans(mod, pairwise ~ Klinik)            # Haupteffekt Klinik (über Gruppen gemittelt)
emmeans(mod, pairwise ~ Gruppe)            # Haupteffekt Gruppe  (über Kliniken gemittelt)
emmeans(mod, pairwise ~ Klinik * Gruppe)   # einfache Effekte / alle Zellvergleiche
# HINWEIS: Bei SIGNIFIKANTER Interaktion sind die Haupteffekte mit Vorsicht zu
#   interpretieren — dann ist v. a. der dritte Vergleich (zellenweise) relevant.


# --- 3b) Visualisierung: Interaktionsplot ---------------------------------

# ZEILE-FÜR-ZEILE:
ggplot(data = df, aes(x = Klinik, y = change_bdi, colour = Gruppe, group = Gruppe)) +
stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
   stat_summary(geom = "point", fun = mean) +
   stat_summary(geom = "line", fun = mean)


# ============================================================================
# 4) LINEARE REGRESSION (mit Interaktion)
#    Frage: Hängt die Baseline-Depression (BDI_T0) von Resilienz ab —
#           und unterscheidet sich dieser Zusammenhang nach Geschlecht?
# ----------------------------------------------------------------------------

# ERKLÄRUNG: Resilienz (metrisch) * Geschlecht (kategorial) -> wir testen, ob
#   die STEIGUNG des Resilienz-Effekts vom Geschlecht abhängt (Moderation).
mod = lm(BDI_T0 ~ Resilienz * Geschlecht, data = df)
summary(mod)

# ERKLÄRUNG: Zentrierung des metrischen Prädiktors (Mittelwert abziehen).
# HINWEIS: WARUM zentrieren? In einem Modell MIT Interaktion bezieht sich der
#   "Haupteffekt" von Geschlecht auf den Wert Resilienz = 0. Unzentriert wäre
#   das eine Resilienz von 0 (außerhalb des Wertebereichs -> kaum interpretierbar).
#   Nach Zentrierung bedeutet Resilienz_c = 0 den MITTLEREN Resilienzwert ->
#   die Koeffizienten werden inhaltlich sinnvoll interpretierbar.
df$Resilienz_c = df$Resilienz - mean(df$Resilienz)
modc = lm(BDI_T0 ~ Resilienz_c * Geschlecht, data = df)
summary(modc)
# HINWEIS: Modellgüte (R², F-Test, Vorhersagegenauigkeit) bleibt identisch zu
#   mod — nur die Interpretation der einzelnen Koeffizienten ändert sich.


# --- 4b) Visualisierung: Regressionsgeraden je Gruppe ---------------------

# ZEILE-FÜR-ZEILE:
ggplot(data = df, aes(x = Resilienz, y = BDI_T0, colour = Geschlecht)) +
  # x = Prädiktor, y = Kriterium, colour = Geschlecht -> je Gruppe eine Farbe.
  geom_point() +
  # Rohdaten als Streudiagramm (jede Person = ein Punkt).
  geom_smooth(method = "lm")
  # Regressionsgerade je Farbgruppe (method = "lm" -> lineare Anpassung).
  #   Das graue Band = 95%-Konfidenzband um die Gerade.
  #   -> Visualisiert die Interaktion: unterschiedliche Steigungen = Moderation.
# HINWEIS: Hier zeigen wir Resilienz UNzentriert (x-Achse bleibt interpretierbar
#   in Originaleinheiten). Die Geraden sind identisch zur zentrierten Version,
#   nur der Nullpunkt der x-Achse verschiebt sich.


# --- 4c) Modellannahmen der Regression prüfen -----------------------------

# ERKLÄRUNG: Normalverteilung der RESIDUEN (nicht der Rohdaten!).
qqnorm(rstandard(modc), cex = 1.5)   # Q-Q-Plot der standardisierten Residuen
qqline(rstandard(modc))              # Referenzgerade: Punkte sollten draufliegen
shapiro.test(rstandard(modc))        # formaler Test (auf Residuen anwenden)

# ERKLÄRUNG: Homoskedastizität = konstante Streuung der Residuen.
#   plot(modc, 1) zeigt "Residuals vs Fitted" -> wir wollen eine strukturlose
#   Punktwolke (keinen Trichter, keine Kurve).
plot(modc, 1, cex = 2)

# ERKLÄRUNG: Multikollinearität — sind Prädiktoren zu stark korreliert?
#   VIF-Faustregeln: > 5 bedenklich, > 10 problematisch.
olsrr::ols_vif_tol(modc)

# TIPP / ERKLÄRUNG: Statt vieler Einzelbefehle alle Annahmen auf einmal.
#   check_model() liefert ein Panel (Linearität, Homoskedastizität,
#   Normalität der Residuen, Einflussreiche Fälle, Kollinearität) — ideal
#   für die Lehre, weil jede Teilgrafik eine Annahme illustriert.
library(performance)
check_model(modc)


# --- 4d) Trendanalyse / einfache Steigungen -------------------------------

# ERKLÄRUNG: emtrends() zerlegt die Interaktion in "simple slopes": Wie groß
#   ist die Resilienz-Steigung GETRENNT je Geschlecht — und ist sie je
#   signifikant von 0 verschieden?
trend = emtrends(modc, specs = ~ Geschlecht, var = "Resilienz_c")
test(trend)
# HINWEIS: Das ist die saubere Anschlussfrage an eine signifikante Interaktion:
#   "In welcher Gruppe wirkt der Prädiktor (wie stark)?"


# ============================================================================
# TABELLEN: Publikationsfertige Ergebnistabelle (am Beispiel der Regression)
# ----------------------------------------------------------------------------
# install.packages(c("broom", "flextable"))   # einmalig
library(broom)        # tidy(): Modell-Output -> aufgeräumter Data Frame
library(flextable)    # formatierte Tabellen, Export nach Word/PNG/PPTX

# ERKLÄRUNG: tidy() zieht aus dem lm-Objekt eine Zeile pro Prädiktor mit
#   Schätzer, SE, t-Wert und p. conf.int = TRUE ergänzt das 95%-CI.
tab <- tidy(modc, conf.int = TRUE)
tab   # Spalten: term, estimate, std.error, statistic, p.value, conf.low, conf.high

# ERKLÄRUNG: Lesbare Labels statt der Roh-Termnamen (z. B. "GeschlechtWeiblich").
# HINWEIS: Wird hier per Position zugewiesen -> Reihenfolge muss zur tidy-Ausgabe
#   passen. Bei modc = BDI_T0 ~ Resilienz_c * Geschlecht ist die Reihenfolge:
#   Intercept, Resilienz_c, GeschlechtWeiblich, Interaktion.
tab$term <- c("(Intercept)",
              "Resilienz (zentriert)",
              "Geschlecht (Weiblich)",
              "Resilienz x Geschlecht")

# Tabelle Schritt für Schritt aufbauen (ohne Pipe)
ft <- flextable(tab)

# metrische Spalten auf 2 Nachkommastellen runden
ft <- colformat_double(ft, j = c("estimate","std.error","statistic","conf.low","conf.high"),
                       digits = 2)

# p-Werte auf 3 Stellen
ft <- colformat_double(ft, j = "p.value", digits = 3)

# verständliche Spaltenköpfe statt der Rohnamen
ft <- set_header_labels(ft, term = "Prädiktor", estimate = "b", std.error = "SE",
                        statistic = "t", p.value = "p",
                        conf.low = "lower", conf.high = "upper")

# Spaltenbreiten automatisch an den Inhalt anpassen
ft <- autofit(ft)

ft   # in RStudio im Viewer-Tab gerendert


# Tabelle als Word-Datei exportieren
save_as_docx(ft, path = "regressionstabelle.docx")

# ============================================================================
# 5) ANOVA MIT MESSWIEDERHOLUNG (within-subjects)
#    Frage: Verändert sich BDI über die drei Zeitpunkte (T0, T1, T2)?
# ----------------------------------------------------------------------------

# ERKLÄRUNG: Für Messwiederholung brauchen wir LONG-Format:
#   eine Zeile pro Person UND Zeitpunkt (statt drei BDI-Spalten nebeneinander).

# Schritt 1: relevante Spalten auswählen (noch im wide-Format).
df_wide = df[, c("ID", "Gruppe", "Klinik", "BDI_T0", "BDI_T1", "BDI_T2")]

# Schritt 2: wide -> long mit pivot_longer().
# KORREKTUR: Im Original stand data = mixed_d — dieses Objekt wurde nie
#            erzeugt. Richtig ist df_wide.
#   cols       = welche Spalten "zusammengefaltet" werden.
#   names_to   = neue Spalte für die alten Spaltennamen (-> "Time").
#   values_to  = neue Spalte für die Werte (-> "BDI").
df_long = pivot_longer(data = df_wide,
                       cols = c("BDI_T0", "BDI_T1", "BDI_T2"),
                       names_to = "Time",
                       values_to = "BDI")

# Schritt 3: Time als FAKTOR mit definierter Reihenfolge.

df_long$Time <- factor(df_long$Time, levels = c("BDI_T0", "BDI_T1", "BDI_T2"))

# ERKLÄRUNG: within = "Time" -> Zeit ist Innersubjektfaktor (jede Person
#   liefert alle drei Werte). aov_ez kümmert sich um die Fehlerstruktur.
model = aov_ez(dv = "BDI", within = c("Time"), id = "ID", data = df_long)
summary(model)
# HINWEIS: aov_ez gibt bei Within-Designs automatisch den Mauchly-Test auf
#   SPHÄRIZITÄT sowie korrigierte p-Werte (Greenhouse-Geisser / Huynh-Feldt)
#   aus. Genau das macht base aov()/anova() NICHT komfortabel — ein weiterer
#   Grund für aov_ez().

# Post-hoc: welche Zeitpunkte unterscheiden sich?
emmeans::emmeans(model, pairwise ~ Time)


# --- 5b) Visualisierung: Verlaufskurve über die Zeit ----------------------

# ZEILE-FÜR-ZEILE:
ggplot(data = df_long, aes(x = Time, y = BDI)) +
  # geom_line(aes(group = ID), color = "gray80", alpha = 0.5) +
  #   ^ optional auskommentiert: dünne Spaghetti-Linien je Person als
  #     Hintergrund (zeigt die individuelle Streuung der Verläufe).
  stat_summary() +
  # ohne Argumente: Default ist Mittelwert + Fehlerbalken (mean_se) pro Zeitpunkt.
  stat_summary(geom = "line", aes(group = 1))
  # verbindet die Gruppen-Mittelwerte zu einer Linie.
  # HINWEIS: group = 1 ist nötig, weil x (Time) ein Faktor ist. Ohne group
  #   "weiß" ggplot nicht, dass die drei Punkte EINE Linie bilden sollen, und
  #   zeichnet gar keine Linie.


# SPAGHETTIPLOT: individuelle Verläufe + mittlerer Verlauf hervorgehoben
# ZEILE-FÜR-ZEILE:
p_spaghetti <- ggplot(data = df_long, aes(x = Time, y = BDI)) +
  geom_line(aes(group = ID), colour = "grey75", alpha = 0.5, linewidth = 0.4) +
  # ^ EINE Linie pro Person: group = ID verbindet die 3 Messwerte derselben ID.
  #   grau + halbtransparent + dünn -> bleibt dezenter Hintergrund ("Spaghetti").
  stat_summary(fun = mean, geom = "line", aes(group = 1),
               colour = "#C0392B", linewidth = 1.4) +
  # ^ mittlerer Verlauf: fun = mean berechnet den Mittelwert je Zeitpunkt,
  #   geom = "line" + group = 1 verbindet sie zu EINER Linie (dick & farbig).
  stat_summary(fun = mean, geom = "point",
               colour = "#C0392B", size = 3) +
  # ^ Mittelwert zusätzlich als Punkt je Zeitpunkt (hebt die Stützstellen hervor).
  stat_summary(fun.data = mean_se, geom = "errorbar",
               colour = "#C0392B", width = 0.1, linewidth = 0.8) +
  # ^ Fehlerbalken (Mittelwert ± 1 SE) um den mittleren Verlauf.
  scale_x_discrete(labels = c("BDI_T0" = "T0", "BDI_T1" = "T1", "BDI_T2" = "T2")) +
  # ^ kürzere Achsenbeschriftung (kosmetisch).
  labs(title = "Individuelle BDI-Verläufe über die Zeit",
       subtitle = "Graue Linien = einzelne Personen · rot = mittlerer Verlauf (± 1 SE)",
       x = "Zeitpunkt", y = "BDI") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

p_spaghetti

ggsave("spaghetti_bdi.pdf", plot = p_spaghetti,
       width = 16, height = 12, units = "cm", device = pdf)


# ============================================================================
# 6) MIXED DESIGN — within (Zeit) x between (Gruppe)
#    Frage: Verlaufen die Gruppen über die Zeit UNTERSCHIEDLICH? (Interaktion)
# ----------------------------------------------------------------------------


# ERKLÄRUNG: within = Zeit (Innersubjekt), between = Gruppe (Zwischensubjekt).
#   Der spannende Term ist die Interaktion Time x Gruppe = unterschiedlicher
#   Verlauf je Therapie.
model = aov_ez(dv = "BDI",
               within = c("Time"),
               between = "Gruppe",
               id = "ID",
               data = df_long)
summary(model)

emmeans(model, pairwise ~ Time)            # Verlauf über Zeit (gemittelt)
emmeans(model, pairwise ~ Gruppe)          # Gruppenunterschied (gemittelt)
emmeans(model, pairwise ~ Time * Gruppe)   # zellenweise -> Kern bei Interaktion


# --- 6b) Visualisierung: Interaktionsverlauf ------------------------------

# ZEILE-FÜR-ZEILE:
ggplot(data = df_long, aes(x = Time, y = BDI, colour = Gruppe)) +
  # colour = Gruppe -> eine Verlaufskurve je Therapiegruppe.
  stat_summary() +
  # Mittelwert ± SE je Zeitpunkt UND Gruppe.
  stat_summary(geom = "line", aes(group = Gruppe)) +
  # verbindet die Mittelwerte INNERHALB jeder Gruppe (group = Gruppe).
  labs(x = "Zeitpunkt", y = "Depressivität") +
  # Achsenbeschriftung lesbar machen (statt der Rohnamen).
  ggtitle("Interaktion Zeit x Gruppe")
  # Titel der Abbildung.


# ============================================================================
# 7) LINEAR MIXED MODEL (LMM) — die flexible Alternative
# ----------------------------------------------------------------------------
library(lmerTest)   # lmer() + p-Werte über Satterthwaite-Freiheitsgrade

# ERKLÄRUNG: Zeit als ZAHL (0, 1, 2) statt Faktor.
#   as.numeric(Time) liefert 1/2/3 (Faktorstufen) -> "-1" macht daraus 0/1/2.
#   Dadurch modellieren wir einen LINEAREN Zeittrend (eine Steigung) statt
#   dreier separater Gruppenmittel.
df_long$Time_num <- as.numeric(df_long$Time) - 1

# ERKLÄRUNG der Modellformel:
#   Time_num * Gruppe -> linearer Zeittrend + Unterschied im Trend je Gruppe.
#   (1 | ID)          -> zufälliger Intercept je Person (Messwiederholung:
#                        wiederholte Werte derselben Person sind abhängig).
#   (1 | Klinik)      -> zufälliger Intercept je Standort (Mehrebenenstruktur).
mod_lmm = lmer(BDI ~ Time_num * Gruppe +
                 (1 | Klinik) +
                 (1 | ID),
               data = df_long)
summary(mod_lmm)

# HINWEIS 1 (Konzept): Das LMM ist die allgemeinere Variante der RM-ANOVA.
#   Vorteile: verträgt unbalancierte/fehlende Daten, erlaubt kontinuierliche
#   Zeit (Steigungen!) und mehrere Zufallsebenen gleichzeitig.
# HINWEIS 2 (typische Rückfrage): Klinik hat nur 3 Stufen. Für einen
#   zufälligen Effekt sind ~5+ Stufen üblich, um die Varianz stabil zu
#   schätzen. Mit 3 Stufen ist (1 | Klinik) diskutabel — Alternative: Klinik
#   als FESTER Effekt. Gut, das im Kurs offen anzusprechen.
# HINWEIS 3: Achte in summary() auf "boundary (singular) fit" — ein Zeichen,
#   dass eine Varianzkomponente (z. B. Klinik) ~0 geschätzt wird.

# ----------------------------------------------------------------------------
# 7a) RANDOM INTERCEPT vs. RANDOM SLOPE — was variiert zwischen den Einheiten?
# ----------------------------------------------------------------------------
# ERKLÄRUNG: Das Modell oben hat NUR Random Intercepts: ( 1 | ... ).
#   Die "1" links vom | steht für den Intercept (= Ausgangsniveau).
#   -> Jede Person / jede Klinik darf ihr eigenes NIVEAU haben, aber ALLE
#      teilen sich dieselbe Steigung (den mittleren Time_num-Effekt).
#
#   Allgemeines Schema:   ( was_variiert | über_welche_Einheiten )
#     (1 | ID)            -> nur Random Intercept (Niveau variiert, Slope fix)
#     (Time_num | ID)     -> Random Intercept + Random Slope für Time
#                            (Kurzform für (1 + Time_num | ID))
#     (0 + Time_num | ID) -> nur Random Slope, kein Random Intercept (selten)
#
# FAUSTREGEL (woran man es festmacht): Ein Random Slope ist nur für Prädiktoren
#   sinnvoll/schätzbar, die INNERHALB der Gruppierungseinheit variieren.
#     - Time_num variiert innerhalb jeder Person   -> (Time_num | ID) möglich
#     - Gruppe ist pro Person konstant (between)   -> KEIN Slope auf ID-Ebene

# Random-Slope-Modell: Personen dürfen sich zusätzlich darin unterscheiden,
#   WIE STARK/SCHNELL sich ihr BDI über die Zeit verändert.
mod_rs = lmer(BDI ~ Time_num * Gruppe +
                (1 | Klinik) +
                (Time_num | ID),   # <- Intercept + Slope je Person
              data = df_long)
summary(mod_rs)

# HINWEIS 4 (Output Random-Slope-Modell): Im "Random effects"-Block taucht jetzt
#   eine ZUSÄTZLICHE Zeile "Time_num" mit eigener Varianz auf PLUS eine Spalte
#   "Corr" = Korrelation zwischen Intercept und Slope je Person.
#     - Slope-Varianz deutlich > 0 -> Verbesserungsraten streuen zwischen Personen.
#     - Corr nahe -1 / +1 oder "singular fit"-Warnung -> Modell überparametrisiert
#       (oft bei nur 3 Zeitpunkten je Person). Dann ist das simplere Modell die
#       ehrlichere Wahl.

# ----------------------------------------------------------------------------
# 7b) MODELLVERGLEICH: Lohnt sich der Random Slope überhaupt?
# ----------------------------------------------------------------------------
# ERKLÄRUNG: anova() auf zwei lmer-Objekte führt einen Likelihood-Ratio-Test
#   durch (refittet automatisch mit ML statt REML, da sich die FIXED effects
#   nicht unterscheiden -> Vergleich der RANDOM-Struktur ist zulässig).
#   Frage: Erklärt das komplexere Modell (mit Slope) signifikant mehr?
anova(mod_lmm, mod_rs)

# SO LIEST DU DEN VERGLEICH:
#   - npar  : Anzahl Modellparameter (mod_rs hat mehr -> Slope-Varianz + Corr).
#   - AIC / BIC : niedriger = besser (Strafterm für Komplexität). BIC bestraft
#                 schärfer. Faustregel: Differenz > ~2-6 ist beachtenswert.
#   - logLik / deviance : Modellgüte (höher logLik = besser).
#   - Chisq + Pr(>Chisq) : der eigentliche LRT.
#       p < .05  -> Random Slope verbessert SIGNIFIKANT -> mod_rs behalten
#                   (Verbesserungsraten variieren bedeutsam zwischen Personen).
#       p >= .05 -> kein Mehrwert -> das sparsamere mod_lmm bevorzugen
#                   (Occam: nicht ohne Grund Parameter verbrennen).
#
# HINWEIS 5: Eine "singular fit"-Warnung bei mod_rs ist selbst eine Antwort:
#   Die Daten stützen den Random Slope nicht -> beim einfacheren Modell bleiben.
# HINWEIS 6: Der LRT auf Varianzkomponenten testet am Rand des Parameterraums
#   (Varianz >= 0) und ist daher leicht KONSERVATIV (echtes p tendenziell etwas
#   kleiner). Für den Kurs unkritisch, aber gut, es erwähnt zu haben.
# ============================================================================
# 


# 8) GGPLOT-GALERIE — Visualisierung vertiefen
#    Referenz-Cheatsheet: https://rstudio.github.io/cheatsheets/data-visualization.pdf
# ----------------------------------------------------------------------------

# --- 8a) Interaktionsplot mit vollständiger Beschriftung & Theme ----------
# ZEILE-FÜR-ZEILE:
ggplot(data = df, aes(x = Resilienz_c, y = BDI_T0, colour = Geschlecht)) +
  geom_point() +                        # Rohdaten (Streudiagramm)
  geom_smooth(method = "lm") +          # Regressionsgerade + 95%-Band je Gruppe
  labs(title = "Interaktion: Resilienz x Geschlecht",   # Haupttitel
       subtitle = "Vorhersage der Depression (BDI Baseline)",  # Untertitel
       x = "Resilienz (zentriert)",     # x-Achsentitel
       y = "BDI Score (T0)",            # y-Achsentitel
       colour = "Geschlecht") +         # Titel der Farb-Legende
  theme_minimal()
  # theme_minimal(): reduziertes, "sauberes" Layout (kein grauer Hintergrund).


ggplot(data = df, aes(x = Resilienz_c, y = BDI_T0, colour = Geschlecht)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_manual(values = c("Männlich" = "#2C7FB8",   # Farbe Gruppe 1
                                 "Weiblich" = "#D95F0E")) + # Farbe Gruppe 2
  labs(title = "Interaktion: Resilienz x Geschlecht",
       subtitle = "Vorhersage der Depression (BDI Baseline)",
       x = "Resilienz (zentriert)",
       y = "BDI Score (T0)",
       colour = "Geschlecht") +
  theme_minimal()


# --- 8b) Histogramm -------------------------------------------------------
# ERKLÄRUNG: geom_histogram() teilt eine metrische Variable in Klassen (bins)
#   und zählt die Häufigkeiten -> Verteilungsform auf einen Blick.
ggplot(data = df, aes(x = BDI_T0)) +
  geom_histogram()
# TIPP: Ohne Angabe nimmt ggplot 30 bins (mit Konsolen-Hinweis). Besser explizit:
# ggplot(data = df, aes(x = BDI_T0)) +
#   geom_histogram(bins = 20, colour = "white")   # bins ODER binwidth setzen

names(df)   # Spaltennamen nachschlagen (praktisch beim Plotten)




# --- 8c) Balkendiagramm der Mittelwerte (1 Gruppierung) -------------------
# HINWEIS: Im Original mit "# Histogramm" überschrieben — ist aber ein
#   BALKENDIAGRAMM (geom = "bar"). Kommentar hier korrigiert.
# ERKLÄRUNG: stat_summary(geom = "bar", fun = mean) -> Balkenhöhe = Mittelwert
#   je Klinik (NICHT Häufigkeit, anders als geom_bar()/geom_col()).

ggplot(data = df, aes(x = Klinik, y = change_bdi)) +
  stat_summary(geom = "bar", fun = mean)

# --- 8d) Balkendiagramm mit zweiter Gruppierung (gruppiert/dodged) --------
# ZEILE-FÜR-ZEILE:
ggplot(data = df, aes(x = Klinik, y = change_bdi, fill = Gruppe)) +
  # fill = Gruppe -> Balken werden je Therapiegruppe eingefärbt.
  stat_summary(geom = "bar", fun = mean, position = position_dodge2(.95))
  # position_dodge2() stellt die Gruppenbalken NEBENEINANDER (statt gestapelt).
  #   Der Wert (~0.95) steuert die Balkenbreite/den Abstand.


# --- 8e) Boxplot ----------------------------------------------------------
# ERKLÄRUNG: Boxplot zeigt Median (Linie), Interquartilsabstand (Box, 25.–75.
#   Perzentil), Whisker (i. d. R. 1.5*IQR) und Ausreißer (Punkte) — robust
#   gegenüber Ausreißern und super zum Gruppenvergleich.
ggplot(data = df, aes(x = Klinik, y = change_bdi)) +
  geom_boxplot()

# --- 8f) Teilgraphen (Facetting) ------------------------------------------
# ERKLÄRUNG: facet_grid() zerlegt EINEN Plot in mehrere kleine Panels nach
#   einer Variable -> bequemer Mehrgruppenvergleich ("small multiples").

# Variante COLS: Kliniken nebeneinander (in Spalten).
ggplot(data = df, aes(x = Resilienz_c, y = BDI_T0, colour = Geschlecht)) +
  geom_point(alpha = 0.6) +             # alpha < 1 -> halbtransparent (Überlapp sichtbar)
  geom_smooth(method = "lm") +          # Regressionsgerade je Geschlecht
  facet_grid(cols = vars(Klinik)) +     # ein Panel je Klinik, horizontal angeordnet
  theme_bw() +                          # weißer Hintergrund + Rahmen um jede Facette
  labs(title = "Zusammenhang Resilienz & Depression nach Standort",
       subtitle = "Vergleich der Kliniken (Facet Grid)",
       x = "Resilienz (zentriert)",
       y = "BDI Score (T0)")

# Variante ROWS: dieselben Panels untereinander (in Zeilen).
ggplot(data = df, aes(x = Resilienz_c, y = BDI_T0, colour = Geschlecht)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(Klinik)) +     # ein Panel je Klinik, vertikal angeordnet
  theme_bw() +
  labs(title = "Zusammenhang Resilienz & Depression nach Standort",
       subtitle = "Vergleich der Kliniken (Facet Grid)",
       x = "Resilienz (zentriert)",
       y = "BDI Score (T0)")
