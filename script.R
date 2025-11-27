# https://stephangoerigk.github.io/CFH_R_bookdown/über-dieses-skript.html
# https://github.com/NicoSteffen/Fit-fuer-R_Statistische_Modellierung_Visualisierung

df = readRDS(url("https://raw.githubusercontent.com/NicoSteffen/Fit-fuer-R_Statistische_Modellierung_Visualisierung/main/klinische_patientendaten.rds"))

str(df)


# Einfaktorielle ANOVA
# Unterscheidet sich delta BDI zwischen den Standorten 

df$change_bdi = df$BDI_T0 - df$BDI_T1

# Voraussetzungen testen

car::leveneTest(change_bdi ~ Klinik, data = df, center = "mean")
by(df$change_bdi, df$Klinik, shapiro.test)

hist(df$change_bdi[df$Klinik == "München"])

table(df$Klinik)

# Zwar signifikant aber i.d.R ANOVA robust gegenüber Verletzungen der NV
# besonders bei n > 30
# Alternativ Kruskal-Wallis mit Wilcoxon (Rangsummen)


mod = lm(change_bdi ~ Klinik, data = df)
anova(mod)

kruskal.test(change_bdi ~ Klinik, data = df)

# aov

library(afex)
mod = aov_ez(dv = "change_bdi", between = "Klinik", id = "ID", data = df)




# post hoc

library(emmeans)
emmeans(mod, pairwise ~ Klinik)

pairwise.wilcox.test(df$change_bdi, 
                                 df$Klinik, 
                                 p.adjust.method = "holm")


# Einfaktorielle
library(ggplot2)

ggplot(data = df, aes(x = Klinik, y = change_bdi)) +
  stat_summary(fun.data = mean_se,  geom = "errorbar") +
  stat_summary(geom = "point", fun = mean) 



# Mehrfaktorielle ANOVA

mod = lm(change_bdi ~ Klinik * Gruppe, data = df)
anova(mod)

# Effekt der Reihenfolge
# balancierte Gruppen als Voraussetzung!! 

# Der sichere Weg 
mod = aov_ez(dv = "change_bdi", between = c("Klinik", "Gruppe"), id = "ID", data = df)
mod


emmeans(mod, pairwise ~ Klinik)
emmeans(mod, pairwise ~ Gruppe)
emmeans(mod, pairwise ~ Klinik * Gruppe)


# Zweifaktorielle
ggplot(data = df, aes(x = Klinik, y = change_bdi, colour = Gruppe)) +
  stat_summary(fun.data = mean_se,  geom = "errorbar") +
  stat_summary(geom = "point", fun = mean) 




# lineare Regression 

mod = lm(BDI_T0 ~ Resilienz * Geschlecht, data = df)
summary(mod)

df$Resilienz_c = df$Resilienz - mean(df$Resilienz)
modc = lm(BDI_T0 ~ Resilienz_c * Geschlecht, data = df)
summary(modc)


ggplot(data = df, aes(x = Resilienz, y = BDI_T0, colour = Geschlecht)) +
  geom_point() +
  geom_smooth(method = "lm")


# Voraussetzungen

# NV der residuen
qqnorm(rstandard(modc), cex = 1.5)
qqline(rstandard(modc))
shapiro.test(rstandard(modc))


# Homoskedastizität
plot(modc, 1, cex = 2)

# Multikollinearität (nicht über 10 oder 5)
olsrr::ols_vif_tol(modc)


# Statt vieler Einzelbefehle:
library(performance)
check_model(modc)






# Trendanalyse 

trend = emtrends(modc, specs = ~ Geschlecht, var = "Resilienz_c")
test(trend)


# ANOVA mit Messwiederholung 

library(tidyr)
df_wide = df[,c("ID", "BDI_T0", "BDI_T1", "BDI_T2")]
df_long = as.data.frame(pivot_longer(data = df_wide, 
                                             cols = c("BDI_T0", "BDI_T1", "BDI_T2"), 
                                             names_to = "Time", 
                                             values_to = "BDI"))

df_long$Time <- factor(df_long$Time, levels = c("BDI_T0", "BDI_T1", "BDI_T2"))

model = aov_ez(dv = "BDI", within = c("Time"), id = "ID", data = df_long)
summary(model)

emmeans::emmeans(model, pairwise ~ Time, adjust = "Tukey")


ggplot(data = df_long, aes(x = Time, y = BDI)) +
  #geom_line(aes(group = ID), color = "gray80", alpha = 0.5) +
  stat_summary() +
  stat_summary(geom = "line", aes(group = 1))




 
# Mixed design 

mixed_d = df[,c("ID","Gruppe","Klinik", "BDI_T0", "BDI_T1", "BDI_T2")]

mixed_d_long <- pivot_longer(data = mixed_d, 
                             cols = c("BDI_T0", "BDI_T1", "BDI_T2"), # Nur diese werden umgeformt
                             names_to = "Time", 
                             values_to = "BDI")

mixed_d_long$Time <- factor(mixed_d_long$Time, levels = c("BDI_T0", "BDI_T1", "BDI_T2"))


model = aov_ez(dv = "BDI", 
               within = c("Time"), 
               between = "Gruppe", 
               id = "ID", 
               data = mixed_d_long)


summary(model)

emmeans(model, pairwise ~ Time)
emmeans(model, pairwise ~ Gruppe)
emmeans(model, pairwise ~ Time * Gruppe)


ggplot(data = mixed_d_long, aes(x = Time, y = BDI, colour = Gruppe)) +
  stat_summary() +
  stat_summary(geom = "line", aes(group = Gruppe)) +
  labs(x = "Zeitpunkt", y = "Depressivität") +
  ggtitle ("Interaktion Zeit x Gruppe")


# LMM

library(lmerTest)

mixed_d_long$Time_num <- as.numeric(mixed_d_long$Time) - 1


mod_lmm = lmer(BDI ~ Time_num * Gruppe +
                (1 | Klinik) +
                 (1 | ID), 
               data = mixed_d_long)

summary(mod_lmm)


# ggplot wenn noch zeit 

ggplot(data = diamonds, aes(x = carat, y = price, colour = cut)) +
  geom_point() +
  geom_smooth(method = "lm")

# histogramm

ggplot(data = diamonds, aes(x = price)) +
  geom_histogram()

ggplot(data = diamonds, aes(x = price)) +
  geom_histogram(binwidth = 30)

# Histogramm

ggplot(data = diamonds, aes(x = cut, y = carat)) +
  stat_summary(geom = "bar", fun = mean) 

# Ballkendiagramm mit mehr als 3 variablen 

ggplot(data = diamonds, aes(x = cut, y = carat, fill = clarity)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge2(.95))

# boxplot

ggplot(data = diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()

# Teilgraphen 

ggplot(data = diamonds, aes(x = carat, y = price, colour = cut)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(cols = vars(cut))

ggplot(data = diamonds, aes(x = carat, y = price, colour = cut)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(cut))


