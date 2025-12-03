
df = readRDS(url("https://raw.githubusercontent.com/NicoSteffen/Fit-fuer-R_Statistische_Modellierung_Visualisierung/main/klinische_patientendaten.rds"))

str(df)
View(df)


# Einfaktorielle ANOVA ----------------------------------------------------

df$change_bdi = df$BDI_T0 - df$BDI_T1

# Voraussetzungen

# varianzhomogenität / Homoskedastizität - Levene test 

library(car)

car::leveneTest(change_bdi ~ Klinik, data = df, center ="mean")

# NV - Shapiro Wilk

by(df$change_bdi, df$Klinik, shapiro.test)

mod = lm(change_bdi ~ Klinik, data = df)
anova(mod)

library(afex)
aov_ez(id = "ID", data = df, dv = "change_bdi", between = "Klinik")

# Post Hoc
library(emmeans)
emmeans(mod, pairwise ~ Klinik)


# kruskal Wallis / Rangplätze 

kruskal.test(change_bdi ~ Klinik, data = df)
pairwise.wilcox.test(df$change_bdi, df$Klinik, p.adjust.method = "holm")


library(ggplot2)

ggplot(data = df, aes(x = Klinik, y = change_bdi)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  stat_summary(geom = "point", fun = mean, size = 3, color = "green")



# Mehrfaktorielle ANOVA ---------------------------------------------------


mod = lm(change_bdi ~ Klinik * Gruppe, data = df)
anova(mod)

# Empfehlung: afex

afex::aov_ez(data = df, id = "ID", between = c("Klinik", "Gruppe"), dv = "change_bdi")

emmeans(mod, pairwise ~ Klinik)
emmeans(mod, pairwise ~ Gruppe)
emmeans(mod, pairwise ~ Klinik * Gruppe)

ggplot(data = df, aes(x = Klinik, y = change_bdi, colour = Gruppe)) +
  stat_summary() +
  geom_point()



# Multiple Regression -----------------------------------------------------


mod = lm(BDI_T0 ~ Resilienz * Geschlecht, data = df)
summary(mod)

# Grand Mean Zentrierung
df$Resilienz_c = df$Resilienz - mean(df$Resilienz)

mod = lm(BDI_T0 ~ Resilienz_c * Geschlecht, data = df)
summary(mod)

trend = emmeans::emtrends(mod, specs = ~ Geschlecht, var = "Resilienz_c")
test(trend)

# NV der Residuen 

qqnorm(rstandard(mod), cex = 1.5)
qqline(rstandard(mod))

shapiro.test(rstandard(mod))

# Homoskedastizität

plot(mod, 1, cex = 2)

# Multikollinearität
olsrr::ols_vif_tol(mod)

library(performance)
check_model(mod)

ggplot(data = df, aes(x = Resilienz_c, y = BDI_T0, colour = Geschlecht)) +
  geom_point() +
  geom_smooth(method = "lm")



# ANOVA mit Messwiederholung ----------------------------------------------

library(tidyr)

df_wide = df[,c("ID","Gruppe", "Klinik", "BDI_T0","BDI_T1",   "BDI_T2")]

df_long = pivot_longer(data = df_wide, 
                       cols = c("BDI_T0", "BDI_T1", "BDI_T2"), 
                       names_to = "Time", 
                       values_to = "BDI")

df_long$Time = factor(df_long$Time, levels = c("BDI_T0","BDI_T1", "BDI_T2"))

mod = afex::aov_ez(id = "ID", data = df_long, within = c("Time"), dv = "BDI")
summary(mod)

emmeans(mod, pairwise ~ Time)


ggplot(data = df_long, aes(x = Time, y = BDI )) +
  geom_line(aes(group = ID), color = "green", alpha = .5) +
  stat_summary() 



# Mixed Design  -----------------------------------------------------------


mixed = aov_ez(dv = "BDI", within = c("Time"), between = "Gruppe", id = "ID", data = df_long)
summary(mixed)


ggplot(data = df_long, aes(x = Time, y = BDI, colour = Gruppe)) +
  stat_summary() +
  stat_summary(geom = "line", aes(group = Gruppe)) +
  labs(x = "Zeitpunkt", y = "Depressivität") +
  ggtitle( "Interaktion Gruppe x Zeit")









