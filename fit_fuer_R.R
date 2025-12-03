df = readRDS(url("https://raw.githubusercontent.com/NicoSteffen/Fit-fuer-R_Statistische_Modellierung_Visualisierung/main/klinische_patientendaten.rds"))

str(df)
View(df)


# Einfaktorielle ANOVA ----------------------------------------------------

# Unterschiedet sich delta BDI zwischen den Standorten 

df$BDI_change = df$BDI_T0 - df$BDI_T1

mod = lm(BDI_change ~ Klinik, data = df)
anova(mod)


# empfehlung: mit package afex 

library(afex)

aov_ez(dv = "BDI_change", between = "Klinik", id ="ID", data = df)


# Test Voraussetzungen 

# Varianzhomogenität (Homoskedastizität) - Levene's Test 

car::leveneTest(BDI_change ~ Klinik, data = df, center = "mean")

# NV - shapiro Wilk

by(df$BDI_change, df$Klinik, shapiro.test)


# Kruskal Wallis /  - Rangsummen 

kruskal.test(BDI_change ~ Klinik, data = df)


# Post Hoc Test 

library(emmeans)

emmeans(mod, pairwise ~ Klinik)

pairwise.wilcox.test(df$BDI_change, df$Klinik, p.adjust.method = "holm")


library(ggplot2)

ggplot(data = df, aes(x = Klinik, y = BDI_change)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  stat_summary(geom = "point", fun = mean, size = 3, color = "red")
  


# Mehrfaktorielle ANOVA  --------------------------------------------------

mod = lm(BDI_change ~ Klinik * Gruppe, data = df)
anova(mod)

# Empfehlung afex

afex::aov_ez(id = "ID", data = df, dv = "BDI_change", between = c("Klinik", "Gruppe"))

emmeans::emmeans(mod, pairwise ~ Klinik)
emmeans::emmeans(mod, pairwise ~ Gruppe)
emmeans::emmeans(mod, pairwise ~ Klinik * Gruppe)

ggplot(data = df, aes(x = Klinik, y = BDI_change, colour = Gruppe)) +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  stat_summary(geom = "point", fun = "mean")
  





names(df)
