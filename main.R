neonatalcare_data = read.csv("neonati.csv")

str(neonatalcare_data)
head(neonatalcare_data)

#descriptive analysis
#Anni.Madre variable

attach(neonatalcare_data)
library("gmodels")
CrossTable(Anni.madre)
summary(Anni.madre)

agen_distribution = table(Anni.madre)
agen_frtable = pln_distribution/sum(agen_distribution)
age_cumsum = cumsum(agen_distribution)
age_cumrel = age_cumsum/sum(agen_distribution)

final_agetable = cbind(ni=agen_distribution, fi=agen_frtable, Ni=age_cumsum, Fi=age_cumrel)
colnames(final_agetable) = c("Absolute Fr.", "Relative Fr.", "Cumulated absolute fr.", "Cumulated relative fr.")
knitr::kable(final_agetable)

#Relevant measures of shape and central tendency calculation

library("moments")
mother_age_data = data.frame(
  Skewness = round(skewness(Anni.madre), 2), 
  Kurtosis = round(kurtosis(Anni.madre)-3, 2),
  St_dev = round(sd(Anni.madre), 2)
)

knitr::kable((mother_age_data), "pipe")

#density plot Anni.madre

library("ggplot2")
ggplot()+
  geom_density(aes(x=Anni.madre), col="black", fill="turquoise")+
  geom_vline(xintercept = mean(Anni.madre), linewidth=1.5, color = "lightcoral")+
  labs(x="Woman in labour age", y="Density", title = "Anni.madre variable", subtitle = "Distribution density curve")+
  geom_text(aes(x = mean(Anni.madre) + 5, y = 0.075, label = sprintf("Mean\n %.3f", mean(Anni.madre))), inherit.aes = FALSE) +
  ylim(0, 0.08)+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#N.gravidanze variable

birthn_distribution = table(N.gravidanze)
birthn_frtable = birthn_distribution/sum(birthn_distribution)
birth_cumsum = cumsum(birthn_distribution)
birth_cumrel = birth_cumsum/sum(birthn_distribution)

final_birthtable = cbind(ni=birthn_distribution, fi=birthn_frtable, Ni=birth_cumsum, Fi=birth_cumrel)
colnames(final_birthtable) = c("Absolute Fr.", "Relative Fr.", "Cumulated absolute fr.", "Cumulated relative fr.")
knitr::kable(final_birthtable)

summary(N.gravidanze)

prior_birth_data = data.frame(
  Skewness = round(skewness(N.gravidanze), 2), 
  Kurtosis = round(kurtosis(N.gravidanze)-3, 2),
  St_dev = round(sd(N.gravidanze), 2)
)

knitr::kable((prior_birth_data), "pipe")

ggplot() +
  geom_bar(
    aes(x = as.factor(N.gravidanze)),
    fill = "lightblue", color = "black", alpha = 0.7
  ) +
  labs(
    x = "Number of pregnancies",
    y = "Frequency",
    title = "N.gravidanze variable",
    subtitle = "Pregnancies distribution"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Fumatrici variable
smokerstable = table(Fumatrici)
knitr::kable((smokerstable), "pipe")

smokersperc = round((104*100)/2500, 2)
knitr::kable(smokersperc)

ggplot() +
  geom_bar(
    aes(x = as.factor(Fumatrici)),
    fill = "lavenderblush4", color = "black", alpha = 0.7
  ) +
  labs(
    x = "Non-smokers/Smokers",
    y = "Observations",
    title = "Fumatrici variable",
    subtitle = "Tobacco usage habits during pregnancy among the n observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Gestazione variable
pln_distribution = table(Gestazione)
pln_frtable = pln_distribution/sum(pln_distribution)
pl_cumsum = cumsum(pln_distribution)
pl_cumrel = pl_cumsum/sum(pln_distribution)

final_pltable = cbind(ni=pln_distribution, fi=pln_frtable, Ni=pl_cumsum, Fi=pl_cumrel)
colnames(final_pltable) = c("Absolute Fr.", "Relative Fr.", "Cumulated absolute fr.", "Cumulated relative fr.")
knitr::kable(final_pltable)

summary(Gestazione)

pregnancy_length_data = data.frame(
  Asimmetria = round(skewness(Gestazione), 2), 
  Curtosi = round(kurtosis(Gestazione)-3, 2),
  Deviazione_Standard = round(sd(Gestazione), 2)
)
knitr::kable(pregnancy_length_data)

ggplot() +
  geom_bar(
    aes(x = as.factor(Gestazione)),
    fill = "wheat", color = "black", alpha = 0.7
  ) +
  labs(
    x = "Weeks in pregnancy",
    y = "Frequency",
    title = "Gestazione variable",
    subtitle = "pregnancy weeks distribution"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Peso variable
weight_class = cut(Peso, breaks = seq(500,5000, by=500), 
                   left=FALSE,
                   labels = paste0("(", seq(500,4500, by=500), "-", seq(1000,5000, by=500), "]")
                   )
weightn_distribution = table(weight_class)
weightn_frtable = weightn_distribution/sum(weightn_distribution)
weight_cumsum = cumsum(weightn_distribution)
weight_cumrel = weight_cumsum/sum(weightn_distribution)

final_weighttable = cbind(ni=weightn_distribution, fi=weightn_frtable, Ni=weight_cumsum, Fi=weight_cumrel)
colnames(final_weighttable) = c("Absolute Fr.", "Relative Fr.", "Cumulated absolute fr.", "Cumulated relative fr.")
knitr::kable(final_weighttable)

summary(Peso)


weight_data = data.frame(
  Skewness = round(skewness(Peso), 2), 
  Kurtosis = round(kurtosis(Peso)-3, 2),
  St_dev = round(sd(Peso), 2)
)
knitr::kable(weight_data)

ggplot()+
  geom_density(aes(x=Peso), col="black", fill="thistle")+
  geom_vline(xintercept = mean(Peso), linewidth=1.5, color = "lightcoral")+
  labs(x="Newborn weight at birth (g)", y="Density", title = "Peso variable", subtitle = "Distribution density curve")+
  geom_text(aes(x = mean(Peso) + 1000, y = 8e-04, label = sprintf("Mean\n %.3f", mean(Peso))), inherit.aes = FALSE) +
  ylim(0, 9e-04)+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#lunghezza variable
length_class = cut(Lunghezza, breaks = seq(300,570, by=30), 
                   left=FALSE,
                   labels = paste0("(", seq(300,540, by=30), "-", seq(330,570, by=30), "]")
                   )
lengthn_distribution = table(length_class)
lengthn_frtable = lengthn_distribution/sum(lengthn_distribution)
length_cumsum = cumsum(lengthn_distribution)
length_cumrel = length_cumsum/sum(lengthn_distribution)

final_lengthtable = cbind(ni=lengthn_distribution, fi=lengthn_frtable, Ni=length_cumsum, Fi=length_cumrel)
colnames(final_lengthtable) = c("Absolute Fr.", "Relative Fr.", "Cumulated absolute fr.", "Cumulated relative fr.")
knitr::kable(final_lengthtable)

summary(Lunghezza)

length_data = data.frame(
  Skewness = round(skewness(Lunghezza), 2), 
  Kurtosis = round(kurtosis(Lunghezza)-3, 2),
  St_dev = round(sd(Lunghezza), 2)
)
knitr::kable(length_data)

plot(density(Lunghezza))
ggplot()+
  geom_density(aes(x=Lunghezza), col="black", fill="springgreen4")+
  geom_vline(xintercept = mean(Lunghezza), linewidth=1.5, color = "lightcoral")+
  labs(x="Newborn length at birth (mm)", y="Density", title = "Lunghezza variable", subtitle = "Distribution density curve")+
  geom_text(aes(x = mean(Lunghezza) -30, y = 0.02, label = sprintf("Mean\n %.3f", mean(Lunghezza))), inherit.aes = FALSE) +
  ylim(0, 0.025)+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Cranio variable
head_class = cut(Cranio, breaks = seq(220,400, by=20), 
                   left=FALSE,
                   labels = paste0("(", seq(220,380, by=20), "-", seq(240,400, by=20), "]")
)
headn_distribution = table(head_class)
headn_frtable = headn_distribution/sum(headn_distribution)
head_cumsum = cumsum(headn_distribution)
head_cumrel = head_cumsum/sum(headn_distribution)

final_headtable = cbind(ni=headn_distribution, fi=headn_frtable, Ni=head_cumsum, Fi=head_cumrel)
colnames(final_headtable) = c("Absolute Fr.", "Relative Fr.", "Cumulated absolute fr.", "Cumulated relative fr.")
knitr::kable(final_headtable)

summary(Cranio)

head_data = data.frame(
  Skewness = round(skewness(Cranio), 2), 
  Kurtosis = round(kurtosis(Cranio)-3, 2),
  St_dev = round(sd(Cranio), 2)
)
knitr::kable(head_data)

plot(density(Cranio))

ggplot()+
  geom_density(aes(x=Cranio), col="black", fill="mintcream")+
  geom_vline(xintercept = mean(Cranio), linewidth=1.5, color = "lightcoral")+
  labs(x="Newborn head diameter size at birth (mm)", y="Density", title = "Cranio variable", subtitle = "Distribution density curve")+
  geom_text(aes(x = mean(Cranio) -20, y = 0.027, label = sprintf("Mean\n %.3f", mean(Cranio))), inherit.aes = FALSE) +
  ylim(0, 0.03)+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Tipo.parto variable
birth_type = table(Tipo.parto)
knitr::kable(birth_type)

ggplot() +
  geom_bar(
    aes(x = as.factor(Tipo.parto)),
    fill = "papayawhip", color = "black", alpha = 0.7
  ) +
  labs(
    x = "Delivery type",
    y = "Total observations",
    title = "Tipo.parto variable",
    subtitle = "Delivery observations by type"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

birthtypeperc = round((728*100)/2500, 2)
knitr::kable(birthtypeperc)

#Ospedale variable
hospital = table(Ospedale)
knitr::kable(hospital)

ggplot() +
  geom_bar(
    aes(x = as.factor(Ospedale)),
    fill = "red", color = "black", alpha = 0.7
  ) +
  labs(
    x = "Hospital",
    y = "Total observations",
    title = "Ospedale variable",
    subtitle = "Birth distribution by hospitals"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Sesso variable
gender = table(Sesso)
knitr::kable(gender)

ggplot() +
  geom_bar(
    aes(x = as.factor(Sesso)),
    fill = "mediumslateblue", color = "black", alpha = 0.7
  ) +
  labs(
    x = "Newborn gender",
    y = "Total Observations",
    title = "Sesso variable",
    subtitle = "Newborn gender distribution"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Gender/weight at birth variables relation.

library(dplyr)
male_stats = neonatalcare_data %>%
  filter(Sesso == "M") %>%
  summarise(
    Mean = mean(Peso, na.rm = TRUE),
    Median = median(Peso, na.rm = TRUE),
    St_dev = sd(Peso, na.rm = TRUE),
    Minimum = min(Peso, na.rm = TRUE),
    Maximum = max(Peso, na.rm = TRUE),
    Quartile1 = quantile(Peso, 0.25, na.rm = TRUE),
    Quartile3 = quantile(Peso, 0.75, na.rm = TRUE)
  )

female_stats = neonatalcare_data %>%
  filter(Sesso == "F") %>%
  summarise(
    Mean = mean(Peso, na.rm = TRUE),
    Median = median(Peso, na.rm = TRUE),
    St_dev = sd(Peso, na.rm = TRUE),
    Minimum = min(Peso, na.rm = TRUE),
    Maximum = max(Peso, na.rm = TRUE),
    Quartile1 = quantile(Peso, 0.25, na.rm = TRUE),
    Quartile3 = quantile(Peso, 0.75, na.rm = TRUE)
  )

gender_weight_table = rbind(
  Males = male_stats,
  Females = female_stats
)

knitr::kable(gender_weight_table)

ggplot(neonatalcare_data, aes(x = Sesso, y = Peso, fill = Sesso))+
  geom_boxplot(outlier.colour = "lightcoral", outlier.size = 2, alpha = 0.7)+
  labs(
    title = "Weight distribution at birth",
    subtitle = "By gender (F/M)",
    x = "Gender",
    y = "Weight (g)"
  )+
  scale_fill_manual(values = c("M" = "powderblue", "F" = "orchid"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

#Anni.madre/Peso relation

ggplot(neonatalcare_data, aes(x = Anni.madre, y = Peso))+
  geom_point(alpha = 0.6, color = "dodgerblue")+
  geom_smooth(method = "lm", color = "darkorange", se = TRUE, linetype = "dashed")+
  labs(
    title = "Relation between mother's age and newborn weight",
    x = "Mother's Age (in years)",
    y = "Newborn Weight (in g)",
    subtitle = "With linear regression line and confidence interval"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#Pregnancies/Newborn weight relation

ggplot(neonatalcare_data, aes(x = factor(N.gravidanze), y = Peso, fill = factor(N.gravidanze))) +
  geom_boxplot(outlier.colour = "lightcoral", outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Weight distribution in relation to pregnancies number",
    x = "Number of past pregnancies",
    y = "Newborn weight (g)"
  ) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

ggplot(neonatalcare_data, aes(x = factor(N.gravidanze), y = Peso, fill = factor(N.gravidanze))) +
  geom_boxplot(outlier.colour = "lightcoral", outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Newborn weight in relation to number of pregnancies",
    subtitle = "Split by gender",
    x = "Number of past pregnancies",
    y = "Newborn weight (g)"
  ) +
  facet_wrap(~ Sesso) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

#Newborn weight and tobacco usage habits relations

ggplot(neonatalcare_data, aes(x= as.factor(Fumatrici), y = Peso, fill = as.factor(Fumatrici)))+
  geom_boxplot(outlier.colour = "lightcoral", outlier.size = 2, alpha = 0.7)+
  labs(
    title = "Weight distribution at birth",
    subtitle = "Smokers Vs. Non-smokers Mothers comparison",
    x = "Tobacco usage habits (0: Non-smoker, 1: Smoker)",
    y = "Newborn weight at birth (g)"
  )+
  scale_fill_manual(values = c("0"="tomato", "1" = "palegreen"),
                    labels = c("Non-smokers", "Smokers"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

t.test(Peso ~ Fumatrici, data = neonatalcare_data)

#Newborn weight and gestation weeks relations

ggplot(neonatalcare_data, aes(x= Gestazione, y = Peso))+
  geom_point(alpha = 0.6, color = "steelblue")+
  geom_smooth(method = "lm", se = TRUE, color = "darkorange", fill = "sandybrown")+
  labs(
    title = "Gestation weeks and newborn weight relations",
    x = "Gestation weeks",
    y = "Wight (g)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#Weight variations split by hospitals

ggplot(neonatalcare_data, aes(x = Ospedale, y = Peso, fill = Ospedale))+
  geom_boxplot(outlier.colour = "lightcoral", outlier.size = 2, alpha = 0.7)+
  labs(
    title = "Newborn weight distribution",
    subtitle = "Split by hospitals",
    x = "Hospital",
    y = "Weight (g)"
  )+
  scale_fill_brewer(palette = "Pastel1")+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
  )

#Delivery type by Hospitals relation

ggplot(data = neonatalcare_data, aes(x = Tipo.parto, y = Peso, fill = Ospedale)) +
  geom_boxplot(outlier.colour = "lightcoral", outlier.size = 2, alpha = 0.7) +
  labs(title = "Newborn weight split by delivery type and hospitals",
       x = "Delivery Type",
       y = "Weight (g)") +
  scale_fill_brewer(palette = "Pastel1")+
  theme_minimal()

anov_result = aov(Peso ~ Tipo.parto * Ospedale, data = neonatalcare_data)
summary(anov_result)

#Weight variation split by delivery type

ggplot(neonatalcare_data, aes(x = Tipo.parto, y = Peso, fill = Tipo.parto))+
  geom_boxplot(outlier.colour = "lightcoral", outlier.size = 2, alpha = 0.7)+
  labs(
    title = "Newborn weight distribution by delivery type",
    subtitle = "Cesarean vs Natural birth",
    x = "Delivery type",
    y = "Weight (g)"
  )+
  scale_fill_manual(values = c("Nat" = "forestgreen", "Ces" = "firebrick"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

#Length and newborn weight, head diameter and newborn weight relations

ggplot(neonatalcare_data, aes(x= Lunghezza, y = Peso))+
  geom_point(alpha = 0.6, color = "darkred")+
  geom_smooth(method = "lm", se = TRUE, color = "darkorange", fill = "sandybrown")+
  labs(
    title = "Length and newborn weight relations",
    x = "Length",
    y = "Weight (g)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggplot(neonatalcare_data, aes(x= Cranio, y = Peso))+
  geom_point(alpha = 0.6, color = "coral4")+
  geom_smooth(method = "lm", se = TRUE, color = "darkorange", fill = "sandybrown")+
  labs(
    title = "Head diamater and newborn weight relations",
    x = "Head diameter",
    y = "Weight (g)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#Let's verify if the response variable follows a normal distribution

moments::skewness(Peso)
moments::kurtosis(Peso)-3

shapiro.test(Peso)

#correlation matrix
library(dplyr)

panel.cor = function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt = format(c(r, 1), digits = digits)[1]
  txt = paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor = 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
}

data_select = neonatalcare_data %>%
  select(Peso, Anni.madre, N.gravidanze, Gestazione, Lunghezza, Cranio)

pairs(data_select, lower.panel=panel.cor, upper.panel = panel.smooth)

#regression models

model_1 = lm(Peso ~ Anni.madre + N.gravidanze +  Fumatrici + Gestazione + Lunghezza + Cranio + Sesso, data = neonatalcare_data)
summary(model_1)

#attempt 2, excluding Anni.Madre

model_2 = update(model_1,~. -Anni.madre)
summary(model_2)

#attempt 3, excluding Fumatrici
model_3 = update(model_2, ~. -Fumatrici)
summary(model_3)

#attempt 4, excluding N.gravidanze
model_4 = update(model_3, ~. -N.gravidanze)
summary(model_4)

#attempts 5, 6, 7
model_5 = update(model_4, ~. -Sesso)
summary(model_5)

model_6 = update(model_5, ~. -Cranio)
summary(model_6)

model_7 = update(model_6, ~. -Lunghezza)
summary(model_7)

BIC(model_1, model_2, model_3, model_4, model_5, model_6, model_7)
library(MASS)

n = nrow(neonatalcare_data)
stepwise.mod = MASS::stepAIC(model_1, direction = "both", k=log(n))
summary(stepwise.mod)

#Residuals 
par(mfrow=c(2,2))
plot(model_3)

#shapiro test on residuals
shapiro.test(residuals(model_3))

residuals_3 = rstandard(model_3)

plot(
  density(residuals_3),
  main = "Standardized model 3 residuals density curve",
  xlab = "Standardized residuals",
  ylab = "Density",
  col = "olivedrab4",
  lwd = 2
)

curve(
  dnorm(x, mean = mean(residuals_3), sd = sd(residuals_3)),
  col = "lightcoral",
  lwd = 2,
  add = TRUE
  )

legend("topright", 
       inset = c(-0.5, 0),
       legend = c("Residuals density curve", "Normal Distribution"),
       col = c("olivedrab4", "lightcoral"),
       lwd = 2,
       bty = "n"
       )
#homoscedasticity BP test
lmtest::bptest(model_3)

plot(neonatalcare_data$Gestazione, resid(model_3),
     main = "Residuals vs Gestazione",
     xlab = "Gestazione", ylab = "Residuals")
abline(h = 0, col = "red")

plot(neonatalcare_data$N.gravidanze, resid(model_3),
     main = "Residuals vs Gestazione",
     xlab = "Gestazione", ylab = "Residuals")
abline(h = 0, col = "red")

#outliers tests
plot(rstudent(model_3))
abline(h=c(-2,2), col=2)
car::outlierTest(model_3)

plot(model_3, which = 4)
points(1551, cooks.distance(model_3)[1551], col = "red", pch = 19) # Evidenzia 1551
cook = cooks.distance(model_3)
cookframe = data.frame(Cooks_max_distance = max(cook))
knitr::kable((cookframe), "pipe")

my_outliers = neonatalcare_data[c(1551,155,1306),]
knitr::kable((my_outliers), "pipe")

library(car)
vif(model_3)

lmtest::dwtest(model_3)

#Predictions with the selected model
forecast = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "F", 
    N.gravidanze = 3, 
    Gestazione = 39, 
    Fumatrici = 0, 
    Lunghezza = round(mean(Lunghezza), 2), 
    Cranio = round(mean(Cranio), 2)
    )
  )

forecast_data = data.frame (
  Previsione = forecast,
  Sesso = "F",
  N.gravidanze = 3,
  Gestazione = 39,
  Fumatrici = 0,
  Lunghezza_media = round(mean(Lunghezza), 2),
  Cranio_medio = round(mean(Cranio), 2)
)

knitr::kable(forecast_data)

mean_gestazione = mean(Gestazione, na.rm=T)
mean_lunghezza_M = mean(Lunghezza[Sesso == "M"], na.rm = TRUE)
mean_cranio_M = mean(Cranio[Sesso == "M"], na.rm = TRUE)

mean_lunghezza_F = mean(Lunghezza[Sesso == "F"], na.rm = TRUE)
mean_cranio_F = mean(Cranio[Sesso == "F"], na.rm = TRUE)

n_gestazione = 39
lunghezza_forecast_M = round((n_gestazione / mean_gestazione) * mean_lunghezza_M, 2)
cranio_forecast_M = round((n_gestazione / mean_gestazione) * mean_cranio_M, 2)

lunghezza_forecast_F = round((n_gestazione / mean_gestazione) * mean_lunghezza_F, 2)
cranio_forecast_F = round((n_gestazione / mean_gestazione) * mean_cranio_F, 2)

forecast_2 = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "M", 
    N.gravidanze = 0, 
    Gestazione = n_gestazione, 
    Fumatrici = 1, 
    Lunghezza = lunghezza_forecast_M, 
    Cranio = cranio_forecast_M
  )
)

forecast_data2 = data.frame (
  Previsione = forecast_2,
  Sesso = "M",
  N.gravidanze = 0,
  Gestazione = n_gestazione,
  Fumatrici = 1,
  Lunghezza_media = lunghezza_forecast_M,
  Cranio_medio = cranio_forecast_M
)

forecast_3 = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "F", 
    N.gravidanze = 0, 
    Gestazione = n_gestazione, 
    Fumatrici = 1, 
    Lunghezza = lunghezza_forecast_F, 
    Cranio = cranio_forecast_F
  )
)

forecast_data3 = data.frame (
  Previsione = forecast_3,
  Sesso = "F",
  N.gravidanze = 0,
  Gestazione = n_gestazione,
  Fumatrici = 1,
  Lunghezza_media = lunghezza_forecast_F,
  Cranio_medio = cranio_forecast_F
)

forecast_4 = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "M", 
    N.gravidanze = 3, 
    Gestazione = n_gestazione, 
    Fumatrici = 0, 
    Lunghezza = lunghezza_forecast_M, 
    Cranio = cranio_forecast_M
  )
)

forecast_data4 = data.frame (
  Previsione = forecast_4,
  Sesso = "M",
  N.gravidanze = 3,
  Gestazione = n_gestazione,
  Fumatrici = 0,
  Lunghezza_media = lunghezza_forecast_M,
  Cranio_medio = cranio_forecast_M
)

forecast_5 = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "F", 
    N.gravidanze = 3, 
    Gestazione = n_gestazione, 
    Fumatrici = 0, 
    Lunghezza = lunghezza_forecast_F, 
    Cranio = cranio_forecast_F
  )
)

forecast_data5 = data.frame (
  Previsione = forecast_5,
  Sesso = "F",
  N.gravidanze = 3,
  Gestazione = n_gestazione,
  Fumatrici = 0,
  Lunghezza_media = lunghezza_forecast_F,
  Cranio_medio = cranio_forecast_F
)

knitr::kable(forecast_data2)
knitr::kable(forecast_data3)
knitr::kable(forecast_data4)
knitr::kable(forecast_data5)

n_gestazione = 41
lunghezza_forecast_M = round((n_gestazione / mean_gestazione) * mean_lunghezza_M, 2)
cranio_forecast_M = round((n_gestazione / mean_gestazione) * mean_cranio_M, 2)

lunghezza_forecast_F = round((n_gestazione / mean_gestazione) * mean_lunghezza_F, 2)
cranio_forecast_F = round((n_gestazione / mean_gestazione) * mean_cranio_F, 2)

forecast_6 = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "M", 
    N.gravidanze = 1, 
    Gestazione = n_gestazione, 
    Fumatrici = 0, 
    Lunghezza = lunghezza_forecast_M, 
    Cranio = cranio_forecast_M
  )
)

forecast_data6 = data.frame (
  Previsione = forecast_6,
  Sesso = "M",
  N.gravidanze = 1,
  Gestazione = n_gestazione,
  Fumatrici = 0,
  Lunghezza_media = lunghezza_forecast_M,
  Cranio_medio = cranio_forecast_M
)

forecast_7 = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "F", 
    N.gravidanze = 1, 
    Gestazione = n_gestazione, 
    Fumatrici = 0, 
    Lunghezza = lunghezza_forecast_M, 
    Cranio = cranio_forecast_M
  )
)

forecast_data7 = data.frame (
  Previsione = forecast_7,
  Sesso = "F",
  N.gravidanze = 2,
  Gestazione = n_gestazione,
  Fumatrici = 0,
  Lunghezza_media = lunghezza_forecast_M,
  Cranio_medio = cranio_forecast_M
)

forecast_8 = predict(
  model_3, 
  newdata = data.frame(
    Sesso = "M", 
    N.gravidanze = 6, 
    Gestazione = n_gestazione, 
    Fumatrici = 1, 
    Lunghezza = lunghezza_forecast_M, 
    Cranio = cranio_forecast_M
  )
)

forecast_data8 = data.frame (
  Previsione = forecast_8,
  Sesso = "M",
  N.gravidanze = 6,
  Gestazione = n_gestazione,
  Fumatrici = 1,
  Lunghezza_media = lunghezza_forecast_M,
  Cranio_medio = cranio_forecast_M
)

knitr::kable(forecast_data6)
knitr::kable(forecast_data7)
knitr::kable(forecast_data8)

#charts on emerging relations

library("effects")
eff = allEffects(model_3)

plot(eff, "N.gravidanze", main = "Effect of N.gravidanze on weight")
plot(eff, "Gestazione", main = "Effect of Gestazione on weight")
plot(eff, "Cranio", main = "Effect of Cranio on weight")
plot(eff, "Lunghezza", main = "Effect of Lunghezza on weight")
plot(eff, "Sesso", main = "Effect of Sesso on weight")

ggplot(data.frame(Observed = neonatalcare_data$Peso, Predicted = fitted(model_3)), 
       aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Valori Predetti vs Osservati", x = "Peso Osservato", y = "Peso Predetto") +
  theme_minimal()
