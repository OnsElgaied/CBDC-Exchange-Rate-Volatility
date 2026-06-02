# ==============================================================================
# PROJET : Analyse Multi-Pays (7 pays) - GARCH & Facet Plot Épuré
# EXPORTATION : Format PDF Vectoriel pour Soumission Académique
# ==============================================================================

# --- ÉTAPE 0 : Nettoyage complet de la session ---
rm(list = ls())  # Vide l'environnement pour repartir de zéro
graphics.off()  # Ferme les anciens périphériques graphiques bloqués

# --- ÉTAPE 1 : Chargement des extensions (Packages) ---
library(readxl)    # Pour lire la base de données
library(writexl)   # Pour exporter les données combinées si besoin
library(dplyr)     # Pour manipuler et filtrer les données
library(zoo)       # Pour la gestion des dates (yearmon)
library(rugarch)   # Pour le modèle gjrGARCH
library(ggplot2)   # Pour la création graphique haute définition

# --- ÉTAPE 2 : Importation de la base et configuration des dates ---
# Chargement de la base globale
df_all <- read_excel("C:/Users/finra/Desktop/ons/18_06_2025/df_all_treat.xlsx", sheet = "all")

# Configuration du dictionnaire des pays avec leurs dates CBDC exactes
cbdc_dates_df <- data.frame(
  COUNTRY = c("Bahamas", "China", "Brazil", "Jamaica", "Nigeria", "Kazakhstan", "India"),
  CBDC_DATE = as.Date(c(
    "2020-10-01",  # Bahamas
    "2020-05-01",  # China
    "2020-11-01",  # Brazil
    "2022-07-01",  # Jamaica (Corrigé : Juillet 2022)
    "2021-10-01",  # Nigeria
    "2023-11-01",  # Kazakhstan (Corrigé : Novembre 2023)
    "2022-12-01"   # India
  ))
)

# Initialisation du tableau final de stockage
all_vol_results <- data.frame()

# Spécification du modèle gjrGARCH(1,1) avec asymétrie
spec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  distribution.model = "norm"
)

# --- ÉTAPE 3 : Boucle automatique de calcul GARCH ---
for (country_name in cbdc_dates_df$COUNTRY) {
  
  # Filtrage et nettoyage pour le pays en cours
  data_country <- df_all %>%
    filter(COUNTRY == country_name) %>%
    select(DATE, LN_R_REER) %>%
    filter(!is.na(LN_R_REER))
  
  # Conversion des formats indispensables
  data_country$LN_R_REER <- as.numeric(data_country$LN_R_REER)
  data_country$DATE      <- as.yearmon(data_country$DATE, format = "%YM%m")
  
  # Extraction de la série des rendements
  rets <- data_country$LN_R_REER
  
  # Estimation sécurisée du modèle
  fit_attempt <- tryCatch({
    ugarchfit(spec, rets)
  }, error = function(e) { NULL })
  
  # Si l'estimation réussit, on extrait et on stocke la volatilité
  if (!is.null(fit_attempt)) {
    vol <- sigma(fit_attempt)
    
    temp_df <- data.frame(
      COUNTRY    = rep(country_name, length(vol)),
      DATE       = as.Date(data_country$DATE),
      Volatility = as.numeric(vol)
    )
    
    all_vol_results <- rbind(all_vol_results, temp_df)
  }
}

# Fusion avec le tableau des dates de la CBDC
all_vol_results <- all_vol_results %>%
  left_join(cbdc_dates_df, by = "COUNTRY")

# --- ÉTAPE 4 : Création du graphique multi-panel professionnel ---
final_facet_plot <- ggplot(all_vol_results, aes(x = DATE, y = Volatility)) +
  geom_line(color = "black", linewidth = 0.5) +
  
  # Lignes verticales de l'événement CBDC pour chaque pays
  geom_vline(aes(xintercept = CBDC_DATE), color = "red", linetype = "dashed", linewidth = 0.5) +
  
  # Répétition des axes X sous CHAQUE pays (scales = "free")
  facet_wrap(~ COUNTRY, scales = "free", ncol = 3) + 
  
  # Formatage de l'axe temporel (Affichage tous les 2 ans)
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  
  # Pas de titre interne (sera géré dans la légende du fichier Word)
  labs(
    x = "Year",
    y = "Conditional Volatility"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold", size = 11), # Titre du pays en gras
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    panel.spacing    = unit(1.5, "lines"), # Espace optimal entre les pays
    panel.grid.minor = element_blank()
  )

# Affichage du rendu final dans l'onglet Plots
print(final_facet_plot)

# --- ÉTAPE 5 : Exportation finale au format PDF Vectoriel ---
ggsave(
  filename = "C:/Users/finra/Desktop/ons/18_06_2025/gjrbycountries/Figure_Volatility_7Countries.pdf",
  plot = final_facet_plot,
  device = "pdf",
  width = 11,      # Largeur idéale pour 3 colonnes de graphiques
  height = 7.5,    # Hauteur proportionnelle
  units = "in"     # Unité : pouces
)