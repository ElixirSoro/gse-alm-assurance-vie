# Générateur de scénarios économiques (GSE) – CAC40 et taux à 10 ans

Application **R Shiny** qui calibre et simule des scénarios économiques pour la **gestion actif-passif (ALM) en assurance vie** : évolution du CAC40 et du taux souverain à 10 ans sur 5 ans, avec option de choc de crise. Projet réalisé dans le cadre du Master Actuariat.

🔗 **Application en ligne :** https://elixirsoro27.shinyapps.io/GSEREPORT-1/

## Modèles

**Actions : mouvement brownien géométrique (GBM)**

$$dS_t = \mu S_t\,dt + \sigma S_t\,dW^S_t$$

Calibration sur les log-rendements journaliers (252 jours ouvrés par an). La moyenne des log-rendements estime $\mu - \sigma^2/2$ ; le drift $\mu$ est reconstitué avant la simulation.

**Taux : modèle de Vasicek**

$$dr_t = a\,(b - r_t)\,dt + \sigma_r\,dW^r_t$$

Calibration par régression linéaire des taux mensuels $r_{t+1} = \alpha + \beta\, r_t + \varepsilon$, d'où $a = -\ln(\beta)/\Delta t$, $b = \alpha/(1-\beta)$ et $\sigma_r$ issu de l'écart-type des résidus. La simulation utilise la solution exacte du processus.

**Dépendance actions / taux**

Les deux browniens sont corrélés : $\rho$ est estimé sur les log-rendements mensuels du CAC40 et les variations mensuelles du taux, puis appliqué par décomposition de Cholesky.

## Fonctionnalités

- Import des historiques CAC40 et taux à 10 ans (CSV ou XLSX).
- Choix de la période de calibration : avant COVID (≤ 2019), après COVID (≥ 2020) ou période complète.
- Simulation de Monte-Carlo : jusqu'à 10 000 scénarios sur 60 mois.
- Choc de crise optionnel à 12 mois : −30 % sur les actions, −1 point sur les taux.
- Visualisations interactives (plotly) : trajectoires historiques et simulées, scénario individuel, distributions à 5 ans, fan chart (quantiles 5 %–95 %), corrélation des valeurs finales.

## Lancer l'application

```r
install.packages(c("shiny", "tidyverse", "lubridate", "readxl", "plotly"))
shiny::runApp()
```

Charger ensuite les fichiers du dossier `data/` :
- `exemple_cac40.csv` (colonnes `Date`, `Price`)
- `exemple_taux_10ans.csv` (colonnes `Date`, `Dernier`, en %)

Ces fichiers sont des **données synthétiques** générées pour la démonstration. Pour une analyse réelle, utiliser les historiques du CAC40 et de l'OAT 10 ans au même format.

## Limites et pistes d'amélioration

- Le modèle de Vasicek est un modèle de taux court, appliqué ici au taux à 10 ans par approximation ; il autorise des taux négatifs. Piste : modèle CIR ou Hull-White.
- Corrélation constante entre actifs. Piste : copules ou corrélation dépendante du régime.
- Le choc de crise est déterministe et uniforme sur tous les scénarios.
- Les scénarios produits peuvent alimenter un calcul de SCR marché (Solvabilité II), qui n'est pas implémenté dans l'application.

## Outils

R · Shiny · tidyverse · lubridate · plotly · readxl
