# Générateur de scénarios économiques (GSE) – CAC40 et taux à 10 ans

Application **R Shiny** qui calibre et simule des scénarios économiques pour la **gestion actif-passif (ALM) en assurance vie** : évolution du CAC40 et du taux souverain à 10 ans sur 5 ans, avec option de choc de crise. Projet réalisé dans le cadre du Master Actuariat.

🔗 **Application en ligne :** https://elixirsoro27.shinyapps.io/gse-alm-assurance-vie/

📄 **Rapport complet :** [rapport/Rapport_GSE_ALM.pdf](rapport/Rapport_GSE_ALM.pdf) (calibration sur données réelles 2011-2025, simulations, lien avec Solvabilité II, limites)

## Résultats clés (calibration 2011-2025, 10 000 scénarios)

| Indicateur | Valeur |
|---|---|
| Volatilité annuelle du CAC 40 | 19,5 % |
| Taux d'équilibre Vasicek (avant 2020 / depuis 2020) | 0,18 % / 3,92 % |
| Corrélation actions-taux (avant 2020 / depuis 2020) | +0,24 / −0,26 |
| Rendement du CAC 40 à 12 mois, quantile 0,5 % | −36,3 % (choc standard Solvabilité II : −39 %) |
| SCR de marché sur un bilan type (formule standard / GSE 2011-2025 / GSE depuis 2020) | 70 / 110 / 80 M€ |

Le choix de la période de calibration change fortement les scénarios de taux : c'est une hypothèse à justifier, pas un détail technique.

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

- Import des historiques CAC40 et taux à 10 ans (CSV ou XLSX). Les dates des exports Investing.com ouverts dans Excel, où jour et mois sont parfois inversés, sont remises dans le bon ordre.
- Choix de la période de calibration : avant COVID (≤ 2019), après COVID (≥ 2020) ou période complète.
- Simulation de Monte-Carlo : jusqu'à 10 000 scénarios sur 60 mois.
- Choc de crise optionnel à 12 mois : −30 % sur les actions, −1 point sur les taux.
- Onglet **SCR de marché** : sur un bilan d'assureur vie modifiable, SCR actions et taux selon la formule standard de Solvabilité II (règlement délégué 2015/35, art. 166, 167 et 169) comparé au SCR tiré des scénarios (perte d'actif net à un an, quantile 99,5 %).
- Visualisations interactives (plotly) : trajectoires historiques et simulées, scénario individuel, distributions à 5 ans, fan chart (quantiles 5 %–95 %), corrélation des valeurs finales.

## Reproduire le rapport

```bash
Rscript analyse/calibration_rapport.R cac40.xlsx bund10ans.xlsx
```

## Lancer l'application

```r
install.packages(c("shiny", "tidyverse", "lubridate", "readxl", "plotly"))
shiny::runApp()
```

Charger ensuite les fichiers du dossier `data/` :
- `exemple_cac40.csv` (colonnes `Date`, `Price`)
- `exemple_taux_10ans.csv` (colonnes `Date`, `Dernier`, en %)

Ces fichiers sont des **données synthétiques** générées pour la démonstration. Pour une analyse réelle, utiliser les historiques du CAC40 et d'un taux souverain à 10 ans au même format (le rapport utilise le Bund allemand, téléchargé sur Investing.com).

## Limites et pistes d'amélioration

- Le modèle de Vasicek est un modèle de taux court, appliqué ici au taux à 10 ans par approximation ; il autorise des taux négatifs. Piste : modèle CIR ou Hull-White.
- Corrélation constante entre actifs, alors qu'elle change de signe entre avant et après 2020. Piste : copules ou modèle à changement de régime.
- Scénarios en monde réel (projection ALM, ORSA) : la valorisation du best estimate demanderait des scénarios risque-neutres.
- Le choc de crise est déterministe et uniforme sur tous les scénarios.
- Le SCR de marché est simplifié : bilan fictif, courbe des taux plate, passif sans participation aux bénéfices ni rachats, sous-modules actions et taux seulement.

## Outils

R · Shiny · tidyverse · lubridate · plotly · readxl
