# diaCOG
Application de diagnostic de COG

<p align = right>
  <img src="https://github.com/observatoire-territoires/diaCOG/www/logo_OT.png"/>
</p>

# Le COG, c’est quoi ?
Le code officiel géographique (COG) désigne l’ensemble des codes Insee associés à chaque territoire administratif pour une année donnée. Pour manipuler des données communales,  il est indispensable de connaître le millésime de COG associé aux données.

# Un diagnostic de COG : pour quoi faire ?
Quand on souhaite manipuler un tableau de données à l’échelon communal mais qu’on ne sait pas à quel millésime de COG il correspond ou quand on a des doutes sur la qualité du référentiel communal, un diagnostic de COG s’impose pour :
- vérifier la cohérence géographique de ses données communales
- identifier les erreurs éventuelles de référentiel communal
- préparer ses fichiers à d’éventuels croisements

# Comment faire son propre diagnostic de COG ?
Deux options sont possibles : 
- directement avec R grâce au package `COGugaison` avec la fonction `diag_COG()`

```r
remotes::install_github("antuki/COGugaison")
library(COGugaison)
diag_COG(exemple_popcom)
```

- avec l’application R Shiny « diaCOG »
https://observatoire-des-territoires.shinyapps.io/diaCOG/

<img src = "https://github.com/observatoire-territoires/diaCOG/diaCOG_exemple.png"/>
