# Pichler-et-al-2019

To install the TraitMatching paper:

```{r}
devtools::install_github(repo = "https://github.com/TheoreticalEcology/Pichler-et-al-2019", subdir = "TraitMatching", 
dependencies = T, build_vignettes = T)

library(TraitMatching)
vignette("TraitMatching")

```
