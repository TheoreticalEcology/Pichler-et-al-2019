## TraitMatching package

### Installation
```{r}
# TM dependencies:
install.packages(c("mlr","keras", "tfprobability", "ranger", "kknn", "xgboost", "missForest", "snow", "liquidSVM"))

devtools::install_github(repo = "https://github.com/TheoreticalEcology/Pichler-et-al-2019", subdir = "TraitMatching")
devtools::install_github(repo = "https://github.com/MaximilianPi/Aranea")

```

### Workflow
```{r}
library(TraitMatching)
species = simulateInteraction(
               inter = matrix(c("A1", "B1"), nrow = 1L),
               weights = list(main = 0, inter = 500),
               setSeed = 42,
               abundance = FALSE,
               traitsA = c(0, 3),
               traitsB = c(0, 3),
               NumberA = 25,
               NumberB = 50)
# We have to transform our three matrices into a community
# object with the createCommunity function.
# Here, you can replace the simulated data with your empiricial data.
# The minOneInter function trims our interaction matrix z
# because probably we observe only species that interaction at least once:
community = createCommunity(species$A, species$B, minOneInter(species$binar(0.07)))
table(community$data$target)
# binar for 0/1 interactions, we have to provide an observation time value,
# the longer we observe the network, the more interactions we will observe.
summary(community$data)
## Fit model
results = runTM(community,
                iters = 10L,
                method = "RF",
                parallel = 5L,
                balanceClasses = "None",
                crossValidation = list(
                  outer = list(method = "CV", iters = 5),
                  inner = list(method = "CV", iters = 3))
                )
print(results)



# extract one of the models and let's see if we can
# infer the causal trait-matching
# (Normally we would fit a new model with the optimal hyperparameters we found on the full data)
model = results$Result$RF$result$models[[1]]
inference = get_Interaction_Strengths(community$data[,-c(1,2)],
                                      target = "target",
                                      model,
                                      depth = 6L,
                                      grid_size = 50L, # subsample from marginal trait distribution
                                      # it would better to use all
                                      # but it is computationally expensive
                                      parallel = 4L,
                                      any = FALSE) # only pairwise interactions
ord_ind = order(inference$pairwise_interactions$Interactions,decreasing = TRUE)
print(inference$pairwise_interactions[ord_ind,,drop=FALSE])

```
