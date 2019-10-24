
## Results for Pichler et al. Machine Learning to predict species interactions

1. [Figure 2](#fig2)
2. [Figure 3](#fig3)
3. [Figure 4](#fig4)
4. [Figure 5](#fig5)


### Figure 2 <a name="fig2"></a>
![Alt text](./Figures/Fig2.svg)
<img src="./Figures/Fig2.svg">

<figcaption>Figure 2: Predictive performance of kNN, CNN, DNN, RF, BRT, naive Bayes, GLM and SVM with simulated plant-pollinator networks (50 plants * 100 pollinators) for baseline scenarios with random interactions and even or uneven species abundances (squares and triangles, respectively), and trait-based interactions with even species abundances (circles). Predictive performance was measured by TSS (a) and AUC (b) for binary interaction data; and Spearman Rho factor (c) for interaction counts. Lowest predictive performance corresponds to zero for TSS, AUC, and Spearman Rho factor.</figcaption>


### Figure 3 <a name="fig3"></a>
![Alt text](./Figures/Fig3.svg)
<img src="./Figures/Fig3.svg">

<figcaption>Figure 3: Predictive performance of different ML methods (naive Bayes, SVM, BRT, kNN, DNN, CNN, RF) and GLM in a global database of plant-pollinator interactions. Dotted lines depict training and solid lines validation performances. Models were sorted from left to right with increasing true skill statistic. The central figure compares directly the models’ performances. Sen = Sensitivity (recall, true positive rate); Spec = Specificity (true negative rate); Prec = Precision; Acc = Accuracy; AUC = Area under the receiver operating characteristic curve (AUC); TSS in % = True skill statistic rescaled to 0 – 1.</figcaption>


### Figure 4 <a name="fig4"></a>
![Alt text](./Figures/Fig4.svg)
<img src="./Figures/Fig4.svg">

<figcaption>Figure 4: Comparison of the top predictive models’ (RF, DNN, BRT, kNN, and GLM) abilities to infer the causal trait-trait interaction structure in simulated networks, using presence-absence data (a) and count data (b). We show the average true positive rate (TPR) and its standard error for one to four true trait-trait interactions based on 8-10 replicate simulations each. Solid red lines display the mean TPR across the interaction scenarios, dotted red lines show a linear regression estimate of TPR against the number of true trait-trait interactions.</figcaption>


### Figure 5 <a name="fig5"></a>
![Alt text](./Figures/Fig5.svg)
<img src="./Figures/Fig5.svg">

<figcaption>Figure 5: a) Elevation profile for the three plant-hummingbird networks in Costa Rica (details see Maglianesi et al. 2014; b) The eight strongest trait-trait interactions (blue – yellow gradient) inferred with the H-statistic from RF models fitted to the combined plant-hummingbird network (colors code the ranking of strengths). Corolla length – bill length and corolla curvature – bill length had the highest interaction strengths.</figcaption>