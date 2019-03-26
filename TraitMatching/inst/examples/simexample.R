# # Example files für die Hilfe
#
x <- GetInteractions()
View(x$interactionmatrix)
species <- GetSpeciesProperties()

hist(x$mainp)
hist(x$interp)
hist(x$randomp)


image(x$interactionmatrix)
heatmap(x$interactionmatrix)

image(test2)

test =  rpois(length(x$interactionmatrix), x$interactionmatrix * 1000000)
test2 = matrix(test, ncol = 100)
table(test2)
table(as.numeric(test>0))


