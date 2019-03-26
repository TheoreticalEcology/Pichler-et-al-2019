# getInteractions = function(model, data, classif = T, ll = T){
#   require(iml)
#   set.seed(42)
#   results = list()
#   predictor = Predictor$new(model = model, data = data[,-10], y =data[,10])
#
#
#   if(classif) loss = "ce"
#   else {
#     if(ll) loss = function(actual, predicted) sum(-2*dpois(round(actual),predicted, log = T))
#     else loss = "rmse"
#   }
#
#   results[["featureImportance"]] = FeatureImp$new(predictor, loss = loss)$results
#
#   AllInteractions = iml::Interaction$new(predictor)$results
#   AllInteractions = AllInteractions[order(AllInteractions$.interaction, decreasing = T),]
#
#   results[["first"]] = iml::Interaction$new(predictor,  feature = AllInteractions$.feature[1])$results
#   results$first = results$first[order(results$first$.interaction, decreasing = T),]
#
#   results[["second"]] = iml::Interaction$new(predictor,  feature = AllInteractions$.feature[2])$results
#   results$second = results$second[order(results$second$.interaction, decreasing = T),]
#
#
#   results[["third"]] = iml::Interaction$new(predictor,  feature = AllInteractions$.feature[3])$results
#   results$third = results$third[order(results$third$.interaction, decreasing = T),]
#
#   results[["All"]] = AllInteractions
#   gc()
#
#
#
#
#
#
#   return(results)
# }
#
#
#
#
#
# pdf(file = "../testSUPER.pdf")
# plotInteractionsTop2(results)
# dev.off()
#
# par(mar = rep(0.1,4),oma = c(2,0,2,0))
# plotInteractionsTop2 = function(results, x1 = c(0.1,0.44), x2 = c(0.66,1), cols = rep("steelblue", 4), lwd = 1.2){
#   plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1),xlab = "", ylab = "", axes = F)
#   pos1Y = rev(seq(0.10,0.9, length.out = 3))
#   dTree = 0.090
#   s = sapply(pos1Y, function(y1,dTree = 0.095,x1,x2) {
#     lines(x = x1, y = rep(y1,2), lty = 2, col = "grey")
#     for(d in c(dTree,-dTree)){
#       lines(x = x2, y = rep(y1,2)+ d, lty = 2, col = "grey")
#     }
#     # Tree
#     lines(y = rep(y1,2), x = c(x1[2]+0.02,x1[2]+0.04), lwd = lwd)
#     lines(y = c(y1+dTree, y1-dTree), x = rep(x1[2]+0.04,2), lwd = lwd)
#     lines(y = rep(y1+dTree,2), x = c(x1[2]+0.04,x1[2]+0.06), lwd = lwd)
#     lines(y = rep(y1-dTree,2), x = c(x1[2]+0.04,x1[2]+0.06), lwd = lwd)
#   },dTree,x1,x2)
#
#
#   for(i in 1:3){
#     rect(xleft = x1[1], xright = results$All$.interaction[i]*0.3+x1[1], ybottom = (pos1Y)[i]-0.02, ytop =(pos1Y)[i]+0.02, col = cols[1], border = cols[2] )
#     text(x = 0.25, y = (pos1Y)[i], labels = paste0(round(results$All$.interaction[i],3)*100," %"),pos = 3,font = 2)
#     text(x = 0, y = (pos1Y)[i], labels = results$All$.feature[i], pos = 4,font = 2)
#   }
#   namesR = c("first", "second", "third")
#   for(i in 1:3){
#     dTree2 = c(dTree, -dTree)
#     for(j in 1:2){
#       text(x = x1[2]+0.07, y = pos1Y[i]+dTree2[j], labels = results[[namesR[i]]]$.feature[j],pos = 4,font = 2)
#       rect(xleft = x2[1], xright = results[[namesR[i]]]$.interaction[j]*0.3+x2[1], ybottom = (pos1Y)[i]-0.02+dTree2, ytop =(pos1Y)[i]+0.02+dTree2, col = cols[3], border = cols[4])
#       text(x = x2[1]+0.15, y = pos1Y[i]+dTree2[j], labels = paste0(round(results[[namesR[i]]]$.interaction[j],3)*100," %"),pos = 3,font = 2)
#     }
#   }
#
#   s = sapply(pos1Y, function(y1,dTree = 0.095,x1,x2) {
#     for(i in 1:2){
#       lines(x = rep(x1[i],2)-0.0001, y = c(y1-0.03,y1+0.03), lwd = lwd)
#     }
#     for(d in c(dTree,-dTree)){
#       for(i in 1:2){
#         lines(x = rep(x2[i],2)-0.0001, y = c(y1-0.03,y1+0.03) + d, lwd = lwd)
#       }
#     }
#   },dTree,x1,x2)
#
# }
#
#
#
#
#
#
#
#
#
