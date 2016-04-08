library(ape)
library(geiger) 
library(laser)
library(phytools)

TryMultipleDivModels <- function(tree) {
	tree.branching <- getBtimes(string=write.tree(tree))
	yule.result <- pureBirth(tree.branching)
	bd.result <- bd(tree.branching)
	ddl.result <- DDL(tree.branching)
	AIC.vector <- c(yule.result$aic, bd.result$aic, ddl.result$aic)
	deltaAIC.vector <- AIC.vector-(min(AIC.vector))
	relative_LH <- (exp(-0.5*deltaAIC.vector))
	AkaikeWeight.vector <- relative_LH/(sum(relative_LH))
	result.list <- list(yule=yule.result, bd=bd.result,  ddl=ddl.result, AIC.vector, deltaAIC.vector, AkaikeWeight.vector)
	
	return(result.list)
}