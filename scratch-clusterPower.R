

   #######################3
  ## RESEARCH? ##########
  #########################
  
  #Set outcome rates in each of three arms to .1 , .12 , .14, 
  #number of clusters per arm to, 10, 20, 50, subjects per cluster  to  5, 25, 100.  
  #Repeat for outcome rates = .5, .15, .25.  Sigma_b = .01, .05, .1, .2, .5; 
 
 # use binom.test() to get exact confidence intervals
  
  library(clusterPower)
  
  probvecs <- list(c(.1 , .12 , .14), c(.5, .15, .25), c(.1, .15, .25))
  clvec <- c(10, 20, 50)
  subvec <- c(5, 25, 100)
  sigbvec <- c(.01, .05, .1, .2, .5)
  
  grid <- expand.grid(clvec, subvec, sigbvec)
  
  colnames(grid) <- c("nclusters", "nsubjects", "sigbsq")
  
  sims1 <- list()
  
simNum <- 12
  
for (j in 1:length(probvecs)) {
  sims1[[j]] <- list()
  for (i in 1:nrow(grid)) {
  sims1[[j]][[i]] <- try(cps.ma.binary(nsim = simNum, 
                                    narms = 3, 
                                    nclusters = grid[i, 1],
                               nsubjects = grid[i, 2], 
                               probs = probvecs[[j]],
                               sigma_b_sq = grid[i, 3], 
                               alpha = 0.05, all.sim.data = FALSE, 
                               seed = 123, cores = "all", 
                               poor.fit.override = TRUE) )
  }
}

#sims1 <- readRDS("cpsmabinary-simulations.RDS")  
  
  
#put it into a spreadsheet format  
gridholder <- list()

for (i in 1:length(sims1)) {
  newgrid <- grid  
  newgrid$trt2.pwr <- NA
  newgrid$trt3.pwr <- NA
  newgrid$trt2.lowerCI <- NA
  newgrid$trt2.upperCI <- NA
  newgrid$trt3.lowerCI <- NA
  newgrid$trt3.upperCI <- NA
  for (j in 1:length(sims1[[i]])) {
    temp <- sims1[[i]][[j]]
    if (length(temp) == 3){
    newgrid$trt2.pwr[j] <- temp[[1]][1,1]
    newgrid$trt3.pwr[j] <- temp[[1]][2,1]
    newgrid$trt2.lowerCI[j] <- temp[[1]][1,2]
    newgrid$trt2.upperCI[j] <- temp[[1]][1,3]
    newgrid$trt3.lowerCI[j] <- temp[[1]][2,2]
    newgrid$trt3.upperCI[j] <- temp[[1]][2,3]
    }
  }
gridholder[[i]] <- newgrid  
} 

names(gridholder) <- probvecs

# make table fxn

library(dplyr)

tables <- list()

for (i in 1:length(gridholder)){
  temp1 <- dplyr::select(gridholder[[i]], nclusters, nsubjects, 
                      sigbsq, trt2.pwr) %>% 
    dplyr::arrange(nclusters, nsubjects) %>%
    reshape2::dcast(nclusters + nsubjects ~ sigbsq)
  temp1$trt <- 2
  temp2 <- dplyr::select(gridholder[[i]], nclusters, nsubjects, 
                       sigbsq, trt3.pwr) %>% 
    dplyr::arrange(nclusters, nsubjects) %>%
    reshape2::dcast(nclusters + nsubjects ~ sigbsq)
  temp2$trt <- 3
  tables[[i]] <- rbind(temp1, temp2)
}


# get exact intervals

getExact <- function(pwr, simNum, outcomeProb){
  successNum <- pwr * simNum
  binom.test(n = simNum, p = pwr*simNum)
}

getExact(pwr = .8, simNum = 150, outcomeProb = 0.1)

holder <- list()

holder[[1]] <- lapply(gridholder[[1]][["trt2.pwr"]], 
                 function(x, ...) getExact(x, ...), 
       simNum = simNum, outcomeProb = probvecs[[1]][2])

holder[[2]] <- lapply(gridholder[[1]][["trt3.pwr"]], 
                      function(x, ...) getExact(x, ...), 
                      simNum = simNum, 
                      outcomeProb = probvecs[[1]][3])

holder[[3]] <- lapply(gridholder[[2]][["trt2.pwr"]], 
                      function(x, ...) getExact(x, ...), 
                      simNum = simNum, outcomeProb = probvecs[[2]][2])

holder[[4]] <- lapply(gridholder[[2]][["trt3.pwr"]], 
                      function(x, ...) getExact(x, ...), 
                      simNum = simNum, 
                      outcomeProb = probvecs[[2]][3])



simNum

probvecs



#saveRDS(gridholder, file = "cpsmabinary-simulations-datatables.RDS")

#readRDS(file = "cpsmabinary-simulations-datatables.RDS")

library(ggplot2)

pd <- position_dodge(0.1)

ggplot(gridholder[[1]], aes(x=nsubjects, y=nclusters, colour=sigsqb)) + 
  geom_errorbar(aes(ymin=trt2Pwr.lowerCI, ymax=trt2Pwr.upperCI), width=.1, 
                position=pd) +
  geom_point(position=pd)

plot(gridholder[[1]]) #plot this stuff yo


