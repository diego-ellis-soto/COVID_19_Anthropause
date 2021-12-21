###################################################
#                                                 #
#         COVID-Movement Analysis Script          #
#     Scott Yanco, PhD;  scott.yanco@yale.edu     #
#                                                 #
###################################################

# Dev version of plotting script for COVID movement project
#
# TODO: Integrate wtth workflwo strcutures (see Project ntoes)
#

#==============================================================================#


#---- Inits ----#
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(ctmm)


#---- Load data ----#
load("out/cougar_testrun_2021-11-22.rdata")
load("out/cougar_testrun_2021-12-07.rdata")

#---- Move Metrics ----#

#ectract amt data from out
move_ls <- list()
for(i in 1:length(out)){
  move_ls[[i]] <- out[[i]]$move_metrics  
}

mm <- do.call("rbind", move_ls) %>% 
  filter(!is.na(.phase))

#make plots
slplot <- ggplot(mm, aes(x=sl_, color = .phase))+
  geom_density()+
  theme_minimal()

taplot <- ggplot(mm, aes(x=ta_, color = .phase))+
  geom_density()+
  theme_minimal()

grid.arrange(slplot, taplot)


#---- Home Rnages ----#

prehr <- list()
for(i in 1:length(out)){
  prehr[[i]] <- data.frame(
    est = ifelse(is.null(out[[i]]$home_ranges[[1]]), NA, summary((out[[i]]$home_ranges[[1]]))$CI[2]),
    cil = ifelse(is.null(out[[i]]$home_ranges[[1]]), NA, summary((out[[i]]$home_ranges[[1]]))$CI[1]),
    cih = ifelse(is.null(out[[i]]$home_ranges[[1]]), NA, summary((out[[i]]$home_ranges[[1]]))$CI[3]),
    phase = "before lockdown"
  )  
}

ldhr <- list()
for(i in 1:length(out)){
  if(length(out[[i]]$home_ranges) >= 2){
    tryCatch({
      ldhr[[i]] <- data.frame(
        est = ifelse(is.null(out[[i]]$home_ranges[[2]]), NA, summary((out[[i]]$home_ranges[[2]]))$CI[2]),
        cil = ifelse(is.null(out[[i]]$home_ranges[[2]]), NA, summary((out[[i]]$home_ranges[[2]]))$CI[1]),
        cih = ifelse(is.null(out[[i]]$home_ranges[[2]]), NA, summary((out[[i]]$home_ranges[[2]]))$CI[3]),
        phase = "lockdown"
      ) # df
    }, #try
    error = function(e){e=NULL}) #catch
  } else {
    ldhr[[i]] <- data.frame(
      est = NA,
      cil = NA,
      cih = NA,
      phase = "lockdown"
    ) #df
  } #else
} # i

afthr <- list()
for(i in 1:length(out)){
  if(length(out[[i]]$home_ranges) >= 3){
    tryCatch({
      afthr[[i]] <- data.frame(
        est = ifelse(is.null(out[[i]]$home_ranges[[3]]), NA, summary((out[[i]]$home_ranges[[3]]))$CI[2]),
        cil = ifelse(is.null(out[[i]]$home_ranges[[3]]), NA, summary((out[[i]]$home_ranges[[3]]))$CI[1]),
        cih = ifelse(is.null(out[[i]]$home_ranges[[3]]), NA, summary((out[[i]]$home_ranges[[3]]))$CI[3]),
        phase = "after lockdown"
      ) # df
    }, #try
    error = function(e){e=NULL}) #catch
  } else {
    afthr[[i]] <- data.frame(
      est = NA,
      cil = NA,
      cih = NA,
      phase = "after lockdown"
    ) #df
  } #else
} # i

hrdat <- do.call("rbind", list(do.call("rbind", prehr), do.call("rbind", ldhr), do.call("rbind", afthr)))

ggplot(hrdat[hrdat$est < 200,], aes(x = phase, y = est))+
  geom_boxplot()+
  # ylim(c(0, 1000))+
  theme_minimal()


#---- Niches  ----#

#-- Niche Breadth
prenb <- list()
for(i in 1:length(out)){
  prenb[[i]] <- data.frame(breadth = ifelse(is.null(out[[i]]$niche_breadth$baseline[["total"]]), NA, out[[i]]$niche_breadth$baseline[["total"]]),
                          phase = "before lockdown"
  )
}

ldnb <- list()
for(i in 1:length(out)){
  ldnb[[i]] <- data.frame(breadth = ifelse(is.null(out[[i]]$niche_breadth$lockdown[["total"]]), NA, out[[i]]$niche_breadth$lockdown[["total"]]),
                          phase = "lockdown"
  )
}

aftnb <- list()
for(i in 1:length(out)){
  aftnb[[i]] <- data.frame(breadth = ifelse(is.null(out[[i]]$niche_breadth$after_lockdown[["total"]]), NA, out[[i]]$niche_breadth$after_lockdown[["total"]]),
                          phase = "after lockdown"
  )
}

nbdat <- do.call("rbind", list(do.call("rbind", prenb), do.call("rbind", ldnb), do.call("rbind", aftnb)))

ggplot(nbdat[nbdat$breadth < 1000,], aes(x = phase, y = breadth))+
  geom_boxplot()+
  # ylim(c(0, 1500))+
  theme_minimal()


#-- Niche Dissimilarity

ndls <- list()
for(i in 1:length(out)){
  #TODO: this firs if statement isn't ctaching individuals missing dissimilarity calcs
  if(!is.null(out[[i]]$niche_dissims)){
    comps <- names(out[[i]]$niche_dissims)
    tmp <- list()
    for(c in 1:length(comps)){
      tmp[[c]] <- data.frame(
        bd = out[[i]]$niche_dissims[[comps[[c]]]]$Bhattacharyya_distance[["total"]], 
        md = out[[i]]$niche_dissims[[comps[[c]]]]$Mahalanobis_distance[["total"]], 
        dr = out[[i]]$niche_dissims[[comps[[c]]]]$Determinant_ratio[["total"]], 
        p1 = gsub( "(.*)-(.*)", "\\1",  comps[[c]]), 
        p2 = gsub( "(.*)-(.*)", "\\2",  comps[[c]]))
    }
    ndls[[i]] <- do.call("rbind", tmp)
  }else{
    nlds[[i]] <- data.frame(
      bd = NA, 
      md = NA, 
      dr = NA, 
      p1 = NA, 
      p2 = NA)
  }
}
nd_dat <- do.call("rbind", ndls)

out[[5]]$niche_dissims[[comps[[1]]]]

out[[5]]$niche_dissims$`after_lockdown-baseline`$Mahalanobis_distance[["total"]]
n <- names(out[[5]]$niche_dissims)
gsub( "(.*)-(.*)", "\\1",  n)
gsub( "(.*)-(.*)", "\\2",  n)

