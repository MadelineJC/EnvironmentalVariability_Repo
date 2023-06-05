#### DOC STRING ===============================================================
# This script uses persistence vectors to compare outcomes between treatments.
# This script begins by summarizing outputs from the deterministic simulations,
# and then summarizes outputs from the demographically stochastic simulations. 
# Then, we look at outputs from the (demographically and) environmentally 
# stochastic simulations, to determine (1) how different thermal scenarios affected 
# the longevity of co-occurrence, (2) how much environmental variation extended 
# periods of co-occurrence compared to the deterministic case, and (3) the maximum 
# period of co-occurrence overall (fully factorial), and by case (magnitude of 
# disturbance varies/frequency fixed; frequency of disturbance varies/magnitude fixed). 

# Outputs are collected in the "Collected summary objects" section. 
#### END DOC STRING ===========================================================

#### Required packages ####
install.packages("dplyr"); library(dplyr)

#### Deterministic #####
Det_LT_Iden_PV <- readRDS("Data/Deterministic_Outputs/Det_LT_Iden_PV.rds")
Det_LT_Semi_PV <- readRDS("Data/Deterministic_Outputs/Det_LT_Semi_PV.rds")
Det_LT_PiAd_PV <- readRDS("Data/Deterministic_Outputs/Det_LT_PiAd_PV.rds")
Det_LT_PjAd_PV <- readRDS("Data/Deterministic_Outputs/Det_LT_PjAd_PV.rds")
Det_LT_PjAd_vB_PV <- readRDS("Data/Deterministic_Outputs/Det_LT_PjAd_vB_PV.rds")
Det_HT_Iden_PV <- readRDS("Data/Deterministic_Outputs/Det_HT_Iden_PV.rds")
Det_HT_Semi_PV <- readRDS("Data/Deterministic_Outputs/Det_HT_Semi_PV.rds")
Det_HT_PiAd_PV <- readRDS("Data/Deterministic_Outputs/Det_HT_PiAd_PV.rds")
Det_HT_PjAd_PV <- readRDS("Data/Deterministic_Outputs/Det_HT_PjAd_PV.rds")
Det_HT_PjAd_vB_PV <- readRDS("Data/Deterministic_Outputs/Det_HT_PjAd_vB_PV.rds")

Intro <- seq(0,49,1)
Det_CoOc <- cbind(Intro,
              Det_LT_Iden_PV,
              Det_LT_Semi_PV,
              Det_LT_PiAd_PV,
              Det_LT_PjAd_PV,
              Det_LT_PjAd_vB_PV,
              Det_HT_Iden_PV,
              Det_HT_Semi_PV,
              Det_HT_PiAd_PV,
              Det_HT_PjAd_PV,
              Det_HT_PjAd_vB_PV)
Maxes <- c()
for (i in 2:ncol(Det_CoOc)){
  Maxes[i-1] <- max(Det_CoOc[ ,i]) # 84 89 88 92 88 84 88 87 90 87
}

#### Demographically stochastic ####
## Low-average temperature environment
D_LT_Iden_PV <- readRDS("Data/DemStoch_Outputs/D_LT_Iden_PV.rds")
D_LT_Semi_PV <- readRDS("Data/DemStoch_Outputs/D_LT_Semi_PV.rds")
D_LT_PiAd_PV <- readRDS("Data/DemStoch_Outputs/D_LT_PiAd_PV.rds")
D_LT_PjAd_PV <- readRDS("Data/DemStoch_Outputs/D_LT_PjAd_PV.rds")
D_LT_PjAd_vB_PV <- readRDS("Data/DemStoch_Outputs/D_LT_PjAd_vB_PV.rds")
## High-average temperature environment
D_HT_Iden_PV <- readRDS("Data/DemStoch_Outputs/D_HT_Iden_PV.rds")
D_HT_Semi_PV <- readRDS("Data/DemStoch_Outputs/D_HT_Semi_PV.rds")
D_HT_PiAd_PV <- readRDS("Data/DemStoch_Outputs/D_HT_PiAd_PV.rds")
D_HT_PjAd_PV <- readRDS("Data/DemStoch_Outputs/D_HT_PjAd_PV.rds")
D_HT_PjAd_vB_PV <- readRDS("Data/DemStoch_Outputs/D_HT_PjAd_vB_PV.rds")
List <- list(D_LT_Iden_PV, D_LT_Semi_PV, D_LT_PiAd_PV, D_LT_PjAd_PV, D_LT_PjAd_vB_PV, D_HT_Iden_PV, D_HT_Semi_PV, D_HT_PiAd_PV, D_HT_PjAd_PV, D_HT_PjAd_vB_PV)

Intros <- c()
for (i in 1:ncol(List[[1]])){
  Intros[i] <- paste0("Intro_", i-1)
}
Dem_Mean_CoOc <- data.frame(Intros)
for (i in 1:length(List)){
  Dem_Mean_CoOc[ ,i + 1] <- colMeans(List[[i]][ , 1:ncol(List[[i]])])
}
colnames(Dem_Mean_CoOc) <- c("Intros", "D_LT_Iden", "D_LT_Semi", "D_LT_PiAd", "D_LT_PjAd", "D_HT_Iden", "D_HT_Semi", "D_HT_PiAd", "D_HT_PjAd")

#### Environmentally stochastic ####
## Low-average temperature environment
E_LT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_Iden_PV.rds")
E_LT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_Semi_PV.rds")
E_LT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_PiAd_PV.rds")
E_LT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_PjAd_PV.rds")
E_LT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_PjAd_vB_PV.rds")
## High-average temperature environment
E_HT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_Iden_PV.rds")
E_HT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_Semi_PV.rds")
E_HT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_PiAd_PV.rds")
E_HT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_PjAd_PV.rds")
E_HT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_PjAd_vB_PV.rds")

#### ... Differences between deterministic and environmentally stochastic outputs by thermal scenario ####
# DESCRIPTION: Here, we analyse which thermal scenario produces the longest period of co-occurrence, 
# relative to the deterministic case. 1s denote the first thermal scenario; 2s denote the second, etc.
#### ... ... Low temp ####
List_LT <- list()
SDs <- as.character(seq(2, 30, 2))
for (l in 1:length(E_LT_Iden_PV[[1]])){
  MaxLT <- data.frame(SDs)
  for (i in 1:length(E_LT_Iden_PV)){
    I <- colMeans(E_LT_Iden_PV[[i]][[l]]) - Det_LT_Iden_PV[l]
    S <- colMeans(E_LT_Semi_PV[[i]][[l]]) - Det_LT_Semi_PV[l]
    Pi <- colMeans(E_LT_PiAd_PV[[i]][[l]]) - Det_LT_PiAd_PV[l]
    Pj <- colMeans(E_LT_PjAd_PV[[i]][[l]]) - Det_LT_PjAd_PV[l]
    Pj_vB <- colMeans(E_LT_PjAd_vB_PV[[i]][[l]]) - Det_LT_PjAd_vB_PV[l]
    x <- cbind(I, S, Pi, Pj, Pj_vB)
    for (j in 1:nrow(x)){
      MaxLT[j,i+1] <- which.max(x[j, ])
    }
  }
  colnames(MaxLT) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  List_LT[[l]] <- MaxLT
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(List_LT) <- names

#### ... ... High temp ####
List_HT <- list()
SDs <- as.character(seq(2, 40, 2))
for (l in 1:length(E_HT_Iden_PV[[1]])){
  MaxHT <- data.frame(SDs)
  for (i in 1:length(E_HT_Iden_PV)){
    I <- colMeans(E_HT_Iden_PV[[i]][[l]]) - Det_HT_Iden_PV[l]
    S <- colMeans(E_HT_Semi_PV[[i]][[l]]) - Det_HT_Semi_PV[l]
    Pi <- colMeans(E_HT_PiAd_PV[[i]][[l]]) - Det_HT_PiAd_PV[l]
    Pj <- colMeans(E_HT_PjAd_PV[[i]][[l]]) - Det_HT_PjAd_PV[l]
    Pj_vB <- colMeans(E_HT_PjAd_vB_PV[[i]][[l]]) - Det_HT_PjAd_vB_PV[l]
    x <- cbind(I, S, Pi, Pj, Pj_vB)
    for (j in 1:nrow(x)){
      MaxHT[j,i+1] <- which.max(x[j, ])
    }
  }
  colnames(MaxHT) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  List_HT[[l]] <- MaxHT
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(List_HT) <- names

#### ... How much does environmental variation extend co-occurrence times? ####
# DESCRIPTION: Here, we analyse the difference in longevity of co-occurrence 
# between environmentally stochastic and deterministic model simulations.
#### ... ... Low temp ####
## Iden
LT_Iden_List <- list()
SDs <- as.character(seq(2, 30, 2))
# This loop goes through the persistence vectors and averages by condition, the subtracts the deterministic outcome from those averages to give average difference between env. stoch. and deterministic outcomes by permutation.
for (l in 1:length(E_LT_Iden_PV[[1]])){ # For each intro time
  Diff <- data.frame(SDs)
  for (i in 1:length(E_LT_Iden_PV)){ # For each frequency
    Diff[ ,i+1] <- colMeans(E_LT_Iden_PV[[i]][[l]]) - Det_LT_Iden_PV[l] # Where different columns are different magnitudes
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_Iden_List[[l]] <- Diff
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(LT_Iden_List) <- names
Maxes <- c()
for (i in 1:length(LT_Iden_List)){ 
  Maxes[i] <- max(data.frame(LT_Iden_List[[i]][ ,2:ncol(LT_Iden_List[[i]])])) # Finds the largest value per intro time (so by mag. and freq.)
}
Maxes; LT_Iden_MaxDiff <- Maxes

# Looking for max. diffs. when freq. of disturbance is intermediate (once/day)
Each10_MaxDiff_LT_Iden <- data.frame(SDs) 
for (i in 1:length(LT_Iden_List)){
  Each10_MaxDiff_LT_Iden[ ,i+1] <- LT_Iden_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_Iden) <- c("SD", names); max(Each10_MaxDiff_LT_Iden[ ,2:length(Each10_MaxDiff_LT_Iden)])
# Each10_MaxDiff_LT_Iden contains the mean difference in longevity of co-occurrence 
# between environmentally stochastic and deterministic model simulations by introduction 
# time and magnitude of disturbance when the frequency of disturbance is intermediate (once/day).

## Semi
LT_Semi_List <- list()
for (l in 1:length(E_LT_Semi_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_LT_Semi_PV)){
    Diff[ ,i+1] <- colMeans(E_LT_Semi_PV[[i]][[l]]) - Det_LT_Semi_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_Semi_List[[l]] <- Diff
}
names(LT_Semi_List) <- names
Maxes <- c()
for (i in 1:length(LT_Semi_List)){
  Maxes[i] <- max(data.frame(LT_Semi_List[[i]][ ,2:ncol(LT_Semi_List[[i]])]))
}
Maxes; LT_Semi_MaxDiff <- Maxes

Each10_MaxDiff_LT_Semi <- data.frame(SDs)
for (i in 1:length(LT_Semi_List)){
  Each10_MaxDiff_LT_Semi[ ,i+1] <- LT_Semi_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_Semi) <- c("SD", names); max(Each10_MaxDiff_LT_Semi[ ,2:length(Each10_MaxDiff_LT_Semi)])

## PiAd
LT_PiAd_List <- list()
for (l in 1:length(E_LT_PiAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_LT_PiAd_PV)){
    Diff[ ,i+1] <- colMeans(E_LT_PiAd_PV[[i]][[l]]) - Det_LT_PiAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_PiAd_List[[l]] <- Diff
}
names(LT_PiAd_List) <- names
Maxes <- c()
for (i in 1:length(LT_PiAd_List)){
  Maxes[i] <- max(data.frame(LT_PiAd_List[[i]][ ,2:ncol(LT_PiAd_List[[i]])]))
}
Maxes; LT_PiAd_MaxDiff <- Maxes

Each10_MaxDiff_LT_PiAd <- data.frame(SDs)
for (i in 1:length(LT_PiAd_List)){
  Each10_MaxDiff_LT_PiAd[ ,i+1] <- LT_PiAd_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_PiAd) <- c("SD", names); max(Each10_MaxDiff_LT_PiAd[ ,2:length(Each10_MaxDiff_LT_PiAd)])

## PjAd
LT_PjAd_List <- list()
for (l in 1:length(E_LT_PjAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_LT_PjAd_PV)){
    Diff[ ,i+1] <- colMeans(E_LT_PjAd_PV[[i]][[l]]) - Det_LT_PjAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_PjAd_List[[l]] <- Diff
}
names(LT_PjAd_List) <- names
Maxes <- c()
for (i in 1:length(LT_PjAd_List)){
  Maxes[i] <- max(data.frame(LT_PjAd_List[[i]][ ,2:ncol(LT_PjAd_List[[i]])]))
}
Maxes; LT_PjAd_MaxDiff <- Maxes

Each10_MaxDiff_LT_PjAd <- data.frame(SDs)
for (i in 1:length(LT_PjAd_List)){
  Each10_MaxDiff_LT_PjAd[ ,i+1] <- LT_PjAd_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_PjAd) <- c("SD", names); max(Each10_MaxDiff_LT_PjAd[ ,2:length(Each10_MaxDiff_LT_PjAd)])

## PjAd, Version B
LT_PjAd_vB_List <- list()
for (l in 1:length(E_LT_PjAd_vB_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_LT_PjAd_vB_PV)){
    Diff[ ,i+1] <- colMeans(E_LT_PjAd_vB_PV[[i]][[l]]) - Det_LT_PjAd_vB_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_PjAd_vB_List[[l]] <- Diff
}
names(LT_PjAd_vB_List) <- names
Maxes <- c()
for (i in 1:length(LT_PjAd_vB_List)){
  Maxes[i] <- max(data.frame(LT_PjAd_vB_List[[i]][ ,2:ncol(LT_PjAd_vB_List[[i]])]))
}
Maxes; LT_PjAd_vB_MaxDiff <- Maxes

Each10_MaxDiff_LT_PjAd_vB <- data.frame(SDs)
for (i in 1:length(LT_PjAd_vB_List)){
  Each10_MaxDiff_LT_PjAd_vB[ ,i+1] <- LT_PjAd_vB_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_PjAd_vB) <- c("SD", names); max(Each10_MaxDiff_LT_PjAd_vB[ ,2:length(Each10_MaxDiff_LT_PjAd_vB)])

#### ... ... High temp ####
## Iden
HT_Iden_List <- list()
SDs <- as.character(seq(2, 40, 2))
for (l in 1:length(E_HT_Iden_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_HT_Iden_PV)){
    Diff[ ,i+1] <- colMeans(E_HT_Iden_PV[[i]][[l]]) - Det_HT_Iden_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_Iden_List[[l]] <- Diff
}
names(HT_Iden_List) <- names
Maxes <- c()
for (i in 1:length(HT_Iden_List)){
  Maxes[i] <- max(data.frame(HT_Iden_List[[i]][ ,2:ncol(HT_Iden_List[[i]])]))
}
Maxes; HT_Iden_MaxDiff <- Maxes

EachNums <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
SD10_MaxDiff_HT_Iden <- data.frame(EachNums)
for (i in 1:length(HT_Iden_List)){
  SD10_MaxDiff_HT_Iden[ ,i+1] <- as.numeric(HT_Iden_List[[i]][5, 2:ncol(HT_Iden_List[[i]])])
}; colnames(SD10_MaxDiff_HT_Iden) <- c("Each", names); max(SD10_MaxDiff_HT_Iden[ ,2:length(SD10_MaxDiff_HT_Iden)])

## Semi
HT_Semi_List <- list()
for (l in 1:length(E_HT_Semi_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_HT_Semi_PV)){
    Diff[ ,i+1] <- colMeans(E_HT_Semi_PV[[i]][[l]]) - Det_HT_Semi_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_Semi_List[[l]] <- Diff
}
names(HT_Semi_List) <- names
Maxes <- c()
for (i in 1:length(HT_Semi_List)){
  Maxes[i] <- max(data.frame(HT_Semi_List[[i]][ ,2:ncol(HT_Semi_List[[i]])]))
}
Maxes; HT_Semi_MaxDiff <- Maxes

SD10_MaxDiff_HT_Semi <- data.frame(EachNums)
for (i in 1:length(HT_Semi_List)){
  SD10_MaxDiff_HT_Semi[ ,i+1] <- as.numeric(HT_Semi_List[[i]][5, 2:ncol(HT_Semi_List[[i]])])
}; colnames(SD10_MaxDiff_HT_Semi) <- c("Each", names); max(SD10_MaxDiff_HT_Semi[ ,2:length(SD10_MaxDiff_HT_Semi)])

## PiAd
HT_PiAd_List <- list()
for (l in 1:length(E_HT_PiAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_HT_PiAd_PV)){
    Diff[ ,i+1] <- colMeans(E_HT_PiAd_PV[[i]][[l]]) - Det_HT_PiAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_PiAd_List[[l]] <- Diff
}
names(HT_PiAd_List) <- names
Maxes <- c()
for (i in 1:length(HT_PiAd_List)){
  Maxes[i] <- max(data.frame(HT_PiAd_List[[i]][ ,2:ncol(HT_PiAd_List[[i]])]))
}
Maxes; HT_PiAd_MaxDiff <- Maxes

SD10_MaxDiff_HT_PiAd <- data.frame(EachNums)
for (i in 1:length(HT_PiAd_List)){
  SD10_MaxDiff_HT_PiAd[ ,i+1] <- as.numeric(HT_PiAd_List[[i]][5, 2:ncol(HT_PiAd_List[[i]])])
}; colnames(SD10_MaxDiff_HT_PiAd) <- c("Each", names); max(SD10_MaxDiff_HT_PiAd[ ,2:length(SD10_MaxDiff_HT_PiAd)])

## PjAd
HT_PjAd_List <- list()
for (l in 1:length(E_HT_PjAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_HT_PjAd_PV)){
    Diff[ ,i+1] <- colMeans(E_HT_PjAd_PV[[i]][[l]]) - Det_HT_PjAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_PjAd_List[[l]] <- Diff
}
names(HT_PjAd_List) <- names
Maxes <- c()
for (i in 1:length(HT_PjAd_List)){
  Maxes[i] <- max(data.frame(HT_PjAd_List[[i]][ ,2:ncol(HT_PjAd_List[[i]])]))
}
Maxes; HT_PjAd_MaxDiff <- Maxes

SD10_MaxDiff_HT_PjAd <- data.frame(EachNums)
for (i in 1:length(HT_PjAd_List)){
  SD10_MaxDiff_HT_PjAd[ ,i+1] <- as.numeric(HT_PjAd_List[[i]][5, 2:ncol(HT_PjAd_List[[i]])])
}; colnames(SD10_MaxDiff_HT_PjAd) <- c("Each", names); max(SD10_MaxDiff_HT_PjAd[ ,2:length(SD10_MaxDiff_HT_PjAd)])

## PjAd, Version B
HT_PjAd_vB_List <- list()
for (l in 1:length(E_HT_PjAd_vB_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(E_HT_PjAd_vB_PV)){
    Diff[ ,i+1] <- colMeans(E_HT_PjAd_vB_PV[[i]][[l]]) - Det_HT_PjAd_vB_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_PjAd_vB_List[[l]] <- Diff
}
names(HT_PjAd_vB_List) <- names
Maxes <- c()
for (i in 1:length(HT_PjAd_vB_List)){
  Maxes[i] <- max(data.frame(HT_PjAd_vB_List[[i]][ ,2:ncol(HT_PjAd_vB_List[[i]])]))
}
Maxes; HT_PjAd_vB_MaxDiff <- Maxes

SD10_MaxDiff_HT_PjAd_vB <- data.frame(EachNums)
for (i in 1:length(HT_PjAd_vB_List)){
  SD10_MaxDiff_HT_PjAd_vB[ ,i+1] <- as.numeric(HT_PjAd_vB_List[[i]][5, 2:ncol(HT_PjAd_vB_List[[i]])])
}; colnames(SD10_MaxDiff_HT_PjAd_vB) <- c("Each", names); max(SD10_MaxDiff_HT_PjAd_vB[ ,2:length(SD10_MaxDiff_HT_PjAd_vB)])

#### ... ... Collected outputs ####
EnvStoch_MeanDiffs <- list(Each10_MaxDiff_LT_Iden,
                           Each10_MaxDiff_LT_Semi,
                           Each10_MaxDiff_LT_PiAd,
                           Each10_MaxDiff_LT_PjAd,
                           Each10_MaxDiff_LT_PjAd_vB,
                           SD10_MaxDiff_HT_Iden,
                           SD10_MaxDiff_HT_Semi,
                           SD10_MaxDiff_HT_PiAd,
                           SD10_MaxDiff_HT_PjAd,
                           SD10_MaxDiff_HT_PjAd_vB)
# DESCRIPTION: Mean_Diffs is a list. The first four elements contain data frames, 
# containing the mean differences in longevity of co-occurrence between environmentally 
# stochastic and deterministic model simulations by introduction time and magnitude 
# of disturbance in the low-average temperature environment when the frequency of 
# disturbance is intermediate (once/day). The last four list elements contain data frames, 
# containing the mean differences in longevity of co-occurrence between environmentally 
# stochastic and deterministic model simulations by introduction time and frequency of 
# disturbance in the low-average temperature environment when the magnitude of 
# disturbance is intermediate (SD = 10).

#### ... Absolute maximum co-occurrence times by permutation, and overall ####
# DESCRIPTION: Here, we find the longest period of co-occurrence by permutation.
#### ... ... Low temp ####
SD_Seq <- seq(2, 30, 2); SDs <- c()
for (i in 1:length(SD_Seq)){
  SDs[i] <- paste0("SD_", SD_Seq[i])
}
Each <- as.character(c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

## Iden
# Absolute maxes by permutation
LT_Iden_List <- list()
for (i in 1:50){
  Iden_List_2 <- list()
  for (j in 1:length(E_LT_Iden_PV)){
    Iden_List_2[[j]] <- E_LT_Iden_PV[[j]][[i]]
  }
  names(Iden_List_2) <- Eaches
  LT_Iden_List[[i]] <- Iden_List_2
}
names(LT_Iden_List) <- names

LT_Iden_AbsMax <- list()
for (i in 1:length(LT_Iden_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_Iden_List[[1]])){
    for (k in 1:length(LT_Iden_List[[1]][[1]])){
     Intro_Max[j, k + 1] <- max(LT_Iden_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_Iden_AbsMax[[i]] <- Intro_Max
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(LT_Iden_AbsMax) <- names

# Absolute maxes overall: gives the maximum longevity of co-occurrence by introduction day, regardless of magnitude/frequency of co-occurrence
LT_Iden_AbsMaxes <- c()
for (i in 1:length(LT_Iden_AbsMax)){
  LT_Iden_AbsMaxes[i] <- max(LT_Iden_AbsMax[[i]][ ,2:ncol(LT_Iden_AbsMax[[i]])])
}

# Looking for max. diffs. when magnitude of disturbance varies, and freq. of disturbance is intermediate (once/day)
Each10_AbsMax_LT_Iden <- c()
for (i in 1:length(LT_Iden_List)){
  Each10_AbsMax_LT_Iden[i] <- max(LT_Iden_List[[i]][["Each_10"]])
}

## Semi
LT_Semi_List <- list()
for (i in 1:50){
  Semi_List_2 <- list()
  for (j in 1:length(E_LT_Semi_PV)){
    Semi_List_2[[j]] <- E_LT_Semi_PV[[j]][[i]]
  }
  names(Semi_List_2) <- Eaches
  LT_Semi_List[[i]] <- Semi_List_2
}
names(LT_Semi_List) <- names

LT_Semi_AbsMax <- list()
for (i in 1:length(LT_Semi_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_Semi_List[[1]])){
    for (k in 1:length(LT_Semi_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_Semi_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_Semi_AbsMax[[i]] <- Intro_Max
}
names(LT_Semi_AbsMax) <- names
LT_Semi_AbsMaxes <- c()
for (i in 1:length(LT_Semi_AbsMax)){
  LT_Semi_AbsMaxes[i] <- max(LT_Semi_AbsMax[[i]][ ,2:ncol(LT_Semi_AbsMax[[i]])])
}

Each10_AbsMax_LT_Semi <- c()
for (i in 1:length(LT_Semi_List)){
  Each10_AbsMax_LT_Semi[i] <- max(LT_Semi_List[[i]][["Each_10"]])
}

## PiAd
LT_PiAd_List <- list()
for (i in 1:50){
  PiAd_List_2 <- list()
  for (j in 1:length(E_LT_PiAd_PV)){
    PiAd_List_2[[j]] <- E_LT_PiAd_PV[[j]][[i]]
  }
  names(PiAd_List_2) <- Eaches
  LT_PiAd_List[[i]] <- PiAd_List_2
}
names(LT_PiAd_List) <- names

LT_PiAd_AbsMax <- list()
for (i in 1:length(LT_PiAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_PiAd_List[[1]])){
    for (k in 1:length(LT_PiAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_PiAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_PiAd_AbsMax[[i]] <- Intro_Max
}
names(LT_PiAd_AbsMax) <- names
LT_PiAd_AbsMaxes <- c()
for (i in 1:length(LT_PiAd_AbsMax)){
  LT_PiAd_AbsMaxes[i] <- max(LT_PiAd_AbsMax[[i]][ ,2:ncol(LT_PiAd_AbsMax[[i]])])
}

Each10_AbsMax_LT_PiAd <- c()
for (i in 1:length(LT_PiAd_List)){
  Each10_AbsMax_LT_PiAd[i] <- max(LT_PiAd_List[[i]][["Each_10"]])
}

## PjAd
LT_PjAd_List <- list()
for (i in 1:50){
  PjAd_List_2 <- list()
  for (j in 1:length(E_LT_PjAd_PV)){
    PjAd_List_2[[j]] <- E_LT_PjAd_PV[[j]][[i]]
  }
  names(PjAd_List_2) <- Eaches
  LT_PjAd_List[[i]] <- PjAd_List_2
}
names(LT_PjAd_List) <- names

LT_PjAd_AbsMax <- list()
for (i in 1:length(LT_PjAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_PjAd_List[[1]])){
    for (k in 1:length(LT_PjAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_PjAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_PjAd_AbsMax[[i]] <- Intro_Max
}
names(LT_PjAd_AbsMax) <- names
LT_PjAd_AbsMaxes <- c()
for (i in 1:length(LT_PjAd_AbsMax)){
  LT_PjAd_AbsMaxes[i] <- max(LT_PjAd_AbsMax[[i]][ ,2:ncol(LT_PjAd_AbsMax[[i]])])
}

Each10_AbsMax_LT_PjAd <- c()
for (i in 1:length(LT_PjAd_List)){
  Each10_AbsMax_LT_PjAd[i] <- max(LT_PjAd_List[[i]][["Each_10"]])
}

## PjAd, Version B
LT_PjAd_vB_List <- list()
for (i in 1:50){
  PjAd_vB_List_2 <- list()
  for (j in 1:length(E_LT_PjAd_vB_PV)){
    PjAd_vB_List_2[[j]] <- E_LT_PjAd_vB_PV[[j]][[i]]
  }
  names(PjAd_vB_List_2) <- Eaches
  LT_PjAd_vB_List[[i]] <- PjAd_vB_List_2
}
names(LT_PjAd_vB_List) <- names

LT_PjAd_vB_AbsMax <- list()
for (i in 1:length(LT_PjAd_vB_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_PjAd_vB_List[[1]])){
    for (k in 1:length(LT_PjAd_vB_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_PjAd_vB_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_PjAd_vB_AbsMax[[i]] <- Intro_Max
}
names(LT_PjAd_vB_AbsMax) <- names
LT_PjAd_vB_AbsMaxes <- c()
for (i in 1:length(LT_PjAd_vB_AbsMax)){
  LT_PjAd_vB_AbsMaxes[i] <- max(LT_PjAd_vB_AbsMax[[i]][ ,2:ncol(LT_PjAd_vB_AbsMax[[i]])])
}

Each10_AbsMax_LT_PjAd_vB <- c()
for (i in 1:length(LT_PjAd_vB_List)){
  Each10_AbsMax_LT_PjAd_vB[i] <- max(LT_PjAd_vB_List[[i]][["Each_10"]])
}

#### ... ... High temp ####
SD_Seq <- seq(2, 40, 2); SDs <- c()
for (i in 1:length(SD_Seq)){
  SDs[i] <- paste0("SD_", SD_Seq[i])
}
Each <- as.character(c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

## Iden
HT_Iden_List <- list()
for (i in 1:50){
  Iden_List_2 <- list()
  for (j in 1:length(E_HT_Iden_PV)){
    Iden_List_2[[j]] <- E_HT_Iden_PV[[j]][[i]]
  }
  names(Iden_List_2) <- Eaches
  HT_Iden_List[[i]] <- Iden_List_2
}
names(HT_Iden_List) <- names

HT_Iden_AbsMax <- list()
for (i in 1:length(HT_Iden_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_Iden_List[[1]])){
    for (k in 1:length(HT_Iden_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_Iden_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_Iden_AbsMax[[i]] <- Intro_Max
}
names(HT_Iden_AbsMax) <- names
HT_Iden_AbsMaxes <- c()
for (i in 1:length(HT_Iden_AbsMax)){
  HT_Iden_AbsMaxes[i] <- max(HT_Iden_AbsMax[[i]][ ,2:ncol(HT_Iden_AbsMax[[i]])])
}

# Get object with every intro, every freq., mag. 10
List <- list()
for (i in 1:length(HT_Iden_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_Iden_List[[i]])){
    df[ ,j+1] <- HT_Iden_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
# Looking for max. diffs. when mag. of disturbance is SD = 10
SD10_AbsMax_HT_Iden <- c()
for (i in 1:length(HT_Iden_List)){
  SD10_AbsMax_HT_Iden[i] <- max(List[[i]])
}

## Semi
HT_Semi_List <- list()
for (i in 1:50){
  Semi_List_2 <- list()
  for (j in 1:length(E_HT_Semi_PV)){
    Semi_List_2[[j]] <- E_HT_Semi_PV[[j]][[i]]
  }
  names(Semi_List_2) <- Eaches
  HT_Semi_List[[i]] <- Semi_List_2
}
names(HT_Semi_List) <- names

HT_Semi_AbsMax <- list()
for (i in 1:length(HT_Semi_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_Semi_List[[1]])){
    for (k in 1:length(HT_Semi_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_Semi_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_Semi_AbsMax[[i]] <- Intro_Max
}
names(HT_Semi_AbsMax) <- names
HT_Semi_AbsMaxes <- c()
for (i in 1:length(HT_Semi_AbsMax)){
  HT_Semi_AbsMaxes[i] <- max(HT_Semi_AbsMax[[i]][ ,2:ncol(HT_Semi_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_Semi_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_Semi_List[[i]])){
    df[ ,j+1] <- HT_Semi_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_Semi <- c()
for (i in 1:length(HT_Semi_List)){
  SD10_AbsMax_HT_Semi[i] <- max(List[[i]])
}

## PiAd
HT_PiAd_List <- list()
for (i in 1:50){
  PiAd_List_2 <- list()
  for (j in 1:length(E_HT_PiAd_PV)){
    PiAd_List_2[[j]] <- E_HT_PiAd_PV[[j]][[i]]
  }
  names(PiAd_List_2) <- Eaches
  HT_PiAd_List[[i]] <- PiAd_List_2
}
names(HT_PiAd_List) <- names

HT_PiAd_AbsMax <- list()
for (i in 1:length(HT_PiAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_PiAd_List[[1]])){
    for (k in 1:length(HT_PiAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_PiAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_PiAd_AbsMax[[i]] <- Intro_Max
}
names(HT_PiAd_AbsMax) <- names
HT_PiAd_AbsMaxes <- c()
for (i in 1:length(HT_PiAd_AbsMax)){
  HT_PiAd_AbsMaxes[i] <- max(HT_PiAd_AbsMax[[i]][ ,2:ncol(HT_PiAd_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_PiAd_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_PiAd_List[[i]])){
    df[ ,j+1] <- HT_PiAd_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_PiAd <- c()
for (i in 1:length(HT_PiAd_List)){
  SD10_AbsMax_HT_PiAd[i] <- max(List[[i]])
}

## PjAd
HT_PjAd_List <- list()
for (i in 1:50){
  PjAd_List_2 <- list()
  for (j in 1:length(E_HT_PjAd_PV)){
    PjAd_List_2[[j]] <- E_HT_PjAd_PV[[j]][[i]]
  }
  names(PjAd_List_2) <- Eaches
  HT_PjAd_List[[i]] <- PjAd_List_2
}
names(HT_PjAd_List) <- names

HT_PjAd_AbsMax <- list()
for (i in 1:length(HT_PjAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_PjAd_List[[1]])){
    for (k in 1:length(HT_PjAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_PjAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_PjAd_AbsMax[[i]] <- Intro_Max
}
names(HT_PjAd_AbsMax) <- names
HT_PjAd_AbsMaxes <- c()
for (i in 1:length(HT_PjAd_AbsMax)){
  HT_PjAd_AbsMaxes[i] <- max(HT_PjAd_AbsMax[[i]][ ,2:ncol(HT_PjAd_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_PjAd_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_PjAd_List[[i]])){
    df[ ,j+1] <- HT_PjAd_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_PjAd <- c()
for (i in 1:length(HT_PjAd_List)){
  SD10_AbsMax_HT_PjAd[i] <- max(List[[i]])
}

## PjAd, Version B
HT_PjAd_vB_List <- list()
for (i in 1:50){
  PjAd_vB_List_2 <- list()
  for (j in 1:length(E_HT_PjAd_vB_PV)){
    PjAd_vB_List_2[[j]] <- E_HT_PjAd_vB_PV[[j]][[i]]
  }
  names(PjAd_vB_List_2) <- Eaches
  HT_PjAd_vB_List[[i]] <- PjAd_vB_List_2
}
names(HT_PjAd_vB_List) <- names

HT_PjAd_vB_AbsMax <- list()
for (i in 1:length(HT_PjAd_vB_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_PjAd_vB_List[[1]])){
    for (k in 1:length(HT_PjAd_vB_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_PjAd_vB_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_PjAd_vB_AbsMax[[i]] <- Intro_Max
}
names(HT_PjAd_vB_AbsMax) <- names
HT_PjAd_vB_AbsMaxes <- c()
for (i in 1:length(HT_PjAd_vB_AbsMax)){
  HT_PjAd_vB_AbsMaxes[i] <- max(HT_PjAd_vB_AbsMax[[i]][ ,2:ncol(HT_PjAd_vB_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_PjAd_vB_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_PjAd_vB_List[[i]])){
    df[ ,j+1] <- HT_PjAd_vB_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_PjAd_vB <- c()
for (i in 1:length(HT_PjAd_vB_List)){
  SD10_AbsMax_HT_PjAd_vB[i] <- max(List[[i]])
}

#### ... ... Colleted outputs ####
EnvStoch_AbsMaxes <- data.frame(Intros,
                                LT_Iden_AbsMaxes,
                                LT_Semi_AbsMaxes,
                                LT_PiAd_AbsMaxes,
                                LT_PjAd_AbsMaxes,
                                LT_PjAd_vB_AbsMaxes,
                                HT_Iden_AbsMaxes,
                                HT_Semi_AbsMaxes,
                                HT_PiAd_AbsMaxes,
                                HT_PjAd_AbsMaxes,
                                HT_PjAd_vB_AbsMaxes)
# DESCRIPTION: AbsMaxes is a data frame. Each column contains the maximum longevity of 
# co-occurrence by introduction day, regardless of magnitude/frequency of co-occurrence. 
# Columns 2-6 show results from the low-average temperature environment; columns
# 7-11 show results from the high-average temperature environment.

EnvStoch_CaseMaxes <- data.frame(Intros,
                                 Each10_AbsMax_LT_Iden,
                                 Each10_AbsMax_LT_Semi,
                                 Each10_AbsMax_LT_PiAd,
                                 Each10_AbsMax_LT_PjAd,
                                 Each10_AbsMax_LT_PjAd_vB,
                                 SD10_AbsMax_HT_Iden,
                                 SD10_AbsMax_HT_Semi,
                                 SD10_AbsMax_HT_PiAd,
                                 SD10_AbsMax_HT_PjAd,
                                 SD10_AbsMax_HT_PjAd_vB)
# DESCRIPTION: CaseMaxes is a data frame. Each column contains the maximum longevity of 
# co-occurrence by introduction day, when magnitude of disturbance varies/frequency of 
# disturbance is intermediate (once/day) (columns 2-6), and when frequency of disturbance 
# varies/magnitude of disturbance is intermediate (SD = 10) (columns 7-11). 

#### Demographically and Environmentally stochastic ####
## Low-average temperature environment
DE_LT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_Iden_PV.rds")
DE_LT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_Semi_PV.rds")
DE_LT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_PiAd_PV.rds")
DE_LT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_PjAd_PV.rds")
DE_LT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_PjAd_vB_PV.rds")
## High-average temperature environment
DE_HT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_Iden_PV.rds")
DE_HT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_Semi_PV.rds")
DE_HT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_PiAd_PV.rds")
DE_HT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_PjAd_PV.rds")
DE_HT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_PjAd_vB_PV.rds")

#### ... Differences between deterministic and demographically/environmentally stochastic outputs by thermal scenario ####
# DESCRIPTION: Here, we analyse which thermal scenario produces the longest period of co-occurrence, 
# relative to the deterministic case. 1s denote the first thermal scenario; 2s denote the second, etc.
#### ... ... Low temp ####
List_LT <- list()
SDs <- as.character(seq(2, 30, 2))
for (l in 1:length(DE_LT_Iden_PV[[1]])){
  MaxLT <- data.frame(SDs)
  for (i in 1:length(DE_LT_Iden_PV)){
    I <- colMeans(DE_LT_Iden_PV[[i]][[l]]) - Det_LT_Iden_PV[l]
    S <- colMeans(DE_LT_Semi_PV[[i]][[l]]) - Det_LT_Semi_PV[l]
    Pi <- colMeans(DE_LT_PiAd_PV[[i]][[l]]) - Det_LT_PiAd_PV[l]
    Pj <- colMeans(DE_LT_PjAd_PV[[i]][[l]]) - Det_LT_PjAd_PV[l]
    Pj_vB <- colMeans(DE_LT_PjAd_vB_PV[[i]][[l]]) - Det_LT_PjAd_vB_PV[l]
    x <- cbind(I, S, Pi, Pj, Pj_vB)
    for (j in 1:nrow(x)){
      MaxLT[j,i+1] <- which.max(x[j, ])
    }
  }
  colnames(MaxLT) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  List_LT[[l]] <- MaxLT
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(List_LT) <- names

#### ... ... High temp ####
List_HT <- list()
SDs <- as.character(seq(2, 40, 2))
for (l in 1:length(DE_HT_Iden_PV[[1]])){
  MaxHT <- data.frame(SDs)
  for (i in 1:length(DE_HT_Iden_PV)){
    I <- colMeans(DE_HT_Iden_PV[[i]][[l]]) - Det_HT_Iden_PV[l]
    S <- colMeans(DE_HT_Semi_PV[[i]][[l]]) - Det_HT_Semi_PV[l]
    Pi <- colMeans(DE_HT_PiAd_PV[[i]][[l]]) - Det_HT_PiAd_PV[l]
    Pj <- colMeans(DE_HT_PjAd_PV[[i]][[l]]) - Det_HT_PjAd_PV[l]
    Pj_vB <- colMeans(DE_HT_PjAd_vB_PV[[i]][[l]]) - Det_HT_PjAd_vB_PV[l]
    x <- cbind(I, S, Pi, Pj, Pj_vB)
    for (j in 1:nrow(x)){
      MaxHT[j,i+1] <- which.max(x[j, ])
    }
  }
  colnames(MaxHT) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  List_HT[[l]] <- MaxHT
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(List_HT) <- names

#### ... How much does demographic/environmental variation extend co-occurrence times? ####
# DESCRIPTION: Here, we analyse the difference in longevity of co-occurrence 
# between demogrpahically/environmentally stochastic and deterministic model simulations.
#### ... ... Low temp ####
## Iden
LT_Iden_List <- list()
SDs <- as.character(seq(2, 30, 2))
# This loop goes through the persistence vectors and averages by condition, the subtracts the deterministic outcome from those averages to give average difference between env. stoch. and deterministic outcomes by permutation.
for (l in 1:length(DE_LT_Iden_PV[[1]])){ # For each intro time
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_LT_Iden_PV)){ # For each frequency
    Diff[ ,i+1] <- colMeans(DE_LT_Iden_PV[[i]][[l]]) - Det_LT_Iden_PV[l] # Where different columns are different magnitudes
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_Iden_List[[l]] <- Diff
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(LT_Iden_List) <- names
Maxes <- c()
for (i in 1:length(LT_Iden_List)){ 
  Maxes[i] <- max(data.frame(LT_Iden_List[[i]][ ,2:ncol(LT_Iden_List[[i]])])) # Finds the largest value per intro time (so by mag. and freq.)
}
Maxes; LT_Iden_MaxDiff <- Maxes

# Looking for max. diffs. when freq. of disturbance is intermediate (once/day)
Each10_MaxDiff_LT_Iden <- data.frame(SDs) 
for (i in 1:length(LT_Iden_List)){
  Each10_MaxDiff_LT_Iden[ ,i+1] <- LT_Iden_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_Iden) <- c("SD", names); max(Each10_MaxDiff_LT_Iden[ ,2:length(Each10_MaxDiff_LT_Iden)])
# Each10_MaxDiff_LT_Iden contains the mean difference in longevity of co-occurrence 
# between environmentally stochastic and deterministic model simulations by introduction 
# time and magnitude of disturbance when the frequency of disturbance is intermediate (once/day).

## Semi
LT_Semi_List <- list()
for (l in 1:length(DE_LT_Semi_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_LT_Semi_PV)){
    Diff[ ,i+1] <- colMeans(DE_LT_Semi_PV[[i]][[l]]) - Det_LT_Semi_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_Semi_List[[l]] <- Diff
}
names(LT_Semi_List) <- names
Maxes <- c()
for (i in 1:length(LT_Semi_List)){
  Maxes[i] <- max(data.frame(LT_Semi_List[[i]][ ,2:ncol(LT_Semi_List[[i]])]))
}
Maxes; LT_Semi_MaxDiff <- Maxes

Each10_MaxDiff_LT_Semi <- data.frame(SDs)
for (i in 1:length(LT_Semi_List)){
  Each10_MaxDiff_LT_Semi[ ,i+1] <- LT_Semi_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_Semi) <- c("SD", names); max(Each10_MaxDiff_LT_Semi[ ,2:length(Each10_MaxDiff_LT_Semi)])

## PiAd
LT_PiAd_List <- list()
for (l in 1:length(DE_LT_PiAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_LT_PiAd_PV)){
    Diff[ ,i+1] <- colMeans(DE_LT_PiAd_PV[[i]][[l]]) - Det_LT_PiAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_PiAd_List[[l]] <- Diff
}
names(LT_PiAd_List) <- names
Maxes <- c()
for (i in 1:length(LT_PiAd_List)){
  Maxes[i] <- max(data.frame(LT_PiAd_List[[i]][ ,2:ncol(LT_PiAd_List[[i]])]))
}
Maxes; LT_PiAd_MaxDiff <- Maxes

Each10_MaxDiff_LT_PiAd <- data.frame(SDs)
for (i in 1:length(LT_PiAd_List)){
  Each10_MaxDiff_LT_PiAd[ ,i+1] <- LT_PiAd_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_PiAd) <- c("SD", names); max(Each10_MaxDiff_LT_PiAd[ ,2:length(Each10_MaxDiff_LT_PiAd)])

## PjAd
LT_PjAd_List <- list()
for (l in 1:length(DE_LT_PjAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_LT_PjAd_PV)){
    Diff[ ,i+1] <- colMeans(DE_LT_PjAd_PV[[i]][[l]]) - Det_LT_PjAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_PjAd_List[[l]] <- Diff
}
names(LT_PjAd_List) <- names
Maxes <- c()
for (i in 1:length(LT_PjAd_List)){
  Maxes[i] <- max(data.frame(LT_PjAd_List[[i]][ ,2:ncol(LT_PjAd_List[[i]])]))
}
Maxes; LT_PjAd_MaxDiff <- Maxes

Each10_MaxDiff_LT_PjAd <- data.frame(SDs)
for (i in 1:length(LT_PjAd_List)){
  Each10_MaxDiff_LT_PjAd[ ,i+1] <- LT_PjAd_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_PjAd) <- c("SD", names); max(Each10_MaxDiff_LT_PjAd[ ,2:length(Each10_MaxDiff_LT_PjAd)])

## PjAd, Version B
LT_PjAd_vB_List <- list()
for (l in 1:length(DE_LT_PjAd_vB_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_LT_PjAd_vB_PV)){
    Diff[ ,i+1] <- colMeans(DE_LT_PjAd_vB_PV[[i]][[l]]) - Det_LT_PjAd_vB_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  LT_PjAd_vB_List[[l]] <- Diff
}
names(LT_PjAd_vB_List) <- names
Maxes <- c()
for (i in 1:length(LT_PjAd_vB_List)){
  Maxes[i] <- max(data.frame(LT_PjAd_vB_List[[i]][ ,2:ncol(LT_PjAd_vB_List[[i]])]))
}
Maxes; LT_PjAd_vB_MaxDiff <- Maxes

Each10_MaxDiff_LT_PjAd_vB <- data.frame(SDs)
for (i in 1:length(LT_PjAd_vB_List)){
  Each10_MaxDiff_LT_PjAd_vB[ ,i+1] <- LT_PjAd_vB_List[[i]][ ,5]
}; colnames(Each10_MaxDiff_LT_PjAd_vB) <- c("SD", names); max(Each10_MaxDiff_LT_PjAd_vB[ ,2:length(Each10_MaxDiff_LT_PjAd_vB)])

#### ... ... High temp ####
## Iden
HT_Iden_List <- list()
SDs <- as.character(seq(2, 40, 2))
for (l in 1:length(DE_HT_Iden_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_HT_Iden_PV)){
    Diff[ ,i+1] <- colMeans(DE_HT_Iden_PV[[i]][[l]]) - Det_HT_Iden_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_Iden_List[[l]] <- Diff
}
names(HT_Iden_List) <- names
Maxes <- c()
for (i in 1:length(HT_Iden_List)){
  Maxes[i] <- max(data.frame(HT_Iden_List[[i]][ ,2:ncol(HT_Iden_List[[i]])]))
}
Maxes; HT_Iden_MaxDiff <- Maxes

EachNums <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
SD10_MaxDiff_HT_Iden <- data.frame(EachNums)
for (i in 1:length(HT_Iden_List)){
  SD10_MaxDiff_HT_Iden[ ,i+1] <- as.numeric(HT_Iden_List[[i]][5, 2:ncol(HT_Iden_List[[i]])])
}; colnames(SD10_MaxDiff_HT_Iden) <- c("Each", names); max(SD10_MaxDiff_HT_Iden[ ,2:length(SD10_MaxDiff_HT_Iden)])

## Semi
HT_Semi_List <- list()
for (l in 1:length(DE_HT_Semi_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_HT_Semi_PV)){
    Diff[ ,i+1] <- colMeans(DE_HT_Semi_PV[[i]][[l]]) - Det_HT_Semi_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_Semi_List[[l]] <- Diff
}
names(HT_Semi_List) <- names
Maxes <- c()
for (i in 1:length(HT_Semi_List)){
  Maxes[i] <- max(data.frame(HT_Semi_List[[i]][ ,2:ncol(HT_Semi_List[[i]])]))
}
Maxes; HT_Semi_MaxDiff <- Maxes

SD10_MaxDiff_HT_Semi <- data.frame(EachNums)
for (i in 1:length(HT_Semi_List)){
  SD10_MaxDiff_HT_Semi[ ,i+1] <- as.numeric(HT_Semi_List[[i]][5, 2:ncol(HT_Semi_List[[i]])])
}; colnames(SD10_MaxDiff_HT_Semi) <- c("Each", names); max(SD10_MaxDiff_HT_Semi[ ,2:length(SD10_MaxDiff_HT_Semi)])

## PiAd
HT_PiAd_List <- list()
for (l in 1:length(DE_HT_PiAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_HT_PiAd_PV)){
    Diff[ ,i+1] <- colMeans(DE_HT_PiAd_PV[[i]][[l]]) - Det_HT_PiAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_PiAd_List[[l]] <- Diff
}
names(HT_PiAd_List) <- names
Maxes <- c()
for (i in 1:length(HT_PiAd_List)){
  Maxes[i] <- max(data.frame(HT_PiAd_List[[i]][ ,2:ncol(HT_PiAd_List[[i]])]))
}
Maxes; HT_PiAd_MaxDiff <- Maxes

SD10_MaxDiff_HT_PiAd <- data.frame(EachNums)
for (i in 1:length(HT_PiAd_List)){
  SD10_MaxDiff_HT_PiAd[ ,i+1] <- as.numeric(HT_PiAd_List[[i]][5, 2:ncol(HT_PiAd_List[[i]])])
}; colnames(SD10_MaxDiff_HT_PiAd) <- c("Each", names); max(SD10_MaxDiff_HT_PiAd[ ,2:length(SD10_MaxDiff_HT_PiAd)])

## PjAd
HT_PjAd_List <- list()
for (l in 1:length(DE_HT_PjAd_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_HT_PjAd_PV)){
    Diff[ ,i+1] <- colMeans(DE_HT_PjAd_PV[[i]][[l]]) - Det_HT_PjAd_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_PjAd_List[[l]] <- Diff
}
names(HT_PjAd_List) <- names
Maxes <- c()
for (i in 1:length(HT_PjAd_List)){
  Maxes[i] <- max(data.frame(HT_PjAd_List[[i]][ ,2:ncol(HT_PjAd_List[[i]])]))
}
Maxes; HT_PjAd_MaxDiff <- Maxes

SD10_MaxDiff_HT_PjAd <- data.frame(EachNums)
for (i in 1:length(HT_PjAd_List)){
  SD10_MaxDiff_HT_PjAd[ ,i+1] <- as.numeric(HT_PjAd_List[[i]][5, 2:ncol(HT_PjAd_List[[i]])])
}; colnames(SD10_MaxDiff_HT_PjAd) <- c("Each", names); max(SD10_MaxDiff_HT_PjAd[ ,2:length(SD10_MaxDiff_HT_PjAd)])

## PjAd, Version B
HT_PjAd_vB_List <- list()
for (l in 1:length(DE_HT_PjAd_vB_PV[[1]])){
  Diff <- data.frame(SDs)
  for (i in 1:length(DE_HT_PjAd_vB_PV)){
    Diff[ ,i+1] <- colMeans(DE_HT_PjAd_vB_PV[[i]][[l]]) - Det_HT_PjAd_vB_PV[l]
  }
  colnames(Diff) <- c("SD", "Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
  Diff <- as.data.frame(sapply(Diff,as.numeric))
  HT_PjAd_vB_List[[l]] <- Diff
}
names(HT_PjAd_vB_List) <- names
Maxes <- c()
for (i in 1:length(HT_PjAd_vB_List)){
  Maxes[i] <- max(data.frame(HT_PjAd_vB_List[[i]][ ,2:ncol(HT_PjAd_vB_List[[i]])]))
}
Maxes; HT_PjAd_vB_MaxDiff <- Maxes

SD10_MaxDiff_HT_PjAd_vB <- data.frame(EachNums)
for (i in 1:length(HT_PjAd_vB_List)){
  SD10_MaxDiff_HT_PjAd_vB[ ,i+1] <- as.numeric(HT_PjAd_vB_List[[i]][5, 2:ncol(HT_PjAd_vB_List[[i]])])
}; colnames(SD10_MaxDiff_HT_PjAd_vB) <- c("Each", names); max(SD10_MaxDiff_HT_PjAd_vB[ ,2:length(SD10_MaxDiff_HT_PjAd_vB)])

#### ... ... Collected outputs ####
DemEnvStoch_MeanDiffs <- list(Each10_MaxDiff_LT_Iden,
                              Each10_MaxDiff_LT_Semi,
                              Each10_MaxDiff_LT_PiAd,
                              Each10_MaxDiff_LT_PjAd,
                              Each10_MaxDiff_LT_PjAd_vB,
                              SD10_MaxDiff_HT_Iden,
                              SD10_MaxDiff_HT_Semi,
                              SD10_MaxDiff_HT_PiAd,
                              SD10_MaxDiff_HT_PjAd,
                              SD10_MaxDiff_HT_PjAd_vB)
# DESCRIPTION: Mean_Diffs is a list. The first four elements contain data frames, 
# containing the mean differences in longevity of co-occurrence between demographically/environmentally 
# stochastic and deterministic model simulations by introduction time and magnitude 
# of disturbance in the low-average temperature environment when the frequency of 
# disturbance is intermediate (once/day). The last four list elements contain data frames, 
# containing the mean differences in longevity of co-occurrence between demographically/environmentally 
# stochastic and deterministic model simulations by introduction time and frequency of 
# disturbance in the low-average temperature environment when the magnitude of 
# disturbance is intermediate (SD = 10).

#### ... Absolute maximum co-occurrence times by permutation, and overall ####
# DESCRIPTION: Here, we find the longest period of co-occurrence by permutation.
#### ... ... Low temp ####
SD_Seq <- seq(2, 30, 2); SDs <- c()
for (i in 1:length(SD_Seq)){
  SDs[i] <- paste0("SD_", SD_Seq[i])
}
Each <- as.character(c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

## Iden
# Absolute maxes by permutation
LT_Iden_List <- list()
for (i in 1:50){
  Iden_List_2 <- list()
  for (j in 1:length(DE_LT_Iden_PV)){
    Iden_List_2[[j]] <- DE_LT_Iden_PV[[j]][[i]]
  }
  names(Iden_List_2) <- Eaches
  LT_Iden_List[[i]] <- Iden_List_2
}
names(LT_Iden_List) <- names

LT_Iden_AbsMax <- list()
for (i in 1:length(LT_Iden_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_Iden_List[[1]])){
    for (k in 1:length(LT_Iden_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_Iden_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_Iden_AbsMax[[i]] <- Intro_Max
}
names <- c()
for (i in 1:50){
  names[i] <- paste0("Intro_", i-1)
}
names(LT_Iden_AbsMax) <- names

# Absolute maxes overall: gives the maximum longevity of co-occurrence by introduction day, regardless of magnitude/frequency of co-occurrence
LT_Iden_AbsMaxes <- c()
for (i in 1:length(LT_Iden_AbsMax)){
  LT_Iden_AbsMaxes[i] <- max(LT_Iden_AbsMax[[i]][ ,2:ncol(LT_Iden_AbsMax[[i]])])
}

# Looking for max. diffs. when magnitude of disturbance varies, and freq. of disturbance is intermediate (once/day)
Each10_AbsMax_LT_Iden <- c()
for (i in 1:length(LT_Iden_List)){
  Each10_AbsMax_LT_Iden[i] <- max(LT_Iden_List[[i]][["Each_10"]])
}

## Semi
LT_Semi_List <- list()
for (i in 1:50){
  Semi_List_2 <- list()
  for (j in 1:length(DE_LT_Semi_PV)){
    Semi_List_2[[j]] <- DE_LT_Semi_PV[[j]][[i]]
  }
  names(Semi_List_2) <- Eaches
  LT_Semi_List[[i]] <- Semi_List_2
}
names(LT_Semi_List) <- names

LT_Semi_AbsMax <- list()
for (i in 1:length(LT_Semi_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_Semi_List[[1]])){
    for (k in 1:length(LT_Semi_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_Semi_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_Semi_AbsMax[[i]] <- Intro_Max
}
names(LT_Semi_AbsMax) <- names
LT_Semi_AbsMaxes <- c()
for (i in 1:length(LT_Semi_AbsMax)){
  LT_Semi_AbsMaxes[i] <- max(LT_Semi_AbsMax[[i]][ ,2:ncol(LT_Semi_AbsMax[[i]])])
}

Each10_AbsMax_LT_Semi <- c()
for (i in 1:length(LT_Semi_List)){
  Each10_AbsMax_LT_Semi[i] <- max(LT_Semi_List[[i]][["Each_10"]])
}

## PiAd
LT_PiAd_List <- list()
for (i in 1:50){
  PiAd_List_2 <- list()
  for (j in 1:length(DE_LT_PiAd_PV)){
    PiAd_List_2[[j]] <- DE_LT_PiAd_PV[[j]][[i]]
  }
  names(PiAd_List_2) <- Eaches
  LT_PiAd_List[[i]] <- PiAd_List_2
}
names(LT_PiAd_List) <- names

LT_PiAd_AbsMax <- list()
for (i in 1:length(LT_PiAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_PiAd_List[[1]])){
    for (k in 1:length(LT_PiAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_PiAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_PiAd_AbsMax[[i]] <- Intro_Max
}
names(LT_PiAd_AbsMax) <- names
LT_PiAd_AbsMaxes <- c()
for (i in 1:length(LT_PiAd_AbsMax)){
  LT_PiAd_AbsMaxes[i] <- max(LT_PiAd_AbsMax[[i]][ ,2:ncol(LT_PiAd_AbsMax[[i]])])
}

Each10_AbsMax_LT_PiAd <- c()
for (i in 1:length(LT_PiAd_List)){
  Each10_AbsMax_LT_PiAd[i] <- max(LT_PiAd_List[[i]][["Each_10"]])
}

## PjAd
LT_PjAd_List <- list()
for (i in 1:50){
  PjAd_List_2 <- list()
  for (j in 1:length(DE_LT_PjAd_PV)){
    PjAd_List_2[[j]] <- DE_LT_PjAd_PV[[j]][[i]]
  }
  names(PjAd_List_2) <- Eaches
  LT_PjAd_List[[i]] <- PjAd_List_2
}
names(LT_PjAd_List) <- names

LT_PjAd_AbsMax <- list()
for (i in 1:length(LT_PjAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_PjAd_List[[1]])){
    for (k in 1:length(LT_PjAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_PjAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_PjAd_AbsMax[[i]] <- Intro_Max
}
names(LT_PjAd_AbsMax) <- names
LT_PjAd_AbsMaxes <- c()
for (i in 1:length(LT_PjAd_AbsMax)){
  LT_PjAd_AbsMaxes[i] <- max(LT_PjAd_AbsMax[[i]][ ,2:ncol(LT_PjAd_AbsMax[[i]])])
}

Each10_AbsMax_LT_PjAd <- c()
for (i in 1:length(LT_PjAd_List)){
  Each10_AbsMax_LT_PjAd[i] <- max(LT_PjAd_List[[i]][["Each_10"]])
}

## PjAd, Version B
LT_PjAd_vB_List <- list()
for (i in 1:50){
  PjAd_vB_List_2 <- list()
  for (j in 1:length(DE_LT_PjAd_vB_PV)){
    PjAd_vB_List_2[[j]] <- DE_LT_PjAd_vB_PV[[j]][[i]]
  }
  names(PjAd_vB_List_2) <- Eaches
  LT_PjAd_vB_List[[i]] <- PjAd_vB_List_2
}
names(LT_PjAd_vB_List) <- names

LT_PjAd_vB_AbsMax <- list()
for (i in 1:length(LT_PjAd_vB_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(LT_PjAd_vB_List[[1]])){
    for (k in 1:length(LT_PjAd_vB_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(LT_PjAd_vB_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  LT_PjAd_vB_AbsMax[[i]] <- Intro_Max
}
names(LT_PjAd_vB_AbsMax) <- names
LT_PjAd_vB_AbsMaxes <- c()
for (i in 1:length(LT_PjAd_vB_AbsMax)){
  LT_PjAd_vB_AbsMaxes[i] <- max(LT_PjAd_vB_AbsMax[[i]][ ,2:ncol(LT_PjAd_vB_AbsMax[[i]])])
}

Each10_AbsMax_LT_PjAd_vB <- c()
for (i in 1:length(LT_PjAd_vB_List)){
  Each10_AbsMax_LT_PjAd_vB[i] <- max(LT_PjAd_vB_List[[i]][["Each_10"]])
}

#### ... ... High temp ####
SD_Seq <- seq(2, 40, 2); SDs <- c()
for (i in 1:length(SD_Seq)){
  SDs[i] <- paste0("SD_", SD_Seq[i])
}
Each <- as.character(c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

## Iden
HT_Iden_List <- list()
for (i in 1:50){
  Iden_List_2 <- list()
  for (j in 1:length(DE_HT_Iden_PV)){
    Iden_List_2[[j]] <- DE_HT_Iden_PV[[j]][[i]]
  }
  names(Iden_List_2) <- Eaches
  HT_Iden_List[[i]] <- Iden_List_2
}
names(HT_Iden_List) <- names

HT_Iden_AbsMax <- list()
for (i in 1:length(HT_Iden_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_Iden_List[[1]])){
    for (k in 1:length(HT_Iden_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_Iden_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_Iden_AbsMax[[i]] <- Intro_Max
}
names(HT_Iden_AbsMax) <- names
HT_Iden_AbsMaxes <- c()
for (i in 1:length(HT_Iden_AbsMax)){
  HT_Iden_AbsMaxes[i] <- max(HT_Iden_AbsMax[[i]][ ,2:ncol(HT_Iden_AbsMax[[i]])])
}

# Get object with every intro, every freq., mag. 10
List <- list()
for (i in 1:length(HT_Iden_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_Iden_List[[i]])){
    df[ ,j+1] <- HT_Iden_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
# Looking for max. diffs. when mag. of disturbance is SD = 10
SD10_AbsMax_HT_Iden <- c()
for (i in 1:length(HT_Iden_List)){
  SD10_AbsMax_HT_Iden[i] <- max(List[[i]])
}

## Semi
HT_Semi_List <- list()
for (i in 1:50){
  Semi_List_2 <- list()
  for (j in 1:length(DE_HT_Semi_PV)){
    Semi_List_2[[j]] <- DE_HT_Semi_PV[[j]][[i]]
  }
  names(Semi_List_2) <- Eaches
  HT_Semi_List[[i]] <- Semi_List_2
}
names(HT_Semi_List) <- names

HT_Semi_AbsMax <- list()
for (i in 1:length(HT_Semi_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_Semi_List[[1]])){
    for (k in 1:length(HT_Semi_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_Semi_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_Semi_AbsMax[[i]] <- Intro_Max
}
names(HT_Semi_AbsMax) <- names
HT_Semi_AbsMaxes <- c()
for (i in 1:length(HT_Semi_AbsMax)){
  HT_Semi_AbsMaxes[i] <- max(HT_Semi_AbsMax[[i]][ ,2:ncol(HT_Semi_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_Semi_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_Semi_List[[i]])){
    df[ ,j+1] <- HT_Semi_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_Semi <- c()
for (i in 1:length(HT_Semi_List)){
  SD10_AbsMax_HT_Semi[i] <- max(List[[i]])
}

## PiAd
HT_PiAd_List <- list()
for (i in 1:50){
  PiAd_List_2 <- list()
  for (j in 1:length(DE_HT_PiAd_PV)){
    PiAd_List_2[[j]] <- DE_HT_PiAd_PV[[j]][[i]]
  }
  names(PiAd_List_2) <- Eaches
  HT_PiAd_List[[i]] <- PiAd_List_2
}
names(HT_PiAd_List) <- names

HT_PiAd_AbsMax <- list()
for (i in 1:length(HT_PiAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_PiAd_List[[1]])){
    for (k in 1:length(HT_PiAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_PiAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_PiAd_AbsMax[[i]] <- Intro_Max
}
names(HT_PiAd_AbsMax) <- names
HT_PiAd_AbsMaxes <- c()
for (i in 1:length(HT_PiAd_AbsMax)){
  HT_PiAd_AbsMaxes[i] <- max(HT_PiAd_AbsMax[[i]][ ,2:ncol(HT_PiAd_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_PiAd_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_PiAd_List[[i]])){
    df[ ,j+1] <- HT_PiAd_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_PiAd <- c()
for (i in 1:length(HT_PiAd_List)){
  SD10_AbsMax_HT_PiAd[i] <- max(List[[i]])
}

## PjAd
HT_PjAd_List <- list()
for (i in 1:50){
  PjAd_List_2 <- list()
  for (j in 1:length(DE_HT_PjAd_PV)){
    PjAd_List_2[[j]] <- DE_HT_PjAd_PV[[j]][[i]]
  }
  names(PjAd_List_2) <- Eaches
  HT_PjAd_List[[i]] <- PjAd_List_2
}
names(HT_PjAd_List) <- names

HT_PjAd_AbsMax <- list()
for (i in 1:length(HT_PjAd_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_PjAd_List[[1]])){
    for (k in 1:length(HT_PjAd_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_PjAd_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_PjAd_AbsMax[[i]] <- Intro_Max
}
names(HT_PjAd_AbsMax) <- names
HT_PjAd_AbsMaxes <- c()
for (i in 1:length(HT_PjAd_AbsMax)){
  HT_PjAd_AbsMaxes[i] <- max(HT_PjAd_AbsMax[[i]][ ,2:ncol(HT_PjAd_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_PjAd_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_PjAd_List[[i]])){
    df[ ,j+1] <- HT_PjAd_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_PjAd <- c()
for (i in 1:length(HT_PjAd_List)){
  SD10_AbsMax_HT_PjAd[i] <- max(List[[i]])
}

## PjAd, Version B
HT_PjAd_vB_List <- list()
for (i in 1:50){
  PjAd_vB_List_2 <- list()
  for (j in 1:length(DE_HT_PjAd_vB_PV)){
    PjAd_vB_List_2[[j]] <- DE_HT_PjAd_vB_PV[[j]][[i]]
  }
  names(PjAd_vB_List_2) <- Eaches
  HT_PjAd_vB_List[[i]] <- PjAd_vB_List_2
}
names(HT_PjAd_vB_List) <- names

HT_PjAd_vB_AbsMax <- list()
for (i in 1:length(HT_PjAd_vB_List)){
  Intro_Max <- data.frame(Each)
  for (j in 1:length(HT_PjAd_vB_List[[1]])){
    for (k in 1:length(HT_PjAd_vB_List[[1]][[1]])){
      Intro_Max[j, k + 1] <- max(HT_PjAd_vB_List[[i]][[j]][ ,k])
    }
    colnames(Intro_Max) <- c("Each", SDs)
  }
  HT_PjAd_vB_AbsMax[[i]] <- Intro_Max
}
names(HT_PjAd_vB_AbsMax) <- names
HT_PjAd_vB_AbsMaxes <- c()
for (i in 1:length(HT_PjAd_vB_AbsMax)){
  HT_PjAd_vB_AbsMaxes[i] <- max(HT_PjAd_vB_AbsMax[[i]][ ,2:ncol(HT_PjAd_vB_AbsMax[[i]])])
}

List <- list()
for (i in 1:length(HT_PjAd_vB_List)){
  df <- data.frame(Sim = seq(1,1000,1))
  for (j in 1:length(HT_PjAd_vB_List[[i]])){
    df[ ,j+1] <- HT_PjAd_vB_List[[i]][[j]][ ,5]
  }
  df <- df[ ,2:length(df)]; colnames(df) <- Eaches
  List[[i]] <- df
}
names(List) <- Intros
SD10_AbsMax_HT_PjAd_vB <- c()
for (i in 1:length(HT_PjAd_vB_List)){
  SD10_AbsMax_HT_PjAd_vB[i] <- max(List[[i]])
}

#### ... ... Colleted outputs ####
DemEnvStoch_AbsMaxes <- data.frame(Intros,
                                   LT_Iden_AbsMaxes,
                                   LT_Semi_AbsMaxes,
                                   LT_PiAd_AbsMaxes,
                                   LT_PjAd_AbsMaxes,
                                   LT_PjAd_vB_AbsMaxes,
                                   HT_Iden_AbsMaxes,
                                   HT_Semi_AbsMaxes,
                                   HT_PiAd_AbsMaxes,
                                   HT_PjAd_AbsMaxes,
                                   HT_PjAd_vB_AbsMaxes)
# DESCRIPTION: AbsMaxes is a data frame. Each column contains the maximum longevity of 
# co-occurrence by introduction day, regardless of magnitude/frequency of co-occurrence. 
# Columns 2-6 show results from the low-average temperature environment; columns
# 7-11 show results from the high-average temperature environment.

DemEnvStoch_CaseMaxes <- data.frame(Intros,
                                    Each10_AbsMax_LT_Iden,
                                    Each10_AbsMax_LT_Semi,
                                    Each10_AbsMax_LT_PiAd,
                                    Each10_AbsMax_LT_PjAd,
                                    Each10_AbsMax_LT_PjAd_vB,
                                    SD10_AbsMax_HT_Iden,
                                    SD10_AbsMax_HT_Semi,
                                    SD10_AbsMax_HT_PiAd,
                                    SD10_AbsMax_HT_PjAd,
                                    SD10_AbsMax_HT_PjAd_vB)
# DESCRIPTION: CaseMaxes is a data frame. Each column contains the maximum longevity of 
# co-occurrence by introduction day, when magnitude of disturbance varies/frequency of 
# disturbance is intermediate (once/day) (columns 2-6), and when frequency of disturbance 
# varies/magnitude of disturbance is intermediate (SD = 10) (columns 7-11).

#### Collected summary objects ####
Det_CoOc
Dem_Mean_CoOc
EnvStoch_MeanDiffs
EnvStoch_AbsMaxes
EnvStoch_CaseMaxes
DemEnvStoch_MeanDiffs
DemEnvStoch_AbsMaxes
DemEnvStoch_CaseMaxes
