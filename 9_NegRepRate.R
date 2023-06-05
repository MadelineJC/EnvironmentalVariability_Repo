#### DOC STRING ===============================================================
# This script uses quadratic thermal performance curves to explore the impacts
# of negative replication rates on the relationship between magnitude of 
# thermal disturbance and longevity of co-occurrence. 

# If you choose to run this script, it will save outputs as .rds files here:
# "EnvironmentalVariability_Repo/Generated_Outputs"

# To see outputs of this script without running it, go to 
# "EnvironmentalVariability_Repo/Outputs/NegativeRepRate_Outputs". Files ending 
# in "...Output.rds" are time series. Files ending in "..._PV" are "persistence 
# vectors (containing the number of time steps for which both parasites populations 
# had abundances greater than one by simulation).
#### END DOC STRING ===========================================================

#### Environmentally stochastic model ####
EnvStoch_Model <- function(Pi_Last, Pj_Last, I_Last, ri, rj){
  ts = 0.1
  k = 0.01; p = 1; o = 1000
  
  Pi_Next = (1 + ri*ts - (1-exp(-k*I_Last*ts)))*Pi_Last
  Pj_Next = (1 + rj*ts - (1-exp(-k*I_Last*ts)))*Pj_Last
  I_Next = (1 + p*(Pi_Last/(Pi_Last + o) + Pj_Last/(Pj_Last + o))*ts)*I_Last
  
  Pi_Last <- Pi_Next
  Pj_Last <- Pj_Next
  I_Last <- I_Next
  
  ## To get pop'ns to 0
  if (Pi_Last < 1){
    Pi_Last <- 0
  }
  if (Pj_Last < 1){
    Pj_Last <- 0
  }
  
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9 || Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last, ri, rj))
}

#### ... Prep objects ####
## Objects for temp draw: 
L <- 499
ar <- 0.25
## Objects for permutation
freq <- c(10)
IntroPt <- seq(0, 8, 2)
sd <- seq(0, 30, 4)
SimNum <- seq(1, 1000, 1)
TT <- round(seq(0, 49.9, 0.1), 2)

#### Identical quadratic TPCs ####
av <- 20
range <- 40

QuadEqn_1 <- function(temp, a = -0.005, b = 0.2, c = 0){
  a*temp^2 + b*temp + c
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp, a = -0.005, b = 0.2, c = 0){
  a*temp^2 + b*temp + c
} # Roots @ 0 and 40; vertex at (20,2)

#### ... Simulation ####
List3 <- vector(mode = "list", length = length(freq))
for (i in 1:length(freq)){ # For frequency of disturbance (every 2, 10, 20 time steps)
  List2 <- vector(mode = "list", length = length(IntroPt))
  for (j in 1:length(IntroPt)){ # For every day on which we'll intro Pj (0, 2, 6, 10, 14, 18)
    List1 <- vector(mode = "list", length = length(sd))
    for (k in 1:length(sd)){ # For every standard deviation we use (2, 15, 25) 
      SD_df <- data.frame(time = seq(0,50,0.1))
      for (l in 1:length(SimNum)){
        temp_sequence <- c()
        temp_sequence[1] <- 0
        for (m in 1:L) {
          temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)
          while(temp_sequence[m + 1] + av <= -20 | temp_sequence[m + 1] + av >= 60)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- QuadEqn_1(temp_sequence)
        rj <- QuadEqn_2(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = EnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
          Pi_Last = Output[1]
          Pj_Last = Output[2]
          I_Last = Output[3]
          Addition <- c(Pi_Last, Pj_Last, I_Last)
          df <- rbind(df, Addition)
          colnames(df) <- c(paste0("Pi_", SimNum[l]),
                            paste0("Pj_", SimNum[l]),
                            paste0("I_", SimNum[l]))
        }
        Inits <- data.frame(1, 0, 1)
        if (IntroPt[j] == 0){
          Inits <- data.frame(1, 1, 1)
        }
        colnames(Inits) <- c(paste0("Pi_", SimNum[l]),
                             paste0("Pj_", SimNum[l]),
                             paste0("I_", SimNum[l]))
        df <- data.frame(rbind(Inits, df))
        SD_df <- data.frame(cbind(SD_df, df))
      }
      List1[[k]] <- SD_df
    }
    List1Names <- c()
    for (o in 1:length(sd)){
      List1Names[o] <- paste0("SD_", sd[o])
    }
    names(List1) <- List1Names
    List2[[j]] <- List1
  }
  List2Names <- c()
  for (p in 1:length(IntroPt)){
    List2Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List2) <- List2Names
  List3[[i]] <- List2
}
List3Names <- c()
for (i in 1:length(freq)){
  List3Names[i] <- paste0("Each_", freq[i])
}
names(List3) <- List3Names

saveRDS(List3, "Generated_Outputs/Iden_QuadTPC_Output.rds")

#### ... Getting co-occurrence time ####
Pis <- seq(2,(ncol(List3[[1]][[1]][[1]])-2),3) # List3[[freq]][[IntroPt]][[sd]] x 1000

List4 <- vector(mode = "list", length = length(IntroPt))
List5 <- vector(mode = "list", length = length(freq))
for (i in 1:length(List3)){ # Loops over frequency of dist.
  temp_list <- List3[[i]] # temp_lsit[[IntroPt]][[sd]] x 1000
  for(j in 1:length(temp_list)){ # Loops over intro times
    PV <- data.frame()
    for (k in 1:length(temp_list[[1]])){ # Loops over SDs
      l=1
      for (m in Pis){
        n = m+1
        temp_df = temp_list[[j]][[k]][,m:n]
        temp_df <- as.data.frame(sapply(temp_df, as.numeric))
        # temp_df %>% mutate_if(is.numeric, round)
        temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
        PV[l,k] = nrow(temp_df)
        l=l+1
      }
    }
    PersistanceVecs <- PV
    names <- vector(mode = "character", length = length(sd))
    for (o in 1:length(sd)){
      names[o] <- paste0("SD_", sd[o])
    }
    colnames(PersistanceVecs) <- names
    List4[[j]] <- PersistanceVecs
  }
  List4Names <- c()
  for (p in 1:length(IntroPt)){
    List4Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List4) <- List4Names
  List5[[i]] <- List4
}
List5Names <- c()
for (q in 1:length(freq)){
  List5Names[q] <- paste0("Each_", freq[q])
}
names(List5) <- List5Names

saveRDS(List5, "Generated_Outputs/Iden_QuadTPC_PV.rds")

#### ... Data cleaning ####
PV_Means <- vector(mode = "list", length = length(List5))
for (i in 1:length(List5)){ # Goes through all disturbance frequencies
  DF <- data.frame(SD = sd)
  for (j in 1:length(List5[[i]])){ # Goes through all intro points
    Means <- c()
    for (k in 1:ncol(List5[[i]][[j]])){
      Means[k] <- mean(List5[[i]][[j]][ , k])
    }
    DF <- data.frame(cbind(DF, Means))
  }
  Names <- c()
  for (l in 1:length(List5[[i]])){
    Names[l] <- paste0("Intro_", IntroPt[l])
  }
  colnames(DF) <- c("SD", Names)
  PV_Means[[i]] <- DF
}
Names <- c()
for (i in 1:length(List5)){
  Names[i] <- paste0("Each_", freq[i])
}
names(PV_Means) <- Names

#### ... Plotting ####
## Each = 10
plot(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_0, type = "l", col = "red3", lwd = 2, xlab = "Magnitude of Temp. Change, by Standard Deviation", ylab = "Mean Co-Oc. Time Between Pi and Pj", main = "Identical Quadratic TPCs, Each = 10", ylim = c(0,150), xaxt = 'none')
axis(side = 1, at = sd)
abline(v = 10); abline(h = Det_LT_Iden_PV[1], col = "red3", lty = 2); abline(h = Det_LT_Iden_PV[3], col = "goldenrod1", lty = 2); abline(h = Det_LT_Iden_PV[5], col = "forestgreen", lty = 2); abline(h = Det_LT_Iden_PV[7], col = "cornflowerblue", lty = 2); abline(h = Det_LT_Iden_PV[9], col = "orchid1", lty = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_2, col = "goldenrod1", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_4, col = "forestgreen", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_6, col = "cornflowerblue", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_8, col = "orchid1", lwd = 2)
legend("topright", legend = c("Intro at 0", "Intro at 2", "Intro at 4", "Intro at 6", "Intro at 8"), col = c("red3", "goldenrod1", "forestgreen", "cornflowerblue", "orchid"), lwd = 2, lty = 1, cex = 0.7)

#### Semi-Identical Quadratic TPCs ####
av <- 25
range <- 50

#### ... Quadratic thermal sub-model ####
QuadEqn_1 <- function(temp){
  -0.005*temp^2 + 0.2*temp + 0
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp){
  -0.005*temp^2 + 0.3*temp - 2.5
} # Roots @ 10 and 50; vertex at (30,2)

#### ... Simulation ####
List3 <- vector(mode = "list", length = length(freq))
for (i in 1:length(freq)){ # For frequency of disturbance (every 2, 10, 20 time steps)
  List2 <- vector(mode = "list", length = length(IntroPt))
  for (j in 1:length(IntroPt)){ # For every day on which we'll intro Pj (0, 2, 6, 10, 14, 18)
    List1 <- vector(mode = "list", length = length(sd))
    for (k in 1:length(sd)){ # For every standard deviation we use (2, 15, 25) 
      SD_df <- data.frame(time = seq(0,50,0.1))
      for (l in 1:length(SimNum)){
        temp_sequence <- c()
        temp_sequence[1] <- 0
        for (m in 1:L) {
          temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)
          while(temp_sequence[m + 1] + av <= -20 | temp_sequence[m + 1] + av >= 70)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- QuadEqn_1(temp_sequence)
        rj <- QuadEqn_2(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = EnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
          Pi_Last = Output[1]
          Pj_Last = Output[2]
          I_Last = Output[3]
          Addition <- c(Pi_Last, Pj_Last, I_Last)
          df <- rbind(df, Addition)
          colnames(df) <- c(paste0("Pi_", SimNum[l]),
                            paste0("Pj_", SimNum[l]),
                            paste0("I_", SimNum[l]))
        }
        Inits <- data.frame(1, 0, 1)
        if (IntroPt[j] == 0){
          Inits <- data.frame(1, 1, 1)
        }
        colnames(Inits) <- c(paste0("Pi_", SimNum[l]),
                             paste0("Pj_", SimNum[l]),
                             paste0("I_", SimNum[l]))
        df <- data.frame(rbind(Inits, df))
        SD_df <- data.frame(cbind(SD_df, df))
      }
      List1[[k]] <- SD_df
    }
    List1Names <- c()
    for (o in 1:length(sd)){
      List1Names[o] <- paste0("SD_", sd[o])
    }
    names(List1) <- List1Names
    List2[[j]] <- List1
  }
  List2Names <- c()
  for (p in 1:length(IntroPt)){
    List2Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List2) <- List2Names
  List3[[i]] <- List2
}
List3Names <- c()
for (i in 1:length(freq)){
  List3Names[i] <- paste0("Each_", freq[i])
}
names(List3) <- List3Names

saveRDS(List3, "Generated_Outputs/Semi_QuadTPC_Output.rds")

#### ... Getting co-occurrence time ####
Pis <- seq(2,(ncol(List3[[1]][[1]][[1]])-2),3) # List3[[freq]][[IntroPt]][[sd]] x 1000

List4 <- vector(mode = "list", length = length(IntroPt))
List5 <- vector(mode = "list", length = length(freq))
for (i in 1:length(List3)){ # Loops over frequency of dist.
  temp_list <- List3[[i]] # temp_lsit[[IntroPt]][[sd]] x 1000
  for(j in 1:length(temp_list)){ # Loops over intro times
    PV <- data.frame()
    for (k in 1:length(temp_list[[1]])){ # Loops over SDs
      l=1
      for (m in Pis){
        n = m+1
        temp_df = temp_list[[j]][[k]][,m:n]
        temp_df <- as.data.frame(sapply(temp_df, as.numeric))
        # temp_df %>% mutate_if(is.numeric, round)
        temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
        PV[l,k] = nrow(temp_df)
        l=l+1
      }
    }
    PersistanceVecs <- PV
    names <- vector(mode = "character", length = length(sd))
    for (o in 1:length(sd)){
      names[o] <- paste0("SD_", sd[o])
    }
    colnames(PersistanceVecs) <- names
    List4[[j]] <- PersistanceVecs
  }
  List4Names <- c()
  for (p in 1:length(IntroPt)){
    List4Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List4) <- List4Names
  List5[[i]] <- List4
}
List5Names <- c()
for (q in 1:length(freq)){
  List5Names[q] <- paste0("Each_", freq[q])
}
names(List5) <- List5Names

saveRDS(List5, "Generated_Outputs/Semi_QuadTPC_PV.rds")

#### ... Data cleaning ####
PV_Means <- vector(mode = "list", length = length(List5))
for (i in 1:length(List5)){ # Goes through all disturbance frequencies
  DF <- data.frame(SD = sd)
  for (j in 1:length(List5[[i]])){ # Goes through all intro points
    Means <- c()
    for (k in 1:ncol(List5[[i]][[j]])){
      Means[k] <- mean(List5[[i]][[j]][ , k])
    }
    DF <- data.frame(cbind(DF, Means))
  }
  Names <- c()
  for (l in 1:length(List5[[i]])){
    Names[l] <- paste0("Intro_", IntroPt[l])
  }
  colnames(DF) <- c("SD", Names)
  PV_Means[[i]] <- DF
}
Names <- c()
for (i in 1:length(List5)){
  Names[i] <- paste0("Each_", freq[i])
}
names(PV_Means) <- Names

#### ... Plotting ####
## Each = 10
plot(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_0, type = "l", col = "red3", lwd = 2, xlab = "Magnitude of Temp. Change, by Standard Deviation", ylab = "Mean Co-Oc. Time Between Pi and Pj", main = "Semi-Identical Quadratic TPCs, Each = 10", ylim = c(0,150), xaxt = 'none')
axis(side = 1, at = sd)
abline(v = 10); abline(h = Det_LT_Semi_PV[1], col = "red3", lty = 2); abline(h = Det_LT_Semi_PV[3], col = "goldenrod1", lty = 2); abline(h = Det_LT_Semi_PV[5], col = "forestgreen", lty = 2); abline(h = Det_LT_Semi_PV[7], col = "cornflowerblue", lty = 2); abline(h = Det_LT_Semi_PV[9], col = "orchid1", lty = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_2, col = "goldenrod1", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_4, col = "forestgreen", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_6, col = "cornflowerblue", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_8, col = "orchid1", lwd = 2)
legend("topright", legend = c("Intro at 0", "Intro at 2", "Intro at 4", "Intro at 6", "Intro at 8"), col = c("red3", "goldenrod1", "forestgreen", "cornflowerblue", "orchid"), lwd = 2, lty = 1, cex = 0.7)

#### Pi-Advantage Quadratic TPCs ####
av <- 20
range <- 50

#### ... Quadratic thermal sub-model ####
QuadEqn_1 <- function(temp){
  -0.005*temp^2 + 0.2*temp + 0
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp){
  -0.005*temp^2 + 0.3*temp - 2.5
} # Roots @ 10 and 50; vertex at (30,2)

#### ... Simulation ####
List3 <- vector(mode = "list", length = length(freq))
for (i in 1:length(freq)){ # For frequency of disturbance (every 2, 10, 20 time steps)
  List2 <- vector(mode = "list", length = length(IntroPt))
  for (j in 1:length(IntroPt)){ # For every day on which we'll intro Pj (0, 2, 6, 10, 14, 18)
    List1 <- vector(mode = "list", length = length(sd))
    for (k in 1:length(sd)){ # For every standard deviation we use (2, 15, 25) 
      SD_df <- data.frame(time = seq(0,50,0.1))
      for (l in 1:length(SimNum)){
        temp_sequence <- c()
        temp_sequence[1] <- 0
        for (m in 1:L) {
          temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)
          while(temp_sequence[m + 1] + av <= -10 | temp_sequence[m + 1] + av >= 60)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- QuadEqn_1(temp_sequence)
        rj <- QuadEqn_2(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = EnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
          Pi_Last = Output[1]
          Pj_Last = Output[2]
          I_Last = Output[3]
          Addition <- c(Pi_Last, Pj_Last, I_Last)
          df <- rbind(df, Addition)
          colnames(df) <- c(paste0("Pi_", SimNum[l]),
                            paste0("Pj_", SimNum[l]),
                            paste0("I_", SimNum[l]))
        }
        Inits <- data.frame(1, 0, 1)
        if (IntroPt[j] == 0){
          Inits <- data.frame(1, 1, 1)
        }
        colnames(Inits) <- c(paste0("Pi_", SimNum[l]),
                             paste0("Pj_", SimNum[l]),
                             paste0("I_", SimNum[l]))
        df <- data.frame(rbind(Inits, df))
        SD_df <- data.frame(cbind(SD_df, df))
      }
      List1[[k]] <- SD_df
    }
    List1Names <- c()
    for (o in 1:length(sd)){
      List1Names[o] <- paste0("SD_", sd[o])
    }
    names(List1) <- List1Names
    List2[[j]] <- List1
  }
  List2Names <- c()
  for (p in 1:length(IntroPt)){
    List2Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List2) <- List2Names
  List3[[i]] <- List2
}
List3Names <- c()
for (i in 1:length(freq)){
  List3Names[i] <- paste0("Each_", freq[i])
}
names(List3) <- List3Names

saveRDS(List3, "Generated_Outputs/PiAd_QuadTPC_Output.rds")

#### ... Getting co-occurrence time ####
Pis <- seq(2,(ncol(List3[[1]][[1]][[1]])-2),3) # List3[[freq]][[IntroPt]][[sd]] x 1000

List4 <- vector(mode = "list", length = length(IntroPt))
List5 <- vector(mode = "list", length = length(freq))
for (i in 1:length(List3)){ # Loops over frequency of dist.
  temp_list <- List3[[i]] # temp_lsit[[IntroPt]][[sd]] x 1000
  for(j in 1:length(temp_list)){ # Loops over intro times
    PV <- data.frame()
    for (k in 1:length(temp_list[[1]])){ # Loops over SDs
      l=1
      for (m in Pis){
        n = m+1
        temp_df = temp_list[[j]][[k]][,m:n]
        temp_df <- as.data.frame(sapply(temp_df, as.numeric))
        # temp_df %>% mutate_if(is.numeric, round)
        temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
        PV[l,k] = nrow(temp_df)
        l=l+1
      }
    }
    PersistanceVecs <- PV
    names <- vector(mode = "character", length = length(sd))
    for (o in 1:length(sd)){
      names[o] <- paste0("SD_", sd[o])
    }
    colnames(PersistanceVecs) <- names
    List4[[j]] <- PersistanceVecs
  }
  List4Names <- c()
  for (p in 1:length(IntroPt)){
    List4Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List4) <- List4Names
  List5[[i]] <- List4
}
List5Names <- c()
for (q in 1:length(freq)){
  List5Names[q] <- paste0("Each_", freq[q])
}
names(List5) <- List5Names

saveRDS(List5, "Generated_Outputs/PiAd_QuadTPC_PV.rds")

#### ... Data cleaning ####
PV_Means <- vector(mode = "list", length = length(List5))
for (i in 1:length(List5)){ # Goes through all disturbance frequencies
  DF <- data.frame(SD = sd)
  for (j in 1:length(List5[[i]])){ # Goes through all intro points
    Means <- c()
    for (k in 1:ncol(List5[[i]][[j]])){
      Means[k] <- mean(List5[[i]][[j]][ , k])
    }
    DF <- data.frame(cbind(DF, Means))
  }
  Names <- c()
  for (l in 1:length(List5[[i]])){
    Names[l] <- paste0("Intro_", IntroPt[l])
  }
  colnames(DF) <- c("SD", Names)
  PV_Means[[i]] <- DF
}
Names <- c()
for (i in 1:length(List5)){
  Names[i] <- paste0("Each_", freq[i])
}
names(PV_Means) <- Names

#### ... Plotting ####
## Each = 10
plot(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_0, type = "l", col = "red3", lwd = 2, xlab = "Magnitude of Temp. Change, by Standard Deviation", ylab = "Mean Co-Oc. Time Between Pi and Pj", main = "Pi-Advantage Quadratic TPCs, Each = 10", ylim = c(0,150), xaxt = 'none')
axis(side = 1, at = sd)
abline(v = 10); abline(h = Det_LT_Semi_PV[1], col = "red3", lty = 2); abline(h = Det_LT_Semi_PV[3], col = "goldenrod1", lty = 2); abline(h = Det_LT_Semi_PV[5], col = "forestgreen", lty = 2); abline(h = Det_LT_Semi_PV[7], col = "cornflowerblue", lty = 2); abline(h = Det_LT_Semi_PV[9], col = "orchid1", lty = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_2, col = "goldenrod1", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_4, col = "forestgreen", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_6, col = "cornflowerblue", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_8, col = "orchid1", lwd = 2)
legend("topright", legend = c("Intro at 0", "Intro at 2", "Intro at 4", "Intro at 6", "Intro at 8"), col = c("red3", "goldenrod1", "forestgreen", "cornflowerblue", "orchid"), lwd = 2, lty = 1, cex = 0.7)

#### Pj-Advantage Quadratic TPCs ####
av <- 30
range <- 50

#### ... Quadratic thermal sub-model ####
QuadEqn_1 <- function(temp){
  -0.005*temp^2 + 0.2*temp + 0
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp){
  -0.005*temp^2 + 0.3*temp - 2.5
} # Roots @ 10 and 50; vertex at (30,2)

#### ... Simulation ####
List3 <- vector(mode = "list", length = length(freq))
for (i in 1:length(freq)){ # For frequency of disturbance (every 2, 10, 20 time steps)
  List2 <- vector(mode = "list", length = length(IntroPt))
  for (j in 1:length(IntroPt)){ # For every day on which we'll intro Pj (0, 2, 6, 10, 14, 18)
    List1 <- vector(mode = "list", length = length(sd))
    for (k in 1:length(sd)){ # For every standard deviation we use (2, 15, 25) 
      SD_df <- data.frame(time = seq(0,50,0.1))
      for (l in 1:length(SimNum)){
        temp_sequence <- c()
        temp_sequence[1] <- 0
        for (m in 1:L) {
          temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)
          while(temp_sequence[m + 1] + av <= -10 | temp_sequence[m + 1] + av >= 60)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- QuadEqn_1(temp_sequence)
        rj <- QuadEqn_2(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = EnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
          Pi_Last = Output[1]
          Pj_Last = Output[2]
          I_Last = Output[3]
          Addition <- c(Pi_Last, Pj_Last, I_Last)
          df <- rbind(df, Addition)
          colnames(df) <- c(paste0("Pi_", SimNum[l]),
                            paste0("Pj_", SimNum[l]),
                            paste0("I_", SimNum[l]))
        }
        Inits <- data.frame(1, 0, 1)
        if (IntroPt[j] == 0){
          Inits <- data.frame(1, 1, 1)
        }
        colnames(Inits) <- c(paste0("Pi_", SimNum[l]),
                             paste0("Pj_", SimNum[l]),
                             paste0("I_", SimNum[l]))
        df <- data.frame(rbind(Inits, df))
        SD_df <- data.frame(cbind(SD_df, df))
      }
      List1[[k]] <- SD_df
    }
    List1Names <- c()
    for (o in 1:length(sd)){
      List1Names[o] <- paste0("SD_", sd[o])
    }
    names(List1) <- List1Names
    List2[[j]] <- List1
  }
  List2Names <- c()
  for (p in 1:length(IntroPt)){
    List2Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List2) <- List2Names
  List3[[i]] <- List2
}
List3Names <- c()
for (i in 1:length(freq)){
  List3Names[i] <- paste0("Each_", freq[i])
}
names(List3) <- List3Names

saveRDS(List3, "Generated_Outputs/PjAd_QuadTPC_Output.rds")

#### ... Getting co-occurrence time ####
Pis <- seq(2,(ncol(List3[[1]][[1]][[1]])-2),3) # List3[[freq]][[IntroPt]][[sd]] x 1000

List4 <- vector(mode = "list", length = length(IntroPt))
List5 <- vector(mode = "list", length = length(freq))
for (i in 1:length(List3)){ # Loops over frequency of dist.
  temp_list <- List3[[i]] # temp_lsit[[IntroPt]][[sd]] x 1000
  for(j in 1:length(temp_list)){ # Loops over intro times
    PV <- data.frame()
    for (k in 1:length(temp_list[[1]])){ # Loops over SDs
      l=1
      for (m in Pis){
        n = m+1
        temp_df = temp_list[[j]][[k]][,m:n]
        temp_df <- as.data.frame(sapply(temp_df, as.numeric))
        # temp_df %>% mutate_if(is.numeric, round)
        temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
        PV[l,k] = nrow(temp_df)
        l=l+1
      }
    }
    PersistanceVecs <- PV
    names <- vector(mode = "character", length = length(sd))
    for (o in 1:length(sd)){
      names[o] <- paste0("SD_", sd[o])
    }
    colnames(PersistanceVecs) <- names
    List4[[j]] <- PersistanceVecs
  }
  List4Names <- c()
  for (p in 1:length(IntroPt)){
    List4Names[p] <- paste0("Intro_", IntroPt[p])
  }
  names(List4) <- List4Names
  List5[[i]] <- List4
}
List5Names <- c()
for (q in 1:length(freq)){
  List5Names[q] <- paste0("Each_", freq[q])
}
names(List5) <- List5Names

saveRDS(List5, "Generated_Outputs/PjAd_QuadTPC_PV.rds")

#### ... Data cleaning ####
PV_Means <- vector(mode = "list", length = length(List5))
for (i in 1:length(List5)){ # Goes through all disturbance frequencies
  DF <- data.frame(SD = sd)
  for (j in 1:length(List5[[i]])){ # Goes through all intro points
    Means <- c()
    for (k in 1:ncol(List5[[i]][[j]])){
      Means[k] <- mean(List5[[i]][[j]][ , k])
    }
    DF <- data.frame(cbind(DF, Means))
  }
  Names <- c()
  for (l in 1:length(List5[[i]])){
    Names[l] <- paste0("Intro_", IntroPt[l])
  }
  colnames(DF) <- c("SD", Names)
  PV_Means[[i]] <- DF
}
Names <- c()
for (i in 1:length(List5)){
  Names[i] <- paste0("Each_", freq[i])
}
names(PV_Means) <- Names

#### ... Plotting ####
## Each = 10
plot(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_0, type = "l", col = "red3", lwd = 2, xlab = "Magnitude of Temp. Change, by Standard Deviation", ylab = "Mean Co-Oc. Time Between Pi and Pj", main = "Pj-Advantage Quadratic TPCs, Each = 10", ylim = c(0,150), xaxt = 'none')
axis(side = 1, at = sd)
abline(v = 10); abline(h = Det_LT_Semi_PV[1], col = "red3", lty = 2); abline(h = Det_LT_Semi_PV[3], col = "goldenrod1", lty = 2); abline(h = Det_LT_Semi_PV[5], col = "forestgreen", lty = 2); abline(h = Det_LT_Semi_PV[7], col = "cornflowerblue", lty = 2); abline(h = Det_LT_Semi_PV[9], col = "orchid1", lty = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_2, col = "goldenrod1", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_4, col = "forestgreen", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_6, col = "cornflowerblue", lwd = 2)
lines(PV_Means[[1]]$SD, PV_Means[[1]]$Intro_8, col = "orchid1", lwd = 2)
legend("topright", legend = c("Intro at 0", "Intro at 2", "Intro at 4", "Intro at 6", "Intro at 8"), col = c("red3", "goldenrod1", "forestgreen", "cornflowerblue", "orchid"), lwd = 2, lty = 1, cex = 0.7)

#### Plotting all quadratic TPCs ####
#### ... Identical ####
temp <- seq(from = -10, to = 50, by = 0.1)
QuadEqn_1 <- function(temp, a = -0.005, b = 0.2, c = 0){
  a*temp^2 + b*temp + c
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp, a = -0.005, b = 0.2, c = 0){
  a*temp^2 + b*temp + c
} # Roots @ 0 and 40; vertex at (20,2)
ri <- QuadEqn_1(temp)
rj <- QuadEqn_2(temp)
r_temp <- data.frame(cbind(temp,ri,rj))
av <- 20
Iden_TPC <- ggplot(r_temp, aes(x = temp))+
  geom_line(aes(y = ri, color = "Thermal range of Pi")) +
  geom_line(aes(y = rj, color = "Thermal range of Pj"), linetype = "dashed")+ 
  geom_vline(aes(xintercept = av, colour = "Average temperature of system (°C)")) +
  geom_hline(aes(yintercept = 0, colour = "0°C"), linetype = 2)+
  labs(title = "Identical Quadratic TPCs",
       x = "Temperature of System (°C)",
       y = "Parasite Replication Rate") +
  scale_color_manual(name = "Legend",
                     values = c("Thermal range of Pi" = "goldenrod1",
                                "Thermal range of Pj" = "springgreen4",
                                "Average temperature of system (°C)" = "#FF99FF",
                                "0°C" = "black"),
                     guide = F) +
  theme_minimal()
Iden_TPC
#### ... Semi-Identical ####
temp <- seq(from = 0, to = 50, by = 0.1)
QuadEqn_1 <- function(temp){
  -0.005*temp^2 + 0.2*temp + 0
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp){
  -0.005*temp^2 + 0.3*temp - 2.5
} # Roots @ 10 and 50; vertex at (30,2)
ri <- QuadEqn_1(temp)
rj <- QuadEqn_2(temp)
r_temp <- data.frame(cbind(temp,ri,rj))
av <- 25
Semi_TPC <- ggplot(r_temp, aes(x = temp))+
  geom_line(aes(y = ri, color = "Thermal range of Pi")) +
  geom_line(aes(y = rj, color = "Thermal range of Pj"), linetype = "dashed")+ 
  geom_vline(aes(xintercept = av, colour = "Average temperature of system (°C)")) +
  geom_hline(aes(yintercept = 0, colour = "0°C"), linetype = 2)+
  labs(title = "Semi-Identical Quadratic TPCs",
       x = "Temperature of System (°C)",
       y = "Parasite Replication Rate") +
  scale_color_manual(name = "Legend",
                     values = c("Thermal range of Pi" = "goldenrod1",
                                "Thermal range of Pj" = "springgreen4",
                                "Average temperature of system (°C)" = "#FF99FF",
                                "0°C" = "black"),
                     guide = F) +
  theme_minimal()
Semi_TPC
#### ... Pi Advantage ####
temp <- seq(from = 0, to = 50, by = 0.1)
QuadEqn_1 <- function(temp, a = -0.005, b = 0.2, c = 0){
  a*temp^2 + b*temp + c
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp, a = -0.005, b = 0.3, c = -2.5){
  a*temp^2 + b*temp + c
} # Roots @ 10 and 50; vertex at (30,2)
ri <- QuadEqn_1(temp)
rj <- QuadEqn_2(temp)
r_temp <- data.frame(cbind(temp,ri,rj))
av <- 20
PiAd_TPC <- ggplot(r_temp, aes(x = temp))+
  geom_line(aes(y = ri, color = "Thermal range of Pi")) +
  geom_line(aes(y = rj, color = "Thermal range of Pj"), linetype = "dashed")+ 
  geom_vline(aes(xintercept = av, colour = "Average temperature of system (°C)")) +
  geom_hline(aes(yintercept = 0, colour = "0°C"), linetype = 2)+
  labs(title = "Pi-Advantage Quadratic TPCs",
       x = "Temperature of System (°C)",
       y = "Parasite Replication Rate") +
  scale_color_manual(name = "Legend",
                     values = c("Thermal range of Pi" = "goldenrod1",
                                "Thermal range of Pj" = "springgreen4",
                                "Average temperature of system (°C)" = "#FF99FF",
                                "0°C" = "black"),
                     guide = F) +
  theme_minimal()
PiAd_TPC
#### ... Pj Advantage ####
temp <- seq(from = 0, to = 50, by = 0.1)
QuadEqn_1 <- function(temp, a = -0.005, b = 0.2, c = 0){
  a*temp^2 + b*temp + c
} # Roots @ 0 and 40; vertex at (20,2)
QuadEqn_2 <- function(temp, a = -0.005, b = 0.3, c = -2.5){
  a*temp^2 + b*temp + c
} # Roots @ 10 and 50; vertex at (30,2)
ri <- QuadEqn_1(temp)
rj <- QuadEqn_2(temp)
r_temp <- data.frame(cbind(temp,ri,rj))
av <- 30
PjAd_TPC <- ggplot(r_temp, aes(x = temp))+
  geom_line(aes(y = ri, color = "Thermal range of Pi")) +
  geom_line(aes(y = rj, color = "Thermal range of Pj"), linetype = "dashed")+ 
  geom_vline(aes(xintercept = av, colour = "Average temperature of system (°C)")) +
  geom_hline(aes(yintercept = 0, colour = "0°C"), linetype = 2)+
  labs(title = "Pj-Advantage Quadratic TPCs",
       x = "Temperature of System (°C)",
       y = "Parasite Replication Rate") +
  scale_color_manual(name = "Legend",
                     values = c("Thermal range of Pi" = "goldenrod1",
                                "Thermal range of Pj" = "springgreen4",
                                "Average temperature of system (°C)" = "#FF99FF",
                                "0°C" = "black"),
                     guide = F) +
  theme_minimal()
PjAd_TPC
