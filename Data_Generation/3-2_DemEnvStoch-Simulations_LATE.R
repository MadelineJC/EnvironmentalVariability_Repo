#### DOC STRING ===============================================================
# This script runs demographically and environmentally stochastic simulations 
# in the low-average temperature environment under each thermal scenario 
# (Identical, Semi-Identical, Pi-Advantage, and Pj-Advantage (Version A)). 

# If you run this script, it will save time series outputs, and persistence 
# vectors (containing the number of time steps for which both parasites 
# populations had abundances greater than one by simulation) as .rds files in 
# a folder called "Generated_Outputs".

# This script takes a number of days to run. To see the outputs as used in the 
# manuscript, go to "EnvironmentalVariability_Repo/Data/EnvStoch-DemEnvStoch_PV_Outputs", 
# and load the following files: 1-DE_LT_Iden_PV.rds, 2-DE_LT_Semi_PV.rds, 
# 3-DE_LT_PiAd_PV.rds, 4-DE_LT_PjAd_PV.rds.
#### END DOC STRING============================================================

#### Required packages ####
install.packages("dplyr"); library(dplyr)

#### Set-Up (Same Across Scripts) ####
#### ... Model Functions ####
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
DemEnvStoch_Model <- function(Pi_Last, Pj_Last, I_Last, ri, rj){
  ts = 0.1
  k = 0.01; p = 1; o = 1000
  
  Pi_B = rpois(1, ri*Pi_Last*ts) 
  Pi_D = rbinom(1, Pi_Last, (1-exp(-k*I_Last*ts))) 
  Pj_B = rpois(1, rj*Pj_Last*ts) 
  Pj_D = rbinom(1, Pj_Last, (1-exp(-k*I_Last*ts)))
  I_B = rpois(1, p*((Pi_Last/(Pi_Last + o)) + (Pj_Last/(Pj_Last + o)))*I_Last*ts) 
  
  Pi_Next = Pi_Last + Pi_B - Pi_D
  Pj_Next = Pj_Last + Pj_B - Pj_D
  I_Next = I_Last + I_B
  
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
  
  return(c(Pi_Last, Pj_Last, I_Last))
}

#### ... Temp Sampling Objects ####
L <- 499
ar <- 0.25
K <- 273.15
range <- 30

freq <- c(1, 2, 5, seq(10, 100, 10))
IntroPt <- seq(0, 49, 1)
sd <- seq(2, range, 2)
SimNum <- seq(1, 1000, 1)
TT <- round(seq(0, 49.9, 0.1), 2)

#### Simulations ####
#### ... Identical TPCs ####
MTE_TPC_Pi <- function(temp){
  lambda_knot <- 1.4; Tknot <- 14+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-9.5)+K; TzH <- 22.5+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}
MTE_TPC_Pj <- function(temp){
  lambda_knot <- 1.4; Tknot <- 14+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-9.5)+K; TzH <- 22.5+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}

# Temp Sampling Objects
av <- 19.9

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
          while(temp_sequence[m + 1] + av <= 0 | temp_sequence[m + 1] + av >= range)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- temp_sequence + K # Kelvin
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- MTE_TPC_Pi(temp_sequence)
        rj <- MTE_TPC_Pj(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = DemEnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
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

saveRDS(List3, file = "Generated_Outputs/DE_LT_Iden.rds")

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
        temp_df %>% mutate_if(is.numeric, round)
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

saveRDS(List5, file = "Generated_Outputs/DE_LT_Iden_PV.rds")

#### ... Semi-Identical TPCs ####
MTE_TPC_Pi <- function(temp){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}
MTE_TPC_Pj <- function(temp){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}

# Temp Sampling Objects
av <- 19.9

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
          while(temp_sequence[m + 1] + av <= 0 | temp_sequence[m + 1] + av >= range)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- temp_sequence + K # Kelvin
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- MTE_TPC_Pi(temp_sequence)
        rj <- MTE_TPC_Pj(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = DemEnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
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

saveRDS(List3, file = "Generated_Outputs/DE_LT_Semi.rds")

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
        temp_df %>% mutate_if(is.numeric, round)
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

saveRDS(List5, file = "Generated_Outputs/DE_LT_Semi_PV.rds")

#### ... Pi-Advantage TPCs ####
MTE_TPC_Pi <- function(temp){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}
MTE_TPC_Pj <- function(temp){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}

# Temp Sampling Objects
av <- 18 

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
          while(temp_sequence[m + 1] + av <= 0 | temp_sequence[m + 1] + av >= range)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- temp_sequence + K # Kelvin
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- MTE_TPC_Pi(temp_sequence)
        rj <- MTE_TPC_Pj(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = DemEnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
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

saveRDS(List3, file = "Generated_Outputs/DE_LT_PiAd.rds")

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
        temp_df %>% mutate_if(is.numeric, round)
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

saveRDS(List5, file = "Generated_Outputs/DE_LT_PiAd_PV.rds")

#### ... Pj-Advantage, Version A TPCs ####
MTE_TPC_Pi <- function(temp){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}
MTE_TPC_Pj <- function(temp){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/temp) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/temp))) + exp((EzH/k)*((1/TzH) - (1/temp))))^(-1)
  return(r)
}

# Temp Sampling Objects
av <- 20.8530117

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
          while(temp_sequence[m + 1] + av <= 0 | temp_sequence[m + 1] + av >= range)
          {temp_sequence[m + 1] <- round(ar * temp_sequence[m] + rnorm(1, 0, sd[k]), digits = 0)}
        }
        temp_sequence <- temp_sequence + av
        temp_sequence <- temp_sequence + K # Kelvin
        temp_sequence <- rep(temp_sequence, each = freq[i]) 
        temp_sequence <- temp_sequence[-(length(TT)+1:length(temp_sequence))]
        ri <- MTE_TPC_Pi(temp_sequence)
        rj <- MTE_TPC_Pj(temp_sequence)
        
        df <- data.frame()
        Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
        for (n in 1:length(TT)){
          
          if (TT[n] == IntroPt[j]){
            Pj_Last <- 1
          }
          
          Output = DemEnvStoch_Model(Pi_Last, Pj_Last, I_Last, ri[n], rj[n])
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

saveRDS(List3, file = "Generated_Outputs/DE_LT_PjAd.rds")

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
        temp_df %>% mutate_if(is.numeric, round)
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

saveRDS(List5, file = "Generated_Outputs/DE_LT_PjAd_PV.rds")
