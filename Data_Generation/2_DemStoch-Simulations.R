#### DOC STRING ===============================================================
# This script runs demographically stochastic simulations in each environment 
# (low- and high-average temperature) under each thermal scenario (Identical, 
# Semi-Identical, Pi-Advantage, Pj-Advantage (Version A), and Pj-Advantage 
# (Version B)). 

# If you run this script, it will save time series outputs, and persistence vectors 
# (containing the number of time steps for which both parasites populations had 
# abundances greater than one by simulation) as .rds files in a folder called 
# "Generated_Outputs".

# To see the outputs as used in the manuscript, go to 
# "EnvironmentalVariability_Repo/Data/DemStoch_Outputs".
#### END DOC STRING============================================================

#### Required packages ####
install.packages("dplyr"); library(dplyr)

ts <- 0.1 

#### Low-Average Temperature Environment ####
#### ... Identical TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 2.13; rj = 2.13; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}

#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_LT_Iden.rds")

#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_LT_Iden_PV.rds")

#### ... Semi-Identical TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 1.79; rj = 1.80; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_LT_Semi.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_LT_Semi_PV.rds")

#### ... Pi-Advantage TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 2.12; rj = 1.54; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_LT_PiAd.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_LT_PiAd_PV.rds")

#### ... Pj-Advantage, Version A TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 1.36; rj = 1.94; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_LT_PjAd.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_LT_PjAd_PV.rds")

#### ... Pj-Advantage, Version B TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 1.54; rj = 2.12; k = 0.01; p = 1; o = 1000

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

  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }

  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0,50,0.1))
SimNum <- seq(1,1000,1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1,49,1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1,1000,1)
  Total_df <- data.frame(time = seq(0,50,0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1,50,0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){

      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }

      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_LT_PjAd_vB.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_LT_PjAd_vB_PV.rds")

#### High-Average Temperature Environment ####
#### ... Identical TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 2.15; rj = 2.15; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}

#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_HT_Iden.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_HT_Iden_PV.rds")

#### ... Semi-Identical TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 1.84; rj = 1.82; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_HT_Semi.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_HT_Semi_PV.rds")

#### ... Pi-Advantage TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 2.11; rj = 1.61; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_HT_PiAd.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_HT_PiAd_PV.rds")

#### ... Pj-Advantage, Version A TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 1.45; rj = 1.96; k = 0.01; p = 1; o = 1000
  
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
  
  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  
  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0, 50, 0.1))
SimNum <- seq(1, 1000, 1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1, 49, 1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1, 1000, 1)
  Total_df <- data.frame(time = seq(0, 50, 0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1, 50, 0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){
      
      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }
      
      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_HT_PjAd.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_HT_PjAd_PV.rds")

#### ... Pj-Advantage, Version A TPCs ####
DiscreteModel_Dem <- function(Pi_Last, Pj_Last, I_Last){
  ts = 0.1
  ri = 1.61; rj = 2.11; k = 0.01; p = 1; o = 1000

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

  ## To prevent the populations from going negative
  if (Pi_Last < 0){
    Pi_Last <- 0
  }
  if (Pj_Last < 0){
    Pj_Last <- 0
  }
  ## To get host death if parasite abundance exceeds D threshold set by Antia
  if (Pi_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }
  if (Pj_Last > 10^9){
    Pi_Last <- 0
    Pj_Last <- 0
    I_Last <- 0
  }

  return(c(Pi_Last, Pj_Last, I_Last))
}
#### ... ... Intro @ same time ####
DemStoch <- data.frame(time = seq(0,50,0.1))
SimNum <- seq(1,1000,1)
TT <- 50/ts
for (i in 1:length(SimNum)){
  df <- data.frame()
  Pi_Last <- 1; Pj_Last <- 1; I_Last <- 1
  for (j in 1:TT){
    Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
    Pi_Last = Output[1]
    Pj_Last = Output[2]
    I_Last = Output[3]
    Addition <- c(Pi_Last, Pj_Last, I_Last)
    df <- rbind(df, Addition)
    colnames(df) <- c(paste0("Pi_", SimNum[i]),
                      paste0("Pj_", SimNum[i]),
                      paste0("I_", SimNum[i]))
  }
  inits <- data.frame(1, 1, 1)
  colnames(inits) <- c(paste0("Pi_", SimNum[i]),
                       paste0("Pj_", SimNum[i]),
                       paste0("I_", SimNum[i]))
  df <- data.frame(rbind(inits, df))
  DemStoch <- data.frame(cbind(DemStoch, df))
}
#### ... ... Intro @ diff. times ####
IntroPt <- seq(1,49,1)
DemIntros <- vector(mode = "list", length=length(IntroPt))
for (i in 1:length(IntroPt)){
  # Replications
  SimNum <- seq(1,1000,1)
  Total_df <- data.frame(time = seq(0,50,0.1))
  for (j in 1:length(SimNum)){
    TT <- seq(0.1,50,0.1)
    TT <- round(TT, 2)
    Indiv_df <- data.frame()
    Pi_Last <- 1; Pj_Last <- 0; I_Last <- 1
    for (k in 1:length(TT)){

      if (TT[k] == IntroPt[i]){
        Pj_Last <- 1
      }

      Output = DiscreteModel_Dem(Pi_Last, Pj_Last, I_Last)
      Pi_Last = Output[1]
      Pj_Last = Output[2]
      I_Last = Output[3]
      Addition <- c(Pi_Last, Pj_Last, I_Last)
      Indiv_df <- rbind(Indiv_df, Addition)
      colnames(Indiv_df) <- c(paste0("Pi_", SimNum[j]),
                              paste0("Pj_", SimNum[j]),
                              paste0("I_", SimNum[j]))
    }
    Inits <- data.frame(1, 0, 1)
    colnames(Inits) <- c(paste0("Pi_", SimNum[j]),
                         paste0("Pj_", SimNum[j]),
                         paste0("I_", SimNum[j]))
    Indiv_df <- data.frame(rbind(Inits, Indiv_df))
    Total_df <- data.frame(cbind(Total_df,Indiv_df))
  }
  DemIntros[[i]] <- Total_df
}
DemStoch <- c(list(DemStoch), DemIntros)

saveRDS(DemStoch, file = "Generated_Outputs/D_HT_PjAd_vB.rds")
#### ... ... Co-occurrence ####
PV <- data.frame()
Pis <- seq(2,(ncol(DemStoch[[1]])-2),3)
for (m in 1:length(DemStoch)){
  j=1
  for (i in Pis){
    k = i+1
    temp_df = DemStoch[[m]][,i:k]
    temp_df <- as.data.frame(sapply(temp_df, as.numeric))
    temp_df %>% mutate_if(is.numeric, round)
    temp_df = temp_df[which(temp_df[,1] > 1e0 & temp_df[,2] > 1e0),]
    PV[j,m] = nrow(temp_df)
    j=j+1
  }
}
PersistanceVecs <- PV
names <- vector(mode = "character", length = 50)
names[1] <- "Intro_0"
for (i in 1:length(IntroPt)){
  names[i+1] <- paste0("Intro_", IntroPt[i])
}
colnames(PersistanceVecs) <- names

saveRDS(PersistanceVecs, file = "Generated_Outputs/D_HT_PjAd_vB_PV.rds")
