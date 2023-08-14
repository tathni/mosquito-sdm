#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Analyze the lab-based thermal trait data and obtains mosquito abundance curves
#######################################################

source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
# Define the Briere function for asymmetric responses like MDR
#------------------------------------------------------
Briere <- function(Temp, c, T0, Tm) {
  output <- (c * Temp * (Temp - T0)) * ((Tm - Temp)^(0.5))
  return(output)
}


#------------------------------------------------------
# Define the quadratic function for concave-down symmetric responses like EFD and pEA
#------------------------------------------------------
quadratic_down <- function(Temp, a, T0, Tm) {
  output <- a * (Temp - T0) * (Tm - Temp)
  return(output)
}


#------------------------------------------------------
# Define the quadratic function for concave-up symmetric responses like u
#------------------------------------------------------
quadratic_up <- function(Temp, a, b, c) {
  output <- (a * (Temp^2)) - (b * Temp) + c
  return(output)
}



#------------------------------------------------------
# Read in and transform mosquito trait data .csv files
# Variables of interest for M(t) equation: EFD, pEA, MDR, u
# Each trait will use the "lowest common denominator" of 5,000 posterior draws
#------------------------------------------------------
#------------------------------------------------------
# Aedes aegypti
#------------------------------------------------------
AeAegypti_Temp <- read.csv("Temperature Trait Data/Trait Trajectories/AeaeDENV.T.csv", stringsAsFactors = FALSE)
colnames(AeAegypti_Temp) <- "Celsius"
AeAegypti_EFD <- read.csv("Temperature Trait Data/Trait Trajectories/AeaeDENV.EFD.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAegypti_pEA <- read.csv("Temperature Trait Data/Trait Trajectories/AeaeDENV.pEA.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAegypti_MDR <- read.csv("Temperature Trait Data/Trait Trajectories/AeaeDENV.MDR.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAegypti_lf <- read.csv("Temperature Trait Data/Trait Trajectories/AeaeDENV.lf.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAegypti_u <- 1/AeAegypti_lf
AeAegypti_u[AeAegypti_u == Inf] <- 0


#------------------------------------------------------
# Aedes albopictus
#------------------------------------------------------
AeAlbopictus_Temp <- read.csv("Temperature Trait Data/Trait Trajectories/AealDENV.T.csv", stringsAsFactors = FALSE)
colnames(AeAlbopictus_Temp) <- "Celsius"
AeAlbopictus_EFOC <- read.csv("Temperature Trait Data/Trait Trajectories/AealDENV.TFD.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAlbopictus_a <- read.csv("Temperature Trait Data/Trait Trajectories/AealDENV.a.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAlbopictus_EFD <- AeAlbopictus_EFOC * AeAlbopictus_a
AeAlbopictus_pEA<- read.csv("Temperature Trait Data/Trait Trajectories/AealDENV.pEA.csv", stringsAsFactors = FALSE)
AeAlbopictus_MDR <- read.csv("Temperature Trait Data/Trait Trajectories/AealDENV.MDR.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAlbopictus_lf <- read.csv("Temperature Trait Data/Trait Trajectories/AealDENV.lf.csv", stringsAsFactors = FALSE) %>%
  setNames(c(paste0("X",1:5000)))
AeAlbopictus_u <- 1/AeAlbopictus_lf
AeAlbopictus_u[AeAlbopictus_u == Inf] <- 0


#------------------------------------------------------
# Anopheles gambiae
#------------------------------------------------------
AnGambiae_Temp <- AeAegypti_Temp
load("Temperature Trait Data/Trait Trajectories/Posterior Samples from Oswaldo Villena/Angamb_post_samps.Rsave")

AnGambiae_EFD <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnGambiae_EFD) <- c(paste0("X",1:5000))
efd.sampsangamb %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnGambiae_Temp[[1]][[j]]
    a <- efd.sampsangamb$qd[[i]]
    T0 <- efd.sampsangamb$T0[[i]]
    Tm <- efd.sampsangamb$Tm[[i]]
    AnGambiae_EFD[[i]][[j]] <- quadratic_down(Temp, a, T0, Tm)
    
    if(AnGambiae_EFD[[i]][[j]] == "NaN") {
      AnGambiae_EFD[[i]][[j]] <- 0
    }
    if(AnGambiae_EFD[[i]][[j]] < 0) {
      AnGambiae_EFD[[i]][[j]] <- 0
    }
  }
}

AnGambiae_pEA <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnGambiae_pEA) <- c(paste0("X",1:5000))
e2a.sampsangamb %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnGambiae_Temp[[1]][[j]]
    a <- e2a.sampsangamb$qd[[i]]
    T0 <- e2a.sampsangamb$T0[[i]]
    Tm <- e2a.sampsangamb$Tm[[i]]
    AnGambiae_pEA[[i]][[j]] <- quadratic_down(Temp, a, T0, Tm)
    
    if(AnGambiae_pEA[[i]][[j]] == "NaN") {
      AnGambiae_pEA[[i]][[j]] <- 0
    }
    if(AnGambiae_pEA[[i]][[j]] < 0) {
      AnGambiae_pEA[[i]][[j]] <- 0
    }
  }
}

AnGambiae_MDR <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnGambiae_MDR) <- c(paste0("X",1:5000))
MDR.sampsangamb %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnGambiae_Temp[[1]][[j]]
    c <- MDR.sampsangamb$c[[i]]
    T0 <- MDR.sampsangamb$T0[[i]]
    Tm <- MDR.sampsangamb$Tm[[i]]
    AnGambiae_MDR[[i]][[j]] <- Briere(Temp, c, T0, Tm)
    
    if(AnGambiae_MDR[[i]][[j]] == "NaN") {
      AnGambiae_MDR[[i]][[j]] <- 0
    }
    if(AnGambiae_MDR[[i]][[j]] < 0) {
      AnGambiae_MDR[[i]][[j]] <- 0
    }
  }
}

AnGambiae_u <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnGambiae_u) <- c(paste0("X",1:5000))
mu.sampsangamb %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnGambiae_Temp[[1]][[j]]
    a <- mu.sampsangamb$qd[[i]]
    b <- mu.sampsangamb$n.slope[[i]]
    c <- mu.sampsangamb$inter[[i]]
    AnGambiae_u[[i]][[j]] <- quadratic_up(Temp, a, b, c)
    
    if(AnGambiae_u[[i]][[j]] == "NaN") {
      AnGambiae_u[[i]][[j]] <- 0
    }
    if(AnGambiae_u[[i]][[j]] < 0) {
      AnGambiae_u[[i]][[j]] <- 0
    }
  }
}

# AnGambiae_Temp <- read.csv("Temperature Trait Data/Trait Trajectories/AngaPfal.T.csv", stringsAsFactors = FALSE)
# colnames(AnGambiae_Temp) <- "Celsius"
# AnGambiae_EFD <- read.csv("Temperature Trait Data/Trait Trajectories/AngaPfal.EFD.csv", stringsAsFactors = FALSE) %>%
#   setNames(c(paste0("X",1:4850)))
# AnGambiae_pEA <- AeAegypti_pEA[1:length(AnGambiae_EFD)]
# AnGambiae_MDR <- read.csv("Temperature Trait Data/Trait Trajectories/AngaPfal.MDR.csv", stringsAsFactors = FALSE) %>%
#   setNames(c(paste0("X",1:4850)))
# AnGambiae_u <- read.csv("Temperature Trait Data/Trait Trajectories/AngaPfal.mu.csv", stringsAsFactors = FALSE) %>%
#   setNames(c(paste0("X",1:4850)))


#------------------------------------------------------
# Anopheles stephensi
#------------------------------------------------------
AnStephensi_Temp <- AeAegypti_Temp
load("Temperature Trait Data/Trait Trajectories/Posterior Samples from Oswaldo Villena/Ansteph_post_samps.Rsave")

AnStephensi_EFD <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnStephensi_EFD) <- c(paste0("X",1:5000))
efd.sampsansteph %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnStephensi_Temp[[1]][[j]]
    a <- efd.sampsansteph$qd[[i]]
    T0 <- efd.sampsansteph$T0[[i]]
    Tm <- efd.sampsansteph$Tm[[i]]
    AnStephensi_EFD[[i]][[j]] <- quadratic_down(Temp, a, T0, Tm)
    
    if(AnStephensi_EFD[[i]][[j]] == "NaN") {
      AnStephensi_EFD[[i]][[j]] <- 0
    }
    if(AnStephensi_EFD[[i]][[j]] < 0) {
      AnStephensi_EFD[[i]][[j]] <- 0
    }
  }
}

AnStephensi_pEA <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnStephensi_pEA) <- c(paste0("X",1:5000))
e2a.sampsansteph %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnStephensi_Temp[[1]][[j]]
    a <- e2a.sampsansteph$qd[[i]]
    T0 <- e2a.sampsansteph$T0[[i]]
    Tm <- e2a.sampsansteph$Tm[[i]]
    AnStephensi_pEA[[i]][[j]] <- quadratic_down(Temp, a, T0, Tm)
    
    if(AnStephensi_pEA[[i]][[j]] == "NaN") {
      AnStephensi_pEA[[i]][[j]] <- 0
    }
    if(AnStephensi_pEA[[i]][[j]] < 0) {
      AnStephensi_pEA[[i]][[j]] <- 0
    }
  }
}

AnStephensi_MDR <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnStephensi_MDR) <- c(paste0("X",1:5000))
MDR.sampsansteph %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnStephensi_Temp[[1]][[j]]
    c <- MDR.sampsansteph$c[[i]]
    T0 <- MDR.sampsansteph$T0[[i]]
    Tm <- MDR.sampsansteph$Tm[[i]]
    AnStephensi_MDR[[i]][[j]] <- Briere(Temp, c, T0, Tm)
    
    if(AnStephensi_MDR[[i]][[j]] == "NaN") {
      AnStephensi_MDR[[i]][[j]] <- 0
    }
    if(AnStephensi_MDR[[i]][[j]] < 0) {
      AnStephensi_MDR[[i]][[j]] <- 0
    }
  }
}

AnStephensi_u <- data.frame(matrix(ncol = 5000, nrow=400))
colnames(AnStephensi_u) <- c(paste0("X",1:5000))
mu.sampsansteph %<>% .[sample(nrow(.), 5000), ]

for(i in 1:5000) {
  for (j in 1:400) {
    Temp <- AnStephensi_Temp[[1]][[j]]
    a <- mu.sampsansteph$qd[[i]]
    b <- mu.sampsansteph$n.slope[[i]]
    c <- mu.sampsansteph$inter[[i]]
    AnStephensi_u[[i]][[j]] <- quadratic_up(Temp, a, b, c)
    
    if(AnStephensi_u[[i]][[j]] == "NaN") {
      AnStephensi_u[[i]][[j]] <- 0
    }
    if(AnStephensi_u[[i]][[j]] < 0) {
      AnStephensi_u[[i]][[j]] <- 0
    }
  }
}


#------------------------------------------------------
# Culex pipiens
#------------------------------------------------------
CxPipiens_Temp <- read.csv("Temperature Trait Data/Trait Trajectories/CxpiWNVx.T.csv", stringsAsFactors = FALSE)
colnames(CxPipiens_Temp) <- "Celsius"
CxPipiens_EFOC <- read.csv("Temperature Trait Data/Trait Trajectories/CxpiWNVx.EFOC.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxPipiens_a <- read.csv("Temperature Trait Data/Trait Trajectories/CxpiWNVx.a.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxPipiens_EFD <- CxPipiens_EFOC * CxPipiens_a
CxPipiens_pLA <- read.csv("Temperature Trait Data/Trait Trajectories/CxpiWNVx.pLA.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxPipiens_EV <- read.csv("Temperature Trait Data/Trait Trajectories/CxpiWNVx.EV.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxPipiens_pEA <- CxPipiens_pLA * CxPipiens_EV
CxPipiens_MDR <- read.csv("Temperature Trait Data/Trait Trajectories/CxpiWNVx.MDR.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxPipiens_lf <- read.csv("Temperature Trait Data/Trait Trajectories/CxpiWNVx.lf.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxPipiens_u <- 1/CxPipiens_lf
CxPipiens_u[CxPipiens_u == Inf] <- 0


#------------------------------------------------------
# Culex quinquefasciatus
#------------------------------------------------------
CxQuinquefasciatus_Temp <- read.csv("Temperature Trait Data/Trait Trajectories/CxquWNVx.T.csv", stringsAsFactors = FALSE)
colnames(CxQuinquefasciatus_Temp) <- "Celsius"
CxQuinquefasciatus_pO <- read.csv("Temperature Trait Data/Trait Trajectories/CxquWNVx.pO.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxQuinquefasciatus_EPR <- read.csv("Temperature Trait Data/Trait Trajectories/CxquWNVx.EPR.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxQuinquefasciatus_EFD <- CxQuinquefasciatus_pO * CxQuinquefasciatus_EPR
CxQuinquefasciatus_pLA <- read.csv("Temperature Trait Data/Trait Trajectories/CxquWNVx.pLA.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxQuinquefasciatus_EV <- read.csv("Temperature Trait Data/Trait Trajectories/CxquWNVx.EV.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxQuinquefasciatus_pEA <- CxQuinquefasciatus_pLA * CxQuinquefasciatus_EV
CxQuinquefasciatus_MDR <- read.csv("Temperature Trait Data/Trait Trajectories/CxquWNVx.MDR.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxQuinquefasciatus_lf <- read.csv("Temperature Trait Data/Trait Trajectories/CxquWNVx.lf.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxQuinquefasciatus_u <- 1/CxQuinquefasciatus_lf
CxQuinquefasciatus_u[CxQuinquefasciatus_u == Inf] <- 0


#------------------------------------------------------
# Culex tarsalis
#------------------------------------------------------
CxTarsalis_Temp <- read.csv("Temperature Trait Data/Trait Trajectories/CxtaWNVx.T.csv", stringsAsFactors = FALSE)
colnames(CxTarsalis_Temp) <- "Celsius"
CxTarsalis_EFOC <- CxPipiens_EFOC
CxTarsalis_a <- read.csv("Temperature Trait Data/Trait Trajectories/CxtaWNVx.a.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxTarsalis_EFD <- CxTarsalis_EFOC * CxTarsalis_a
CxTarsalis_pLA <- read.csv("Temperature Trait Data/Trait Trajectories/CxtaWNVx.pLA.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxTarsalis_pEA <- CxTarsalis_pLA
CxTarsalis_MDR <- read.csv("Temperature Trait Data/Trait Trajectories/CxtaWNVx.MDR.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxTarsalis_lf <- read.csv("Temperature Trait Data/Trait Trajectories/CxtaWNVx.lf.csv", stringsAsFactors = FALSE) %>%
  .[, sample(ncol(.), 5000)] %>%
  setNames(c(paste0("X",1:5000)))
CxTarsalis_u <- 1/CxTarsalis_lf
CxTarsalis_u[CxTarsalis_u == Inf] <- 0



#------------------------------------------------------
# Define a function that calculates mosquito abundance, M(t)
#------------------------------------------------------
M <- function(temp, EFD, pEA, MDR, u) {
  output <- (EFD * pEA * MDR) / (u^2)
  output[output == Inf] <- 0
  output[is.na(output)] <- 0
  row.names(output) <- temp$Celsius
  return(output)
}



#------------------------------------------------------
# Create a matrix of M(t) values for each species
#------------------------------------------------------
AeAegypti_M <- M(AeAegypti_Temp,
                 AeAegypti_EFD,
                 AeAegypti_pEA,
                 AeAegypti_MDR,
                 AeAegypti_u)

AeAlbopictus_M <- M(AeAlbopictus_Temp,
                    AeAlbopictus_EFD,
                    AeAlbopictus_pEA,
                    AeAlbopictus_MDR,
                    AeAlbopictus_u)

AnGambiae_M <- M(AnGambiae_Temp,
                 AnGambiae_EFD,
                 AnGambiae_pEA,
                 AnGambiae_MDR,
                 AnGambiae_u)

AnStephensi_M <- M(AnStephensi_Temp,
                   AnStephensi_EFD,
                   AnStephensi_pEA,
                   AnStephensi_MDR,
                   AnStephensi_u)

CxPipiens_M <- M(CxPipiens_Temp,
                 CxPipiens_EFD,
                 CxPipiens_pEA,
                 CxPipiens_MDR,
                 CxPipiens_u)

CxQuinquefasciatus_M <- M(CxQuinquefasciatus_Temp,
                          CxQuinquefasciatus_EFD,
                          CxQuinquefasciatus_pEA,
                          CxQuinquefasciatus_MDR,
                          CxQuinquefasciatus_u)

CxTarsalis_M <- M(CxTarsalis_Temp,
                  CxTarsalis_EFD,
                  CxTarsalis_pEA,
                  CxTarsalis_MDR,
                  CxTarsalis_u)



#------------------------------------------------------
# Calculate mean and 95% CI for each species' M(t)
#------------------------------------------------------
reorder <- function(data) {
  data$temp <- rownames(data)
  data <- subset(data, select = c(2,1))
  colnames(data) <- c("temp","Mt")
  return(data %>% data.frame)
}

AeAegypti_Mean <- rowMeans(AeAegypti_M) %>% data.frame %>% reorder()
AeAegypti_upperCI <- apply(AeAegypti_M, 1, quantile, probs = 0.975) %>% data.frame %>% reorder()
AeAegypti_lowerCI <- apply(AeAegypti_M, 1, quantile, probs = 0.025) %>% data.frame %>% reorder()

AeAlbopictus_Mean <- rowMeans(AeAlbopictus_M) %>% data.frame %>% reorder()
AeAlbopictus_upperCI <- apply(AeAlbopictus_M, 1, quantile, probs = 0.975) %>% data.frame %>% reorder()
AeAlbopictus_lowerCI <- apply(AeAlbopictus_M, 1, quantile, probs = 0.025) %>% data.frame %>% reorder()

AnGambiae_Mean <- rowMeans(AnGambiae_M) %>% data.frame %>% reorder()
AnGambiae_upperCI <- apply(AnGambiae_M, 1, quantile, probs = 0.975) %>% data.frame %>% reorder()
AnGambiae_lowerCI <- apply(AnGambiae_M, 1, quantile, probs = 0.025) %>% data.frame %>% reorder()

AnStephensi_Mean <- rowMeans(AnStephensi_M) %>% data.frame %>% reorder()
AnStephensi_upperCI <- apply(AnStephensi_M, 1, quantile, probs = 0.975) %>% data.frame %>% reorder()
AnStephensi_lowerCI <- apply(AnStephensi_M, 1, quantile, probs = 0.025) %>% data.frame %>% reorder()

CxPipiens_Mean <- rowMeans(CxPipiens_M) %>% data.frame %>% reorder()
CxPipiens_upperCI <- apply(CxPipiens_M, 1, quantile, probs = 0.975) %>% data.frame %>% reorder()
CxPipiens_lowerCI <- apply(CxPipiens_M, 1, quantile, probs = 0.025) %>% data.frame %>% reorder()

CxQuinquefasciatus_Mean <- rowMeans(CxQuinquefasciatus_M) %>% data.frame %>% reorder()
CxQuinquefasciatus_upperCI <- apply(CxQuinquefasciatus_M, 1, quantile, probs = 0.975) %>% data.frame %>% reorder()
CxQuinquefasciatus_lowerCI <- apply(CxQuinquefasciatus_M, 1, quantile, probs = 0.025) %>% data.frame %>% reorder()

CxTarsalis_Mean <- rowMeans(CxTarsalis_M) %>% data.frame %>% reorder()
CxTarsalis_upperCI <- apply(CxTarsalis_M, 1, quantile, probs = 0.975) %>% data.frame %>% reorder()
CxTarsalis_lowerCI <- apply(CxTarsalis_M, 1, quantile, probs = 0.025) %>% data.frame %>% reorder()



#------------------------------------------------------
# Plotting and saving M(t) graphs
#------------------------------------------------------
plot_Mcurve <- function (mean, upperCI, lowerCI, inputColor) {
  plot(mean$temp, mean$Mt, type = "l",
       xlab = "Temperature (�C)", ylab = "Mosquito Abundance (M(T))",
       col = inputColor, lwd=4,
       ylim=c(0, max(upperCI$Mt)))
  polygon(c(mean$temp, rev(mean$temp)), c(upperCI$Mt, rev(lowerCI$Mt)),
          col =  adjustcolor(inputColor, alpha.f = 0.05), border = NA)
  lines(mean$temp, upperCI$Mt, lty = 'dashed', col = alpha(rgb(0,0,0), 0.5))
  lines(mean$temp, lowerCI$Mt, lty = 'dashed', col = alpha(rgb(0,0,0), 0.5))
}
  
pdf("! Figures/Mosquito Abundance Curves/Aedes aegypti M-curve.pdf")
plot_Mcurve(AeAegypti_Mean, AeAegypti_upperCI, AeAegypti_lowerCI, "dodgerblue4")
dev.off()

pdf("! Figures/Mosquito Abundance Curves/Aedes albopictus M-curve.pdf")
plot_Mcurve(AeAlbopictus_Mean, AeAlbopictus_upperCI, AeAlbopictus_lowerCI, "dodgerblue4")
dev.off()

pdf("! Figures/Mosquito Abundance Curves/Anopheles gambiae M-curve.pdf")
plot_Mcurve(AnGambiae_Mean, AnGambiae_upperCI, AnGambiae_lowerCI, "dodgerblue4")
dev.off()

pdf("! Figures/Mosquito Abundance Curves/Anopheles stephensi M-curve.pdf")
plot_Mcurve(AnStephensi_Mean, AnStephensi_upperCI, AnStephensi_lowerCI, "dodgerblue4")
dev.off()

pdf("! Figures/Mosquito Abundance Curves/Culex pipiens M-curve.pdf")
plot_Mcurve(CxPipiens_Mean, CxPipiens_upperCI, CxPipiens_lowerCI, "dodgerblue4")
dev.off()

pdf("! Figures/Mosquito Abundance Curves/Culex quinquefasciatus M-curve.pdf")
plot_Mcurve(CxQuinquefasciatus_Mean, CxQuinquefasciatus_upperCI, CxQuinquefasciatus_lowerCI, "dodgerblue4")
dev.off()

pdf("! Figures/Mosquito Abundance Curves/Culex tarsalis M-curve.pdf")
plot_Mcurve(CxTarsalis_Mean, CxTarsalis_upperCI, CxTarsalis_lowerCI, "dodgerblue4")
dev.off()



# #------------------------------------------------------
# # Proof of concept of the workflow: plot 15 posterior draws from a few traits at every temperature --> M(T) --> M(T) mean and CI
# #------------------------------------------------------
# list_cols <- c(paste0("X",1:5000))
# 
# pdf("AeAegypti_EFD.pdf")
# plot(AeAegypti_Temp$Celsius, AeAegypti_EFD$X1, type="l", lwd=2, col="red",
#      xlab = "AeAegypti_Temperature", ylab = "AeAegypti_EFD",
#      ylim=c(0,10))
# for (i in 2:15) {
#   lines(AeAegypti_Temp$Celsius, AeAegypti_EFD[[paste0("X",i)]], type="l", lwd=2, col="red")
# }
# dev.off()
# 
# pdf("AeAegypti_pEA.pdf")
# plot(AeAegypti_Temp$Celsius, AeAegypti_pEA$X1, type="l", lwd=2, col="blue",
#      xlab = "AeAegypti_Temperature", ylab = "AeAegypti_pEA",
#      ylim=c(0,1.2))
# for (i in 2:15) {
#   lines(AeAegypti_Temp$Celsius, AeAegypti_pEA[[paste0("X",i)]], type="l", lwd=2, col="blue")
# }
# dev.off()
# 
# pdf("AeAegypti_MDR.pdf")
# plot(AeAegypti_Temp$Celsius, AeAegypti_MDR$X1, type="l", lwd=2, col="darkgreen",
#      xlab = "AeAegypti_Temperature", ylab = "AeAegypti_MDR",
#      ylim=c(0,0.2))
# for (i in 2:15) {
#   lines(AeAegypti_Temp$Celsius, AeAegypti_MDR[[paste0("X",i)]], type="l", lwd=2, col="darkgreen")
# }
# dev.off()
# 
# pdf("AeAegypti_u.pdf")
# plot(AeAegypti_Temp$Celsius, AeAegypti_u$X1, type="l", lwd=2, col="orange",
#      xlab = "AeAegypti_Temperature", ylab = "AeAegypti_u",
#      ylim=c(0,1))
# for (i in 2:15) {
#   lines(AeAegypti_Temp$Celsius, AeAegypti_u[[paste0("X",i)]], type="l", lwd=2, col="orange")
# }
# dev.off()
# 
# pdf("AeAegypti_M.pdf")
# plot(AeAegypti_Temp$Celsius, AeAegypti_M$X1, type="l", lwd=2, col="purple",
#      xlab = "AeAegypti_Temperature", ylab = "AeAegypti_M(T)",
#      ylim=c(0,850))
# for (i in 2:15) {
#   lines(AeAegypti_Temp$Celsius, AeAegypti_M[[paste0("X",i)]], type="l", lwd=2, col="purple")
# }
# dev.off()



