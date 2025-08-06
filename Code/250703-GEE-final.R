# Install required packages if not already installed
# install.packages(c("geeM", "doParallel", "foreach"))

################# Load Your Library ################################
library(ggplot2)
library(geepack)
library(LassoGEE)
library(doParallel)
library(foreach)
library(tidyverse)

## Put your directory
setwd("C:/Users/86139/Desktop/PARA Note System/Projects/Inter-University Health Data/2025-Inter-Univer-Health-Data/Code")

################### Define Your Functions ##################################
count_missing_per_column <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}

impute_mixed <- function(X) {
  mode_val <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
  }
  for (col in names(X)) {
    if (is.numeric(X[[col]]) || is.integer(X[[col]])) {
      med <- median(X[[col]], na.rm = TRUE)
      X[[col]][is.na(X[[col]])] <- med
    } else if (is.factor(X[[col]]) || is.character(X[[col]])) {
      m <- mode_val(X[[col]])
      X[[col]][is.na(X[[col]])] <- m
      if (is.factor(X[[col]])) X[[col]] <- factor(X[[col]])
    }
  }
  return(X)
}

fill_na_moving_average <- function(y, window = 3) {
  # y: numeric vector with possible NAs
  # window: size of the moving average window (must be odd)
  if (window %% 2 == 0) stop("Window size must be odd.")
  n <- length(y)
  half_win <- floor(window / 2)
  y_filled <- y
  for (i in which(is.na(y))) {
    # Get window indices, excluding the NA itself
    idx <- (max(1, i - half_win)):(min(n, i + half_win))
    idx <- idx[!is.na(y[idx]) & idx != i]
    if (length(idx) > 0) {
      y_filled[i] <- mean(y[idx], na.rm = TRUE)
    }
    # If all neighbors are NA, leave as NA
  }
  return(y_filled)
}

################### Prepare your data ####################3
train <- read.csv('../Data/hrsWaveCleaned.csv')


colToDrop <- c("RwWORK", "RwJLIFT", "RwJSTRES", 
               "RwAnyCogImp", "RwLOST", 
               "RwWANDER", "RwHALUC", 
               "RwALONE", "RwJOCCSD.1",
               "RwAgeBand", "RAEduYrsBand",
               "RwLIVBRO")

train <- train[, !(colnames(train) %in% colToDrop)]

library(dplyr)
## Relevel Maritial status
train <- train %>% mutate(
  RwMSTAT = case_when(
    .$RwMSTAT >= 0 & .$RwMSTAT <= 3 ~ "Married/Partnered",
    .$RwMSTAT >= 4 & .$RwMSTAT <= 7 ~ "Separated/Divorced/Widowed",
    .$RwMSTAT >= 8 ~ "Not Married"
  )
)

## Relevel Living Siblings
train <- train %>% mutate(
  RwLIVSIB = case_when(
    .$RwLIVSIB == 0 ~ "None",
    .$RwLIVSIB >= 1 | .$RwLIVSIB <= 3  ~ "Few",
    .$RwLIVSIB >= 4 | .$RwLIVSIB <= 8  ~ "Medium",
    .$RwLIVSIB >= 9 ~ "Many",
  )
)

## Relevel Child Ever Born
train <- train %>% mutate(
  RAEVBRN = case_when(
    .$RAEVBRN == 0 ~ "None",
    .$RAEVBRN >= 1 | .$RAEVBRN <= 3  ~ "Few",
    .$RAEVBRN >= 4 | .$RAEVBRN <= 8  ~ "Medium",
    .$RAEVBRN >= 9 ~ "Many",
  )
)
train <- select(train, -RAChildBand)

## Relevel jobs 
train$RwJOCCSD <-  sapply(train$RwJOCCSD, function(x) {
  ifelse(x == "", "Retired", x) 
})

## Input and output vars
inputVars <- c(
  "RwAGEM_B", "RwJOCCSD", "RAEDYRS",
  "RAGENDER", "RARACEM", "RwMSTAT", "RwLIVSIB", "RAEVBRN",
  "RwCENREG", "RwPhyLim", "RwCogLim",
  "RwHIBP", "RwDIAB", "RwCANCR",
  "RwLUNG", "RwHEART", "RwSTROK", "RwPSYCH", 
  "RwVIGACT", "RwSMOKEV", "RwDRINK"
)

responseVars <- c(
  "RwTR20", "RwMSTOT"
)

## Convert categorical variables to factors
cateVars <- c("RwJOCCSD", 
              "RwCENREG", "RwMSTAT",    
              "RwLIVSIB", "RwHIBP", "RwDIAB", "RwCANCR",
              "RwLUNG", "RwHEART", "RwSTROK", "RwPSYCH", "RwVIGACT",    
              "RwSMOKEV", "RwDRINK", "RwPhyLim", "RwCogLim", "RwJOCCSD",
              "RAEVBRN", "RAGENDER", "RARACEM")

for (col in cateVars) {
  train[[col]] <- as.factor(train[[col]])
}

## Set reference level job
train$RwJOCCSD <- relevel(train$RwJOCCSD, ref="Retired")
train$RAEVBRN <- relevel(train$RAEVBRN, ref="None")
train$RwLIVSIB <- relevel(train$RwLIVSIB, ref="None")
train$RwMSTAT <- relevel(train$RwMSTAT, ref="Married/Partnered")
train$RAGENDER <- relevel(train$RAGENDER, ref="F")
train$RARACEM <- relevel(train$RARACEM, ref="White")

## Modelling recall index
recallFormula <- as.formula(paste("RwTR20 ~ ", paste(inputVars, collapse = " + ")))
mentalFormula <- as.formula(paste("RwMSTOT ~ ", paste(inputVars, collapse = " + ")))

train <- train %>% select(c("HHIDPN", "Wave", responseVars, inputVars))
train <- impute_mixed(train)

### Fitting Full Recall Model the 
bestRecallFit <- geeglm(
  recallFormula,
  family = poisson("log"),
  data = train,
  id = train$HHIDPN,
  waves = train$Wave,
  corstr = "independence"
)

bestRecallSmry <- summary(bestRecallFit)
bestRecallCov <- vcov(bestRecallFit)
bestRecallQIC <- QIC(bestRecallFit)

print(bestRecallSmry$coefficients %>% filter(`Pr(>|W|)` < 0.05))
print(bestRecallCov)
print(bestRecallQIC)
# print(bestANOVA)

### Fitting Full Mental Model the 
bestMentalFit <- geeglm(
  mentalFormula,
  family = poisson("log"),
  data = train,
  id = train$HHIDPN,
  waves = train$Wave,
  corstr = "independence"
)

bestMentalSmry <- summary(bestMentalFit)
bestMentalCov <- vcov(bestMentalFit)
bestMentalQIC <- QIC(bestMentalFit)

print(bestMentalSmry$coefficients %>% filter(`Pr(>|W|)` < 0.05))
print(bestMentalCov)
print(bestMentalQIC)
# print(bestANOVA)

###### Confidence Interval for occupation-specific Recall Score#############################
createCI <- function(smryTable, modelCov) {
  occEffectIdx <- startsWith(rownames(smryTable$coefficients), "RwJOCCSD")
  occEffectEst <- smryTable$coefficients[occEffectIdx, "Estimate"]
  occEffectSD <- diag(modelCov)[occEffectIdx] %>% sqrt()
  occEffectNames <- c("Farming/Forestry/Fishing", 
                      "Food/Personal Service", 
                      "Healthcare",
                      "Protective Armed Forces", 
                      "Management/Clerical/Business","Sales", 
                      "Skilled Trades/Production/Manual",
                      "STEM/Professional/Technical")
  linearCI_low <- occEffectEst - 1.96 * occEffectSD
  linearCI_up <- occEffectEst + 1.96 * occEffectSD
  
  occEffectCI <- cbind(linearCI_low, occEffectEst, linearCI_up) %>% exp()
  colnames(occEffectCI) <- c("Low", "Estimate", "High")
  occEffectCI <- as.data.frame(occEffectCI)
  occEffectCI$Occupation <- occEffectNames
  
  occEffectCI
}

recallCI <- createCI(bestRecallSmry, bestRecallCov)
mentalCI <- createCI(bestMentalSmry, bestMentalCov)
totalCI <- rbind(recallCI, mentalCI)
totalCI$Score <- rep(c("Recall", "Mental Status"), each = 8)


# Forest plot
createForest <- function(ciTable){
  ggplot(ciTable, aes(x = Estimate, y = Occupation, 
                      group = Score, color = Score)) +
    geom_point(size = 3, position=position_dodge(0.5)) + 
    geom_errorbarh(aes(xmin = Low, xmax = High), height = 0.2,
                   position=position_dodge(0.5)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_text(aes(x = 0.9, y = Occupation,
                  label = sprintf("%.2f (%.2f, %.2f)", Estimate, Low, High),
                  group = Score),
              hjust = -0.5, size = 3.5,
              position = position_dodge(0.7)) +
    scale_x_continuous(labels = scales::percent, limits = c(0.98, 1.12), 
                       expand = expansion(mult = c(0, 0.1))) +
    scale_y_discrete(labels = ~ str_wrap(gsub('/', ' ', .x), 15))+
    labs(
      x = "Percent of Retirees' Performance (95% CI)",
      y = "Occupation Groups",
      title = "Post-Retirement Workers' Cognitive Performance",
    ) +
    coord_cartesian(xlim = c(0.9, 1.2)) + 
    theme(axis.text.y = element_text(hjust = 1, size = 8, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 15, 
                                    family = "serif", face = "bold"),
          axis.title.y = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.9,0.5))
}

createForest(totalCI)

ggplot(totalCI, aes(x = Estimate, y = Occupation, 
                    group = Score, color = Score)) +
  geom_point(size = 3, position=position_dodge(0.5)) + 
  geom_errorbarh(aes(xmin = Low, xmax = High), height = 0.2,
                 position=position_dodge(0.5)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_text(aes(x = 0.9, y = Occupation,
                label = sprintf("%.2f (%.2f, %.2f)", Estimate, Low, High),
                group = Score),
            hjust = -0.5, size = 3.5,
            position = position_dodge(0.7)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.98, 1.12), 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(labels = ~ str_wrap(gsub('/', ' ', .x), 15))+
  labs(
    x = "Percent of Retirees' Performance (95% CI)",
    y = "Occupation Groups",
    title = "Post-Retirement Workers' Cognitive Performance",
  ) +
  coord_cartesian(xlim = c(0.9, 1.2)) + 
  theme(axis.text.y = element_text(hjust = 1, size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15, 
                                  family = "serif", face = "bold"),
        axis.title.y = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9,0.5))


###### Confidence Interval: any occupation-specific rate change ############
interacRecallFit <- update(bestRecallFit, RwTR20~.+RwAGEM_B:RwJOCCSD)
interacMentalFit <- update(bestMentalFit, RwMSTOT~.+RwAGEM_B:RwJOCCSD)

interacRecallSmry <- summary(interacRecallFit)
interacMentalSmry <- summary(interacMentalFit)

interacRecallCov <- vcov(interacRecallFit)
interacMentalCov <- vcov(interacMentalFit)


interacRecallCI <- createCI(interacRecallSmry, interacRecallCov)
interacMentalCI <- createCI(interacMentalSmry, interacMentalCov)
interacTotalCI <- rbind(interacRecallCI, interacMentalCI)
interacTotalCI$Score <- rep(c("Recall", "Mental Status"), each=8)

# createForest(interacTotalCI)

ggplot(interacTotalCI, aes(x = Estimate, y = Occupation, 
                    group = Score, color = Score)) +
  geom_point(size = 3, position=position_dodge(0.5)) + 
  geom_errorbarh(aes(xmin = Low, xmax = High), height = 0.2,
                 position=position_dodge(0.5)) +
  geom_vline(xintercept = 1, linetype = "dashed") + 
  geom_text(aes(x = 0.6, y = Occupation,
                label = sprintf("%.2f (%.2f, %.2f)", Estimate, Low, High),
                group = Score),
            hjust = 0, size = 3.5, 
            position = position_dodge(0.7)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.6, 1.1), 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(labels = ~ str_wrap(gsub('/', ' ', .x), 15))+
  labs(
    x = "Percent of Retirees' Performance (95% CI)",
    y = "Occupation Groups",
    title = "Post-Retirement Workers' Cognitive Performance",
  ) +
  coord_cartesian(xlim = c(0.6, 1.1)) + 
  theme(axis.text.y = element_text(hjust = 1, size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15, 
                                  family = "serif", face = "bold"),
        axis.title.y = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9,0.4))
