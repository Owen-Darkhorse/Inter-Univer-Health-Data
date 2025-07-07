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
train <- read.csv('../Data/hrsTrain.csv')
test <- read.csv('../Data/hrsTest.csv')

colToDrop <- c("X", "RwJLIFT", "RwJSTRES", 
               "RwAnyCogImp", "RwLOST", "RwWANDER", "RwHALUC", "RwALONE", "RwJOCCSD.1")

train <- train[, !(colnames(train) %in% colToDrop)]
test <- test[, !(colnames(test) %in% colToDrop)]

library(dplyr)
## Relevel Maritial status
train <- train %>% mutate(
  RwMSTAT = case_when(
    .$RwMSTAT >= 0 & .$RwMSTAT <= 3 ~ "Married/Partnered",
    .$RwMSTAT >= 4 & .$RwMSTAT <= 7 ~ "Separated/Divorced/Widowed",
    .$RwMSTAT >= 8 ~ "Not Married"
  )
)

test <- test %>% mutate(
  RwMSTAT = case_when(
    .$RwMSTAT >= 0 & .$RwMSTAT <= 3 ~ "Married/Partnered",
    .$RwMSTAT >= 4 & .$RwMSTAT <= 7 ~ "Separated/Divorced/Widowed",
    .$RwMSTAT >= 8 ~ "Not Married"
  )
)

train$RwJOCCSD <-  sapply(train$RwJOCCSD, function(x) {
  ifelse(x == "", "Retired", x) 
})

test$RwJOCCSD <-  sapply(test$RwJOCCSD, function(x) {
  ifelse(x == "", "Retired", x) 
})
## Input and output vars
inputVars <- c(
  "RwAGEM_B", "RwJOCCSD", "RAEDYRS",
  "RwCENREG", "RwMSTAT", "RwLIVBRO", "RwHIBP", "RwDIAB", "RwCANCR",
  "RwLUNG", "RwHEART", "RwSTROK", "RwPSYCH", "RwVIGACT",    
  "RwSMOKEV", "RwDRINK", "RwPhyLim", "RwCogLim",
  "RAChildBand", "RAGENDER", "RARACEM")

responseVars <- c(
  "RwTR20"
)

# "RwMSTOT"

## Convert categorical variables to factors
cateVars <- c("RwJOCCSD", 
              "RwCENREG", "RwMSTAT",    
              "RwLIVBRO", "RwHIBP", "RwDIAB", "RwCANCR",
              "RwLUNG", "RwHEART", "RwSTROK", "RwPSYCH", "RwVIGACT",    
              "RwSMOKEV", "RwDRINK", "RwPhyLim", "RwCogLim", "RwJOCCSD",
              "RAChildBand", "RAGENDER", "RARACEM")

for (col in cateVars) {
  train[[col]] <- as.factor(train[[col]])
  test[[col]] <- as.factor(test[[col]])
}

## Sample 20% of training set for feature selection
sampleIdx <- sample(unique(train$HHIDPN), 0.2*length(unique(train$HHIDPN)))
idVec <- train$HHIDPN[train$HHIDPN %in% sampleIdx]

### Prepare your training design matrix for var selection
X.train <- train[train$HHIDPN %in% sampleIdx,]%>% 
  select(c("HHIDPN", "Wave", responseVars,inputVars))
X.train <- impute_mixed(X.train)
y.train <- train$RwTR20[train$HHIDPN %in% sampleIdx]
y.train <- fill_na_moving_average(y.train)


inputVars_valid <- inputVars[sapply(inputVars, function(var) length(unique(X.train[[var]])) > 1)]
fullFormula <- paste("RwTR20 ~ -1 + ", paste(inputVars_valid, collapse = " + "))
X.train_design_df <- model.matrix(as.formula(fullFormula), data = X.train)
X.train_design_df <- X.train_design_df[,!(colnames(X.train_design_df) %in% c("RwJOCCSDRetired","RARACEMWhite"))]

# Define correlation structures and lambda grid
cor_structs <- c("independence", "exchangeable", "AR-1")
lambda_grid <- c(seq(0.01,0.09)) # Replace with your desired lambda values
# , seq(0.1,0.9), seq(1,15)

# Create all combinations of corstr and lambda
param_grid <- expand.grid(corstr = cor_structs, lambda = lambda_grid, stringsAsFactors = FALSE)

# Set up parallel backend
# n_cores <- min(4, parallel::detectCores())
# cl <- makeCluster(n_cores)
# registerDoParallel(cl)

# Fit models in parallel
# results <- foreach(i = 1:nrow(param_grid), .packages = "LassoGEE") %dopar% {
#   LassoGEE(
#     X.train_design_df,
#     y.train,
#     id = X.train$HHIDPN,
#     family = poisson,
#     lambda = param_grid$lambda[i],
#     corstr = param_grid$corstr[i],
#     method = "RWL",
#     verbose = TRUE
#   )
# }
# stopCluster(cl)
# 
# 
# # Name the results for clarity
# names(results) <- paste(param_grid$corstr, "lambda", param_grid$lambda, sep = "_")
# 
# qicVec <- c()
# for (modName in names(results)){
#   qicVec <- c(qicVec, IC(results[[modName]], "AIC"))
# }
# 
# qicSmry <- data.frame("Lambda" = param_grid$lambda, 
#                       "Correlation" = param_grid$corstr, 
#                       "AIC" = qicVec)
# 
# ggplot(data = qicSmry) +
#   geom_line(aes(x = Lambda, y = AIC, 
#                 group = factor(Correlation), 
#                 color = factor(Correlation)))
# 
# qicSmry[which.min(qicSmry$AIC),]



### The Best Fitting Model on the training set
# X <- train[, c("HHIDPN", "Wave", "RwTR20", inputVars)]
# y <- X$RwTR20
# X <- impute_mixed(X)
# X$y <- y
# 
# test <- impute_mixed(test)


write.csv(X, '../Data/hrsTrain.csv')
write.csv(test, '../Data/hrsTest.csv')


# X$resp <- cbind(X$Rtest# X$resp <- cbind(X$RwTR20, 20 - X$RwTR20) ## Number of success and number of failures
fullFormula <- as.formula(paste("RwTR20 ~ -1 + ", 
                                paste(inputVars, 
                                      collapse = " + ")))
X.train <- train %>%select(c("HHIDPN", "Wave", responseVars, inputVars))
X.train <- impute_mixed(X.train)
y.train <- train$RwTR20
y.train <- fill_na_moving_average(y.train)


X.train$RwJOCCSD <- relevel(X.train$RwJOCCSD, ref="Retired")

# inputVars_valid <- inputVars[sapply(inputVars, function(var) length(unique(X.train[[var]])) > 1)]
# fullFormula <- formula(paste("RwTR20 ~ -1 + ", paste(inputVars_valid, collapse = " + ")))
# X.train_design_df <- model.matrix(as.formula(fullFormula), data = X.train)
# X.train_design_df <- X.train_design_df[,!(colnames(X.train_design_df) %in% c("RwJOCCSDRetired","RARACEMWhite"))]
# X.train_design_df$RwTR20 <- y.train

fullFormula <- formula(paste("RwTR20 ~ ", paste(inputVars, collapse = " + ")))

bestFit <- geeglm(
  fullFormula,
  family = poisson("log"),
  data = X.train,
  id = X.train$HHIDPN,
  waves = X.train$Wave,
  corstr = "independence"
)

bestSmry <- summary(bestFit)
bestCov <- vcov(bestFit)
bestQIC <- QIC(bestFit)
# bestANOVA <- anova(bestFit)

print(bestSmry$coefficients %>% filter(`Pr(>|W|)` < 0.05))
print(bestCov)
print(bestQIC)
# print(bestANOVA)


###### Confidence Interval for occupation-specific #############################
occEffectIdx <- startsWith(rownames(bestSmry$coefficients), "RwJOCCSD")
occEffectEst <- bestSmry$coefficients[occEffectIdx, "Estimate"]
occEffectSD <- diag(bestCov)[occEffectIdx] %>% sqrt()
occEffectNames <- c("Farming/Forestry/Fishing", "Food/Personal Service", 
                    "Healthcare","High Risk Occupations", 
                    "Management/Clerical/Business","Sales", 
                    "Skilled Trades/Production/Manual",
                    "STEM/Professional/Technical")

linearCI_low <- occEffectEst - 1.96 * occEffectSD
linearCI_up <- occEffectEst + 1.96 * occEffectSD

occEffectCI <- cbind(linearCI_low, occEffectEst, linearCI_up) %>% exp()
colnames(occEffectCI) <- c("Low", "Estimate", "High")
occEffectCI <- as.data.frame(occEffectCI)
occEffectCI$Occupation <- occEffectNames


# Forest plot
ggplot(occEffectCI, aes(x = Estimate, y = Occupation)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = Low, xmax = High), height = 0.2, color = "#0072B2") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = sprintf("%.2f (%.2f, %.2f)", Estimate, Low, High)), 
            x = 0.9, hjust = 0, size = 3.5) +
  scale_x_continuous(limits = c(0.98, 1.12), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Effect Estimate (95% CI)",
    y = "Occupation Group",
    title = "Recall Scores Ratios, Occupation-specific vs Retirement Recall Scores"
  ) +
  coord_cartesian(xlim = c(0.9, 1.15)) + 
  theme(
    axis.text.y = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.y = element_blank()
  )
  # theme_minimal(base_size = 13)

###### Confidence Interval: any occupation-specific rate change ############
interacFit <- update(bestFit, RwTR20~.+RwAGEM_B:RwJOCCSD)

interacSmry <- summary(interacFit)
interacCov <- vcov(interacFit)
occInteracIdx <- startsWith(rownames(interacSmry$coefficients), "RwAGEM_B:RwJOCCSD")
occInteracEst <- interacSmry$coefficients[occInteracIdx, "Estimate"]
occInteracSD <- diag(interacCov)[occInteracIdx] %>% sqrt()
occInteracNames <- c("Farming/Forestry/Fishing", "Food/Personal Service", "Healthcare",
                    "High Risk Occupations", "Management/Clerical/Business",
                    "Sales", "Skilled Trades/Production/Manual", "STEM/Professional/Technical")

linearCI_low <- occInteracEst - 1.96 * occInteracSD
linearCI_up <- occInteracEst + 1.96 * occInteracSD

occInteracCI <- cbind(linearCI_low, occInteracEst, linearCI_up) %>% exp()
colnames(occInteracCI) <- c("Low", "Estimate", "High")
occInteracCI <- as.data.frame(occInteracCI)
occInteracCI$Occupation <- occInteracNames

ggplot(occInteracCI, aes(x = Estimate, y = Occupation)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = Low, xmax = High), height = 0.2, color = "#0072B2") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = sprintf("%.3f (%.3f, %.3f)", Estimate, Low, High)), 
            x = 0.95, hjust = 0, size = 3.5) +
  scale_x_continuous(limits = c(0.98, 1.05), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Effect Estimate (95% CI)",
    y = "Occupation Group",
    title = "Annual Recall Scores Change, Occupation-specific vs Retirement"
  ) +
  coord_cartesian(xlim = c(0.95, 1.025)) + 
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.y = element_blank()
  )


###### Evaluate Generalizability on the Test Set ###############
test <- read.csv("../Data/hrsTest.csv")
test.y <- test$RwTR20
count_missing_per_column(test)

test <- model.matrix(fullFormula, test)
test <- test[,-c(2,8,33)]
colnames(test) <- colnames(X_design_df)
test <- as.data.frame(test)

y.pred <- predict(bestFit, newdata = test, type = "response") 
plot(test.y, y.pred)