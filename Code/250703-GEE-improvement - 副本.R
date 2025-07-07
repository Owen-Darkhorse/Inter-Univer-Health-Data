# Install required packages if not already installed
# install.packages(c("geeM", "doParallel", "foreach"))

################# Load Your Library ################################
library(ggplot2)
library(geepack)
library(LassoGEE)
library(doParallel)
library(foreach)
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
train <- train %>% mutate(
  RwMSTAT = case_when(
    .$RwMSTAT >= 0 & .$RwMSTAT <= 3 ~ "Level 1",
    .$RwMSTAT >= 4 & .$RwMSTAT <= 8 ~ "Level 2",
    .$RwMSTAT >= 9 & .$RwMSTAT <= 14 ~ "Level 3",
  )
)

cateVars <- c("RwJOCCSD", "RwJPHYS", "RwJSTOOP", "RwJSIGHT", "RwCENREG", "RwMSTAT",    
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
  
inputVars <- c(
  "RwAGEM_B", "RwJOCCSD", "RAEDYRS",
  # "RwJPHYS", "RwJSTOOP", "RwJSIGHT", 
  "RwCENREG", "RwMSTAT", "RwLIVBRO", "RwHIBP", "RwDIAB", "RwCANCR",
  "RwLUNG", "RwHEART", "RwSTROK", "RwPSYCH", "RwVIGACT",    
  "RwSMOKEV", "RwDRINK", "RwPhyLim", "RwCogLim",
  "RAChildBand", "RAGENDER", "RARACEM")

### Prepare your design matrix 
fullFormula <- paste(" ~ ", paste(inputVars, collapse = " + "))
baseFormula <- "RwRecProp ~ RwAGEM_B + RAEDYRS + RwJOCCSD"

X <- train[train$HHIDPN %in% sampleIdx,]%>% select(inputVars)
X <- impute_mixed(X)
y <- train$RwRecProp[train$HHIDPN %in% sampleIdx]
y <- fill_na_moving_average(y)
# y <- 20*cbind(y, 1-y)
# colnames(y) = c("Success", "Failure")

idVec <- train$HHIDPN[train$HHIDPN %in% sampleIdx]
X_design_df <- model.matrix(as.formula(fullFormula), 
                            cbind(X, "RwRecProp" = y))


# Define correlation structures and lambda grid
cor_structs <- c("independence", "exchangeable", "AR-1")
lambda_grid <- c(seq(0.01,0.09), seq(0.1,0.9), seq(1,15)) # Replace with your desired lambda values
# Create all combinations of corstr and lambda
param_grid <- expand.grid(corstr = cor_structs, lambda = lambda_grid, stringsAsFactors = FALSE)

# Set up parallel backend
n_cores <- min(4, parallel::detectCores())
cl <- makeCluster(n_cores)
registerDoParallel(cl)


library(LassoGEE)
# Fit models in parallel
results <- foreach(i = 1:nrow(param_grid), .packages = "LassoGEE") %dopar% {
  LassoGEE(
    X_design_df,
    y,
    id = idVec,
    family = binomial("logit"),
    lambda = param_grid$lambda[i],
    corstr = param_grid$corstr[i],
    method = "RWL",
    verbose = TRUE
  )
}

## Test Trial 
stopCluster(cl)


# Name the results for clarity
names(results) <- paste(param_grid$corstr, "lambda", param_grid$lambda, sep = "_")

qicVec <- c()
for (modName in names(results)){
  qicVec <- c(qicVec, IC(results[[modName]], "AIC"))
}

qicSmry <- data.frame("Lambda" = param_grid$lambda, 
                      "Correlation" = param_grid$corstr, 
                      "AIC" = qicVec)

ggplot(data = qicSmry) +
  geom_line(aes(x = Lambda, y = AIC, 
                group = factor(Correlation), 
                color = factor(Correlation)))

qicSmry[which.min(qicSmry$AIC),]



### The Best Fitting Model on the training set
X <- train[, c("HHIDPN", "Wave", "RwTR20", inputVars)]
y <- X$RwTR20
X <- impute_mixed(X)
X$y <- y

test <- impute_mixed(test)


write.csv(X, '../Data/hrsTrain.csv')
write.csv(test, '../Data/hrsTest.csv')


# X$resp <- cbind(X$Rtest# X$resp <- cbind(X$RwTR20, 20 - X$RwTR20) ## Number of success and number of failures
fullFormula <- as.formula(paste("RwTR20 ~ -1 + ", 
                                paste(inputVars, 
                                      collapse = " + ")))
X_design_df <- model.matrix(fullFormula, X) %>% data.frame()
X_design_df$y <- y
# count_missing_per_column(X)
# count_missing_per_column(y)

X_sub <- X_design_df
const_cols <- sapply(X_sub, function(col) length(unique(col)) == 1)
X_sub <- X_sub[, !const_cols]

X_design_df <- X_design_df[, -c(2, 8, 37)]

bestFit <- geeglm(
  y~.,
  family = poisson("log"),
  data = X_design_df[1:100, ],
  id = X$HHIDPN[1:100],
  waves = X$Wave[1:100],
  corstr = "independence"
)

head(X)
# LassoGEE(
#   train,
#   y,
#   id = idVec,
#   family = binomial("logit"),
#   lambda = 0.000,
#   corstr = "independence",
#   method = "RWL",
#   verbose = TRUE
# )$betaest