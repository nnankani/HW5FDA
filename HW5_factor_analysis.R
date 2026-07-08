# ISyE 4803 Financial Data Analysis
# Homework 5 — Factor Analysis
# Part A: Two-Factor Model
# Neal Nankani

# Clear the environment once at the beginning
rm(list = ls())

# ------------------------------------------------------------
# Step 1: Load and inspect the data
# ------------------------------------------------------------

stock_data <- read.csv(
  "Stock_FX_Bond.csv",
  stringsAsFactors = FALSE
)

# Verify that the dataset loaded correctly
dim(stock_data)
names(stock_data)

# ------------------------------------------------------------
# Step 2: Select the eight adjusted-closing-price columns
# ------------------------------------------------------------

stock_columns <- c(
  "GM_AC",
  "F_AC",
  "UTX_AC",
  "CAT_AC",
  "MRK_AC",
  "PFE_AC",
  "IBM_AC",
  "MSFT_AC"
)

stock_prices <- stock_data[, stock_columns]

# Verify the selected stock-price data
dim(stock_prices)
names(stock_prices)
head(stock_prices)

# Check for missing values
colSums(is.na(stock_prices))

# ------------------------------------------------------------
# Step 3: Calculate daily log returns
# ------------------------------------------------------------

stock_returns <- log(
  stock_prices[-1, ] /
    stock_prices[-nrow(stock_prices), ]
)

# Preserve the stock column names
names(stock_returns) <- stock_columns

# Verify the return data
dim(stock_returns)
names(stock_returns)
head(stock_returns)

# Check for missing or non-finite returns
colSums(is.na(stock_returns))
colSums(!is.finite(as.matrix(stock_returns)))

# ------------------------------------------------------------
# Step 4: Fit the unrotated two-factor model
# ------------------------------------------------------------

fa_fit <- stats::factanal(
  x = stock_returns,
  factors = 2,
  rotation = "none"
)

# Display the complete factor-analysis output
print(fa_fit, cutoff = 0)

# ------------------------------------------------------------
# Step 5: Extract the loadings and uniquenesses
# ------------------------------------------------------------

factor_loadings <- as.matrix(fa_fit$loadings)
uniquenesses <- fa_fit$uniquenesses

cat("\nFactor Loadings:\n")
print(round(factor_loadings, 6))

cat("\nUniquenesses:\n")
print(round(uniquenesses, 6))