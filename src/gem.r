# ---
# title: "Card & Krueger (1994) Minimum Wage DiD Analysis"
# author: "Analysis using R"
# date: "`r Sys.Date()`"
# output: html_notebook
# ---

# ## 1. Setup: Load Libraries

# Use pacman to load/install packages
# install.packages("pacman") # Run this once if you don't have pacman
library(pacman)
pacman::p_load(
  tidyverse,  # For data manipulation (dplyr, tidyr) and plotting (ggplot2)
  readxl,     # For reading Excel files
  fixest,     # For fast fixed-effects regression (feols)
  modelsummary, # For creating regression tables (alternative to stargazer)
  ggplot2,    # For plotting
  knitr       # For knitting the notebook and kable tables
)

# Optional: Set a path to your data directory.
# Replace this with the actual path to YOUR data file or ensure the script
# is run with the working directory set to the folder containing the 'data' subfolder.
# data_path <- "D:/analysis/Econometrics/project/card_kruger/data/"
data_file <- 'D:/analysis/Econometrics/project/card_kruger/data/fast-food-data.xlsx' # Assuming the file is in the working directory or specified path

# Define a helper function for nice tables (using kable from knitr)
nice_table <- function(x, ...) {
  knitr::kable(x, digits = 3, ...)
}

# Set default theme for ggplot
theme_set(theme_minimal())

# ## 2. Load Data

# Check if the file exists before attempting to load
if (!file.exists(data_file)) {
  stop("Error: Data file not found at specified location: ", data_file)
}

# Load the dataset using readxl::read_excel
raw_dat <- read_excel(data_file)

# ## 3. Explore Raw Data

cat("Structure of the raw dataset:\n")
str(raw_dat)

cat("\nSummary of the raw dataset:\n")
summary(raw_dat)
# Note: The raw data is in wide format, with variables for period 1 (before)
# and period 2 (after, indicated by '2' suffix).

# ## 4. Data Cleaning and Reshaping

# Reshape data from wide to long format
# Stores measured before the wage change (wave 1) will have post = 0
# Stores measured after the wage change (wave 2) will have post = 1

# Select wave 1 variables, rename ID, mark as pre-period (post=0)
# df_pre <- raw_dat %>%
#   select(
#     store_id = id, # Assuming 'X' is the store identifier
#     chain, state, co_owned, starts_with("empft"), starts_with("emppt"),
#     starts_with("nmgrs"), starts_with("wage_st")
#     # Add any other time-varying controls if needed
#     ) %>%
#   select(-ends_with("2")) #%>% # Remove any accidentally included wave 2 vars
#   mutate(post = 0)

# Select wave 2 variables, rename ID and wave 2 variables, mark as post-period (post=1)
# df_post <- raw_dat %>%
#   select(
#     store_id = id,
#     chain, state, co_owned, ends_with("2") # Select ID, time-invariant vars, and wave 2 vars
#   ) %>%
#   rename_with(~ gsub("2$", "", .x), ends_with("2")) %>% # Remove '2' suffix
#   mutate(post = 1)

# Combine pre and post dataframes
# long_df <- bind_rows(df_pre, df_post)

# Calculate full-time equivalent employment (FTE)
# FTE = Full-time employees + 0.5 * Part-time employees
# Also create the treatment dummy (1 = NJ, 0 = PA)
df <- long_df %>%
  mutate(
    # Ensure employment vars are numeric, coercing errors to NA
    empft = as.numeric(empft),
    emppt = as.numeric(emppt),
    # Calculate total FTE, handle potential NAs
    emp_total = empft + (0.5 * emppt),
    # Create treatment dummy: 1 if NJ, 0 if PA
    treatment = ifelse(state == "nj", 1, 0),
    # Ensure potential factor variables are treated as such
    chain = factor(chain),
    state = factor(state)
    ) %>%
  # Select final columns for analysis
  select(
    store_id, state, treatment, post, chain, co_owned,
    empft, emppt, emp_total, wage_st, nmgrs
  ) %>%
  # Arrange for easier viewing (optional)
  arrange(store_id, post)

cat("\nStructure of the final long dataset:\n")
str(df)

cat("\nSummary of the final long dataset:\n")
# Use kable for a cleaner summary table view
summary(df) %>% nice_table()

# Check for missing values in key variables
cat("\nMissing values summary:\n")
colSums(is.na(df)) %>% nice_table(col.names = c("Missing Count"))


# ## 5. Exploratory Data Analysis (EDA)

# Calculate average employment by state and time period
avg_emp_summary <- df %>%
  group_by(state, post) %>%
  summarise(avg_emp = mean(emp_total, na.rm = TRUE), .groups = 'drop') # Use .groups='drop'

cat("\nAverage FTE Employment by State and Period:\n")
print(avg_emp_summary)

# Visualize the trends using ggplot2
plot_title <- "Average FTE Employment Before (0) and After (1) Policy Change"
emp_plot <- ggplot(avg_emp_summary, aes(x = factor(post), y = avg_emp, color = state, group = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = plot_title,
    x = "Period (0 = Before, 1 = After)",
    y = "Average FTE Employment",
    color = "State"
  ) +
  scale_color_manual(values = c("nj" = "blue", "pa" = "red")) # Assign colors

print(emp_plot)
# This plot visually represents the core idea of DiD: comparing the change in NJ
# (blue line) to the change in PA (red line).


# ## 6. Running the Difference-in-Differences (DiD) Model

# We use fixest::feols for efficient estimation.

# Model 1: Basic DiD
# Y = emp_total
# treatment = 1 if NJ, 0 if PA
# post = 1 if after policy, 0 if before
# Interaction term `treatment:post` captures the DiD effect
m1_basic_did <- feols(emp_total ~ treatment * post, data = df)

# Model 2: DiD with Covariates
# Adding control variables: chain, company ownership, starting wage, num managers
m2_did_cov <- feols(emp_total ~ treatment * post + chain + co_owned + wage_st + nmgrs,
                    data = df)

# Model 3: DiD with Store Fixed Effects (FE) and Covariates
# Controls for all time-invariant store characteristics
# `treatment` main effect is absorbed by store FE. DiD effect is still `treatment:post`
m3_did_fe <- feols(emp_total ~ treatment * post + chain + co_owned + wage_st + nmgrs | store_id,
                   data = df)

# Model 4: DiD with Store FE, Covariates, and Clustered Standard Errors (SEs)
# Cluster SEs by state, the level of treatment assignment.
# Be cautious with few clusters (only 2 states).
m4_did_fe_clust <- feols(emp_total ~ treatment * post + chain + co_owned + wage_st + nmgrs | store_id,
                         data = df,
                         cluster = ~state)

# Alternative: Cluster SEs by store (if concerned about serial correlation within stores)
# m4b_did_fe_clust_store <- feols(emp_total ~ treatment * post + chain + co_owned + wage_st + nmgrs | store_id,
#                          data = df,
#                          cluster = ~store_id)


# ## 7. Results Presentation

cat("\n--- DiD Model Results ---\n")

# Display results using modelsummary (modern alternative to stargazer)
# Provides clean tables in various formats (default is console)
models <- list(
  "Basic DiD" = m1_basic_did,
  "DiD + Cov" = m2_did_cov,
  "DiD + Cov + FE" = m3_did_fe,
  "DiD + Cov + FE (Clust SE)" = m4_did_fe_clust
  # Add m4b_did_fe_clust_store if you ran it
)

# Customize the output: focus on key coefficients, add GOF stats
modelsummary(models,
             stars = TRUE, # Add significance stars
             coef_map = c("treatment:post" = "NJ x Post (DiD Effect)", # Rename interaction term
                          "treatment" = "NJ (Treatment)",
                          "post" = "Post-Policy Period"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"), # Goodness-of-fit stats
             title = "DiD Estimates of Minimum Wage Effect on FTE Employment")

cat("\n--- Interpretation Notes ---\n")
cat("The coefficient 'NJ x Post (DiD Effect)' estimates the average causal effect of the minimum wage increase on FTE employment in New Jersey relative to Pennsylvania.\n")
cat("- A positive coefficient suggests employment increased more (or decreased less) in NJ relative to PA.\n")
cat("- A negative coefficient suggests employment decreased more (or increased less) in NJ relative to PA.\n")
cat("Model 3 (with Store FE) and Model 4 (with Store FE and Clustered SEs) are often preferred as they control for unobserved store-specific characteristics and potential error correlation.\n")
cat("Statistical significance (indicated by stars) suggests whether the observed effect is likely different from zero.\n")

# End of notebook