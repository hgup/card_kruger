# Ensure necessary libraries are loaded
library(tidyverse)
library(knitr) # For kable table formatting

# Assume 'df' dataframe from the previous notebook example exists and is loaded
# If not, you need to run the data loading and cleaning steps first.

# Helper function for Standard Error of the Mean (SEM)
sem <- function(x, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  sd(x) / sqrt(length(x))
}

# --- 1. Distribution of Store Types ---

store_dist <- df %>%
  # Keep only one observation per store_id (e.g., from wave 1)
  filter(post == 0) %>%
  group_by(state) %>%
  summarise(
    n_stores = n(),
    pct_bk = mean(chain == "bk", na.rm = TRUE) * 100,
    pct_kfc = mean(chain == "kfc", na.rm = TRUE) * 100,
    pct_roys = mean(chain == "roys", na.rm = TRUE) * 100,
    # Assuming 'wendys' is the 4th chain, adjust if necessary
    pct_wendys = mean(!chain %in% c("bk", "kfc", "roys"), na.rm = TRUE) * 100,
    pct_co_owned = mean(co_owned == 1, na.rm = TRUE) * 100,
    .groups = 'drop'
  ) %>%
  # Pivot to match table structure (State as columns)
  pivot_longer(cols = -c(state, n_stores), names_to = "Variable", values_to = "Percentage") %>%
  pivot_wider(names_from = state, values_from = Percentage) %>%
  # Add t-test (optional, requires comparing proportions - using simple means comparison here for demo)
   mutate(t_stat = NA) # Placeholder for t-stat, proper test needed for proportions

# --- 2. Means in Wave 1 (post == 0) ---

wave1_means <- df %>%
  filter(post == 0) %>%
  # Calculate percentage full-time *before* summarizing
  mutate(pct_ft = (empft / (empft + emppt)) * 100) %>%
  group_by(state) %>%
  summarise(
    # FTE Employment
    mean_emp_total = mean(emp_total, na.rm = TRUE),
    sem_emp_total = sem(emp_total, na.rm = TRUE),
    t_emp_total = tryCatch(t.test(emp_total ~ state)$statistic, error=function(e) NA), # Get t-stat directly

    # Percentage Full-Time
    mean_pct_ft = mean(pct_ft, na.rm = TRUE),
    sem_pct_ft = sem(pct_ft, na.rm = TRUE),
    t_pct_ft = tryCatch(t.test(pct_ft ~ state)$statistic, error=function(e) NA),

    # Starting Wage
    mean_wage_st = mean(wage_st, na.rm = TRUE),
    sem_wage_st = sem(wage_st, na.rm = TRUE),
    t_wage_st = tryCatch(t.test(wage_st ~ state)$statistic, error=function(e) NA),

    # Add calculations for other available variables if needed
    .groups = 'drop'
  )

# --- 3. Means in Wave 2 (post == 1) ---

wave2_means <- df %>%
  filter(post == 1) %>%
  mutate(pct_ft = (empft / (empft + emppt)) * 100) %>%
  group_by(state) %>%
  summarise(
    # FTE Employment
    mean_emp_total = mean(emp_total, na.rm = TRUE),
    sem_emp_total = sem(emp_total, na.rm = TRUE),
    t_emp_total = tryCatch(t.test(emp_total ~ state)$statistic, error=function(e) NA),

    # Percentage Full-Time
    mean_pct_ft = mean(pct_ft, na.rm = TRUE),
    sem_pct_ft = sem(pct_ft, na.rm = TRUE),
    t_pct_ft = tryCatch(t.test(pct_ft ~ state)$statistic, error=function(e) NA),

    # Starting Wage
    mean_wage_st = mean(wage_st, na.rm = TRUE),
    sem_wage_st = sem(wage_st, na.rm = TRUE),
    t_wage_st = tryCatch(t.test(wage_st ~ state)$statistic, error=function(e) NA),

    # Add calculations for other available variables if needed
    .groups = 'drop'
  )

# --- 4. Combine and Format Table ---

# Function to format Mean (SEM)
format_stat <- function(mean_val, sem_val, digits = 1) {
  sprintf(paste0("%.", digits, "f (%.", digits+1, "f)"), mean_val, sem_val)
}

# Prepare Wave 1 data for binding
wave1_table <- wave1_means %>%
  pivot_longer(cols = -state, names_to = "stat_name", values_to = "value") %>%
  separate(stat_name, into = c("type", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = c(state, type), values_from = value) %>%
  mutate(
    NJ_Formatted = format_stat(nj_mean, nj_sem),
    PA_Formatted = format_stat(pa_mean, pa_sem),
    t_stat = coalesce(nj_t, pa_t) # t-stat is the same regardless of group
  ) %>%
  select(variable, NJ_Formatted, PA_Formatted, t_stat) %>%
  mutate(Variable = case_when(
    variable == "emp_total" ~ "a. FTE employment",
    variable == "pct_ft" ~ "b. Percentage full-time",
    variable == "wage_st" ~ "c. Starting wage",
    TRUE ~ variable # Keep original name if no match
  )) %>%
  select(Variable, NJ = NJ_Formatted, PA = PA_Formatted, `t a` = t_stat)

# Prepare Wave 2 data for binding
wave2_table <- wave2_means %>%
  pivot_longer(cols = -state, names_to = "stat_name", values_to = "value") %>%
  separate(stat_name, into = c("type", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = c(state, type), values_from = value) %>%
  mutate(
    NJ_Formatted = format_stat(nj_mean, nj_sem),
    PA_Formatted = format_stat(pa_mean, pa_sem),
    t_stat = coalesce(nj_t, pa_t)
  ) %>%
  select(variable, NJ_Formatted, PA_Formatted, t_stat) %>%
    mutate(Variable = case_when(
    variable == "emp_total" ~ "a. FTE employment",
    variable == "pct_ft" ~ "b. Percentage full-time",
    variable == "wage_st" ~ "c. Starting wage",
    TRUE ~ variable # Keep original name if no match
  )) %>%
  select(Variable, NJ = NJ_Formatted, PA = PA_Formatted, `t a` = t_stat)

# Prepare Store Dist data
store_dist_table <- store_dist %>%
  select(-n_stores) %>%
    mutate(Variable = case_when(
    Variable == "pct_bk" ~ "a. Burger King",
    Variable == "pct_kfc" ~ "b. KFC",
    Variable == "pct_roys" ~ "c. Roy Rogers",
    Variable == "pct_wendys" ~ "d. Wendy's",
    Variable == "pct_co_owned" ~ "e. Company-owned",
    TRUE ~ Variable
  )) %>%
  mutate(across(c(nj, pa), ~sprintf("%.1f", .))) %>% # Format percentages
  select(Variable, NJ = nj, PA = pa, `t a` = t_stat)


# --- Display the Tables ---

cat("--- 1. Distribution of Store Types (percentages) ---\n")
kable(store_dist_table, align = 'lrrr', caption = "Store Distribution (%)")

cat("\n--- 2. Means in Wave 1 ---\n")
kable(wave1_table, align = 'lrrr', caption = "Wave 1 Means (SEM)")

cat("\n--- 3. Means in Wave 2 ---\n")
kable(wave2_table, align = 'lrrr', caption = "Wave 2 Means (SEM)")

cat("\nNOTE: This table only includes variables available in the provided 'df' dataframe.\n")
cat("Formatting presents Mean (SEM). T-statistics compare NJ vs PA within the wave.\n")
cat("Variables like price, hours, recruiting bonus, etc., are not included.\n")


