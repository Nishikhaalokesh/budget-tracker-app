# ============================================================
# FILE: functions_app.R
# PURPOSE: All data creation, math, and helper functions
#          for the Budget Tracker App
# AUTHOR: Nishikhaa Lokesh
# DATE: April 2026
# ============================================================

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(scales)


# ============================================================
# SECTION 1 — CREATE SAMPLE DATA
# ============================================================

create_sample_data_app <- function() {
  
  set.seed(42)
  
  categories <- c("Rent", "Groceries", "Dining Out", "Transport",
                  "Subscriptions", "Remittance", "Entertainment",
                  "Healthcare", "Clothing", "Savings")
  
  months <- seq(as.Date("2024-10-01"), as.Date("2025-03-31"), by = "month")
  
  transactions <- map_dfr(months, function(m) {
    tibble(
      date        = m + sample(0:27, 20, replace = TRUE),
      category    = sample(categories, 20, replace = TRUE,
                           prob = c(0.15,0.15,0.12,0.10,
                                    0.08,0.10,0.08,0.07,0.07,0.08)),
      amount_usd  = round(case_when(
        category == "Rent"          ~ rnorm(20, 900, 50),
        category == "Groceries"     ~ rnorm(20, 120, 20),
        category == "Dining Out"    ~ rnorm(20, 60,  15),
        category == "Transport"     ~ rnorm(20, 80,  10),
        category == "Subscriptions" ~ rnorm(20, 30,   5),
        category == "Remittance"    ~ rnorm(20, 200, 30),
        category == "Entertainment" ~ rnorm(20, 50,  15),
        category == "Healthcare"    ~ rnorm(20, 40,  20),
        category == "Clothing"      ~ rnorm(20, 70,  25),
        category == "Savings"       ~ rnorm(20, 100, 20),
        TRUE                        ~ rep(50, 20)
      ), 2),
      currency    = sample(c("USD","GBP","INR","CNY"), 20,
                           replace = TRUE,
                           prob = c(0.60, 0.15, 0.15, 0.10)),
      description = paste(category, "expense"),
      month_label = format(m, "%B %Y")
    )
  }) %>%
    filter(amount_usd > 0) %>%
    arrange(date)
  
  write_csv(transactions, "data/transactions_app.csv")
  message("✅ transactions_app.csv saved")
  
  budgets <- tibble(
    category       = categories,
    monthly_budget = c(950, 150, 80, 90, 35, 250, 60, 50, 80, 150)
  )
  
  write_csv(budgets, "data/budgets_app.csv")
  message("✅ budgets_app.csv saved")
  
  goals <- tibble(
    goal_name     = c("Emergency Fund", "Engagement Ring",
                      "New MacBook",    "Home Visit"),
    target_amount = c(3000, 1500, 800, 600),
    saved_so_far  = c(900,  300,  200, 150),
    deadline      = as.Date(c("2025-12-01","2025-07-01",
                              "2025-06-01","2025-05-01")),
    currency      = c("USD","USD","USD","INR"),
    priority      = c(2, 1, 3, 1)
  )
  
  write_csv(goals, "data/goals_app.csv")
  message("✅ goals_app.csv saved")
  
  group_members <- tibble(
    member_id   = 1:2,
    member_name = c("Person 1", "Person 2"),
    currency    = c("USD", "INR"),
    role        = c("primary", "partner")
  )
  
  write_csv(group_members, "data/group_members_app.csv")
  message("✅ group_members_app.csv saved")
  
  group_expenses <- tibble(
    date         = as.Date(c("2025-03-01","2025-03-05","2025-03-10")),
    description  = c("Dinner out", "Groceries", "Utility bill"),
    total_amount = c(120, 85, 60),
    currency     = c("USD", "USD", "USD"),
    paid_by      = c("Person 1", "Person 2", "Person 1"),
    split_type   = c("equal", "equal", "equal")
  )
  
  write_csv(group_expenses, "data/group_expenses_app.csv")
  message("✅ group_expenses_app.csv saved")
  
  message("\n🎉 All data files created in /data folder!")
}


# ============================================================
# SECTION 2 — CURRENCY CONVERTER
# ============================================================

fallback_rates_app <- list(
  USD = 1.00,
  GBP = 1.27,
  INR = 0.012,
  CNY = 0.14
)

convert_to_usd_app <- function(amount, from_currency) {
  rate <- fallback_rates_app[[from_currency]]
  if(is.null(rate)) rate <- 1
  return(round(as.numeric(amount) * rate, 2))
}














convert_from_usd_app <- function(amount_usd, to_currency) {
  rate <- fallback_rates_app[[to_currency]]
  if (is.null(rate)) rate <- 1
  return(round(amount_usd / rate, 2))
}


# ============================================================
# SECTION 3 — BUDGET SUMMARY
# ============================================================

budget_summary_app <- function(income_usd,
                               spending_df,
                               budgets_df) {
  summary <- spending_df %>%
    group_by(category) %>%
    summarise(actual_spent = sum(amount_usd), .groups = "drop") %>%
    right_join(budgets_df, by = "category") %>%
    mutate(
      actual_spent = replace_na(actual_spent, 0),
      remaining    = monthly_budget - actual_spent,
      pct_used     = round((actual_spent / monthly_budget) * 100, 1),
      status       = case_when(
        pct_used >= 100 ~ "Over budget",
        pct_used >= 80  ~ "Warning",
        TRUE            ~ "On track"
      )
    )
  
  total_spent  <- sum(summary$actual_spent)
  total_saved  <- income_usd - total_spent
  savings_rate <- round((max(total_saved, 1) / income_usd) * 100, 1)
  
  list(
    by_category  = summary,
    total_income = income_usd,
    total_spent  = total_spent,
    total_saved  = max(total_saved, 1),
    savings_rate = savings_rate,
    over_budget  = summary %>% filter(status == "Over budget")
  )
}


# ============================================================
# SECTION 4 — GOAL ADVISOR
# ============================================================

goal_advisor_app <- function(goal_name,
                             goal_amount,
                             months_to_goal,
                             current_spending_df,
                             num_people = 1) {
  
  monthly_needed    <- round(goal_amount / months_to_goal, 2)
  per_person_needed <- round(monthly_needed / num_people, 2)
  
  cuttable <- c("Dining Out", "Entertainment", "Clothing",
                "Subscriptions", "Groceries")
  
  suggestions <- current_spending_df %>%
    filter(category %in% cuttable) %>%
    group_by(category) %>%
    summarise(current_spend = sum(amount_usd), .groups = "drop") %>%
    mutate(
      suggested_cut = round(current_spend * 0.25, 2),
      new_budget    = round(current_spend * 0.75, 2)
    ) %>%
    arrange(desc(suggested_cut))
  
  total_cuttable <- sum(suggestions$suggested_cut)
  achievable     <- total_cuttable >= monthly_needed
  
  list(
    goal_name         = goal_name,
    goal_amount       = goal_amount,
    monthly_needed    = monthly_needed,
    per_person_needed = per_person_needed,
    suggestions       = suggestions,
    total_cuttable    = total_cuttable,
    achievable        = achievable,
    months_needed     = ceiling(goal_amount / total_cuttable),
    message           = if (achievable) {
      paste0("✅ You can save for ", goal_name,
             "! Cut these categories by 25% and you'll reach $",
             goal_amount, " in ", months_to_goal, " months.")
    } else {
      paste0("⚠️ You can free up $", total_cuttable,
             "/month but need $", monthly_needed,
             "/month. You'll reach your goal in ",
             ceiling(goal_amount / total_cuttable), " months instead.")
    }
  )
}


# ============================================================
# SECTION 5 — GOAL PRIORITY RANKER
# ============================================================

rank_goals_app <- function(goals_df) {
  goals_df %>%
    mutate(
      days_left      = as.numeric(deadline - Sys.Date()),
      amount_needed  = target_amount - saved_so_far,
      monthly_needed = round((amount_needed / (days_left / 30)), 2),
      urgency        = case_when(
        days_left <= 45  ~ "🔴 Urgent",
        days_left <= 120 ~ "🟡 Soon",
        TRUE             ~ "🟢 On track"
      )
    ) %>%
    arrange(days_left)
}


# ============================================================
# SECTION 6 — 6-MONTH PROJECTION
# ============================================================

six_month_projection_app <- function(income_usd,
                                     monthly_spend_usd,
                                     current_savings_usd,
                                     extra_monthly_saving = 0) {
  monthly_net <- income_usd - monthly_spend_usd + extra_monthly_saving
  
  tibble(
    month          = 1:6,
    month_label    = format(Sys.Date() %m+% months(0:5), "%B %Y"),
    net_this_month = monthly_net,
    cumulative     = current_savings_usd + (monthly_net * 1:6),
    status         = if_else(monthly_net >= 0, "Saving", "Deficit")
  )
}


# ============================================================
# SECTION 7 — GROUP EXPENSE SPLITTER
# ============================================================

split_expense_app <- function(description,
                              total_amount,
                              currency,
                              paid_by,
                              members,
                              split_type    = "equal",
                              custom_splits = NULL) {
  n         <- length(members)
  total_usd <- tryCatch(as.numeric(convert_to_usd_app(total_amount, currency)[1]), error=function(e) as.numeric(total_amount))
  
  splits <- if (split_type == "equal") {
    tibble(
      member     = members,
      owes_usd   = rep(round(total_usd / n, 2), n),
      owes_local = map_dbl(owes_usd,
                           ~convert_from_usd_app(.x, currency))
    )
  } else {
    tibble(
      member     = members,
      owes_usd   = round(custom_splits * total_usd, 2),
      owes_local = map_dbl(owes_usd,
                           ~convert_from_usd_app(.x, currency))
    )
  }
  
  splits <- splits %>%
    mutate(status = if_else(member == paid_by, "Paid ✅", "Owes 💸"))
  
  list(
    description = description,
    total_usd   = total_usd,
    paid_by     = paid_by,
    splits      = splits
  )
}


# ============================================================
# SECTION 8 — SUBSCRIPTIONS AUDIT
# ============================================================

subscriptions_checklist_app <- function() {
  tibble(
    service   = c("Netflix", "Spotify", "Amazon Prime", "Disney+",
                  "YouTube Premium", "Gym", "iCloud", "Google One",
                  "Microsoft 365", "LinkedIn Premium",
                  "Hulu", "Apple TV+"),
    cost_usd  = c(15.49,  9.99, 14.99,  7.99,
                  13.99, 40.00,  2.99,  2.99,
                  9.99, 39.99,  7.99,  9.99),
    necessary = FALSE
  )
}


# ============================================================
# SECTION 9 — PROFILE SAVE & LOAD
# ============================================================

save_profile_app <- function(name, income, currency, mode, categories_df) {
  profile <- tibble(
    name     = name,
    income   = income,
    currency = currency,
    mode     = mode
  )
  write_csv(profile,    "data/profile_app.csv")
  write_csv(categories_df, "data/user_categories_app.csv")
  message("✅ Profile saved!")
}

load_profile_app <- function() {
  profile <- read_csv("data/profile_app.csv", show_col_types = FALSE)
  cats    <- read_csv("data/user_categories_app.csv", show_col_types = FALSE, col_types = cols(category = col_character(), amount = col_double()))
  list(
    name       = profile$name[1],
    income     = profile$income[1],
    currency   = profile$currency[1],
    mode       = profile$mode[1],
    categories = cats
  )
}

profile_exists_app <- function() {
  cats <- read_csv("data/user_categories_app.csv", show_col_types = FALSE)
  nrow(cats) > 0
}
