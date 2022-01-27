## -----------------------------------------------------------------------------
library(imaginator)
set.seed(1234)
tbl_policy <- policies_simulate(
  n = 10,
  num_years = 5)

## -----------------------------------------------------------------------------
tbl_policy <- policies_simulate(
  n = 10,
  num_years = 5,
  retention = 0.9,
  growth = 0.1)

## -----------------------------------------------------------------------------
# Gradually expanding book of business
tbl_policy <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.9,
  growth = 0.2)

# Gradually contracting book of business
tbl_policy <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.8,
  growth = 0.1)

## -----------------------------------------------------------------------------
tbl_policy <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.0,
  growth = 1.0)

## -----------------------------------------------------------------------------
tbl_policy <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = c(0.95, 0.9, 0.85, 0.8),
  growth = c(0.25, 0.2, 0.1, 0.05))

## -----------------------------------------------------------------------------
tbl_policy <- policies_simulate(
  n = 100,
  num_years = 5,
  retention = 0.9, 
  growth = runif(4, .05, .15))

## -----------------------------------------------------------------------------
tbl_renewals <- policies_renew(tbl_policy, retention = 0.8)

tbl_new_business <- policies_grow(tbl_policy, growth = 0.2)

## -----------------------------------------------------------------------------
tbl_gl_ca <- policies_simulate(
  n = 5, 
  num_years = 5, 
  additional_columns = list(line = "GL", state = "CA"))

## ----echo=FALSE---------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
tbl_gl_ca %>% 
  arrange(policyholder_id) %>% 
  head() %>% 
  knitr::kable()

## -----------------------------------------------------------------------------
tbl_gl_ca <- policies_simulate(
  n = 500, 
  num_years = 5, 
  additional_columns = list(line = "GL", state = "CA"), 
  retention = 0.5, 
  growth = .01)

tbl_gl_ny <- policies_simulate(
  n = 50, 
  num_years = 5, 
  additional_columns = list(line = "GL", state = "NY"), 
  retention = 0.9, 
  growth = .5)

tbl_gl <- dplyr::bind_rows(tbl_gl_ca, tbl_gl_ny)

## -----------------------------------------------------------------------------
tbl_policy <- policies_simulate(
  n = 5, 
  policy_years = 1:3, 
  growth = c(1, 0.5))

## ----echo=FALSE---------------------------------------------------------------
tbl_policy %>% 
  mutate(PolicyYear = lubridate::year(policy_effective_date)) %>% 
  group_by(PolicyYear) %>% 
  summarise(MaxPolicyholderID = max(policyholder_id)) %>% 
  knitr::kable()

