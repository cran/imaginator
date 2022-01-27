## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, results = 'asis')
library(dplyr)
library(knitr)

## -----------------------------------------------------------------------------
library(imaginator)
set.seed(12345)
tbl_policy <- policies_simulate(2, 2001:2005)

## -----------------------------------------------------------------------------
tbl_claim_transaction <- claims_by_wait_time(
  tbl_policy,
  claim_frequency = 2,
  payment_frequency = 3,
  occurrence_wait = 10,
  report_wait = 5,
  pay_wait = 5,
  pay_severity = 50)

## ----echo = FALSE-------------------------------------------------------------
tbl_claim_transaction %>% 
  filter(policyholder_id == 1, lubridate::year(policy_effective_date) == 2001) %>% 
  select(claim_id, occurrence_date, report_date, payment_date, payment_amount) %>% 
  kable()

## ---- message = FALSE---------------------------------------------------------
library(distributions3)
tbl_claim_transaction <- claims_by_wait_time(
  tbl_policy,
  claim_frequency = 2,
  payment_frequency = Poisson(2),
  occurrence_wait = Poisson(10),
  report_wait = Poisson(5),
  pay_wait = Poisson(5),
  pay_severity = LogNormal(log(50), 0.5 * log(50)))

## ----echo=FALSE---------------------------------------------------------------
tbl_claim_transaction %>% 
  filter(policyholder_id == 1, lubridate::year(policy_effective_date) == 2001) %>% 
  select(claim_id, occurrence_date, report_date, payment_date, payment_amount) %>% 
  kable()

## -----------------------------------------------------------------------------
set.seed(12345)
tbl_policy <- policies_simulate(2, 2001:2005)

lstFreq <- list(
    4
  , 3
  , 2
  , 1
)

lstSev <- list(
  250
)
lstSev[1:4] <- lstSev[1]

tbl_ibnyr_fixed <- claims_by_first_report(
    tbl_policy
  , frequency = lstFreq
  , payment_severity = lstSev
  , lags = 1:4)

## -----------------------------------------------------------------------------
tbl_ibnyr_fixed %>% 
  filter(policyholder_id == 1) %>% 
  filter(policy_effective_date == min(policy_effective_date)) %>% 
  kable()

## -----------------------------------------------------------------------------
lstFreq <- list(
    Poisson(4)
  , Poisson(3)
  , Poisson(2)
  , Poisson(1)
)

lstSev <- list(
  LogNormal(log_mu = log(10000), log_sigma = .5*log(10000))
)
lstSev[1:4] <- lstSev[1]

tbl_ibnyr_random <- claims_by_first_report(
    tbl_policy
  , frequency = lstFreq
  , payment_severity = lstSev
  , lags = 1:4)

## -----------------------------------------------------------------------------
tbl_ibnyr_random %>% 
  filter(policyholder_id == 1) %>% 
  filter(policy_effective_date == min(policy_effective_date)) %>% 
  kable()

## -----------------------------------------------------------------------------
fixedLinks <- list(2, 1.5, 1.25)

## -----------------------------------------------------------------------------
tbl_claims_fixed <- claims_by_link_ratio(
  tbl_ibnyr_fixed,
  links = fixedLinks,
  lags = 1:4)

## -----------------------------------------------------------------------------
tbl_claims_fixed %>% 
  filter(policyholder_id == 1) %>% 
  filter(
    policy_effective_date == min(policy_effective_date), 
    claim_id %in% c(1, 41)) %>%
  arrange(claim_id, lag) %>% 
  kable()

## -----------------------------------------------------------------------------
normalLinks <- list(  
  Normal(2, 1),
  Normal(1.5, .5),
  Normal(1.25, .5))

tbl_claims_random <- claims_by_link_ratio(
  tbl_ibnyr_random, 
  links = normalLinks, 
  lags = 1:4)

## -----------------------------------------------------------------------------
tbl_claims_random %>% 
  filter(policyholder_id == 1) %>% 
  filter(
    policy_effective_date == min(policy_effective_date), 
    claim_id %in% c(1, 41)) %>%
  arrange(claim_id, lag) %>% 
  kable()

