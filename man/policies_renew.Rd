% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimulatePolicies.R
\name{policies_renew}
\alias{policies_renew}
\title{Simulate policy renewal}
\usage{
policies_renew(tbl_policy, retention)
}
\arguments{
\item{tbl_policy}{Data frame of policy data}

\item{retention}{Scalar value greater than or equal to zero}
}
\value{
A data frame, as follows:
\describe{
  \item{policy_effective_date}{Date}
  \item{policy_expiration_date}{Date}
  \item{exposure}{double}
  \item{policyholder_id}{integer}
}
}
\description{
Given a policy data frame, this will construct renewal data frames. The number of policies which
renew is governed by the the \code{Retention} parameter.
}
