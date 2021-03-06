% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimulatePolicies.R
\name{RenewPolicies}
\alias{RenewPolicies}
\title{Simulate policy renewal}
\usage{
RenewPolicies(dfPolicy, Retention)
}
\arguments{
\item{dfPolicy}{Data frame of policy data}

\item{Retention}{Scalar value greater than or equal to zero}
}
\description{
Given a policy data frame, this will construct renewal data frames. The number of policies which
renew is governed by the the \code{Retention} parameter.
}
