% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hinteraction.R
\name{get_Interaction_Strengths}
\alias{get_Interaction_Strengths}
\title{Calculate interaction strengths}
\usage{
get_Interaction_Strengths(data, model, any = TRUE, groups = NULL,
  grid_size = 50L, target = NULL, depth = 4L, any_n_times = 5,
  parallel = FALSE)
}
\arguments{
\item{data}{data.frame w/e or w/o target}

\item{model}{model of class mlr}

\item{any}{calculate overall x vs any interaction strengths}

\item{groups}{vector with group letters, for example c("A", "B") for A1, A2,...B1,...}

\item{grid_size}{size of subsampled marginal variable distribution}

\item{target}{target variable, character name}

\item{depth}{n variables with highest overall interaction strengths are used for pairwise interaction strengths}

\item{any_n_times}{mean of any_n_times overall interaction strength}

\item{parallel}{if FALSE no parallelization, otherwise numerical(n) cores are used}
}
\description{
wrapper around \code{\link{any_interaction}} and \code{\link{pairwise_interaction}}.
}
\author{
Maximilian Pichler
}
