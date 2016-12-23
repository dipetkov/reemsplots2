#' reemsplots2
#'
#' This package implements several functions to inspect and visualize the results of analyzing geo-referenced genetic data with EEMS as well as to evaluate the EEMS model fit.
#' \describe{
#' \item{make_eems_plots}{Produce contour plots of the effective migration and diversity
#' rates, as well as scatter plots of observed vs fitted genetic dissimilarities.}
#' \item{plot_population_grid}{Plot the population grid constructed by EEMS.}
#' \item{plot_voronoi_tiles}{Takes random samples from the posterior distribution of the migration and diversity rates to give an idea of the posterior variance.}
#' \item{plot_resid_heatmap}{Computes the matrix of residuals (differences between the observed and fitted dissimilarities) and plots it as a heat map.}
#' }
#' In addition to the help pages, see the README page on \href{https://github.com/dipetkov/eems}{github} for examples.
#' @name reemsplots2
#' @useDynLib reemsplots2
#' @docType package
#' @import RcppEigen dplyr ggplot2 tidyr sp
#' @import graphics grDevices
#' @importFrom Rcpp sourceCpp
#' @importFrom stats lm setNames
#' @importFrom utils read.table
#' @importFrom scales rescale
NULL

globalVariables(c("x", "y", "z", "xend", "yend", "obsrvd", "fitted", "size",
                  "rate", "value", "iter", "logl", "pilogl"))
