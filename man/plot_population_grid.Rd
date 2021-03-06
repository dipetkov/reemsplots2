% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_population_grid.R
\name{plot_population_grid}
\alias{plot_population_grid}
\title{Plot the constructed population grid}
\usage{
plot_population_grid(mcmcpath, longlat, add_demes = FALSE,
  col_demes = "black", col_outline = "black", col_grid = "gray50")
}
\arguments{
\item{mcmcpath}{A vector of EEMS output directories, for the same dataset. Warning: There is minimal checking that the directories all correspond to the same dataset.}

\item{longlat}{A logical value indicating whether the coordinates are given as pairs (longitude, latitude) or (latitude, longitude).}

\item{add_demes}{A logical value indicating whether to add the observed demes or not.}

\item{col_demes}{The color of the demes. Defaults to black.}

\item{col_outline}{The color of the habitat outline. Defaults to white.}

\item{col_grid}{The color of the population grid. Defaults to gray.}
}
\description{
Given an EEMS output directory, this function generates one figure to visualize the EEMS population grid. All edges are shown in the same color to visualize the grid before estimating the migration and diversity rates. This can be helpful if EEMS throws the error "The population grid is not connected."
}
\examples{
# Use the provided example or supply the path to your own EEMS run
mcmcpath <- system.file("extdata", "EEMS-example", package = "reemsplots2")
plots <- plot_population_grid(mcmcpath, longlat = TRUE)
plots

# An interesting example where the habitat has an unusual shape and the grid is unconnected
mcmcpath <- system.file("extdata", "EEMS-popgrid", package = "reemsplots2")
plots <- plot_population_grid(mcmcpath, longlat = FALSE)
plots
}
\seealso{
\code{\link{make_eems_plots}}, \code{\link{plot_resid_heatmap}}, \code{\link{plot_voronoi_tiles}}
}
