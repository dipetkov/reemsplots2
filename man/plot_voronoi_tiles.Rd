% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_voronoi_tiles.R
\name{plot_voronoi_tiles}
\alias{plot_voronoi_tiles}
\title{Sample Voronoi diagrams from the posterior of m and q}
\usage{
plot_voronoi_tiles(mcmcpath, longlat, num_draws = 1, add_seeds = TRUE,
  eems_colors = NULL, m_colscale = NULL, q_colscale = NULL)
}
\arguments{
\item{mcmcpath}{A vector of EEMS output directories, for the same dataset. Warning: There is minimal checking that the directories all correspond to the same dataset.}

\item{longlat}{A logical value indicating whether the coordinates are given as pairs (longitude, latitude) or (latitude, longitude).}

\item{num_draws}{Number of times to sample from the posterior. The default is 1.}

\item{add_seeds}{A logical value indicating whether to add the Voronoi centers or not.}

\item{eems_colors}{The EEMS color scheme as a vector of colors, ordered from low to high. Defaults to a DarkOrange to Blue divergent palette with six orange shades, white in the middle, six blue shades. Acknowledgement: The default color scheme is adapted from the \code{dichromat} package.}

\item{m_colscale}{A fixed range for log10-transformed migration rates. If the estimated rates fall outside the specified range, then the color scale is ignored. The default range is \code{[-2.5, +2.5]}.}

\item{q_colscale}{A fixed range for log10-transformed diversity rates. The default range is \code{-0.1, +0.1}.}
}
\description{
Given a set of EEMS output directories, this function takes random samples from the posterior distribution of the migration rates m and the diversity rates q. Each draw is visualized as a Voronoi diagram with the help of the \code{deldir} package.
}
\examples{
# Use the provided example or supply the path to your own EEMS run
mcmcpath <- system.file("extdata", "EEMS-example", package = "reemsplots2")

# Plot a series of Voronoi diagrams for the EEMS model parameters:
# the effective migration rates (m) and the effective diversity rates (q).
plots <- plot_voronoi_tiles(mcmcpath, longlat = TRUE, num_draws = 2)
plots
}
\seealso{
\code{\link[deldir]{deldir}}, \code{\link{make_eems_plots}}, \code{\link{plot_population_grid}}, \code{\link{plot_resid_heatmap}}
}
