#' Plot the constructed population grid
#'
#' Given an EEMS output directory, this function generates one figure to visualize the EEMS population grid. All edges are shown in the same color to visualize the grid before estimating the migration and diversity rates. This can be helpful if EEMS throws the error "The population grid is not connected."
#' @inheritParams make_eems_plots
#' @examples
#' # Use the provided example or supply the path to your own EEMS run
#' mcmcpath <- system.file("extdata", "EEMS-example", package = "reemsplots2")
#' plots <- plot_population_grid(mcmcpath, longlat = TRUE)
#' plots
#'
#' # An interesting example where the habitat has an unusual shape and the grid is unconnected
#' mcmcpath <- system.file("extdata", "EEMS-popgrid", package = "reemsplots2")
#' plots <- plot_population_grid(mcmcpath, longlat = FALSE)
#' plots
#' @seealso \code{\link{make_eems_plots}}, \code{\link{plot_resid_heatmap}}, \code{\link{plot_voronoi_tiles}}
#' @export

plot_population_grid <- function(mcmcpath, longlat,
                                 add_demes = FALSE, col_demes = "black",
                                 col_outline = "black", col_grid = "gray50") {

  func_params <- list(add_outline = TRUE, add_grid = TRUE,
                      add_demes = add_demes, col_demes = col_demes,
                      col_grid = col_grid, col_outline = col_outline)
  plot_params <- check_plot_params(func_params)

  dimns <- read_dimns(mcmcpath, longlat)
  p <- ggplot() + coord_quickmap() + theme_void()
  p <- filled_contour_graph(p, dimns, plot_params)
  list(popgrid = p)
}
