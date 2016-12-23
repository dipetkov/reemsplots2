#' Sample Voronoi diagrams from the posterior of m and q
#'
#' Given a set of EEMS output directories, this function takes random samples from the posterior distribution of the migration rates m and the diversity rates q. Each draw is visualized as a Voronoi diagram with the help of the \code{deldir} package.
#' @param num_draws Number of times to sample from the posterior. The default is 1.
#' @param add_seeds A logical value indicating whether to add the Voronoi centers or not.
#' @inheritParams make_eems_plots
#' @examples
#' \donttest{
#' # Use the provided example or supply the path to your own EEMS run
#' mcmcpath <- system.file("extdata", "EEMS-example", package = "reemsplots2")
#'
#' # Plot a series of Voronoi diagrams for the EEMS model parameters:
#' # the effective migration rates (m) and the effective diversity rates (q).
#' plots <- plot_voronoi_tiles(mcmcpath, longlat = TRUE, num_draws = 2)
#' plots
#' }
#' @seealso \code{\link[deldir]{deldir}}, \code{\link{make_eems_plots}}, \code{\link{plot_population_grid}}, \code{\link{plot_resid_heatmap}}
#' @export

plot_voronoi_tiles <- function(mcmcpath, longlat, num_draws = 1,
                               add_seeds = TRUE, eems_colors = NULL,
                               m_colscale = NULL, q_colscale = NULL) {
  check_mcmcpath_contents(mcmcpath)
  load_required_packages("deldir")
  func_params <- list(add_seeds = add_seeds, eems_colors = eems_colors,
                      m_colscale = m_colscale, q_colscale = q_colscale)
  plot_params <- check_plot_params(func_params)

  plots <- list()
  for (draw in seq(num_draws)) {
    p <- random_voronoi_diagram_(sample(mcmcpath, 1), longlat,
                                 plot_params, num_draws, is_mrates = TRUE)
    plots[[paste0("mtiles", draw)]] <- p
  }
  for (draw in seq(num_draws)) {
    p <- random_voronoi_diagram_(sample(mcmcpath, 1), longlat,
                                 plot_params, num_draws, is_mrates = FALSE)
    plots[[paste0("qtiles", draw)]] <- p
  }
  plots
}

random_voronoi_diagram_ <- function(mcmcpath, longlat, plot_params,
                                    num_draws = 1, is_mrates = TRUE) {
  message("Plotting Voronoi tessellation of estimated effective rates")
  if (is_mrates) {
    colscale <- plot_params$m_colscale
    title <- "log(m)"
  } else {
    colscale <- plot_params$q_colscale
    title <- "log(q)"
  }
  dimns <- read_dimns(mcmcpath, longlat)
  voronoi <- read_voronoi(mcmcpath, longlat, is_mrates)
  # Choose one saved posterior draw at random
  iter <- sample(seq_along(voronoi$tiles), 1)
  message(paste0("Draw ", iter))
  # Jump over stored parameters for draws 1 to (iter - 1)
  skip <- sum(voronoi$tiles[iter:1][-1])
  now_tiles <- voronoi$tiles[iter]
  now_rates <- voronoi$rates[(skip + 1):(skip + now_tiles)]
  now_xseed <- voronoi$xseed[(skip + 1):(skip + now_tiles)]
  now_yseed <- voronoi$yseed[(skip + 1):(skip + now_tiles)]
  outer <- as_data_frame(dimns$outer) %>% setNames(c("x", "y"))
  seeds <- data_frame(x = now_xseed, y = now_yseed)
  poly <- NULL
  if (now_tiles == 1) {
    poly <- data_frame(x = rep(dimns$xlim, each = 2),
                       y = rep(dimns$ylim, times = 2),
                       rate = now_rates)
  } else {
    tile_list <-
      deldir::tile.list(deldir::deldir(now_xseed, now_yseed,
                                       rw = c(dimns$xlim, dimns$ylim)))
    for (t in seq(now_tiles))
      poly <- bind_rows(poly, as_data_frame(tile_list[[t]][c("x", "y")]) %>%
                          mutate(id = t, rate = now_rates[t]))
  }
  limits <- range(poly$rate, colscale, na.rm = TRUE, finite = TRUE)
  p <- ggplot(poly, aes(x = x, y = y)) +
    geom_polygon(aes(fill = rate, group = id), color = "white") +
    geom_path(data = outer) +
    scale_fill_gradientn(colors = plot_params$eems_colors,
                         limits = limits, name = title) +
    theme_void()
  if (plot_params$add_seeds) p <- p + geom_point(data = seeds, shape = 1)
  p
}
