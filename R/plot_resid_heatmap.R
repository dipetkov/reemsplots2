#' Plot a heatmap of the residual pairwise dissimilarities \code{|observed - fitted|}
#'
#' Given a set of EEMS output directories, this function generates a heatmap of the n-by-n matrix of residual dissimilarities between pairs of individuals. The residuals are the differences between the observed and the fitted dissimilarities; the rows and columns, which correspond to individuals, are in the same order as the samples in \code{datapath.coord} and \code{datapath.diffs}.
#' @details
#' Applicable only in the case of SNP data when the observed dissimilarity matrix is computed explicitly.
#' @param datapath The full path and the file name of the input \code{diffs} matrix.
#' @param hm_colors The heatmap color palette as a vector of colors, ordered from low to high. Defaults to a white-to-red palette.
#' @param hm_scale A fixed range for the heatmap colors. The default is NULL, so the color space is the observed range of the residuals.
#' @inheritParams make_eems_plots
#' @examples
#' # Use the provided example or supply the path to your own EEMS run
#' extdata <- system.file("extdata", package = "reemsplots2")
#' datapath <- file.path(extdata, "EEMS-barrier")
#' mcmcpath <- file.path(extdata, "EEMS-barrier")
#'
#' plots <- plot_resid_heatmap(datapath, mcmcpath, hm_colors = c("gray99", "red"))
#' plots
#' @seealso \code{\link{make_eems_plots}}, \code{\link{plot_population_grid}}, \code{\link{plot_voronoi_tiles}}
#' @export

plot_resid_heatmap <- function(datapath, mcmcpath,
                               hm_colors = NULL, hm_scale = NULL) {
  load_required_packages(c("Matrix", "scales"))
  check_mcmcpath_contents(mcmcpath)
  p <- plot_resid_heatmap_(datapath, mcmcpath, hm_colors, hm_scale)
  list(residhm = p)
}

plot_resid_heatmap_ <- function(datapath, mcmcpath,
                                hm_colors, hm_scale) {
  message("Generate heatmap of n-by-n matrix of residuals ",
          "(observed - fitted). See plots$residhm.")
  stopifnot(file.exists(paste0(datapath, ".diffs")))
  diffs <- as.matrix(read.table(paste0(datapath, ".diffs")))
  n <- nrow(diffs)
  m <- length(mcmcpath)
  delta <- matrix(0, n, n)
  for (path in mcmcpath) {
    ipmap <- read_vector(file.path(path, "ipmap.txt"))
    indicator <- as.matrix(Matrix::spMatrix(n, n_distinct(ipmap),
                                            i = seq(n), j = ipmap,
                                            x = rep(1, n)))
    d_hat <- as.matrix(read.table(file.path(path, "rdistJtDhatJ.txt")))
    delta <- delta + indicator %*% d_hat %*% t(indicator)
  }
  resid <- abs(diffs - (delta / m))
  diag(resid) <- NA
  hm_limits <- range(resid, hm_scale, na.rm = TRUE, finite = TRUE)
  colnames(resid) <- seq_len(ncol(resid))
  tiles <- resid %>%
    as.data.frame() %>%
    as_data_frame() %>%
    mutate(row = row_number()) %>%
    gather(col, value, - row) %>%
    mutate(col = as.integer(col))
  ggplot(tiles, aes(x = col, y = row, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(colors = hm_colors, limits = hm_limits,
                         name = "|err|", na.value = "white") +
    coord_equal() +
    theme_void()
}
