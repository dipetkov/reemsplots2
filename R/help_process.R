
check_mcmcpath_contents <- function(mcmcpath) {
  for (path in mcmcpath) {
    for (file in c("rdistJtDobsJ.txt", "rdistJtDhatJ.txt", "rdistoDemes.txt",
                   "mcmcmtiles.txt", "mcmcmrates.txt", "mcmcxcoord.txt",
                   "mcmcycoord.txt", "mcmcqtiles.txt", "mcmcqrates.txt",
                   "mcmcwcoord.txt", "mcmczcoord.txt", "mcmcpilogl.txt",
                   "outer.txt", "demes.txt", "edges.txt", "ipmap.txt",
                   "eemsrun.txt")) {
      if (!file.exists(file.path(path, file)))
        stop("Each EEMS output folder should include ", file)
    }
  }
}
read_vector <- function(file) {
  stopifnot(file.exists(file))
  scan(file, what = numeric(), quiet = TRUE)
}
read_matrix <- function(file, ncol = 2) {
  stopifnot(file.exists(file))
  matrix(scan(file, what = numeric(), quiet = TRUE),
         ncol = ncol, byrow = TRUE)
}
read_dimns <- function(mcmcpath, longlat, nmrks = 100) {
  outer <- read_matrix(file.path(mcmcpath, "outer.txt"))
  ipmap <- read_vector(file.path(mcmcpath, "ipmap.txt"))
  demes <- read_matrix(file.path(mcmcpath, "demes.txt"))
  edges <- read_matrix(file.path(mcmcpath, "edges.txt"))
  dist_metric <- get_dist_metric(mcmcpath)
  if (!longlat) {
    outer <- outer[, c(2, 1)]
    demes <- demes[, c(2, 1)]
  }
  xlim <- range(outer[, 1])
  ylim <- range(outer[, 2])
  aspect <- (diff(ylim) / diff(xlim)) / cos(mean(ylim) * pi / 180)
  aspect <- abs(aspect)
  if (aspect > 1) {
    nxmrks <- nmrks
    nymrks <- round(nxmrks * aspect)
  } else {
    nymrks <- nmrks
    nxmrks <- round(nymrks / aspect)
  }
  # Construct a rectangular "raster" of equally spaced pixels/marks
  xmrks <- seq(from = xlim[1], to = xlim[2], length = nxmrks)
  ymrks <- seq(from = ylim[1], to = ylim[2], length = nymrks)
  marks <- cbind(rep(xmrks, times = nymrks), rep(ymrks, each = nxmrks))
  # Exclude pixels that fall outside the habitat outline
  outer_poly <-
    sp::SpatialPolygons(list(Polygons(list(Polygon(outer, hole = FALSE)), "1")))
  marks <- sp::SpatialPoints(marks)[outer_poly, ]
  marks <- marks@coords
  outer <- as_data_frame(outer) %>% setNames(c("x", "y"))
  ipmap <- data_frame(id = ipmap) %>% count(id)
  demes <- as_data_frame(demes) %>% setNames(c("x", "y")) %>%
    mutate(id = row_number()) %>%
    left_join(ipmap) %>% arrange(id) %>%
    mutate(n = if_else(is.na(n), 0L, n))
  edges <- bind_cols(demes[edges[, 1], ] %>% select(x, y),
                     demes[edges[, 2], ] %>% select(x, y)) %>%
    setNames(c("x", "y", "xend", "yend"))
  list(marks = marks, nmrks = nrow(marks), xlim = xlim, ylim = ylim,
       outer = outer, demes = demes, edges = edges,
       dist_metric = dist_metric)
}
read_voronoi <- function(mcmcpath, longlat, is_mrates) {
  if (is_mrates) {
    rates <- read_vector(file.path(mcmcpath, "mcmcmrates.txt"))
    tiles <- read_vector(file.path(mcmcpath, "mcmcmtiles.txt"))
    xseed <- read_vector(file.path(mcmcpath, "mcmcxcoord.txt"))
    yseed <- read_vector(file.path(mcmcpath, "mcmcycoord.txt"))
  } else {
    rates <- read_vector(file.path(mcmcpath, "mcmcqrates.txt"))
    tiles <- read_vector(file.path(mcmcpath, "mcmcqtiles.txt"))
    xseed <- read_vector(file.path(mcmcpath, "mcmcwcoord.txt"))
    yseed <- read_vector(file.path(mcmcpath, "mcmczcoord.txt"))
  }
  if (!longlat) {
    tempi <- xseed
    xseed <- yseed
    yseed <- tempi
  }
  list(rates = log10(rates), tiles = tiles, xseed = xseed, yseed = yseed)
}
# Get the distance metric from `eemsrun.txt`; use `euclidean` by default.
get_dist_metric <- function(mcmcpath) {
  dist_metric <- "euclidean"
  lines <- tolower(readLines(file.path(mcmcpath, "eemsrun.txt")))
  for (line in lines) {
    if (grepl("\\s*distance\\s*=\\s*", line))
      dist_metric <- gsub("\\s*distance\\s*=\\s*(\\w+)", "\\1", line)
  }
  if (dist_metric != "euclidean" && dist_metric != "greatcirc")
    stop("eemsrun.txt should specify `euclidean` or `greatcirc` distance.")
  dist_metric
}
default_eems_colors <- function() {
  # Use the default DarkOrange to Blue color scheme, which combines
  # two color schemes from the `dichromat` package. These are based
  # on a collection of color schemes for scientific graphics:
  # See http://geog.uoregon.edu/datagraphics/color_scales.htm
  # To reproduce the default eems colors,
  # let oranges be dichromat::colorschemes$BluetoDarkOrange.12[12:7]
  # and blues be dichromat::colorschemes$BrowntoBlue.12[7:12]
  c("#994000", "#CC5800", "#FF8F33", "#FFAD66", "#FFCA99", "#FFE6CC",
    "#FBFBFB",
    "#CCFDFF", "#99F8FF", "#66F0FF", "#33E4FF", "#00AACC", "#007A99")
}
# Check that a string, or a vector of strings, represents a hex color
is_color <- function(x) {
  if (is.null(x)) return(FALSE)
  sapply(x, function(x) {
    tryCatch(is.matrix(col2rgb(x)), error = function(e) FALSE)
  })
}
theme_void <- function() {
  theme(line = element_blank(), rect = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(hjust = 0),
        legend.text.align = 1)
}
filled_contour_rates <- function(z, dimns) {
  w <- cbind(dimns$marks, z)
  colnames(w) <- c("x", "y", "z")
  ggplot(as_data_frame(w), aes(x = x, y = y)) +
    geom_tile(aes(fill = z)) +
    scale_x_continuous(limits = dimns$xlim) +
    scale_y_continuous(limits = dimns$ylim) +
    coord_quickmap() +
    theme_void() +
    theme(legend.text.align = 1)
}
filled_contour_graph <- function(p, dimns, plot_params) {
  if (plot_params$add_grid) {
    p <- p + geom_segment(data = dimns$edges,
                          aes(x = x, y = y, xend = xend, yend = yend),
                          color = plot_params$col_grid)
  }
  if (plot_params$add_demes) {
    p <- p + geom_point(data = dimns$demes %>% filter(n > 0),
                        aes(x = x, y = y, size = n), shape = 1,
                        color = plot_params$col_demes) +
      scale_size_continuous(guide = FALSE)
  }
  if (plot_params$add_outline) {
    p <- p + geom_path(data = dimns$outer, aes(x = x, y = y),
                       color = plot_params$col_outline)
  }
  p
}
filled_eems_contour <- function(dimns, zmean, plot_params, is_mrates) {
  if (is_mrates) {
    title <- "log(m)"
    colscale <- plot_params$m_colscale
  } else {
    title <- "log(q)"
    colscale <- plot_params$q_colscale
  }
  limits <- range(zmean, colscale, na.rm = TRUE, finite = TRUE)
  p <- filled_contour_rates(zmean, dimns)
  p <- filled_contour_graph(p, dimns, plot_params) +
    scale_fill_gradientn(colors = plot_params$eems_colors,
                         limits = limits, name = title)
  p
}
filled_prob_contour <- function(dimns, probs, plot_params, is_mrates) {
  probs <- (probs + 1) / 2
  probs[probs < 0] <- 0
  probs[probs > 1] <- 1
  if (is_mrates) r <- "m" else r <- "q"
  breaks <- c(1 - plot_params$prob_level, plot_params$prob_level)
  labels <- c(paste0("P{log(", r, ") < 0} = ", plot_params$prob_level),
              paste0("P{log(", r, ") > 0} = ", plot_params$prob_level))
  p <- filled_contour_rates(probs, dimns)
  p <- filled_contour_graph(p, dimns, plot_params) +
    geom_contour(aes(z = z), breaks = breaks, color = "white") +
    scale_fill_gradientn(colors = default_eems_colors(),
                         limits = c(0, 1), name = "",
                         breaks = breaks, labels = labels)
  p
}
decompose_distances <- function(diffs, sizes = NULL) {
  # Diffs can have NAs on the main diagonal; these elements correspond to demes
  # with a single observation. For such deme a, no dissimilarities between
  # two distinct individuals are observed. I approximate diffs(a,a) with the
  # average diffs(b,b) computed across demes b with multiple samples.
  if (!is.null(sizes))
    diag(diffs)[sizes < 2] <- mean(diag(diffs)[sizes >= 2])
  within <- diag(diffs)
  selfsim <- matrix(within, nrow(diffs), ncol(diffs))
  between <- diffs - (selfsim + t(selfsim)) / 2
  between <- between[upper.tri(between, diag = FALSE)]
  list(within = within, between = between)
}
check_plot_params <- function(pars) {

  if (is.logical(pars$add_grid)) pars$add_grid <- pars$add_grid[1]
  else pars$add_grid <- FALSE
  if (is_color(pars$col_grid)) pars$col_grid <- pars$col_grid[1]
  else pars$col_grid <- "#BBBBBB"

  if (is.logical(pars$add_outline)) pars$add_outline <- pars$add_outline[1]
  else pars$add_outline <- FALSE
  if (is_color(pars$col_outline)) pars$col_outline <- pars$col_outline[1]
  else pars$col_outline <- "#EEEEEE"

  if (is.logical(pars$add_demes)) pars$add_demes <- pars$add_demes[1]
  else pars$add_demes <- FALSE
  if (is_color(pars$col_demes)) pars$col_demes <- pars$col_demes[1]
  else pars$col_demes <- "#000000"
  if (is.logical(pars$add_seeds)) pars$add_seeds <- pars$add_seeds[1]
  else pars$add_seeds <- TRUE

  if (is.numeric(pars$m_colscale)) pars$m_colscale <- pars$m_colscale
  else pars$m_colscale <- c(-2.5, 2.5)
  if (is.numeric(pars$q_colscale)) pars$q_colscale <- pars$q_colscale
  else pars$q_colscale <- c(-0.1, 0.1)

  if (length(pars$eems_colors) < 2 || any(!is_color(pars$eems_colors)))
    pars$eems_colors <- default_eems_colors()

  if (is.null(pars$prob_level)) prob_level <- 0.9
  else prob_level <- pars$prob_level
  prob_level <- prob_level[prob_level > 0.5 & prob_level < 1]
  if (length(prob_level) != 1) prob_level <- 0.9
  pars$prob_level <- prob_level
  pars
}
geo_distm <- function(coord, longlat, plot_params) {
  if (!longlat) coord <- coord[, c(2, 1)]
  dist <- sp::spDists(coord, longlat = TRUE)
  dist <- dist[upper.tri(dist, diag = FALSE)]
  dist
}
pairwise_dist <- function(mcmcpath, longlat, plot_params) {
  # List of observed demes, with number of samples taken collected Each row
  # specifies: x coordinate, y coordinate, n samples
  obs_demes <- read_matrix(file.path(mcmcpath[1], "rdistoDemes.txt"), ncol = 3)
  sizes <- obs_demes[, 3]
  npops <- nrow(obs_demes)
  demes <- seq(npops)
  diffs_obs <- matrix(0, npops, npops)
  diffs_hat <- matrix(0, npops, npops)
  for (path in mcmcpath) {
    tempi <- read_matrix(file.path(path, "rdistoDemes.txt"), ncol = 3)
    if (sum(dim(obs_demes) != dim(tempi)) || sum(obs_demes != tempi)) {
      message("EEMS results for at least two different population grids. ",
              "Plot pairwise dissimilarity for each grid separately.")
      return(list(between = data_frame(), within = data_frame(),
                  ibd = data_frame()))
    }
    diffs_obs <- diffs_obs +
      as.matrix(read.table(file.path(path, "rdistJtDobsJ.txt")))
    diffs_hat <- diffs_hat +
      as.matrix(read.table(file.path(path, "rdistJtDhatJ.txt")))
  }
  diffs_obs <- diffs_obs / length(mcmcpath)
  diffs_hat <- diffs_hat / length(mcmcpath)
  alpha <- matrix(demes, nrow = npops, ncol = npops)
  beta <- t(alpha)
  tempi <- matrix(sizes, npops, npops)
  smaller_deme <- pmin(tempi, t(tempi))
  smaller_deme <- smaller_deme[upper.tri(smaller_deme, diag = FALSE)]
  alpha <- alpha[upper.tri(alpha, diag = FALSE)]
  beta <- beta[upper.tri(beta, diag = FALSE)]
  # Under pure isolation by distance, we expect the genetic dissimilarities
  # between demes increase with the geographic distance separating them
  dist <- geo_distm(obs_demes[, 1:2], longlat, plot_params)
  if (sum(sizes > 1) < 2) {
    message("There should be at least two observed demes ",
            "to plot pairwise dissimilarities")
    return(NULL)
  }
  bw_obs <- decompose_distances(diffs_obs, sizes)
  bw_hat <- decompose_distances(diffs_hat)
  b_component <- data_frame(alpha_x = obs_demes[, 1][alpha],
                            alpha_y = obs_demes[, 2][alpha],
                            beta_x = obs_demes[, 1][beta],
                            beta_y = obs_demes[, 2][beta],
                            fitted = bw_hat$between,
                            obsrvd = bw_obs$between,
                            size = smaller_deme)
  w_component <- data_frame(alpha_x = obs_demes[, 1][demes],
                            alpha_y = obs_demes[, 2][demes],
                            fitted = bw_hat$within,
                            obsrvd = bw_obs$within,
                            size = sizes)
  g_component <- data_frame(alpha_x = obs_demes[, 1][alpha],
                            alpha_y = obs_demes[, 2][alpha],
                            beta_x = obs_demes[, 1][beta],
                            beta_y = obs_demes[, 2][beta],
                            fitted = dist,
                            obsrvd = bw_obs$between,
                            size = smaller_deme)
  list(between = b_component, within = w_component, ibd = g_component)
}
load_required_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE))
      stop("The ", package, " package is required. ",
           "Please install it first.")
    else
      message("Loading ", package, ".")
  }
}
