#' Create a new basic plot of a correlation matrix
#' @description `ggcorrr()` initializes a ggcorrr object. It can be used to
#' declare the input data frame for a graphic, specify the set of plot aesthetics and
#' handle coordinate, colours and legend. If `data` is not a ggcorrr_df, will
#' call `fortify_corr()` function to trans data to ggcorrr_df.
#' @param data default dataset to use for plot.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param type character, "full" (default), "upper" or "lower", display full matrix,
#' lower triangular or upper triangular matrix.
#' @param show_diag a logical value indicating whether diag should be ploted.
#' @param panel_backgroud background colour of plotting area.
#' @param grid_colour grid line colour.
#' @param grid_size grid line size.
#' @param grid_linetype grid line type.
#' @param axis_x_position the position of the x axis  ("auto", "bottom", "top").
#' @param axis_y_position the position of the y axis  ("auto", "left", "right").
#' @param colours vector of colours to use for n-colour gradient.
#' @param legend_title a character string or expression indicating a title of guide.
#' @param legend_position the position of legends ("none", "auto", "left", "right", "bottom", "top",
#' or two-element numeric vector).
#' @param legend_breaks a numeric vector of positions.
#' @param legend_labels a character vector giving labels (must be same length as breaks).
#' @param ... pass to `fortify_corr()` function.
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 coord_fixed
#' @export
#' @examples
#' df <- fortify_corr(mtcars)
#' ggcorrr(df, aes(x, y)) # defaults
#' ggcorrr(df, aes(x, y), type = "upper") # upper triangle
#' ggcorrr(df, aes(x, y), type = "lower") # lower triangle
ggcorrr <- function(
  data,
  mapping,
  type = c("full", "lower", "upper"),
  show_diag = TRUE,
  panel_backgroud = NA,
  grid_colour = "grey50",
  grid_size = 0.25,
  grid_linetype = "solid",
  axis_x_position = c("auto", "bottom", "top"),
  axis_y_position = c("auto", "left", "right"),
  colours = NULL,
  legend_title = NULL,
  legend_position = c("auto", "none", "left", "right", "bottom", "top"),
  legend_breaks = NULL,
  legend_labels = NULL,
  ...
)
{
  type <- match.arg(type)
  axis_x_position <- match.arg(axis_x_position)
  axis_y_position <- match.arg(axis_y_position)
  legend_position <- match.arg(legend_position)
  others <- list(...)
  if(!inherits(data, "ggcorrr_df"))
    data <- fortify_corr(data, type = type, ...)
  if(missing(mapping)) {
    if(isTRUE(others[["corr_test"]])) {
      if(! "corr_test_method" %in% names(others)) {
        mapping <- aes(x = x, y = y, r = r, p = p,
                       low = low, upp = upp, fill = r)
      } else {
        mapping <- switch (others[["corr_test_method"]],
          pearson = aes(x = x, y = y, r = r, p = p,
                        low = low, upp = upp, fill = r),
          spearman = aes(x = x, y = y, r = r, p = p,
                         fill = r),
          kendall = aes(x = x, y = y, r = r, p = p,
                        fill = r))
      }
    } else
    mapping <- aes(x = x, y = y, r = r, fill = r)
  }

  if(axis_x_position == "auto")
    axis_x_position <- switch (type,
                               full = "bottom",
                               lower = "bottom",
                               upper = "top")
  if(axis_y_position == "auto")
    axis_y_position <- switch (type,
                               full = "left",
                               lower = "left",
                               upper = "right")
  if(is.null(legend_title)) {
    if("corr_method" %in% names(others)) {
      mm <- others[["corr_method"]]
      legend_title <- switch(mm,
                    pearson = expression(paste("Pearson's ", rho)),
                    spearman = expression(paste("Spearman's ", rho)),
                    kendall = expression(paste("Kendall's ", rho))
                    )
    } else
      legend_title <- expression(paste("Pearson's ", rho))
  }
  if(legend_position == "auto")
    legend_position <- switch (type,
                               full = "right",
                               lower = "left",
                               upper = "right")
  if(is.null(legend_breaks))
    legend_breaks <- seq(-1, 1, length.out = 5)
  if(is.null(legend_labels))
    legend_labels <- legend_breaks
  p <- ggplot(data = data, mapping = mapping) +
    geom_tile(fill = panel_backgroud,
              colour = grid_colour,
              size = grid_size,
              linetype = grid_linetype)
  xy_scale <- list(scale_x_discrete(position = axis_x_position),
                      scale_y_discrete(position = axis_y_position))

  colour_scale <- list(scale_fill_gradient2n(breaks = legend_breaks,
                               labels = legend_labels,
                               expand = TRUE,
                               colours = colours,
                               limits = c(-1, 1)),
                       scale_colour_gradient2n(breaks = legend_breaks,
                               labels = legend_labels,
                               expand = TRUE,
                               colours = colours,
                               limits = c(-1, 1)))
  guide <- guides(
    fill = guide_colourbar(title = legend_title, nbin = 40),
    colour = guide_colourbar(title = legend_title, nbin = 40)
  )

  p + xy_scale + colour_scale + guide +
    theme_corr(legend.position = legend_position) + coord_fixed(expand = FALSE)
}
