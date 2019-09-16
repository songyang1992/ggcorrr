#' ggcorrr themes
#' @description These are ggcorrr themes which control
#' all non-data display.
#' @param legend.position the position of legends ("none", "left", "right",
#' "bottom", "top", or two-element numeric vector).
#' @param ... other params pass to \code{\link[ggplot2]{theme}}.
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @export
#' @examples
#' df <- fortify_corr(mtcars)
#' ggplot(df, aes(x, y, r = r)) + geom_circle2() +
#' coord_fixed() + theme_corr()
theme_corr <- function(
  legend.position = "right",
  ...
)
{
  base_theme <- theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = legend.position,
    ...
  )
  base_theme
}
