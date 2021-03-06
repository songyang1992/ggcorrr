#' Significance marks based on center.
#'
#'
#' @eval rd_aesthetics("geom", "mark")
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used, the default value is 2.
#' @param nsmall the minimum number of digits to the right of the decimal
#'     point in formatting real/complex numbers in non-scientific formats,
#'     the default value is 2.
#' @param sig_level significance level，the defaults values is `c(0.05, 0.01, 0.001)`.
#' @param mark significance mark，the defaults values is `c("*", "**", "***")`.
#' @param sig_thres if not NULL, just when `p` is not larger than `sig_thres` will be ploted.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @rdname geom_mark
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 GeomText
#' @importFrom ggplot2 draw_key_text
geom_mark <- function(mapping = NULL, data = NULL,
                     stat = "identity", position = "identity",
                     ...,
                     nudge_x = 0,
                     nudge_y = 0,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMark,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_mark
#' @format NULL
#' @usage NULL
#' @export
GeomMark <- ggproto("GeomMark", GeomText,
                   required_aes = c("x", "y", "p"),

                   default_aes = aes(
                     r = NULL, colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                     vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                   ),

                   draw_panel = function(data, panel_params, coord, digits = 2,
                                         nsmall = 2, sig_level = c(0.05, 0.01, 0.001),
                                         mark = c("*", "**", "***"), sig_thres = NULL, na.rm = FALSE) {
                     stopifnot(length(sig_level) == length(mark))
                     if(!is.null(sig_thres))
                       data <- subset(data, p <= sig_thres, drop = FALSE)
                     if(!is.null(data$r)) {
                       num <- format_number(data$r, digits, nsmall)
                       star <- sig_mark(data$p, sig_level, mark)
                       data$label <- paste0(num, star)
                     } else {
                       star <- sig_mark(data$p, sig_level, mark)
                       data$label <- star
                     }
                     GeomText$draw_panel(data, panel_params, coord)
                   },

                   draw_key = draw_key_text
)

#' @noRd
sig_mark <- function(p,
                     sig_level = c(0.05, 0.01, 0.001),
                     mark = c("*", "**", "***")) {
  if(!is.numeric(p))
    p <- as.numeric(p)
  ord <- order(sig_level)
  sig_level <- sig_level[ord]
  mark <- mark[ord]
  brks <- c(0, sig_level, 1)
  lbs <- c(mark, "")
  pp <- cut(p, breaks = brks, labels = lbs, include.lowest = FALSE, right = TRUE)
  ifelse(p == 0, "***", as.character(pp))
}

