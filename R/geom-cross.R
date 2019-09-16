#' Circles based on center and radius
#'
#'
#' @eval rd_aesthetics("geom", "cross")
#' @param r the radius of an outer circle, defualt value is sqrt(2)/2.
#' @param sig_thres significance threshold.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @rdname geom_cross
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomSegment
#' @importFrom ggplot2 draw_key_blank
#' @importFrom grid grobTree


geom_cross <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCross,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_cross
#' @format NULL
#' @usage NULL
#' @export
GeomCross <- ggproto(
  "GeomCross", Geom,
  default_aes = aes(colour = "red", fill = NA,
                    size = 0.5, linetype = 1, alpha = NA),

  required_aes = c("x", "y", "p"),

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre",
                        sig_thres = 0.05, r = 0.6) {
    if (!coord$is_linear()) {
      warning("geom_cross is not implemented for non-linear coordinates",
              call. = FALSE)
    }
    aesthetics <- setdiff(
      names(data), c("x", "y", "p")
    )
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      if(row$p <= sig_thres)
        return(grid::nullGrob())
      if(row$p > sig_thres) {
        cross <- point_to_cross(row$x, row$y, r)
        aes <- new_data_frame(row[aesthetics])[rep(1, 2), ]
        GeomSegment$draw_panel(cbind(cross, aes), panel_params, coord)
      }
    })

    ggplot2:::ggname("cross", do.call("grobTree", polys))
  },

  draw_key = draw_key_blank
)

#' @noRd
point_to_cross <- function(x, y, r = sqrt(2)/2) {
  xx <- c(x - 0.5 * r, x - 0.5 * r)
  xend <- c(x + 0.5 * r, x + 0.5 * r)
  yy <- c(y - 0.5 * r, y + 0.5 * r)
  yend <- c(y + 0.5 * r, y - 0.5 * r)

  new_data_frame(list(
    x = xx,
    y = yy,
    xend = xend,
    yend = yend
  ))
}



