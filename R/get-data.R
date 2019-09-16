#' Extracting the data subsets from a ggcorrr_df
#'
#'@description
#' \code{get_lower_data}, \code{get_upper_data} and \code{get_diag_data}
#' extracts left-bottom, right-top, diag rows. \code{get_diag_trip}
#' drop the diag rows. \code{get_sig_data} extracts the rows base on
#' significance test values. Except for the \code{get_sig_data} function,
#' other functions only support ggcorrr_df, more see @seealso \code{\link[ggcorrr]{fortify_corr}}.
#'
#' @param df a ggcorrr_df or data.frame.
#' @param x the name of x coordinates in df.
#' @param y the name of y coordinates in df.
#' @param show_diag should diag rows dropped? Defaults to TRUE.
#' @param p the name of significant varible in df.
#' @param sig_thres Significance level.
#'
#' @return return a modified df.
#' @rdname get_data
#' @export
#' @examples
#' # generate a ggcorrr_df
#' df <- fortify_corr(mtcars, corr_test = TRUE)
#'
#' # get lower data
#' get_lower_data(df)
#'
#' # get upper data
#' get_upper_data(df)
#'
#' # get diag data
#' get_diag_data(df)
#'
#' # drop diag rows
#' get_diag_tri(df)
#'

get_lower_data <- function(df, x = "x", y = "y", show_diag = TRUE)
{
  if(!inherits(df, "ggcorrr_df"))
    stop("Just support 'ggcorrr_df'.", call. = FALSE)
  x <- df[[x]]
  y <- df[[y]]
  idx <- as.integer(x) + as.integer(y)
  n <- length(levels(x))
  if(show_diag) {
    df <- df[idx <= n + 1, , drop = FALSE]
  } else {
    df <- df[idx < n + 1, , drop = FALSE]
  }
  df
}

#' @rdname get_data
#' @export
get_upper_data <- function(df, x = "x", y = "y", show_diag = TRUE)
{
  if(!inherits(df, "ggcorrr_df"))
    stop("Just support 'ggcorrr_df'.", call. = FALSE)
  x <- df[[x]]
  y <- df[[y]]
  idx <- as.integer(x) + as.integer(y)
  n <- length(levels(x))
  if(show_diag) {
    df <- subset(df, idx >= n + 1, drop = FALSE)
  } else {
    df <- subset(df, idx > n + 1, drop = FALSE)
  }
  df
}

#' @rdname get_data
#' @export
get_diag_tri <- function(df, x = "x", y = "y")
{
  if(!inherits(df, "ggcorrr_df"))
    stop("Just support 'ggcorrr_df'.", call. = FALSE)
  x <- df[[x]]
  y <- df[[y]]
  idx <- as.integer(x) + as.integer(y)
  n <- length(levels(x))
  subset(df, idx != n + 1, drop = FALSE)
}

#' @rdname get_data
#' @export
get_diag_data <- function(df, x = "x", y = "y")
{
  if(!inherits(df, "ggcorrr_df"))
    stop("Just support 'ggcorrr_df'.", call. = FALSE)
  x <- df[[x]]
  y <- df[[y]]
  idx <- as.integer(x) + as.integer(y)
  n <- length(levels(x))
  subset(df, idx == n + 1, drop = FALSE)
}

#' @rdname get_data
#' @export
get_sig_data <- function(df, p = "p", sig_thres = 0.05)
{
  if(length(sig_thres) > 1) {
    warning("`sig_level` have multi elements, just use the first")
    sig_thres <- sig_thres[1]
  }
  p <- df[[p]]
  subset(df, p <= sig_thres, drop = FALSE)
}

