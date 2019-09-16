#' Fortify a correalation test data to data.frame
#'
#'@description
#' Rather than generate a correalation matrix to data.frame by hand,
#' I recommend using the this function, becuase this function is
#' specially designed to accommodate other functions in ggcorrr package.
#'
#' @param x a matrix or data.frame.
#' @param type a string of c("full", "upper", "lower") indicating return full matrix, upper or
#'     lower triangle.
#' @param is_corr a logical value indicating whether x is a correalation matrix.
#' @param corr_use an optional character string giving a method for computing
#'     covariances in the presence of missing values. This must be (an abbreviation of)
#'     one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or
#'     "pairwise.complete.obs".
#' @param corr_method a character string indicating which correlation coefficient
#'      (or covariance) is to be computed. One of "pearson" (default), "kendall", or
#'      "spearman": can be abbreviated.
#' @param corr_test a logical value indicating whether should compute correalation test.
#' @param corr_test_alt indicates the alternative hypothesis and must be one of
#'    "two.sided", "greater" or "less". You can specify just the initial letter.
#'    "greater" corresponds to positive association, "less" to negative association.
#' @param corr_test_method a character string indicating which correlation coefficient
#'     is to be used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.
#' @param cluster a logical value indicating whether should reorder x by clustering.
#' @param cluster_order Character, the ordering method for the correlation matrix.
#' \itemize{
#'    \item{\code{"AOE"} for the angular order of the eigenvectors.
#'      It is calculated from the order of the angles, \eqn{a_i}:
#'      \deqn{ a_i = tan (e_{i2} / e_{i1}), if e_{i1} > 0}
#'      \deqn{ a_i = tan (e_{i2} / e_{i1}) + \pi, otherwise.}
#'      where \eqn{e_1} and \eqn{e_2} are the largest two eigenvalues
#'      of matrix \code{corr}.
#'      See Michael Friendly (2002) for details.}
#'    \item{\code{"FPC"} for the first principal component order.}
#'    \item{\code{"hclust"} for hierarchical clustering order.}
#'    \item{\code{"alphabet"} for alphabetical order.}
#' }
#' @param cluster_method Character, the agglomeration method to be used when
#'   \code{order} is \code{hclust}. This should be one of \code{"ward"},
#'   \code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"},
#'   \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.
#'
#' @return return a data.frame.
#'
#' @details
#' cor matrix is computed by \code{\link[stats]{cor}}.
#'
#' correalatin test is computed by \code{\link[stats]{cor.test}},
#' when \code{corr_test_method = "pearson"}, the return value includes
#' the p values, confidence low values and upper values, other methods
#' include only p values.
#'
#' Clustering method borrowed from \code{corrplot}, the detail please see
#' \code{?corrplot:::corrMatOrder}.
#'
#' @seealso Function \code{\link{cor}}, \code{\link{cor.test}}, \code{\link{hclust}}.
#' @keywords correalation, p-value, confidence, significance
#' @examples
#' # generate a ggcorrr_df
#' df <- fortify_corr(mtcars, corr_test = TRUE)
#'
#' @export

fortify_corr <- function(
  x,
  type = c("full", "upper", "lower"),
  show_diag = FALSE,
  is_corr = FALSE,
  corr_use = "everything",
  corr_method = "pearson",
  corr_test = FALSE,
  corr_test_alt = "two.sided",
  corr_test_method = "pearson",
  cluster = FALSE,
  cluster_order = "hclust",
  cluster_method = "complete"
  )
{
  if(!(is.matrix(x) || is.data.frame(x)))
    stop("Need a matrix or data.frame.", call. = FALSE)
  type <- match.arg(type)
  if(inherits(x, "ggcor_df")) {
    df <- x
  } else {
    if(is_corr) {
      if(!is.matrix(x))
        x <- as.matrix(x)
      stopifnot(nrow(x) == ncol(x))

      if(cluster) {
        ord <- corr_order(x, cluster_order, cluster_method)
        x <- x[ord, ord]
      }
      df <- corr_to_df(x)
      class(df) <- c("ggcorrr_df", "data.frame")
    } else {
      colnames(x) <- make.names(colnames(x))
      m <- corr_matrix(x, use = corr_use, method = corr_method)
      if(cluster) {
        ord <- corr_order(m, cluster_order, cluster_method)
        m <- m[ord, ord]
      }
      df <- corr_to_df(m)
      if(corr_test) {
        conf <- cor_test(x, alternative = corr_test_alt, method = corr_test_method)
        p <- conf$p
        low <- conf$low
        upp <- conf$upp
        if(cluster) {
          p <- p[ord, ord]
          low <- low[ord, ord]
          upp <- upp[ord, ord]
        }
        df$p <- as.vector(p)
        if(corr_test_method == "pearson") {
          df$low <- as.vector(low)
          df$upp <- as.vector(upp)
        }
      }
      class(df) <- c("ggcorrr_df", "data.frame")
      }
    }
  switch (type,
    full = df,
    upper = get_upper_data(df, show_diag = show_diag),
    lower = get_lower_data(df, show_diag = show_diag)
  )
}




