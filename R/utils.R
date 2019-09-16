#' @noRd
corr_matrix <- function(x,
                        use = "everything",
                        method = "pearson")
{
  if(!(is.matrix(x) || is.data.frame(x)))
    stop("Need a matrix or data.frame.", call. = FALSE)
  if(is.null(colnames(x)))
    colnames(x) <- paste0("x", 1:ncol(x))
  corr <- cor(x, use = use, method = method)
  corr
}

#' @noRd
corr_to_df <- function(corr)
{
  if(!(is.matrix(corr) || is.data.frame(corr)))
    stop("Need a matrix or data.frame.", call. = FALSE)
  n <- nrow(corr)
  m <- ncol(corr)
  if(n != m)
    stop("The rows and columns of `corr` must be same.", call. = FALSE)
  if(!is.matrix(corr))
    corr <- data.matrix(corr)
  name <- rownames(corr) %||% colnames(corr)
  name <- make.names(name, unique = TRUE)
  r <- as.vector(corr)
  x <- rep(name, each = n)
  y <- rep(name, m)
  data.frame(x = factor(x, levels = name),
             y = factor(y, levels = rev(name)),
             r = r)
}

#' @noRd
cor_test <- function(x,
                     alternative = "two.sided",
                     method = "pearson", ...) {
  x <- as.matrix(x)
  n <- ncol(x)
  p <- low <- upp <- matrix(NA, n, n)
  diag(p) <- 0
  diag(low) <- diag(upp) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {

      tmp <- cor.test(x = x[,i], y = x[,j], ...)
      p[i,j] <- p[j,i] <- tmp$p.value

      # only "pearson" method provides confidence intervals
      if (!is.null(tmp$conf.int)) {
        low[i,j] <- low[j,i] <- tmp$conf.int[1]
        upp[i,j] <- upp[j,i] <- tmp$conf.int[2]
      }
    }
  }

  list(
    p = p,
    low = low,
    upp = upp
  )
}

#' @noRd
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

#' @noRd
format_number <- function(x, digits = 2, nsmall = 2) {
  if(!is.numeric(x))
    stop("`x` must be a numeric vector.", call. = FALSE)
  x <- round(x, digits = digits)
  format(x, nsmall = nsmall)
}
#' @noRd
`%||%` <- function(x, y)
{
  if(is.null(x)) y else x
}

#' @noRd
.default_colors <- c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                     "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                     "#4393C3", "#2166AC", "#053061")
#' @noRd
rd_aesthetics <- function(type, name) {
  obj <- switch(type,
                geom = ggplot2:::check_subclass(name, "Geom", env = globalenv()),
                stat = ggplot2:::check_subclass(name, "Stat", env = globalenv())
  )
  aes <- rd_aesthetics_item(obj)

  c(
    "@section Aesthetics:",
    paste0(
      "\\code{", type, "_", name, "()} ",
      "understands the following aesthetics (required aesthetics are in bold):"
    ),
    "\\itemize{",
    paste0("  \\item ", aes),
    "}",
    "Learn more about setting these aesthetics in \\code{vignette(\"ggplot2-specs\")}."
  )
}

#' @noRd
rd_aesthetics_item <- function(x) {
  req <- x$required_aes
  all <- union(req, sort(x$aesthetics()))

  ifelse(all %in% req,
         paste0("\\strong{\\code{", all, "}}"),
         paste0("\\code{", all, "}")
  )
}


