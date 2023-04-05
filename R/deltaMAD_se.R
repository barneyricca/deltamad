# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#

#' delta.mad.se
#'
#' @param <s1> numeric vector
#' @param <s2> numeric vector
#' @param <alpha> numeric
#' @param <R> numeric
#' @keywords nonparametric, effect-size, standard error
#' @description This function computes a bootstrap standard error for a
#' nonparametric estimate ("delta_MAD") of effect size.
#' @export
#' @author Bernard P. Ricca bricca@uccs.edu
#' @references Ricca, B. & Blaine, E. B. (2022). Notes on nonparametric measures
#' of effect size. Journal of Experimental Education, 90(1), 249-258.
#' DOI: 10.1080/00220973.2020.1781752
#' @examples
#' set.seed(42)
#' sample(1:10, size = 20, replace = TRUE) -> s1
#' sample(5:13, size = 18, replace = TRUE) -> s2
#' delta.mad.se(s1, s2)
delta.mad.se <- function(s1,
                         s2,
                         alpha = 0.05,   # Confidence level
                         abs_v = FALSE,  # Use absolute value in bootstrapping?
                         R = 5000) {     # Number of replications
  # Computes a bootstrapped standard error for delta_MAD

  1e-12 -> eps                    # For fuzzy comparison to zero; arbitrary
  # Check for zero-length inputs:
  if(length(s1) == 0 |            # Is s1 zero-length?
     length(s2) == 0) {           # Is s2 zero-length?
    cat("Zero-length input\n")    # If either input is zero-length, say so
    return(NULL)                  #
  }

  # Check for numeric inputs:
  if(mode(s1) != "numeric" |      # Is s1 numeric?
     mode(s2) != "numeric") {     # Is s2 numeric?
    cat("Numeric input required\n")     # If either input isn't numeric...
    return(NULL)                  #
  }

  local_mad_T <- function(x1, x2) {
    return(
      ifelse(                # delta_MAD calculation; faster than calling
        abs(median(x1) - median(x2)) < eps,  # 0 isn't always 0
        0,
        (median(x1) - median(x2)) /
          (((length(x1) - 1) * mad(x1) + (length(x2) - 1) * mad(x2)) /
             (length(x1) + length(x2) - 2))))
  }

  local_mad_F <- function(x1, x2) {
    return(
      ifelse(                # delta_MAD calculation; faster than calling
        abs(median(x1) - median(x2)) < eps,  # 0 isn't always 0
        0,
        (median(x1) - median(x2)) /
          (((length(x1) - 1) * mad(x1) + (length(x2) - 1) * mad(x2)) /
             (length(x1) + length(x2) - 2))))
  }

  replicate(R,
            sample(s2,
                   length(s2),
                   replace = TRUE)) ->
    samples

  if(abs_v == TRUE) {
    apply(samples,
          2,
          local_mad_T,
          s1) ->
      es
  } else {
    apply(samples,
          2,
          local_mad_F,
          s1) ->
      es
  }

  return(quantile(x = es,
                  probs = c(alpha/2, (1 - alpha/2))))
}
