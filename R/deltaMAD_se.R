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
#' @keywords nonparametric, effect-size, standard error
#' @description This function computes the standard error of a nonparametric
#' estimate ("delta_MAD") of effect size.
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
                         alpha = 0.05,  # Confidence level
                         R = 1000) {    # Number of replications
  # Compute a bootstrapped standard error for delta_MAD
  # Check for zero-length inputs
  if(length(s1) == 0 |
     length(s2) == 0) {
    cat("Zero-length input\n")
    return(NULL)
  }

  # Check for numeric inputs
  if(mode(s1) != "numeric" |
     mode(s2) != "numeric") {
    cat("Numeric input required\n")
    return(NULL)
  }

  replicate()
  sample()
  sample()
  delta.mad(sa1, sa2)

  return(c(quantile(alpha/2),
           quantile(1 - alpha/2)))
}
