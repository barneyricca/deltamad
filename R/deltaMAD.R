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
#' delta.mad
#'
#' @param <s1> numeric vector
#' @param <s2> numeric vector
#' @keywords nonparametric, effect-size
#' @description This function computes a nonparametric estimate ("delta_MAD")
#' of effect size.
#' @export
#' @author Bernard P. Ricca bricca@uccs.edu
#' @references Ricca, B. & Blaine, E. B. (2022). Notes on nonparametric measures
#' of effect size. Journal of Experimental Education, 90(1), 249-258.
#' DOI: 10.1080/00220973.2020.1781752
#' @examples
#' set.seed(42)
#' sample(1:10, size = 20, replace = TRUE) -> s1
#' sample(5:13, size = 18, replace = TRUE) -> s2
#' delta.mad(s1, s2)

delta.mad <- function(s1,         # First data vector
                      s2) {       # Second data vector
  # Check for zero-length inputs
  if(length(s1) == 0 |            # Is s1 zero-length?
     length(s2) == 0) {           # Is s2 zero-length?
    cat("Zero-length input\n")    # If either input is zero-length, say so
    return(NULL)                  #
  }

  # Check for numeric inputs
  if(mode(s1) != "numeric" |      # Is s1 numeric?
     mode(s2) != "numeric") {     # Is s2 numeric?
    cat("Numeric input required\n")     # If either input isn't numeric...
    return(NULL)                  #
  }

  return(                         # Calculation of delta_MAD
    ifelse(abs(median(s1) - median(s2)) < 1e-12,   # Arbitrary cutoff
           0,
           abs(median(s1) - median(s2)) /
             (((length(s1) - 1) * mad(s1) - (length(s2) - 1) * mad(s2)) /
                (length(s1) + length(s2) - 2))))
}
