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

delta.mad <- function(s1,
                      s2) {
  return(
    ifelse(abs(median(s1) - median(s2)) < 1e-12,
           0,
           abs(median(s1) - median(s2)) /
             (((length(s1) - 1) * mad(s1) - (length(s2) - 1) * mad(s2)) /
                (length(s1) + length(s2) - 2))))
}

# Still to do: Bootstrap se for delta.mad
