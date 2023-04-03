\name{delta.mad.se}
\alias{delta.mad.se}
\title{Bootstrapped se for delta MAD}
\usage{
delta.mad.se(x, y)
}
\description{
Computes a bootstrapped se for delta MAD (a nonparametric estimate of effect size.)
}
\examples{
1:9 -> x
seq(3,10,length.out = 9) -> y
delta.mad.se(x,y)
}
