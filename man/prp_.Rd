% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prp_.R
\name{prp_}
\alias{prp_}
\title{My favorite customization of \link[rpart.plot]{prp}}
\usage{
prp_(
  object,
  main = deparse1(object$terms[[2L]]),
  type = 4L,
  extra = 101,
  under = TRUE,
  branch.lwd = 2.5,
  varlen = 0L,
  faclen = 0L,
  ...
)
}
\arguments{
\item{object}{\link[rpart]{rpart} object}

\item{main}{\link[base]{character} scalar}

\item{type, extra, under, branch.lwd, varlen, faclen, ...}{see \link[rpart.plot]{prp}}
}
\value{
Function \link{prp_} does not have a returned value.
}
\description{
..
}
\note{
Function \code{ggdendro::ggdendrogram} will plot \link[rpart]{rpart} object, but not very good.

Another option is \code{?partykit::plot.party}, which seems to requires numeric endpoint and/or branching criterion (?)
}
\references{
\url{http://www.di.fc.ul.pt/~jpn/r/tree/tree.html}

Inspired by function \code{heat.tree()} in \url{http://www.milbo.org/rpart-plot/prp.pdf}
}
