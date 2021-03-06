% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy2.R
\name{tidy2}
\alias{tidy2}
\title{broom::tidy workarounds and additions.}
\usage{
tidy2(x, ...)
}
\arguments{
\item{x}{An object to be converted into a tidy data.frame}

\item{...}{extra arguments}
}
\value{
All tidying methods return a \code{data.frame} without rownames. The
  structure depends on the method chosen.
}
\description{
Tidy the result of a test into a summary data.frame. The output of tidy is
always a data.frame with disposable row names. It is therefore suited for
further manipulation by packages like dplyr, reshape2, ggplot2 and ggvis.
}
\section{Getting Help}{
 Always look at the \code{broom::tidy} equivalent for
  more help on a function. Almost always a \code{tidy2} method will be nearly
  identical to a \code{broom::tidy} method just with a few things changed.
}

