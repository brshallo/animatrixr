% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_matrix.R
\name{seq_matrix_lp}
\alias{seq_matrix_lp}
\title{Sequence of Matrices, Linear and Polar}
\usage{
seq_matrix_lp(from, to, ...)
}
\arguments{
\item{from}{Matrix}

\item{to}{Matrix}

\item{...}{Passed into `seq()` function, generally should only input `length.out`.}
}
\value{
List of matrices.
}
\description{
Make sequence of matrices, changing rotation of a unit vector on a polar axis
and remaining component of transformation changes linearly. Both happen
simultaneously so a rotation will only look like a rotation if done on a unit
vector.
}
