% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggdodge.R
\name{ggdodge_custom_dodge_amount}
\alias{ggdodge_custom_dodge_amount}
\title{Custom Dodge Enough}
\usage{
ggdodge_custom_dodge_amount(vec, pointsize)
}
\arguments{
\item{vec}{numeric vector used to determine closeness of points}

\item{pointsize}{the size of the points.}
}
\value{
vec dodged to avoid overlap
}
\description{
Supply a numeric vector and size of points as supplied to geom_point.
This function returns an appropriate x/y axis (whatever you did NOT supply in vec) to avoid points overlapping
}
