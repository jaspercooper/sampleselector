% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/find_sets.R
\name{find_sets}
\alias{find_sets}
\title{Find the sets within a sample_selection object that meet some criterion.}
\usage{
find_sets(sample_selection_object, criteria)
}
\arguments{
\item{sample_selection_object}{A list of admissable samples, created by a create_set() function (create_set_spatial_buffer() etc.)}

\item{criteria}{A vector of criteria to be met}
}
\value{
A list of admissable sets of units that meet the spatial criterion.
}
\description{
Find the sets within a sample_selection object that meet some criterion.
}

