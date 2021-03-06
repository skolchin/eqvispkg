% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r-capstone.r
\name{eq_create_label}
\alias{eq_create_label}
\title{Make up a label for visualization popup
To be used with eq_map() function to make more informative popup labe;}
\usage{
eq_create_label(data)
}
\arguments{
\item{data}{The NOAA dataset}
}
\description{
Make up a label for visualization popup
To be used with eq_map() function to make more informative popup labe;
}
\examples{
\dontrun{
  data \%>\% mutate(popup_text = eq_create_label(.)) \%>\% eq_map(annot_col = "popup_text")
}

}
