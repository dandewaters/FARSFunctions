% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read in Facility Analysis Reporting System data for selected years}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{Integer of the years to be read.}
}
\value{
Returns a list of tibbles of the FARS data for each month of the selected years.
}
\description{
Read in FARS data for specified years.
}
\details{
Throws an error if specified years are invalid.
}
\examples{
\dontrun{fars_read_years(c(2013, 2014, 2015))}

}
\seealso{
\url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
}
