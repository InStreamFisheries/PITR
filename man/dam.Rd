\name{dam}
\alias{dam}
\docType{data}
\title{Dataset to illustrate the PITR functions}
\description{A dataset that can be used to illustrate the PITR functions, \code{\link{det_eff}}, \code{\link{direction}}, \code{\link{direction_total}} and \code{\link{first_last}}.}
\format{Dataset contains 809 detections on the following 12 variables.

\item{\code{reader}}{a character vector of PIT readers} \cr
\item{\code{antenna}}{a numeric vector of the antenna number} \cr
\item{\code{det_type}}{a character vector of the detection record} \cr
\item{\code{date}}{a character vector of date} \cr
\item{\code{time}}{a character vector of time} \cr
\item{\code{date_time}}{date and time in POSIXct format} \cr
\item{\code{time_zone}}{time zone where data were collected} \cr
\item{\code{dur}}{a character vector of the duration of the detection} \cr
\item{\code{tag_type}}{a character vector of the tag type} \cr
\item{\code{tag_code}}{a character vector of the tag codes} \cr
\item{\code{consec_det}}{a character vector of the number of consecutive detections} \cr
\item{\code{no_empt_scan_prior}}{a character vector of the number of empty scans} \cr

}
\examples{
dam$date_time <- as.POSIXct(dam$date_time, format = "%Y-%m-%d %H:%M:%S")
det_eff(dam, "month", "up")
first_last(dam, “day”, “2015-10-15 08:00:00”)
}
\keyword{datasets}
