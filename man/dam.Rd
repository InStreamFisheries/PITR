\name{dam}
\alias{dam}
\docType{data}
\title{Dataset to illustrate the PITR functions}
\description{A dataset that can be used to illustrate the PITR functions, \code{\link{det_eff}}, \code{\link{direction}}, \code{\link{direction_total}} and \code{\link{first_last}}.}
\format{Dataset contains 809 detections on the following 12 variables.

\code{reader} - a character vector of PIT readers \cr
\code{antenna} - a numeric vector of the antenna number \cr
\code{det_type} - a character vector of the detection record \cr
\code{date} - a character vector of date \cr
\code{time} - a character vector of time \cr
\code{date_time} - date and time in POSIXct format \cr
\code{time_zone} - time zone where data were collected \cr
\code{dur} - a character vector of the duration of the detection \cr
\code{tag_type} - a character vector of the tag type \cr
\code{tag_code} - a character vector of the tag codes \cr
\code{consec_det} - a character vector of the number of consecutive detections \cr
\code{no_empt_scan_prior} - a character vector of the number of empty scans \cr

}
\examples{
det_eff(dam, "month", "up")
first_last(dam, “day”, “2015-10-15 08:00:00”)
}
\keyword{datasets}
