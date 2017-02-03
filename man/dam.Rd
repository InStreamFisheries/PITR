\name{dam}
\alias{dam}
\docType{data}
\title{Dataset to illustrate the PITR functions}
\description{A dataset that can be used to illustrate the PITR functions, \code{\link{det_eff}}, \code{\link{direction}}, \code{\link{direction_total}} and \code{\link{first_last}}.}
\format{Dataset contains 809 detections on the following 12 variables.

reader              a character vector of PIT readers
antenna             a numeric vector of the antenna number
det_type            a character vector of the detection record
date				        a character vector of date
time				        a character vector of time
date_time		    	  date and time in POSIXct format
time_zone           time zone where data were collected, default is time zone of user’s computer
dur				          a character vector of the duration of the detection
tag_type			      a character vector of the tag type
tag_code		    	  a character vector of the tag codes
consec_det			    a character vector of the number of consecutive detections
no_empt_scan_prior	a character vector of the number of empty scans

}
\examples{
det_eff(dam, "month", "up")
}
\keyword{datasets}
