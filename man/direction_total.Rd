% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/direction_total.R
\name{direction_total}
\alias{direction_total}
\title{Summarizes upstream and downstream movements on arrays}
\usage{
direction_total(data, resolution = NULL, start_date = NULL,
  end_date = NULL)
}
\arguments{
\item{data}{telemetry dataset created using \code{\link{old_pit}},
\code{\link{new_pit}} or \code{\link{array_config}}}

\item{resolution}{summarize data by year, month, week, day or hour (optional)}

\item{start_date}{start date of period of interest, default is first date in
dataset}

\item{end_date}{end date of period of interest, default is last date in
dataset}
}
\value{
Data frame summarizing upstream and downstream movements on arrays.
}
\description{
Function summarizes upstream or downstream movements of each
  fish (based on their first and last movements) on each array over a
  user-defined period of time. This function can assist in determining
  residence time between unique arrays. Data can be summarized by year,
  month, week, day or hour.
}
\details{
\code{direction_total} summarizes movements by array. Multiple
  arrays can be present in \code{data}, and each array can have up to four
  antennas that must be ordered sequentially from \emph{downstream} (1) to
  \emph{upstream} (4). If no arrays are specified, \code{direction_tota} defaults
  the reader names to array names and summarizes by reader/array. Any single
  antenna data that have not been renamed using \code{array_config} (i.e.,
  antenna number is NA) will not be included in the \code{direction_total}
  function.

  Users can apply the direction_total function to the original dataset
  created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or
  use an updated dataset created by the \code{\link{array_config}} function.
  Arguments \code{start_date} and \code{end_date}, if specified, must be
  entered as yyyy-mm-dd hh:mm:ss. Default for the resolution argument will
  summarize upstream and downstream movements over the entire time period
  present in the dataset.
}
\examples{
# Load test dataset containing detections from a multi reader with two antennas
oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")

# Summarize by month
direction_total(data = oregon_rfid, resolution = "month")

# Summarize by year with a start date of 2015-11-11 10:30:00
direction_total(data = oregon_rfid, resolution = "year", start_date = "2015-11-11 10:30:00")
}
