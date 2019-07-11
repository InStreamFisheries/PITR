#' @title Summarizes upstream and downstream movements on arrays
#'
#' @description Function summarizes upstream or downstream movements of each
#'   fish (based on their first and last movements) on each array over a
#'   user-defined period of time. This function can assist in determining
#'   residence time between unique arrays. Data can be summarized by year,
#'   month, week, day or hour.
#' @param data telemetry dataset created using \code{\link{old_pit}},
#'   \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize data by year, month, week, day or hour (optional)
#' @param start_date start date of period of interest, default is first date in
#'   dataset
#' @param end_date end date of period of interest, default is last date in
#'   dataset
#' @return Data frame summarizing upstream and downstream movements on arrays.
#' @details \code{direction_total} summarizes movements by array. Multiple
#'   arrays can be present in \code{data}, and each array can have up to four
#'   antennas that must be ordered sequentially from \emph{downstream} (1) to
#'   \emph{upstream} (4). If no arrays are specified, \code{direction_tota} defaults
#'   the reader names to array names and summarizes by reader/array. Any single
#'   antenna data that have not been renamed using \code{array_config} (i.e.,
#'   antenna number is NA) will not be included in the \code{direction_total}
#'   function.
#'
#'   Users can apply the direction_total function to the original dataset
#'   created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or
#'   use an updated dataset created by the \code{\link{array_config}} function.
#'   Arguments \code{start_date} and \code{end_date}, if specified, must be
#'   entered as yyyy-mm-dd hh:mm:ss. Default for the resolution argument will
#'   summarize upstream and downstream movements over the entire time period
#'   present in the dataset.
#' @examples
#' # Load test dataset containing detections from a multi reader with two antennas
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' # Summarize by month
#' direction_total(data = oregon_rfid, resolution = "month")
#'
#' # Summarize by year with a start date of 2015-11-11 10:30:00
#' direction_total(data = oregon_rfid, resolution = "year", start_date = "2015-11-11 10:30:00")
#' @export

direction_total <- function(data,
                            resolution = NULL,
                            start_date = NULL,
                            end_date = NULL) {

  if (is.null(start_date)) start_date <- min(data$date_time) else start_date <- lubridate::ymd_hms(start_date, tz = data$time_zone[1])
  if (is.null(end_date)) end_date <- max(data$date_time) else end_date <- lubridate::ymd_hms(end_date, tz = data$time_zone[1])

  # Remove single reader rows from data set (created with pit_data function)
  xv <- subset(data, antenna != "NA")

  # Filter data
  rg <- dplyr::filter(xv, date_time >= start_date & date_time <= end_date)

  # Create new temporal columns
  rg <- rg %>%
    dplyr::mutate(year = lubridate::year(date_time)) %>%
    dplyr::mutate(month = lubridate::month(date_time)) %>%
    dplyr::mutate(week = lubridate::week(date_time)) %>%
    dplyr::mutate(day = lubridate::day(date_time)) %>%
    dplyr::mutate(hour = lubridate::hour(date_time))

  # If the array column doesn't exist, create it by duplicating the reader column
  if (!"array" %in% names(rg)) rg$array <- rg$reader

  if (is.null(resolution)) {

    dir <- plyr::ddply(rg, c("array", "tag_code"), function(x) {
      xx <- x[order(x$date_time), ]
      # If the diffference between two consecutive detections is positive then
      # up/down (direction) = up, if it's negative then direction = down, if
      # it's 0 then direction = N.
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N") # Remove rows where direction is N
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]
    dir_t <- plyr::ddply(dir_cs, c("array", "tag_code"), direction_summary)
    return(dir_t)
  }

  else if (resolution == "hour") {
    dir <- plyr::ddply(rg, c("array", "tag_code", "year", "month", "day", "hour"), function(x) {
      xx <- x[order(x$date_time), ]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N")
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]
    dir_t <- plyr::ddply(dir_cs, c("array", "tag_code", "year", "month", "day", "hour"),
                         direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t <- dir_t %>%
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
      dplyr::mutate(date = update(date, hour = hour)) %>%
      dplyr::mutate(date = lubridate::ymd_hms(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, tag_code, year, month, day, hour, date, first_det, first_dir,
                    last_det, last_dir, time_diff_days, time_diff_mins)

    return(dir_t)
  }

  else if (resolution == "day") {

    dir <- plyr::ddply(rg, c("array", "tag_code", "year", "month", "day"), function(x) {
      xx <- x[order(x$date_time), ]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N")
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]
    dir_t <- plyr::ddply(dir_cs, c("array", "tag_code", "year", "month", "day"), direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t <- dir_t %>%
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, tag_code, year, month, day, date, first_det, first_dir, last_det,
                    last_dir, time_diff_days, time_diff_mins)

    return(dir_t)
  }

  else if (resolution == "week") {

    dir <- plyr::ddply(rg, c("array", "tag_code", "year", "month", "week"), function(x) {
      xx <- x[order(x$date_time), ]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N")
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]
    dir_t <- plyr::ddply(dir_cs, c("array", "tag_code", "year", "month", "week"), direction_summary)

    # Add a date column with the first day of the week and the first hour
    dir_t <- dir_t %>%
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>%  # Start at Jan 1
      dplyr::mutate(first.day = as.numeric(format(date, "%w"))) %>%
      dplyr::mutate(date = date + 7 * week - first.day - 7) %>%  # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, tag_code, year, month, week, date, first_det, first_dir, last_det,
                    last_dir, time_diff_days, time_diff_mins)

    return(dir_t)
  }

  else if (resolution == "month") {

    dir <- plyr::ddply(rg, c("array", "tag_code", "year", "month"), function(x) {
      xx <- x[order(x$date_time), ]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N")
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]
    dir_t <- plyr::ddply(dir_cs, c("array", "tag_code", "year", "month"), direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t <- dir_t %>%
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, month, 1))) %>%
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, tag_code, year, month, date, first_det, first_dir, last_det,
                    last_dir, time_diff_days, time_diff_mins)

    return(dir_t)
  }

  else if (resolution == "year") {

    dir <- plyr::ddply(rg, c("array", "tag_code", "year"), function(x) {
      xx <- x[order(x$date_time), ]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N")
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]
    dir_t <- plyr::ddply(dir_cs, c("array", "tag_code", "year"), direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t <- dir_t %>%
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>%
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, tag_code, year, date, first_det, first_dir, last_det, last_dir,
                    time_diff_days, time_diff_mins)

    return(dir_t)
  }
}

direction_summary <- function(x){
  x[order(x$date_time), ]
  first_det <- min(x$date_time)
  first_dir <- dplyr::first(x$direction)
  last_det <- max(x$date_time)
  last_dir <- dplyr::last(x$direction)

  # Calculate time differences b/w first and last detections
  time_diff <- lubridate::interval(first_det, last_det)
  time_diff_days <- round(time_diff / lubridate::ddays(1), 2)
  time_diff_mins <- round(time_diff / lubridate::dminutes(1), 2)
  data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
}
