#' @title Summarizes first and last detections on individual antennas
#'
#' @description Function summarizes the first and last detections and the
#'   difference in time (in minutes and days) between the first and last
#'   detections for each fish on each antenna over a user-defined period of time.
#'   Data can be summarized by year, month, week, day or hour.
#' @param data telemetry dataset created using \code{\link{old_pit}},
#'   \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize data by year, month, week, day or hour (optional)
#' @param start_date start date of period of interest, default is first date in
#'   dataset
#' @param end_date end date of period of interest, default is last date in
#'   dataset
#' @return Data frame summarizing the first and last detections on individual
#'   antennas.
#' @details \code{first_last} summarizes first and last detections for each tag
#'   on each antenna. In the data output, antennas are organized into arrays. If
#'   no array name has been specified \code{first_last} defaults the reader name
#'   to the array name and summarizes by reader/array. Users can apply the
#'   \code{first_last} function to the original
#'   dataset created by the \code{\link{old_pit}} or \code{\link{new_pit}}
#'   function, or use an updated dataset created by the
#'   \code{\link{array_config}} function. Arguments \code{start_date} and
#'   \code{end_date}, if specified, must be entered as yyyy-mm-dd hh:mm:ss.
#'   Default for the resolution argument will summarize the first and last
#'   detections on antennas over the entire time period present in the dataset.
#' @examples
#' # Load test dataset containing detections from a multi reader with two antennas
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' # Summarize first and last detections by day with a start date of 2015-10-15 08:00:00
#' first_last(data = oregon_rfid, resolution = "day", start_date = "2015-10-15 08:00:00")
#'
#' # Summarize first and last detections by month
#' first_last(data = oregon_rfid, resolution = "month")
#' @export

first_last <- function(data,
                       resolution = NULL,
                       start_date = NULL,
                       end_date = NULL) {

  if (is.null(start_date)) start_date <- min(data$date_time) else start_date <- lubridate::ymd_hms(start_date, tz = data$time_zone[1])
  if (is.null(end_date)) end_date <- max(data$date_time) else end_date <- lubridate::ymd_hms(end_date, tz = data$time_zone[1])

  # Filter data and create temporal columns
  rg <- dplyr::filter(data, date_time >= start_date  & date_time <= end_date) %>% 
    dplyr::mutate(year = lubridate::year(date_time)) %>%
    dplyr::mutate(month = lubridate::month(date_time)) %>%
    dplyr::mutate(week = lubridate::week(date_time)) %>%
    dplyr::mutate(day = lubridate::day(date_time)) %>%
    dplyr::mutate(hour = lubridate::hour(date_time))

  # If the reader column doesn't exist, create it by duplicating the reader column
  if (!"array" %in% names(rg)) rg$array <- rg$reader

  if (is.null(resolution)) {
    
    fl.df <- rg %>% 
      dplyr::group_by(array, antenna, tag_code) %>% 
      group_modify(~diff_func(.x)) %>% 
      ungroup
    
    return(fl.df)
  }

  if (resolution == "year") {
    
    fl.df <- rg %>% 
      dplyr::group_by(array, antenna, tag_code, year) %>% 
      dplyr::group_modify(~diff_func(.x)) %>% 
      ungroup %>% 
      # Add a date column that represents the first moment in time at the level of subsetting
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>% 
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>% 
      dplyr::select(array, antenna, tag_code, year, date, first_det, last_det,
                           time_diff_days, time_diff_mins)
    
    return(fl.df)
  }

  if (resolution == "month") {
    
    fl.df <- rg %>% 
      dplyr::group_by(array, antenna, tag_code, year, month) %>% 
      dplyr::group_modify(~diff_func(.x)) %>% 
      ungroup %>% 
      # Add a date column that represents the first moment in time at the level of subsetting
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, month, 1))) %>%
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, antenna, tag_code, year, month, date, first_det, last_det,
                    time_diff_days, time_diff_mins)
    
    return(fl.df)
  }

  if (resolution == "week") {
    
    fl.df <- rg %>% 
      dplyr::group_by(array, antenna, tag_code, year, month, week) %>% 
      dplyr::group_modify(~diff_func(.x)) %>% 
      ungroup %>% 
      # Add a date column that represents the first moment in time at the level of subsetting
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>%  # Start at Jan 1
      dplyr::mutate(first.day = as.numeric(format(date, "%w"))) %>%
      dplyr::mutate(date = date + 7 * week - first.day - 7) %>%  # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, antenna, tag_code, year, month, week, date, first_det, last_det,
                    time_diff_days, time_diff_mins)

    return(fl.df)
  }

  if (resolution == "day") {

    fl.df <- rg %>% 
      dplyr::group_by(array, antenna, tag_code, year, month, week, day) %>% 
      dplyr::group_modify(~diff_func(.x)) %>% 
      ungroup %>% 
      # Add a date column that represents the first moment in time at the level of subsetting
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
      dplyr::mutate(date = lubridate::ymd(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, antenna, tag_code, year, month, week, day, date, first_det, last_det,
                    time_diff_days, time_diff_mins)
    
    return(fl.df)
  }

  if (resolution == "hour") {
    
    fl.df <- rg %>% 
      dplyr::group_by(array, antenna, tag_code, year, month, week, day, hour) %>% 
      dplyr::group_modify(~diff_func(.x)) %>% 
      ungroup %>% 
      # Add a date column that represents the first moment in time at the level of subsetting
      dplyr::mutate(date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
      dplyr::mutate(date = update(date, hour = hour)) %>%
      dplyr::mutate(date = lubridate::ymd_hms(date, tz = data$time_zone[1])) %>%
      dplyr::select(array, antenna, tag_code, year, month, week, day, hour, date, first_det,
                    last_det, time_diff_days, time_diff_mins)
    
    return(fl.df)
  }
}

diff_func <- function(x) {

  first_det <- min(x$date_time)
  last_det <- max(x$date_time)

  # Calculate time difference in minutes using lubridate
  time_diff_mins <- round((lubridate::interval(first_det,last_det) / lubridate::dminutes()), 2)
  time_diff_days <- round((lubridate::interval(first_det,last_det) / lubridate::ddays()), 2)

  data.frame(first_det,
             last_det,
             time_diff_days,
             time_diff_mins)
}
