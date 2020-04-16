#' @title Computes the detection efficiency of antennas or arrays
#'
#' @description Function that computes the detection efficiency of antennas or
#'   arrays based on the array configuration and assumptions of the
#'   direction of fish movement (up, down or resident). Data can be summarized
#'   by year, month, week, day or hour.
#' @param data telemetry dataset created using \code{\link{old_pit}},
#'   \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize by year, month, week, day or hour (optional)
#' @param by_array summarize by array (TRUE) or antenna (FALSE)
#' @param array_sequence vector of array names in order from downstream to
#'   upstream
#' @param direction user-specified direction of fish movement ("up", "down", or "resident"; required)
#' @param start_date start date of period of interest, default is first date in
#'   dataset
#' @param end_date end date of period of interest, default is last date in
#'   dataset
#' @return Data frame summarizing the detection efficiency of antennas or
#'   arrays.
#' @details Function computes detection efficiency for readers or arrays. If
#'   calculating detection efficiency for readers, antennas for each reader must be
#'   numbered in consecutive order from \emph{downstream} to \emph{upstream} (see Examples).
#'   If antennas were not numbered correctly in the field, users can apply the
#'   \code{array_config} function to restructure the configuration of antennas.
#'
#'   Users can also summarize detection efficiency by array through the argument
#'   \code{by_array}. Note that individual antennas do not need to be numbered in
#'   consecutive order if detection efficiency is being summarized by array.
#'   If \code{by_array = TRUE}, the user must list the array names in order from \emph{downstream}
#'   to \emph{upstream} using the \code{array_sequence} argument.
#'
#'   The argument \code{direction} is required and must either be "up", "down", or "resident".
#'   Up and down movements are relative to the direction of flow (i.e., up would be against the
#'   flow of the river), while resident movements occur in both directions. Arguments
#'   \code{start_date} and \code{end_date}, if specified, must be entered as yyyy-mm-dd hh:mm:ss.
#'   Default for the resolution argument will summarize detection efficiency
#'   over the entire dataset. Users can apply the \code{det_eff} function to the original dataset created by
#'   the \code{\link{old_pit}} or \code{\link{new_pit}} function, or use an updated
#'   dataset created by the \code{\link{array_config}} function.
#' @examples
#' # Load test dataset containing detections from a multi reader with two antennas
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' # Summarize by individual antenna and by month for fish assumed to be moving upstream
#' det_eff(data = oregon_rfid, resolution = "month", by_array = FALSE, direction = "up")
#'
#' # Summarize by individual antenna and by week for fish assumed to be moving upstream with a start date of 2016-10-11 08:45:00
#' det_eff(data = oregon_rfid, resolution = "week", by_array = FALSE, direction = "up", start_date = "2016-10-11 08:45:00")
#'
#' # Summarize by individual antenna and by day for fish assumed to be moving downstream
#' det_eff(data = oregon_rfid, resolution = "day", by_array = FALSE, direction = "down")
#'
#' # Summarize by individual antenna and by month for fish assumed to be resident
#' det_eff(data = oregon_rfid, resolution = "month", by_array = FALSE, direction = "resident")
#'
#' # Example study:
#' # Studying an upstream migration in a river that contains two, two-antenna arrays.
#' # The most downstream array is array_one, followed by array_two upstream.
#' # Summarize by array and by month for fish assumed to be moving upstream
#' det_eff(data = multi_array, resolution = "month", by_array = TRUE, array_sequence = c("array_one", "array_two"), direction = "up")
#' @export

det_eff <- function(data,
                    resolution = NULL,
                    by_array = FALSE,
                    array_sequence = NULL,
                    direction,
                    start_date = NULL,
                    end_date = NULL) {
  
  
  if (is.null(start_date)) start_date <- min(data$date_time) else start_date <- lubridate::ymd_hms(start_date, tz = data$time_zone[1])
  if (is.null(end_date)) end_date <- max(data$date_time) else end_date <- lubridate::ymd_hms(end_date, tz = data$time_zone[1])
  
  # Filter data and add temporal columns
  rg <- data %>% 
    dplyr::filter(date_time >= start_date  & date_time <= end_date) %>% 
    dplyr::mutate(year = lubridate::year(date_time)) %>%
    dplyr::mutate(month = lubridate::month(date_time)) %>%
    dplyr::mutate(week = lubridate::week(date_time)) %>%
    dplyr::mutate(day = lubridate::day(date_time)) %>%
    dplyr::mutate(hour = lubridate::hour(date_time))
  
  # Antenna Detection Efficiency --------------------
  if (!isTRUE(by_array)) { # by_array = FALSE
    
    # If the array column doesn't exist, create it by duplicating the reader column
    if (!"array" %in% names(rg)) rg$array <- rg$reader
    
    # A user might run this function with antennas set to NA because they came
    # from single data NAs are ok if by_array == TRUE because you specify the
    # array sequence (but by creating an array column you create antenna numbers)
    if (sum(is.na(rg$antenna) > 0))
      print("Warning: Detection efficiency will not be calculated for readers with no antenna number. Number antennas using array_config()")
    
    # Resolution NULL
    if (is.null(resolution)) {
      
      if (direction == "up") {
        det <- rg %>% 
          # Group by array and antenna
          dplyr::group_by(array, antenna) %>% 
          # Apply custom function
          dplyr::group_modify(~up_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(array, antenna) %>% 
          dplyr::group_modify(~down_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(array, antenna) %>% 
          dplyr::group_modify(~resident_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      det_clean <- det %>%
        dplyr::select(array, antenna, det_eff, no_unique_tag, no_x_antenna_tag,
                      no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags) %>%
        dplyr::filter(antenna != "NA") # Filter rows without antenna values (single arrays not ID'd as arrays)
      return(det_clean)
    }
    
    # Resolution Year
    if (resolution == "year") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, array, antenna) %>% 
          dplyr::group_modify(~up_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, array, antenna) %>% 
          dplyr::group_modify(~down_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, array, antenna) %>% 
          dplyr::group_modify(~resident_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      # Add a date column with the first day of the first month of year and rename columns
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>%
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, Date, antenna, det_eff, no_unique_tag, no_x_antenna_tag,
                      no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags) %>%
        dplyr::filter(antenna != "NA")
      return(det_clean)
    }
    
    # Resolution Month
    if (resolution == "month") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, array, antenna) %>% 
          dplyr::group_modify(~up_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, array, antenna) %>% 
          dplyr::group_modify(~down_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, array, antenna) %>% 
          dplyr::group_modify(~resident_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      # Add a date column with the first day of the month and the first hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, month, 1))) %>%
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, Date, antenna, det_eff, no_unique_tag, no_x_antenna_tag,
                      no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags) %>%
        dplyr::filter(antenna != "NA")
      return(det_clean)
    }
    
    # Resolution Week
    if (resolution == "week") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, array, antenna) %>% 
          dplyr::group_modify(~up_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, array, antenna) %>% 
          dplyr::group_modify(~down_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, array, antenna) %>% 
          dplyr::group_modify(~resident_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      # Add a date column with the first day of the week and the first hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>%
        dplyr::mutate(first.day = as.numeric(format(Date, "%w"))) %>% # Determine what day of the week January 1 is for each year
        dplyr::mutate(Date = Date + 7 * week - first.day - 7) %>% # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, week, Date, antenna, det_eff, no_unique_tag,
                      no_x_antenna_tag, no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags) %>%
        dplyr::filter(antenna != "NA")
      
      return(det_clean)
    }
    
    # Resolution Day
    if (resolution == "day") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, array, antenna) %>% 
          dplyr::group_modify(~up_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, array, antenna) %>% 
          dplyr::group_modify(~down_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, array, antenna) %>% 
          dplyr::group_modify(~resident_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      # Add a date column with the day and the first hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, week, day, Date, antenna, det_eff, no_unique_tag,
                      no_x_antenna_tag, no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags) %>%
        dplyr::filter(antenna != "NA")
      
      return(det_clean)
    }
    
    # Resolution Hour
    if (resolution == "hour") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, hour, array, antenna) %>% 
          dplyr::group_modify(~up_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, hour, array, antenna) %>% 
          dplyr::group_modify(~down_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, hour, array, antenna) %>% 
          dplyr::group_modify(~resident_func(.x, all = rg), keep = TRUE) %>% 
          ungroup
      }
      
      # Add a date column down to the hour
      # Add a date column with the day month and hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
        dplyr::mutate(Date = update(Date, hour = hour)) %>%
        dplyr::mutate(Date = lubridate::ymd_hms(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, week, day, hour, Date, antenna, det_eff, no_unique_tag,
                      no_x_antenna_tag, no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags) %>%
        dplyr::filter(antenna != "NA")
      
      return(det_clean)
    }
    
  } # End of antenna section
  
  # Array Detection Efficiency --------------------
  if (isTRUE(by_array)) {
    
    if (is.null(array_sequence)) stop("Error: array_sequence must be provided")
    
    # Subset out any arrays not part of array_sequence and add array number columns
    number_of_arrays <- length(array_sequence)
    numeric_array_names <- seq(from = 1, to = number_of_arrays, by = 1)
    
    rg <- rg %>% 
      dplyr::filter(array %in% array_sequence) %>% 
      dplyr::mutate(array_number = plyr::mapvalues(array, 
                                                   from = array_sequence, 
                                                   to = numeric_array_names)) %>% 
      dplyr::mutate(as.numeric = as.numeric(array_number))
    
    # Resolution Null
    if (is.null(resolution)) {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(array) %>% 
          dplyr::group_modify(~up_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(array) %>% 
          dplyr::group_modify(~down_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(array) %>% 
          dplyr::group_modify(~resident_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      det_clean <- det %>%
        dplyr::select(array, antenna, det_eff, no_unique_tag, no_x_antenna_tag,
                      no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags)
      
      return(det_clean)
    }
    
    # Resolution Year
    if (resolution == "year") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, array) %>% 
          dplyr::group_modify(~up_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, array) %>% 
          dplyr::group_modify(~down_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, array) %>% 
          dplyr::group_modify(~resident_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      # Add a date column with the first day of the first month of year
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>%
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, Date, antenna, det_eff, no_unique_tag, no_x_antenna_tag,
                      no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags)
      
      return(det_clean)
    }
    
    # Resolution Month
    if (resolution == "month") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, array) %>% 
          dplyr::group_modify(~up_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, array) %>% 
          dplyr::group_modify(~down_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, array) %>% 
          dplyr::group_modify(~resident_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      # Add a date column with the first day of the month and the first hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, month, 1))) %>%
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, Date, antenna, det_eff, no_unique_tag, no_x_antenna_tag,
                      no_other_antenna_tag, no_missed_tags) %>%
        dplyr:: rename(detection_efficiency = det_eff,
                       shared_detections = no_unique_tag,
                       detections_on_array = no_x_antenna_tag,
                       detections_not_on_array = no_other_antenna_tag,
                       missed_detections = no_missed_tags)
      
      return(det_clean)
    }
    
    # Resolution Week
    if (resolution == "week") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, array) %>% 
          dplyr::group_modify(~up_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, array) %>% 
          dplyr::group_modify(~down_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, array) %>% 
          dplyr::group_modify(~resident_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      # Add a date column with the first day of the week nd the first hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, 1, 1))) %>%
        dplyr::mutate(first.day = as.numeric(format(Date, "%w"))) %>% # Determine what day of the week January 1 is for each year
        dplyr::mutate(Date = Date + 7 * week - first.day - 7) %>% # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, week, Date, antenna, det_eff, no_unique_tag,
                      no_x_antenna_tag, no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags)
      
      return(det_clean)
    }
    
    # Resolution Day
    if (resolution == "day") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, array) %>% 
          dplyr::group_modify(~up_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, array) %>% 
          dplyr::group_modify(~down_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, array) %>% 
          dplyr::group_modify(~resident_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      # Add a date column with the day and the first hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
        dplyr::mutate(Date = lubridate::ymd(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, week, day, Date, antenna, det_eff, no_unique_tag,
                      no_x_antenna_tag, no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags)
      
      return(det_clean)
    }
    
    # Resolution Hour
    if (resolution == "hour") {
      
      if (direction == "up") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, hour, array) %>% 
          dplyr::group_modify(~up_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "down") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, hour, array) %>% 
          dplyr::group_modify(~down_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      if (direction == "resident") {
        det <- rg %>% 
          dplyr::group_by(year, month, week, day, hour, array) %>% 
          dplyr::group_modify(~resident_func_array(.x, all = rg)) %>% 
          ungroup
      }
      
      # Add a date column down to the hour
      # Add a date column with the day month and hour
      det_clean <- det %>%
        dplyr::mutate(Date = lubridate::ymd(sprintf("%s-%s-%s", year, month, day))) %>%
        dplyr::mutate(Date = update(Date, hour = hour)) %>%
        dplyr::mutate(Date = lubridate::ymd_hms(Date, tz = data$time_zone[1])) %>%
        dplyr::select(array, year, month, week, day, hour, Date, antenna, det_eff, no_unique_tag,
                      no_x_antenna_tag, no_other_antenna_tag, no_missed_tags) %>%
        dplyr::rename(date = Date,
                      detection_efficiency = det_eff,
                      shared_detections = no_unique_tag,
                      detections_on_array = no_x_antenna_tag,
                      detections_not_on_array = no_other_antenna_tag,
                      missed_detections = no_missed_tags)
      
      return(det_clean)
    }
    
  } # End of array portion
  
} # End of main funtion

# Internal Functions --------------------

up_func <- function(x, all) {
  
  # Unique tags above x (the array for which effiency is being calculated)
  other_antenna_tag <- unique(subset(all, antenna > x$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X
  
  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # Unique tags at x
  
  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
  
  no_other_antenna_tag <- length(unique(subset(all, antenna > x$antenna[1], na.rm = TRUE)$tag_code)) # calculate the number of unique tag codes for all antennas UPSTREAM of antenna x
  
  no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas UPSTREAM of antenna x
  
  # Calculate detection efficicency: the number of tags detected at antenna x that were detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
  det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)
  
  no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x
  
  data.frame(det_eff,
             no_unique_tag,
             no_x_antenna_tag,
             no_other_antenna_tag,
             no_missed_tags)
}

down_func <- function(x, all) {
  
  # Unique tags below x (the array for which effiency is being calculated)
  other_antenna_tag <- unique(subset(all, antenna < x$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas downstream of antenna X
  
  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x
  
  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
  
  no_other_antenna_tag <- length(unique(subset(all, antenna < x$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x
  
  no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x
  
  # Calculate detection efficicency: the number of tags detected at antenna x that were detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
  det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)
  
  no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x
  
  data.frame(det_eff,
             no_unique_tag,
             no_x_antenna_tag,
             no_other_antenna_tag,
             no_missed_tags)
}

resident_func <- function(x, all) {
  
  # Unique tags at arrays other than x (the array for which effiency is being calculated)
  other_antenna_tag <- unique(subset(all, antenna != x$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X
  
  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x
  
  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
  
  no_other_antenna_tag <- length(unique(subset(all, antenna != x$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x
  
  no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x
  
  # Calculate detection efficicency: the number of tags detected at antenna x that were detected at antennas other than x divided by the total number of tags detected other than antenna x
  det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)
  
  no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x
  
  data.frame(det_eff,
             no_unique_tag,
             no_x_antenna_tag,
             no_other_antenna_tag,
             no_missed_tags)
}

up_func_array <- function(x, all) {
  
  # Unique tags above x (the array for which effiency is being calculate)
  other_antenna_tag <- unique(subset(all, array_number > x$array_number[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X
  
  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x
  
  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
  
  no_other_antenna_tag <- length(unique(subset(all, array_number > x$array_number[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x
  
  no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x
  
  # Calculate detection efficicency: the number of tags detected at antenna x that were detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
  det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)
  
  no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x
  
  data.frame(antenna = NA,
             det_eff,
             no_unique_tag,
             no_x_antenna_tag,
             no_other_antenna_tag,
             no_missed_tags)
}

down_func_array <- function(x, all) {
  
  # Unique tags below x (the array for which effiency is being calculate)
  other_antenna_tag <- unique(subset(all, array_number < x$array_number[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X
  
  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x
  
  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
  
  no_other_antenna_tag <- length(unique(subset(all, array_number < x$array_number[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x
  
  no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x
  
  #calculate detection efficicency: the number of tags detected at antenna x that were detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
  det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)
  
  no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x
  
  data.frame(antenna = NA,
             det_eff,
             no_unique_tag,
             no_x_antenna_tag,
             no_other_antenna_tag,
             no_missed_tags)
}

resident_func_array <- function(x, all) {
  
  # Unique tags at arrays other than x (the array for which effiency is being calculate)
  other_antenna_tag <- unique(subset(all, array_number != x$array_number[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X
  
  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x
  
  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
  
  no_other_antenna_tag <- length(unique(subset(all, array_number != x$array_number[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x
  
  no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x
  
  # Calculate detection efficicency: the number of tags detected at antenna x that were detected at antennas other than x divided by the total number of tags detected other than antenna x
  det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)
  
  no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x
  
  data.frame(antenna = NA,
             det_eff,
             no_unique_tag,
             no_x_antenna_tag,
             no_other_antenna_tag,
             no_missed_tags)
}
