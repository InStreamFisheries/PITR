#' @title Computes the detection efficiency of antennas or arrays
#'
#' @description Function that computes the detection efficiency of antennas or arrays based on the array configuration and the user’s assumption of the direction of fish movement (up, down or resident). Data can be summarized by year, month, week, day or hour.
#' @param data telemetry dataset created using \code{\link{old_pit}}, \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize by year, month, week, day or hour (optional)
#' @param by_array summarize by array (TRUE) or antenna (FALSE)
#' @param array_sequence vector of array names in order from downstream to upstream
#' @param direction user-specified direction of fish movement (required)
#' @param start_date start date of period of interest, default is first date in dataset
#' @param end_date end date of period of interest, default is last date in dataset
#' @return Data frame summarizing the detection efficiency of antennas or arrays.
#' @details Users must have individual antennas across arrays numbered in consecutive order from downstream to upstream to correctly compute detection efficiency by antenna (see Examples). If antennas were not numbered correctly in the field, users can apply the \code{array_config} function to restructure the configuration of antennas. Users can summarize detection efficiency by array through the argument by_array. Note that individual antennas do not need to be numbered in consecutive order if detection efficiency is being summarized by array. If by_array = TRUE, the user must list the array names in order from downstream to upstream using the \code{array_sequence} argument. Users can apply the \code{det_eff} function to the original dataset created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or use an updated dataset created by the \code{\link{array_config}} function. Arguments \code{start_date} and \code{end_date}, if specified, must be entered as yyyy-mm-dd hh:mm:ss. Default for the resolution argument will summarize detection efficiency over the entire time period present in the dataset.
#' @examples
#' #load test dataset containing detections from a multi reader with two antennas
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' #summarize by individual antenna and by month for fish assumed to be moving upstream
#' det_eff(data = oregon_rfid, resolution = "month", by_array = FALSE, direction = "up")
#'
#' #summarize by individual antenna and by week for fish assumed to be moving upstream with a start date of 2016-10-11 08:45:00
#' det_eff(data = oregon_rfid, resolution = "week", by_array = FALSE, direction = "up", start_date = "2016-10-11 08:45:00")
#'
#' #summarize by individual antenna and by day for fish assumed to be moving downstream
#' det_eff(data = oregon_rfid, resolution = "day", by_array = FALSE, direction = "down")
#'
#' #summarize by individual antenna and by month for fish assumed to be resident
#' det_eff(data = oregon_rfid, resolution = "month", by_array = FALSE, direction = "resident")
#'
#' #example study
#' #studying an upstream migration in a river that contains two, two-antenna arrays
#' #user must ensure that the downstream array (array_one) contains two antennas numbered 1 and 2, whereby antenna 1 is the most downstream antenna in that array
#' #next, the upstream array (array_two) must contain two antennas numbered 3 and 4, whereby antenna 3 is the most downstream antenna in that array
#' #overall, the antennas from the two arrays are numbered from 1 (downstream) to 4 (upstream) and the detection efficiency for upstream migrating fish can be correctly computed by antenna or array
#'
#' #summarize by array and by month for fish assumed to be moving upstream
#' det_eff(data = multi_array, resolution = "month", by_array = TRUE, array_sequence = c("array_one", "array_two"), direction = "up")
#' @export

det_eff <- function(data,
                         resolution = NULL,
                         by_array = FALSE,
                         array_sequence = NULL,
                         direction,
                         start_date = NULL,
                         end_date = NULL) {

  if (is.null(start_date)) start_date <- min(data$date_time) else start_date <- ymd_hms(start_date, tz = data$time_zone[1])
  if (is.null(end_date)) end_date <- max(data$date_time) else end_date <- ymd_hms(end_date, tz = data$time_zone[1])

  #Filter data
  rg <- dplyr::filter(data, date_time >= start_date  & date_time <= end_date)

  #create new temporal columns
  rg <- rg %>%
    mutate(year = year(date_time)) %>%
    mutate(month = month(date_time)) %>%
    mutate(week = week(date_time)) %>%
    mutate(day = day(date_time)) %>%
    mutate(hour = hour(date_time))

  ###################################################
  ######### Antenna Detection Efficiency ############
  ###################################################

  if (!isTRUE(by_array)) {

    # If the array column doesn't exist, create it by duplicating the reader column
    if(!"array" %in% names(rg)) rg$array <- rg$reader

    ### RESOLUTION = NULL ###
    if (is.null(resolution)) {

      if (direction == "up") {
        det <- ddply(rg, c("array", "antenna"), up_func)
      }

      if (direction == "down") {
        det <- ddply(rg, c("array", "antenna"), down_func)
      }

      if (direction == "resident") {
        nested <- ddply(rg, c("array", "antenna"), resident_func)
      }

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[, c("array","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      # Name columns for output
      names(det_clean) <- c("array", "antenna", "detection_efficiency", "shared_detections", "detections_on_array", "detections_not_on_array", "missed_detections")

      # Filter rows without antenna values (single arrays not ID'd as arrays)
      det_clean <- dplyr::filter(det_clean, antenna != "NA")
      return(det_clean)
    }

    ### YEAR ###

    if (resolution == "year") {

      if (direction == "up") {
        det <- ddply(rg, c("year"), function(x) {
          nested <- ddply(x, c("array", "antenna"), up_func)
        })
      }

      if (direction == "down") {
        det <- ddply(rg, c("year"), function(x) {
          nested <- ddply(x, c("array", "antenna"), down_func)
        })
      }

      if (direction == "resident") {
        det <- ddply(rg, c("year"), function(x) {
          nested <- ddply(x, c("array", "antenna"), resident_func)
        })
      }

      # Add a date column with the first day of the first month of year
      det$Date <- ymd(sprintf("%s-%s-%s", det$year,1,1))
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      names(det_clean) <- c("array", "year", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      #Filter rows without antenna values (single arrays not ID'd as arrays)
      det_clean <- dplyr::filter(det_clean, antenna != "NA")
      return(det_clean)

    }

    ### MONTH ###

    if (resolution == "month") {

      if (direction == "up") {
        det <- ddply(rg, c("year", "month"), function(x) {
          nested <- ddply(x, c("array", "antenna"), up_func)
        })
      }

      if (direction == "down") {
        det <- ddply(rg, c("year", "month"), function(x) {
          nested <- ddply(x, c("array", "antenna"), down_func)
        })
      }

      if (direction == "resident") {
        det <- ddply(rg, c("year", "month"), function(x) {
          nested <- ddply(x, c("array", "antenna"), resident_func)
        })
      }

      # Add a date column with the first day of the month and the first hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, det$month,1))
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      det_clean <- det[,c("array","year","month","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      names(det_clean) <- c("array", "year", "month", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      #Filter rows without antenna values (single arrays not ID'd as arrays)
      det_clean <- dplyr::filter(det_clean, antenna != "NA")

      return(det_clean)

    }

    ### WEEK ###


    if (resolution == "week") {

      if (direction == "up") {

        det <- ddply(rg, c("year", "month", "week"), function(x) {
          nested <- ddply(x, c("array", "antenna"), up_func)
        })
      }

      if (direction == "down") {
        det <- ddply(rg, c("year", "month", "week"), function(x) {
          nested <- ddply(x, c("array", "antenna"), down_func)
        })
      }

      if (direction == "resident") {
        det <- ddply(rg, c("year", "month", "week"), function(x) {
          nested <- ddply(x, c("array", "antenna"), resident_func)
        })
      }

      # Add a date column with the first day of the week and the first hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, 1, 1)) # Start at Jan 1
      # Determine what day of the week January 1 is for each year
      det$first.day <- as.numeric(format(det$Date, "%w"))
      det$Date <- det$Date + 7*det$week - det$first.day - 7 # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","month","week","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      names(det_clean) <- c("array", "year", "month", "week", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      #Filter rows without antenna values (single arrays not ID'd as arrays)
      det_clean <- dplyr::filter(det_clean, antenna != "NA")
      return(det_clean)

    }

    ### DAY ###

    if (resolution == "day") {

      if (direction == "up") {
        det <- ddply(rg, c("year", "month", "week", "day"), function(x) {
          nested <- ddply(x, c("array", "antenna"), up_func)
        })
      }

      if (direction == "down") {
        det <- ddply(rg, c("year", "month", "week", "day"), function(x) {
          nested <- ddply(x, c("array", "antenna"), down_func)
        })
      }

      if (direction == "resident") {
        det <- ddply(rg, c("year", "month", "week", "day"), function(x) {
          nested <- ddply(x, c("array", "antenna"), resident_func)
        })
      }

      # Add a date column with the day and the first hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, det$month, det$day))
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","month","week","day","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      names(det_clean) <- c("array", "year", "month", "week", "day", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      # Filter rows without antenna values (single arrays not ID'd as arrays)
      det_clean <- dplyr::filter(det_clean, antenna != "NA")
      return(det_clean)

    }

    ### HOUR ###

    if (resolution == "hour") {

      if (direction == "up") {

        det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {
          nested <- ddply(x, c("array", "antenna"), up_func)
        })
      }

      if (direction == "down") {
        det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {
          nested <- ddply(x, c("array", "antenna"), down_func)
        })
      }

      if (direction == "resident") {
        det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {
          nested <- ddply(x, c("array", "antenna"), resident_func)
        })
      }


      # Add a date column down to the hour
      # Add a date column with the day month and hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, det$month, det$day))
      det$Date <- update(det$Date, hour = det$hour)
      det$Date <- ymd_hms(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","month","week","day","hour","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      names(det_clean) <- c("array", "year", "month", "week", "day", "hour", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      #Filter rows without antenna values (single arrays not ID'd as arrays)
      det_clean <- dplyr::filter(det_clean, antenna != "NA")
      return(det_clean)

    }

  } # End of antenna section


  #################################################
  ######### Array Detection Efficiency ############
  #################################################

  if (isTRUE(by_array)) {

    if (is.null(array_sequence)) stop("Error: array_sequence must be provided")

    # Subset out any arrays not part of array_sequence
    rg <- subset(rg, array %in% array_sequence)

    number_of_arrays <- length(array_sequence)
    numeric_array_names <- seq(from = 1, to = number_of_arrays, by = 1)
    rg$array_number <- plyr::mapvalues(rg$array, from = array_sequence, to = numeric_array_names) # Note that you can load the plyr function without loading plyr as long as dplyr is loaded.
    rg$as.numeric <- as.numeric(rg$array_number)

    ### RESOLUTION = NULL ###
    if (is.null(resolution)) {

      if (direction == "up") {
        det <- ddply(rg, c("array"), up_func_array)
      }

      if (direction == "down") {
        det <- ddply(rg, c("array"), down_func_array)
      }

      if (direction == "resident") {
        det <- ddply(rg, c("array"), resident_func_array)
      }

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      # Name columns for output
      names(det_clean) <- c("array", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")
      return(det_clean)
    }


    ### YEAR ###

    if (resolution == "year") {

      if (direction == "up") {
        det <- ddply(rg, c("year"), function(x) {
          nested <- ddply(x, c("array"), up_func_array)
        })
      }

      if (direction == "down") {
        det <- ddply(rg, c("year"), function(x) {
          nested <- ddply(x, c("array"), down_func_array)
        })
      }

      if (direction == "resident") {
        det <- ddply(rg, c("year"), function(x) {
          nested <- ddply(x, c("array"), resident_func_array)
        })
      }

      # Add a date column with the first day of the first month of year
      det$Date <- ymd(sprintf("%s-%s-%s", det$year,1,1))
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      names(det_clean) <- c("array", "year", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      return(det_clean)

    }

    ### MONTH ###

    if (resolution == "month") {

      if (direction == "up") {
        det <- ddply(rg, c("year", "month"), function(x) {
          nested <- ddply(x, c("array"), up_func_array)
        })
      }

      if (direction == "down") {
        det <- ddply(rg, c("year", "month"), function(x) {
          nested <- ddply(x, c("array"), down_func_array)
        })
      }

      if (direction == "resident") {
        det <- ddply(rg, c("year", "month"), function(x) {
          nested <- ddply(x, c("array"), resident_func_array)
        })
      }

      # Add a date column with the first day of the month and the first hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, det$month, 1))
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","month","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      # Name columns for output
      names(det_clean) <- c("array", "year", "month", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      return(det_clean)

    }

    ### WEEK ###

    if (resolution == "week") {

      if (direction == "up") {

        det <- ddply(rg, c("year", "month", "week"), function(x) {
          nested <- ddply(x, c("array"), up_func_array)
        })
      }

      if (direction == "down") {

        det <- ddply(rg, c("year", "month", "week"), function(x) {
          nested <- ddply(x, c("array"), down_func_array)
        })
      }

      if (direction == "resident") {

        det <- ddply(rg, c("year", "month", "week"), function(x) {
          nested <- ddply(x, c("array"), resident_func_array)
        })
      }

      # Add a date column with the first day of the week nd the first hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, 1, 1)) # Start at Jan 1
      # Determine what day of the week January 1 is for each year
      det$first.day <- as.numeric(format(det$Date,"%w"))
      det$Date <- det$Date + 7*det$week - det$first.day - 7 # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","month","week","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      # Name columns for output
      names(det_clean) <- c("array", "year", "month", "week", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      return(det_clean)

    }

    ### DAY ###

    if (resolution == "day") {

      if (direction == "up") {

        det <- ddply(rg, c("year", "month", "week", "day"), function(x) {
          nested <- ddply(x, c("array"), up_func_array)
        })
      }

      if (direction == "down") {

        det <- ddply(rg, c("year", "month", "week", "day"), function(x) {
          nested <- ddply(x, c("array"), down_func_array)
        })
      }

      if (direction == "resident") {

        det <- ddply(rg, c("year", "month", "week", "day"), function(x) {
          nested <- ddply(x, c("array"), resident_func_array)
        })
      }

      # Add a date column with the day and the first hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, det$month, det$day))
      det$Date <- ymd(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","month","week","day","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      # Name columns for output
      names(det_clean) <- c("array", "year", "month", "week", "day", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      return(det_clean)

    }

    ### HOUR ###

    if (resolution == "hour") {

      if (direction == "up") {

        det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {
          nested <- ddply(x, c("array"), up_func_array)
        })
      }

      if (direction == "down") {

        det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {
          nested <- ddply(x, c("array"), down_func_array)
        })
      }

      if (direction == "resident") {

        det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {
          nested <- ddply(x, c("array"), resident_func_array)
        })
      }

      # Add a date column down to the hour
      # Add a date column with the day month and hour
      det$Date <- ymd(sprintf("%s-%s-%s", det$year, det$month, det$day))
      det$Date <- update(det$Date, hour = det$hour)
      det$Date <- ymd_hms(det$Date, tz = data$time_zone[1])

      # Re-order the columns to be more intuitive and fit with Joel's original structure
      det_clean <- det[,c("array","year","month","week","day","hour","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

      names(det_clean) <- c("array", "year", "month", "week", "day", "hour", "date", "antenna", "detection_efficiency", "shared_detections",
                            "detections_on_array", "detections_not_on_array", "missed_detections")

      return(det_clean)

    }

  } # end of array portion

}

up_func <- function(x) {

  # Unique tags above x (the array for which effiency is being calculated)
  other_antenna_tag <- unique(subset(rg, antenna > x$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # Unique tags at x

  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x

  no_other_antenna_tag <- length(unique(subset(rg, antenna > x$antenna[1], na.rm = TRUE)$tag_code)) # calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

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

down_func <- function(x) {

  # Unique tags below x (the array for which effiency is being calculated)
  other_antenna_tag <- unique(subset(rg, antenna < x$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas downstream of antenna X

  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x

  no_other_antenna_tag <- length(unique(subset(rg, antenna < x$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

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

resident_func <- function(x) {

  # Unique tags at arrays other than x (the array for which effiency is being calculated)
  other_antenna_tag <- unique(subset(rg, antenna != x$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x

  no_other_antenna_tag <- length(unique(subset(rg, antenna != x$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

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

up_func_array <- function(x) {

  # Unique tags above x (the array for which effiency is being calculate)
  other_antenna_tag <- unique(subset(rg, array_number > x$array_number[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x

  no_other_antenna_tag <- length(unique(subset(rg, array_number > x$array_number[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

  no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x

  # Calculate detection efficicency: the number of tags detected at antenna x that were detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
  det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

  no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

  data.frame(antenna=NA,
             det_eff,
             no_unique_tag,
             no_x_antenna_tag,
             no_other_antenna_tag,
             no_missed_tags)
}

down_func_array <- function(x) {

  # Unique tags below x (the array for which effiency is being calculate)
  other_antenna_tag <- unique(subset(rg, array_number < x$array_number[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X

  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x

  no_other_antenna_tag <- length(unique(subset(rg, array_number < x$array_number[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

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

resident_func_array <- function(x) {

  # Unique tags at arrays other than x (the array for which effiency is being calculate)
  other_antenna_tag <- unique(subset(rg, array_number != x$array_number[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

  x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

  no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x

  no_other_antenna_tag <- length(unique(subset(rg, array_number != x$array_number[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

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
