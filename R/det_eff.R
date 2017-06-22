#' @title Computes the detection efficiency of individual antennas
#'
#' @description Function that computes the detection efficiency of individual antennas or arrays based on the array configuration and the user’s assumption of the direction of fish movement (up, down or resident). Data can be summarized by year, month, week, day or hour.
#' @param data telemetry dataset created using \code{\link{old_pit}}, \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize data by year, month, week, day or hour
#' @param direction user-specified direction of fish movement
#' @param start_date start date of period of interest, default is first date in dataset
#' @param end_date end date of period of interest, default is last date in dataset
#' @return Dataframe summarizing the detection efficiency of individual antennas.
#' @details Users must have individual antennas across arrays numbered in consecutive order from downstream to upstream (or upstream to downstream) to correctly compute detection efficiency. If antennas were not numbered correctly in the field, users can use the \code{array_config} function to restructure the configuration of antennas. Users can apply the \code{det_eff} function to the original dataset created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or use an updated dataset created by the \code{\link{array_config}} function. Arguments \code{start_date} and \code{end_date}, if specified, must be entered as yyyy-mm-dd hh:mm:ss.
#' @examples
#' #example study
#' #studying an upstream migration in a river that contains two, two-antenna arrays
#' #user must ensure that the downstream array contains two antennas numbered 1 and 2, whereby antenna 1 is the most downstream antenna in that array
#' #next, the upstream array must contain two antennas numbered 3 and 4, whereby antenna 3 is the most downstream antenna in that array
#' #overall, the antennas from the two arrays are numbered from 1 (downstream) to 4 (upstream) and the detection efficiency for upstream migrating fish can be correctly computed
#'
#' #load test dataset
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' #compute by month for fish assumed to be moving upstream
#'det_eff(oregon_rfid, "month", "up")
#'
#' #compute by week for fish assumed to be moving upstream with a start date of 2016-10-11 08:45:00
#' det_eff(oregon_rfid, "week", "up", start_date = "2016-10-11 08:45:00")
#'
#' #compute by day for fish assumed to be moving downstream
#' det_eff(oregon_rfid, "day", "down")
#'
#' #compute by month for fish assumed to be resident
#' det_eff(oregon_rfid, "month", "resident")
#' @export

det_eff <- function(data, resolution = NULL, direction, start_date = min(data$date_time), end_date = max(data$date_time)) {
  #Remove single reader rows from data set (created with pit_data function)
  #xv<- subset(dat, antenna != "NA")

  # Need to format the dates away from character so that the filtering will work.
  start_date <- ymd_hms(start_date,tz=data$time_zone[1])
  end_date <- ymd_hms(end_date,tz=data$time_zone[1])

  #Filter data
  rg <- dplyr::filter(data, date_time >= start_date  & date_time <= end_date)

  #create new temporal columns
  rg$year  <- year(rg$date_time)
  rg$month <- month(rg$date_time)
  rg$week  <- week(rg$date_time)
  rg$day   <- day(rg$date_time)
  rg$hour  <- hour(rg$date_time)

  # If the reader column doesn't exist, create it by duplicating the reader column
  if(!"array" %in% names(rg)) rg$array <- rg$reader

  ### RESOLUTION = NULL ###
  if (is.null(resolution)) {

    if (direction == "up") {

      det <- ddply(rg, c("array", "antenna"), function(x) {

        # Unique tags above x (the array for which effiency is being calculate)
        other_antenna_tag <- unique(subset(rg, antenna > x$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

        # Unique tags at x
        x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

        no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
        #print(antenna_tag)

        # Number of unique tags above x
        no_other_antenna_tag <- length(unique(subset(rg, antenna > x$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

        # Number of unique on both antennas
        no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x

        #calculate detection efficicency: the number of tags detected at antenna x that were
        #detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
        det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

        no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

        data.frame(det_eff,
                   no_unique_tag,
                   no_x_antenna_tag,
                   no_other_antenna_tag,
                   no_missed_tags)
      })
    }

    if (direction == "down") {

      det <- ddply(rg, c("array", "antenna"), function(x) {

        # Unique tags below x (the array for which effiency is being calculate)
        other_antenna_tag <- unique(subset(rg, antenna < x$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X

        # Unique tags at x
        x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

        no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
        #print(antenna_tag)

        # Number of unique tags below x
        no_other_antenna_tag <- length(unique(subset(rg, antenna < x$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

        # Number of unique on both antennas
        no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x

        #calculate detection efficicency: the number of tags detected at antenna x that were
        #detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
        det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

        no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

        data.frame(det_eff,
                   no_unique_tag,
                   no_x_antenna_tag,
                   no_other_antenna_tag,
                   no_missed_tags)
      })
    }

    if (direction == "resident") {

      nested <- ddply(rg, c("array", "antenna"), function(x) {

        # Unique tags at arrays other than x (the array for which effiency is being calculate)
        other_antenna_tag <- unique(subset(rg, antenna != x$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

        # Unique tags at x
        x_antenna_tag <- unique(x$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

        no_x_antenna_tag <- length(unique(x$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
        #print(antenna_tag)

        # Number of unique tags other than x
        no_other_antenna_tag <- length(unique(subset(rg, antenna != x$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

        # Number of unique on both antennas
        no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x

        #calculate detection efficicency: the number of tags detected at antenna x that were
        #detected at antennas other than x divided by the total number of tags detected other than antenna x
        det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

        no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

        data.frame(det_eff,
                   no_unique_tag,
                   no_x_antenna_tag,
                   no_other_antenna_tag,
                   no_missed_tags)
      })
    }
    # Re-order the columns to be more intuitive and fit with Joel's original structure
    det_clean <- det[,c("array","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

    # Name columns for output
    # A new column has been added and I have altered the column names.
    # For original column names see the det_eff_function.R file in the pre temoral resolution tests folder
    names(det_clean) <- c("array", "antenna", "detection_efficiency", "shared_detections",
                          "detections_on_array", "detections_not_on_array", "missed_detections")

    #Filter rows without antenna values (single arrays not ID'd as arrays)
    det_clean <- dplyr::filter(det_clean, antenna != "NA")
    return(det_clean)
  }

  ### YEAR ###

  # Nested the functions so that the calculations of all tag numbers could be subsetted by time.
  # With the current test data set the smaller time periods may not make sense. We may need a different data set such as the seton to test.

  if (resolution == "year") {

    if (direction == "up") {

      det <- ddply(rg, c("year"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags above x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags above x
          no_other_antenna_tag <- length(unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "down") {

      det <- ddply(rg, c("year"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags below x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags below x
          no_other_antenna_tag <- length(unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "resident") {

      det <- ddply(rg, c("year"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags at arrays other than x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags other than x
          no_other_antenna_tag <- length(unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas other than x divided by the total number of tags detected other than antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }


    # Add a date column with the first day of the first month of year
    det$Date <- ymd(sprintf("%s-%s-%s",det$year,1,1))
    det$Date <- ymd(det$Date,tz=data$time_zone[1])

    # Re-order the columns to be more intuitive and fit with Joel's original structure
    det_clean <- det[,c("array","year","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

    # Name columns for output
    # A new column has been added and I have altered the column names.
    # For original column names see the det_eff_function.R file in the pre temoral resolution tests folder
    names(det_clean) <- c("array", "year", "date", "antenna", "detection_efficiency", "shared_detections",
                          "detections_on_array", "detections_not_on_array", "missed_detections")

    #Filter rows without antenna values (single arrays not ID'd as arrays)
    det_clean <- dplyr::filter(det_clean, antenna != "NA")
    return(det_clean)

  }

  ### MONTH ###

  # Nested the functions so that the calculations of all tag numbers could be subsetted by time.
  # With the current test data set the smaller time periods may not make sense. We may need a different data set such as the seton to test.

  if (resolution == "month") {

    if (direction == "up") {

      det <- ddply(rg, c("year", "month"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags above x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags above x
          no_other_antenna_tag <- length(unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "down") {

      det <- ddply(rg, c("year", "month"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags below x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags below x
          no_other_antenna_tag <- length(unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "resident") {

      det <- ddply(rg, c("year", "month"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags at arrays other than x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags other than x
          no_other_antenna_tag <- length(unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas other than x divided by the total number of tags detected other than antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }


    # Add a date column with the first day of the month and the first hour
    det$Date <- ymd(sprintf("%s-%s-%s",det$year,det$month,1))
    det$Date <- ymd(det$Date,tz=data$time_zone[1])

    # Re-order the columns to be more intuitive and fit with Joel's original structure
    det_clean <- det[,c("array","year","month","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

    # Name columns for output
    # A new column has been added and I have altered the column names.
    # For original column names see the det_eff_function.R file in the pre temoral resolution tests folder
    names(det_clean) <- c("array", "year", "month", "date", "antenna", "detection_efficiency", "shared_detections",
                          "detections_on_array", "detections_not_on_array", "missed_detections")

    #Filter rows without antenna values (single arrays not ID'd as arrays)
    det_clean <- dplyr::filter(det_clean, antenna != "NA")

    return(det_clean)

  }

  ### WEEK ###

  # Nested the functions so that the calculations of all tag numbers could be subsetted by time.
  # With the current test data set the smaller time periods may not make sense. We may need a different data set such as the seton to test.

  if (resolution == "week") {

    if (direction == "up") {

      det <- ddply(rg, c("year", "month", "week"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags above x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags above x
          no_other_antenna_tag <- length(unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "down") {

      det <- ddply(rg, c("year", "month", "week"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags below x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags below x
          no_other_antenna_tag <- length(unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "resident") {

      det <- ddply(rg, c("year", "month", "week"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags at arrays other than x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags other than x
          no_other_antenna_tag <- length(unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas other than x divided by the total number of tags detected other than antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    # Add a date column with the first day of the week nd the first hour
    det$Date <- ymd(sprintf("%s-%s-%s",det$year,1,1)) # Start at Jan 1
    # Determine what day of the week January 1 is for each year
    det$first.day <- as.numeric(format(det$Date,"%w"))
    det$Date <- det$Date + 7*det$week - det$first.day - 7 # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
    det$Date <- ymd(det$Date,tz=data$time_zone[1])

    # Re-order the columns to be more intuitive and fit with Joel's original structure
    det_clean <- det[,c("array","year","month","week","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

    # Name columns for output
    # A new column has been added and I have altered the column names.
    # For original column names see the det_eff_function.R file in the pre temoral resolution tests folder
    names(det_clean) <- c("array", "year", "month", "week", "date", "antenna", "detection_efficiency", "shared_detections",
                          "detections_on_array", "detections_not_on_array", "missed_detections")

    #Filter rows without antenna values (single arrays not ID'd as arrays)
    det_clean <- dplyr::filter(det_clean, antenna != "NA")
    return(det_clean)

  }

  ### DAY ###

  # Nested the functions so that the calculations of all tag numbers could be subsetted by time.
  # With the current test data set the smaller time periods may not make sense. We may need a different data set such as the seton to test.

  if (resolution == "day") {

    if (direction == "up") {

      det <- ddply(rg, c("year", "month", "week", "day"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags above x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags above x
          no_other_antenna_tag <- length(unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "down") {

      det <- ddply(rg, c("year", "month", "week", "day"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags below x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags below x
          no_other_antenna_tag <- length(unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "resident") {

      det <- ddply(rg, c("year", "month", "week", "day"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags at arrays other than x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags other than x
          no_other_antenna_tag <- length(unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas other than x divided by the total number of tags detected other than antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    # Add a date column with the day and the first hour
    det$Date <- ymd(sprintf("%s-%s-%s",det$year,det$month,det$day))
    det$Date <- ymd(det$Date,tz=data$time_zone[1])

    # Re-order the columns to be more intuitive and fit with Joel's original structure
    det_clean <- det[,c("array","year","month","week","day","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

    # Name columns for output
    # A new column has been added and I have altered the column names.
    # For original column names see the det_eff_function.R file in the pre temoral resolution tests folder
    names(det_clean) <- c("array", "year", "month", "week", "day", "date", "antenna", "detection_efficiency", "shared_detections",
                          "detections_on_array", "detections_not_on_array", "missed_detections")

    #Filter rows without antenna values (single arrays not ID'd as arrays)
    det_clean <- dplyr::filter(det_clean, antenna != "NA")
    return(det_clean)

  }

  ### HOUR ###

  # Nested the functions so that the calculations of all tag numbers could be subsetted by time.
  # With the current test data set the smaller time periods may not make sense. We may need a different data set such as the seton to test.

  if (resolution == "hour") {

    if (direction == "up") {

      det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags above x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas upstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags above x
          no_other_antenna_tag <- length(unique(subset(x, antenna > y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "down") {

      det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags below x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code) #select unique tag codes for all antennas downstream of antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags below x
          no_other_antenna_tag <- length(unique(subset(x, antenna < y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas downstream of antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) #the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas downstream of x divided by the total number of tags detected downstream of antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    if (direction == "resident") {

      det <- ddply(rg, c("year", "month", "week", "day", "hour"), function(x) {

        #print()
        nested <- ddply(x, c("array", "antenna"), function(y) {

          # Unique tags at arrays other than x (the array for which effiency is being calculate)
          other_antenna_tag <- unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code) # select unique tag codes for all antennas other than antenna X

          # Unique tags at x
          x_antenna_tag <- unique(y$tag_code, na.rm = TRUE) # select all unique tag codes for antenna x

          no_x_antenna_tag <- length(unique(y$tag_code, na.rm = TRUE)) # The number of unique tag codes for antenna x
          #print(antenna_tag)

          # Number of unique tags other than x
          no_other_antenna_tag <- length(unique(subset(x, antenna != y$antenna[1], na.rm = TRUE)$tag_code)) #calculate the number of unique tag codes for all antennas other than antenna x

          # Number of unique on both antennas
          no_unique_tag <- sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) # the number of tags that are in both antenna x and antennas downstream of antenna x

          #calculate detection efficicency: the number of tags detected at antenna x that were
          #detected at antennas other than x divided by the total number of tags detected other than antenna x
          det_eff <- round(sum(x_antenna_tag %in% other_antenna_tag, na.rm = TRUE) / no_other_antenna_tag, 2)

          no_missed_tags <- no_other_antenna_tag - no_unique_tag # Calculate number of tags seen at other antennas but missed at antenna x

          data.frame(det_eff,
                     no_unique_tag,
                     no_x_antenna_tag,
                     no_other_antenna_tag,
                     no_missed_tags)
        })
      })
    }

    # Add a date column down to the hour
    # Add a date column with the day month and hour
    det$Date <- ymd(sprintf("%s-%s-%s",det$year,det$month,det$day))
    det$Date <- update(det$Date,hour=det$hour)
    det$Date <- ymd_hms(det$Date,tz=data$time_zone[1])

    # Re-order the columns to be more intuitive and fit with Joel's original structure
    det_clean <- det[,c("array","year","month","week","day","hour","Date","antenna","det_eff","no_unique_tag","no_x_antenna_tag","no_other_antenna_tag","no_missed_tags")]

    # Name columns for output
    # A new column has been added and I have altered the column names.
    # For original column names see the det_eff_function.R file in the pre temoral resolution tests folder
    names(det_clean) <- c("array", "year", "month", "week", "day", "hour", "date", "antenna", "detection_efficiency", "shared_detections",
                          "detections_on_array", "detections_not_on_array", "missed_detections")

    #Filter rows without antenna values (single arrays not ID'd as arrays)
    det_clean <- dplyr::filter(det_clean, antenna != "NA")
    return(det_clean)

  }


}
