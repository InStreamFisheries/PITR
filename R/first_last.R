#' @title Summarizes first and last detections on individual antennas
#'
#' @description Function summarizes the first and last detections and the difference in time (in minutes and days) between first and last detections on each antenna over a user-defined period of time. Data can be summarized by year, month, week, day or hour.
#' @param data telemetry dataset created using \code{\link{old_pit}}, \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize data by year, month, week, day or hour
#' @param start_date start date of period of interest, default is first date in dataset
#' @param end_date end date of period of interest, default is last date in dataset
#' @return Data frame summarizing the first and last detections on individual antennas.
#' @details Users can apply the \code{first_last} function to the original dataset created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or use an updated dataset created by the \code{\link{array_config}} function. Arguments \code{start_date} and \code{end_date}, if specified, must be entered as yyyy-mm-dd hh:mm:ss. Default for the resolution argument (NULL) will summarize the first and last detections on antennas over the entire time period present in the dataset.
#' @examples
#' #load test dataset
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' #summarize first and last detections by day with a start date of 2015-10-15 08:00:00
#' first_last(data = oregon_rfid, resolution = "day", start_date = "2015-10-15 08:00:00")
#'
#' #summarize first and last detections by month
#' first_last(data = oregon_rfid, resolution = "month")
#' @export

first_last <- function(data, resolution = NULL, start_date = min(data$date_time), end_date = max(data$date_time)) {

  # Need to format the dates away from character so that the filtering will work.

  start_date <- ymd_hms(start_date,tz=data$time_zone[1])
  end_date <- ymd_hms(end_date,tz=data$time_zone[1])

  #Filter data
  rg <- filter(data, date_time >= start_date  & date_time <= end_date)

  #create new temporal columns
  rg$year  <- year(rg$date_time)
  rg$month <- month(rg$date_time)
  rg$week  <- week(rg$date_time)
  rg$day   <- day(rg$date_time)
  rg$hour  <- hour(rg$date_time)

  # If the reader column doesn't exist, create it by duplicating the reader column
  if(!"array" %in% names(rg)) rg$array <- rg$reader

  if (is.null(resolution)) {

    fl.df <- ddply(rg, c("array", "antenna", "tag_code"), function(x){

      first_det <- min(x$date_time)
      last_det <- max(x$date_time)

      #Calculate time difference in minutes using lubridate
      time_diff_mins <- round((interval(first_det,last_det)/dminutes()),2)
      time_diff_days <- round((interval(first_det,last_det)/ddays()),2)


      data.frame(first_det,
                 last_det,
                 time_diff_days,
                 time_diff_mins)
    })

    return(fl.df)

  }

  if (resolution == "year") {

    fl.df <- ddply(rg, c("array", "antenna", "tag_code", "year"), function(x){

      first_det <- min(x$date_time)
      last_det <- max(x$date_time)

      #Calculate time difference in minutes using lubridate
      time_diff_min <- round((interval(first_det,last_det)/dminutes()),2)

      #Calculate time difference in minutes using lubridate
      time_diff_mins <- round((interval(first_det,last_det)/dminutes()),2)
      time_diff_days <- round((interval(first_det,last_det)/ddays()),2)


      data.frame(first_det,
                 last_det,
                 time_diff_days,
                 time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    fl.df$date <- ymd(sprintf("%s-%s-%s",fl.df$year,1,1))
    fl.df$date <- ymd(fl.df$date,tz=data$time_zone[1])

    fl.df <- select(fl.df,array,antenna,tag_code,year,date,first_det,last_det,time_diff_days,time_diff_mins)

    return(fl.df)

  }

  if (resolution == "month") {

    fl.df <- ddply(rg, c("array", "antenna", "tag_code", "year", "month"), function(x){

      first_det <- min(x$date_time)
      last_det <- max(x$date_time)

      #Calculate time difference in minutes using lubridate
      time_diff_mins <- round((interval(first_det,last_det)/dminutes()),2)
      time_diff_days <- round((interval(first_det,last_det)/ddays()),2)


      data.frame(first_det,
                 last_det,
                 time_diff_days,
                 time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    fl.df$date <- ymd(sprintf("%s-%s-%s",fl.df$year,fl.df$month,1))
    fl.df$date <- ymd(fl.df$date,tz=data$time_zone[1])

    fl.df <- select(fl.df,array,antenna,tag_code,year,month,date,first_det,last_det,time_diff_days,time_diff_mins)

    return(fl.df)

  }

  if (resolution == "week") {

    fl.df <- ddply(rg, c("array", "antenna", "tag_code", "year", "month", "week"), function(x){

      first_det <- min(x$date_time)
      last_det <- max(x$date_time)

      #Calculate time difference in minutes using lubridate
      time_diff_mins <- round((interval(first_det,last_det)/dminutes()),2)
      time_diff_days <- round((interval(first_det,last_det)/ddays()),2)


      data.frame(first_det,
                 last_det,
                 time_diff_days,
                 time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    fl.df$date <- ymd(sprintf("%s-%s-%s",fl.df$year,1,1)) # Start at Jan 1
    # Determine what day of the week January 1 is for each year
    fl.df$first.day <- as.numeric(format(fl.df$date,"%w"))
    fl.df$date <- fl.df$date + 7*fl.df$week - fl.df$first.day - 7 # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
    fl.df$date <- ymd(fl.df$date,tz=data$time_zone[1])

    fl.df <- select(fl.df,array,antenna,tag_code,year,month,week,date,first_det,last_det,time_diff_days,time_diff_mins)

    return(fl.df)

  }

  if (resolution == "day") {

    fl.df <- ddply(rg, c("array", "antenna", "tag_code", "year", "month", "week", "day"), function(x){

      first_det <- min(x$date_time)
      last_det <- max(x$date_time)

      #Calculate time difference in minutes using lubridate
      time_diff_mins <- round((interval(first_det,last_det)/dminutes()),2)
      time_diff_days <- round((interval(first_det,last_det)/ddays()),2)


      data.frame(first_det,
                 last_det,
                 time_diff_days,
                 time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    fl.df$date <- ymd(sprintf("%s-%s-%s",fl.df$year,fl.df$month,fl.df$day))
    fl.df$date <- ymd(fl.df$date,tz=data$time_zone[1])

    fl.df <- select(fl.df,array,antenna,tag_code,year,month,week,day,date,first_det,last_det,time_diff_days,time_diff_mins)

    return(fl.df)

  }

  if (resolution == "hour") {

    fl.df <- ddply(rg, c("array", "antenna", "tag_code", "year", "month", "week", "day", "hour"), function(x){

      first_det <- min(x$date_time)
      last_det <- max(x$date_time)

      #Calculate time difference in minutes using lubridate
      time_diff_mins <- round((interval(first_det,last_det)/dminutes()),2)
      time_diff_days <- round((interval(first_det,last_det)/ddays()),2)


      data.frame(first_det,
                 last_det,
                 time_diff_days,
                 time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    fl.df$date <- ymd(sprintf("%s-%s-%s",fl.df$year,fl.df$month,fl.df$day))
    fl.df$date <- update(fl.df$date,hour=fl.df$hour)
    fl.df$date <- ymd_hms(fl.df$date,tz=data$time_zone[1])

    fl.df <- select(fl.df,array,antenna,tag_code,year,month,week,day,hour,date,first_det,last_det,time_diff_days,time_diff_mins)

    return(fl.df)

  }


}
