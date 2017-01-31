#' @title Summarizes first and last detections on each antenna
#'
#' @description Function summarizes the first and last detections and the difference in time (in minutes and days) between first and last detections on each antenna over a user-defined period of time. Data can be summarized by year, month, week, day or hour.
#' @param data telemetry dataset created using old_pit, new_pit or array_config
#' @param resolution summarize data by year, month, week, day or hour
#' @param start_date start date of period of interest, default is first date in dataset
#' @param end_date end date of period of interest, default is last date in dataset
#' @details Users can apply the first_last function to the original dataset created by the old_pit or new_pit function, or use the updated dataset created by the array_config function. Arguments start_date and end_date must be entered as “yyyy-mm-dd hh:mm:ss”.
#' @examples
#' #summarize first and last detections by day with a start date of 2015-10-15 08:00:00
#' first_last(data, “day”, “2015-10-15 08:00:00”)
#'
#' #summarize first and last detections by month
#' first_last(data, “month”)

first_last <- function( dat, resolution = NULL, start_date = min(dat$date_time), end_date = max(dat$date_time) ) {

  # Need to format the dates away from character so that the filtering will work.

  start_date <- ymd_hms(start_date,tz=dat$time_zone[1])
  end_date <- ymd_hms(end_date,tz=dat$time_zone[1])

  #Filter data
  rg <- filter(dat, date_time >= start_date  & date_time <= end_date)

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
    fl.df$date <- ymd(fl.df$date,tz=dat$time_zone[1])

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
    fl.df$date <- ymd(fl.df$date,tz=dat$time_zone[1])

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
    fl.df$date <- ymd(fl.df$date,tz=dat$time_zone[1])

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
    fl.df$date <- ymd(fl.df$date,tz=dat$time_zone[1])

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
    fl.df$date <- ymd_hms(fl.df$date,tz=dat$time_zone[1])

    fl.df <- select(fl.df,array,antenna,tag_code,year,month,week,day,hour,date,first_det,last_det,time_diff_days,time_diff_mins)

    return(fl.df)

  }


}
