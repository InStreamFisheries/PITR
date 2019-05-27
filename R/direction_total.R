#' @title Summarizes upstream and downstream movements on arrays
#'
#' @description Function summarizes upstream and downstream movements of each fish on each array over a user-defined period of time. Such a function can assist in determining residence time between unique arrays. Data can be summarized by year, month, week, day or hour.
#' @param data telemetry dataset created using \code{\link{old_pit}}, \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize data by year, month, week, day or hour (optional)
#' @param start_date start date of period of interest, default is first date in dataset
#' @param end_date end date of period of interest, default is last date in dataset
#' @return Data frame summarizing upstream and downstream movements on arrays.
#' @details Users can apply the direction_total function to the original dataset created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or use an updated dataset created by the \code{\link{array_config}} function. Arguments \code{start_date} and \code{end_date}, if specified, must be entered as yyyy-mm-dd hh:mm:ss. Default for the resolution argument will summarize upstream and downstream movements over the entire time period present in the dataset.
#' @examples
#' #load test dataset containing detections from a multi reader with two antennas
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' #summarize by month
#' direction_total(data = oregon_rfid, resolution = “month”)
#'
#' #summarize by year with a start date of 2015-11-11 10:30:00
#' direction_total(data = oregon_rfid, resolution = "year", start_date = "2015-11-11 10:30:00")
#' @export

direction_total <- function(data,
                            resolution = NULL,
                            start_date = NULL,
                            end_date = NULL) {

  if (is.null(start_date)) start_date <- min(data$date_time) else start_date <- ymd_hms(start_date, tz = data$time_zone[1])
  if (is.null(end_date)) end_date <- max(data$date_time) else end_date <- ymd_hms(end_date, tz = data$time_zone[1])

  # Remove single reader rows from data set (created with pit_data function)
  xv <- subset(data, antenna != "NA")

  # Filter data
  rg <- filter(xv, date_time >= start_date & date_time <= end_date)

  # Create new temporal columns
  rg <- rg %>%
    mutate(year = year(date_time)) %>%
    mutate(month = month(date_time)) %>%
    mutate(week = week(date_time)) %>%
    mutate(day = day(date_time)) %>%
    mutate(hour = hour(date_time))

  # If the array column doesn't exist, create it by duplicating the reader column
  if(!"array" %in% names(rg)) rg$array <- rg$reader

  if(is.null(resolution)) {

    dir <- ddply(rg, c("array", "tag_code"), function(x){
      xx <- x[order(x$date_time),]
      # If the diffference between two consecutive detections is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      xx$direction<- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N") # Remove rows where direction is N
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]
    dir_t <- ddply(dir_cs, c("array", "tag_code"), direction_summary)
    return(dir_t)
  }

  else if(resolution == "hour") {
    dir <- ddply(rg, c("array", "tag_code", "year", "month", "day", "hour"), function(x) {
      xx<- x[order(x$date_time), ]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N")
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]

    dir_t <- ddply(dir_cs, c("array", "tag_code", "year", "month", "day", "hour"), direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s", dir_t$year, dir_t$month, dir_t$day))
    dir_t$date <- update(dir_t$date, hour = dir_t$hour)
    dir_t$date <- ymd_hms(dir_t$date, tz = data$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,day,hour,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)

  }

  else if(resolution == "day") {

    dir<- ddply(rg, c("array", "tag_code", "year", "month", "day"), function(x){
      xx <- x[order(x$date_time), ]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c<- subset(dir, direction != "N")
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]
    dir_t<- ddply(dir_cs, c("array", "tag_code", "year", "month", "day"), direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s", dir_t$year, dir_t$month, dir_t$day))
    dir_t$date <- ymd(dir_t$date, tz = data$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,day,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)

  }

  else if(resolution == "week") {

    dir<- ddply(rg, c("array", "tag_code", "year", "month", "week"), function(x) {
      xx <- x[order(x$date_time),]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c <- subset(dir, direction != "N")
    dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t <- ddply(dir_cs, c("array", "tag_code", "year", "month", "week"), direction_summary)

    # Add a date column with the first day of the week nd the first hour
    dir_t$date <- ymd(sprintf("%s-%s-%s", dir_t$year, 1, 1)) # Start at Jan 1
    # Determine what day of the week January 1 is for each year
    dir_t$first.day <- as.numeric(format(dir_t$date, "%w"))
    dir_t$date <- dir_t$date + 7*dir_t$week - dir_t$first.day - 7 # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
    dir_t$date <- ymd(dir_t$date, tz = data$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,week,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)
  }

  else if(resolution == "month"){

    dir <- ddply(rg, c("array", "tag_code", "year", "month"), function(x) {
      xx <- x[order(x$date_time),]
      xx$direction<- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c<- subset(dir, direction != "N")
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code", "year", "month"), direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s", dir_t$year, dir_t$month, 1))
    dir_t$date <- ymd(dir_t$date,tz = data$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)
  }


  else if(resolution == "year"){

    dir<- ddply(rg, c("array", "tag_code", "year"), function(x){
      xx <- x[order(x$date_time),]
      xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))
      data.frame(xx)
    })

    dir_c<- subset(dir, direction != "N")
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code", "year"), direction_summary)

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s", dir_t$year, 1, 1))
    dir_t$date <- ymd(dir_t$date, tz = data$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)
  }

}

direction_summary <- function(x){
  x[order(x$date_time),]
  first_det <- min(x$date_time)
  first_dir <- first(x$direction)
  last_det <- max(x$date_time)
  last_dir <- last(x$direction)

  # Calculate time differences b/w first and last detections
  time_diff <- interval(first_det, last_det)
  time_diff_days <- round(time_diff/ddays(1), 2)
  time_diff_mins <- round(time_diff/dminutes(1), 2)
  data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
}
