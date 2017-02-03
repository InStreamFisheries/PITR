#' @title Summarizes upstream and downstream movements on arrays
#'
#' @description Function summarizes upstream and downstream movements of each fish on each array over a user-defined period of time. Such a function can assist in determining residence time between unique arrays. Data can be summarized by year, month, week, day or hour.
#' @param dat telemetry dataset created using \code{\link{old_pit}}, \code{\link{new_pit}} or \code{\link{array_config}}
#' @param resolution summarize data by year, month, week, day or hour
#' @param start_date start date of period of interest, default is first date in dataset
#' @param end_date end date of period of interest, default is last date in dataset
#' @return A dataframe summarizing upstream and downstream movements on arrays.
#' @details Users can apply the direction_total function to the original dataset created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or use the updated dataset created by the \code{\link{array_config}} function. Arguments \code{start_date} and \code{end_date}, if specified, must be entered as yyyy-mm-dd hh:mm:ss.
#' @examples
#' #summarize by month
#' direction_total(dam, “month”)
#'
#' #summarize by year with a start date of 2015-11-11 10:30:00
#' direction_total(dam, “year”, “2015-11-11 10:30:00”)
#' @export

direction_total <- function(dat, resolution=NULL, start_date = min(dat$date_time), end_date = max(dat$date_time)) {

  # Need to format the dates away from character so that the filtering will work.
  start_date <- ymd_hms(start_date,tz=dat$time_zone[1])
  end_date <- ymd_hms(end_date,tz=dat$time_zone[1])

  #Remove single reader rows from data set (created with pit_data function)
  xv<- subset(dat, antenna != "NA")

  #Filter data
  rg <- filter(xv, date_time >= start_date & date_time <= end_date)

  #create new temporal columns
  rg$year<- year(rg$date_time)
  rg$month <- month(rg$date_time)
  rg$week <- week(rg$date_time)
  rg$day <- day(rg$date_time)
  rg$hour <- hour(rg$date_time)

  # If the reader column doesn't exist, create it by duplicating the reader column
  if(!"array" %in% names(rg)) rg$array <- rg$reader

  if(is.null(resolution)){
    #For each array/ tag code...
    dir<- ddply(rg, c("array", "tag_code"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))

      #If the diffference between two consecutive sdettions is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      xx$direction<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
      data.frame(xx)
    })

    #Remove rows where direction is N
    dir_c<- subset(dir, direction != "N")
    #Sort by array, tag code and date-time
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$direction)
      last_det<- max(x$date_time)
      last_dir<- last(x$direction)


      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })
    return(dir_t)
  }

  else if(resolution == "hour"){
    #For each array/ tag code...
    dir<- ddply(rg, c("array", "tag_code", "year", "month", "day", "hour"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))

      #If the diffference between two consecutive sdettions is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      xx$direction<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))

      data.frame(xx)
    })
    #Remove rows where direction is N
    dir_c<- subset(dir, direction != "N")
    #Sort by array, tag code and date-time
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code", "year", "month", "day", "hour"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$direction)
      last_det<- max(x$date_time)
      last_dir<- last(x$direction)

      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s",dir_t$year,dir_t$month,dir_t$day))
    dir_t$date <- update(dir_t$date,hour=dir_t$hour)
    dir_t$date <- ymd_hms(dir_t$date,tz=dat$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,day,hour,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)

  }

  else if(resolution == "day"){

    #create new 'days' column with a posixct format
    #rg$day <- as.POSIXct(paste(rg$date, format = "%Y:%m:%d"))

    #For each array/ tag code...
    dir<- ddply(rg, c("array", "tag_code", "year", "month", "day"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))

      #If the diffference between two consecutive sdettions is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      xx$direction<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))

      data.frame(xx)
    })

    #Remove rows where direction is N
    dir_c<- subset(dir, direction != "N")
    #Sort by array, tag code and date-time
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code", "year", "month", "day"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$direction)
      last_det<- max(x$date_time)
      last_dir<- last(x$direction)


      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s",dir_t$year,dir_t$month,dir_t$day))
    dir_t$date <- ymd(dir_t$date,tz=dat$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,day,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)

  }

  else if(resolution == "week"){

    #create new week column
    #rg$week <- week(rg$date)

    #For each array/ tag code...
    dir<- ddply(rg, c("array", "tag_code", "year", "month", "week"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))

      #If the diffference between two consecutive sdettions is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      xx$direction<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))

      data.frame(xx)
    })

    #Remove rows where direction is N
    dir_c<- subset(dir, direction != "N")
    #Sort by array, tag code and date-time
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code", "year", "month", "week"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$direction)
      last_det<- max(x$date_time)
      last_dir<- last(x$direction)


      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })

    # Add a date column with the first day of the week nd the first hour
    dir_t$date <- ymd(sprintf("%s-%s-%s",dir_t$year,1,1)) # Start at Jan 1
    # Determine what day of the week January 1 is for each year
    dir_t$first.day <- as.numeric(format(dir_t$date,"%w"))
    dir_t$date <- dir_t$date + 7*dir_t$week - dir_t$first.day - 7 # Add in 7 days for each week up to the specified week minus the first.day and minus one week to get the start of the week
    dir_t$date <- ymd(dir_t$date,tz=dat$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,week,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)
  }

  else if(resolution == "month"){

    #create new month column
    #rg$month <- month(rg$date_time)

    #For each array/ tag code...
    dir<- ddply(rg, c("array", "tag_code", "year", "month"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))

      #If the diffference between two consecutive sdettions is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      xx$direction<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))

      data.frame(xx)
    })

    #Remove rows where direction is N
    dir_c<- subset(dir, direction != "N")
    #Sort by array, tag code and date-time
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code", "year", "month"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$direction)
      last_det<- max(x$date_time)
      last_dir<- last(x$direction)


      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s",dir_t$year,dir_t$month,1))
    dir_t$date <- ymd(dir_t$date,tz=dat$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,month,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)
  }


  else if(resolution == "year"){

    #create new week column
    #rg$year <- year(rg$date_time)

    #For each array/ tag code...
    dir<- ddply(rg, c("array", "tag_code", "year"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))

      #If the diffference between two consecutive sdettions is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
      xx$direction<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))

      data.frame(xx)
    })

    #Remove rows where direction is N
    dir_c<- subset(dir, direction != "N")
    #Sort by array, tag code and date-time
    dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

    dir_t<- ddply(dir_cs, c("array", "tag_code", "year"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$direction)
      last_det<- max(x$date_time)
      last_dir<- last(x$direction)


      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })

    # Add a date column that represents the first moment in time at the level of subsetting
    dir_t$date <- ymd(sprintf("%s-%s-%s",dir_t$year,1,1))
    dir_t$date <- ymd(dir_t$date,tz=dat$time_zone[1])

    dir_t <- select(dir_t,array,tag_code,year,date,first_det,first_dir,last_det,last_dir,time_diff_days,time_diff_mins)

    return(dir_t)
  }

}
