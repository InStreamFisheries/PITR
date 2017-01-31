#' @title Computes the direction of movement
#'
#' @description Function that determines the direction of movement if there were two or more antennas deployed in the study.
#' @param dat telemetry dataset created using old_pit, new_pit or array_config
#' @return A dataframe summarizing the direction of movement.
#' @details User can apply the direction function to the original dataset created by the old_pit or new_pit function, or use the updated dataset created by the array_config function.
#' @examples
#' #determine the direction of fish movement
#' direction(new$all_det)
#' @export

direction <- function(dat){

  # If the reader column doesn't exist, create it by duplicating the reader column
  if(!"array" %in% names(dat)) dat$array <- dat$reader

  #Remove single reader rows form data set (breated with pit_data function)
  xv<- subset(dat, antenna != "NA")
  #For each reader/ tag code...
  dir<- ddply(xv, c("array","tag_code"), function(x){
    #Order by date_time
    xx<- x[order(x$date_time),]
    #If the diffferenc between two consecutive sdettions is positive then up/down (direction) = up, if it's negative then direction = down, if it's 0 then direction = N.
    xx$direction<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
    #Calculate the number of antennas apart that consequtuve detections occur
    xx$no_ant<- c(0,abs(diff(xx$antenna)))
    data.frame(xx)
  })
  #Remove rows where direction is N
  dir_c<- subset(dir, direction != "N")
  #Sort by reader, tag code and date-time
  dir_cs<- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time),]

  dir_cs <- select(dir_cs,array,reader,antenna,det_type,date,time,date_time,dur,tag_type,tag_code,consec_det,no_empt_scan_prior,direction,no_ant)
  return(dir_cs)


}
