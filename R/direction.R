#' @title Computes the direction of movement for each tag detection
#'
#' @description Function that determines the direction of movement for each tag detection
#'    on each antenna if there were two or more antennas deployed in a study.
#' @param data telemetry dataset created using \code{\link{old_pit}},
#'   \code{\link{new_pit}} or \code{\link{array_config}}
#' @return Data frame summarizing the direction of movement.
#' @details Antennas must be numbered sequentially and any antennas from single readers
#'   that have not be renamed using \code{array_config} (i.e., the antenna number is NA)
#'   will not be considered by the function. Users can apply the direction function to the original dataset
#'   created by the \code{\link{old_pit}} or \code{\link{new_pit}} function, or
#'   use an updated dataset created by the \code{\link{array_config}} function.
#' @examples
#' # Load test dataset containing detections from a multi reader with two antennas
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' # Determine the direction of fish movement
#' direction(data = oregon_rfid)
#' @export

direction <- function(data) {

  # If the array column doesn't exist, create it by duplicating the reader column
  if (!"array" %in% names(data)) data$array <- data$reader

  # Remove single reader rows form data set (created with pit_data function)
  xv <- subset(data, antenna != "NA")

  dir <- plyr::ddply(xv, c("array", "tag_code"), function(x) {
    xx <- x[order(x$date_time), ]

    # If the diffferenc between two consecutive sdettions is positive then
    # up/down (direction) = up, if it's negative then direction = down, if it's
    # 0 then direction = N.
    xx$direction <- ifelse(c(0, diff(xx$antenna)) > 0, "up", ifelse(c(0, diff(xx$antenna)) < 0, "down", "N"))

    # Calculate the number of antennas apart that consequtive detections occur
    xx$no_ant <- c(0, abs(diff(xx$antenna)))
    data.frame(xx)

  })

  dir_c <- subset(dir, direction != "N") # Remove rows where direction is N

  # Sort by reader, tag code and date-time
  dir_cs <- dir_c[order(dir_c$array, dir_c$tag_code, dir_c$date_time), ]

  dir_cs <- dplyr::select(dir_cs, array, reader, antenna, det_type, date, time, date_time,
                          dur, tag_type, tag_code, consec_det, no_empt_scan_prior, direction, no_ant)
  return(dir_cs)

}
