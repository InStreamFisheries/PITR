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
  
  dir <- data %>%  
    # Remove single reader rows form data set (created with pit_data function)
    dplyr::filter(antenna != "NA") %>% 
    dplyr::arrange(date_time) %>% 
    dplyr::group_by(array, tag_code) %>% 
    dplyr::mutate(direction = ifelse(c(0, diff(antenna)) > 0, "up", 
                                     ifelse(c(0, diff(antenna)) < 0, "down", 
                                            "N"))) %>% 
    ungroup %>% 
    # Calculate the number of antennas apart that consequtive detections occur
    dplyr::mutate(no_ant = c(0, abs(diff(antenna)))) %>% 
    # Remove rows where direction is N
    dplyr::filter(direction != "N") %>% 
    # Sort by reader, tag code and date-time
    dplyr::arrange(array, tag_code, date_time) %>% 
    dplyr::select(array, reader, antenna, det_type, date, time, date_time,
                  dur, tag_type, tag_code, consec_det, no_empt_scan_prior, direction, no_ant)
}
