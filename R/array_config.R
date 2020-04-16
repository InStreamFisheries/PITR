#' @title Restructure the configuration of arrays
#'
#' @description Function allows users to combine unique readers into an array,
#'   split readers with multiple antennas into single readers, and rename antennas.
#'   Use of this function allows users to
#'   manage data for further analysis using \code{\link{det_eff}},
#'   \code{\link{direction}}, \code{\link{direction_total}}, and
#'   \code{\link{first_last}} functions.
#' @param data telemetry dataset created using \code{\link{old_pit}} or
#'   \code{\link{new_pit}} function
#' @param configuration either "combine", "split" or "rename_antennas"
#' @param array_name name of array
#' @param r1 name of reader 1
#' @param r2 name of reader 2
#' @param r3 name of reader 3
#' @param r4 name of reader 4
#' @param reader_name name of reader to split or for renaming antennas
#' @param new_reader_1_antennas specific antennas to be grouped into reader 1
#' @param ao1 old antenna 1
#' @param ao2 old antenna 2
#' @param ao3 old antenna 3
#' @param ao4 old antenna 4
#' @param an1 new antenna 1
#' @param an2 new antenna 2
#' @param an3 new antenna 3
#' @param an4 new antenna 4
#' @return Updated dataset for further analysis using \code{\link{det_eff}},
#'   \code{\link{direction}}, \code{\link{direction_total}} and
#'   \code{\link{first_last}} functions.
#' @details This function allows complex antenna arrangements to be organized into
#'   arrays, or antennas in single arrays to be renamed sequentially. The \code{array_config}
#'   function is iterative and the user may need to run the function several times
#'   to reach the desired study configuration (see examples).
#'
#'   Function is dependent on what the user defines in the
#'   \code{configuration} argument:
#'
#'    \enumerate{
#'      \item \code{configuration = "split"}: Used to split multi
#'      readers into two or more single readers to allow the combination of single
#'      readers into a user-specified array (see Example 1). The arguments
#'      \code{reader_name} and \code{new_reader_1_antennas} are required for "split".
#'      Argument \code{new_reader_1_antennas} is used to define the
#'      antennas to group into reader 1. All other antennas are grouped into reader 2.
#'      \item \code{configuration = "combine"}: Used to combine up to four \emph{single} readers into one array
#'      (see Example 2). Arguments \code{array_name} and \code{r1} through \code{rx} are required for "combine".
#'      If a user wants to combine a multi reader with another
#'      reader (either a multi or single reader), the user first must split the
#'      multi reader into two or more single readers and then combine them together
#'      into an array. If a user needed to split a multi reader with four single readers for use in
#'      "combine", they would need to run configuration "split" four times.
#'      \item \code{configuration = "rename_antennas"}: Used to either rename antennas part
#'      of an array (if \code{array_name} is specified) or antennas part of a
#'      reader (if \code{reader_name} is specified) (see Example 3). Arguments
#'      \code{ao1} through \code{aox} and \code{an1} through \code{anx} are require for
#'      "rename_antennas". Arguments with the
#'      prefix \emph{ao} correspond to the old (or current) antenna numbering scheme,
#'      whereas arguments with the prefix \emph{an} correspond to the new antenna
#'      numbering scheme. Users can rename up to four antennas for one reader or one array.
#'    }
#' @examples
#' # Load test dataset containing detections from a multi reader with two antennas
#' oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#'
#' # Example 1: Split a multi reader with two antennas into two single readers
#' # Users can split multi readers with two or more antennas into two or more single readers that can later be combined into user-defined arrays
#' split <- array_config(data = oregon_rfid, configuration = "split", reader_name = "dam", new_reader_1_antennas = "1")
#'
#' # Example 2: Combine two single readers into an array called fishway
#' # Users can combine two or more single readers (using a raw or split dataset) into arrays
#' # If there is an existing dataset with two or more readers (multi or single readers or a combination), the user can start by combining the readers into arrays
#' combine <- array_config(data = split, configuration = "combine", array_name = "fishway", r1 = "dam_1", r2 = "dam_2")
#'
#' # Example 3: Rename the two antennas. Antenna 1 becomes antenna 3, and antenna 2 becomes antenna 4.
#' rename_one <- array_config(data = combine, configuration = "rename_antennas", array_name = "fishway", ao1 = 1, an1 = 3)
#' rename_two <- array_config(data = rename_one, configuration = "rename_antennas", array_name = "fishway", ao1 = 2, an1 = 4)
#' # The above calls could be combined:
#' rename_multiple <- array_config(data = combine, configuration = "rename_antennas", array_name = "fishway", ao1 = 1, an1 = 3, ao2 = 2, an2 = 4)
#' @export

array_config <- function (data,
                          configuration,
                          array_name = NULL,
                          r1 = NULL,
                          r2 = NULL,
                          r3 = NULL,
                          r4 = NULL,
                          reader_name = NULL,
                          new_reader_1_antennas = NULL,
                          ao1 = NULL,
                          ao2 = NULL,
                          ao3 = NULL,
                          ao4 = NULL,
                          an1 = NULL,
                          an2 = NULL,
                          an3 = NULL,
                          an4 = NULL) {

  # Combine up to four readers with single antennas into one array.
  # The readers should be inputed in order so that the new antenna numbers 1, 2,
  # 3, 4 are in order of downstream to upstream

  if (is.null(configuration)) stop("Error: configuration must be specified")

  if (configuration == "combine") {

    if (is.null(array_name)) stop("Error: array_name must be specified")

    if (!is.null(array_name)) {

      # Select data that comes from readers you want to combine
      rd <- dplyr::filter(data, reader %in% c(r1, r2, r3, r4))

      # Select all other data to merge back in later
      rr <- dplyr::filter(data, !(reader %in% c(r1, r2, r3, r4)))

      # Assign new antenna numbers based on readers (r1, r2, r3 and r4 get
      # antenna 3"s 1,2,3 and 4 respectively).
      # If r1 exists, give new antenna
      # number 1 (so it would be antenna 1 on "new PIT array")
      if (!is.null(r1)) {
        x1 <- which(rd$reader == r1)
        rd$antenna[x1] <- 1
      }

      if (!is.null(r2)) {
        x2 <- which(rd$reader == r2)
        rd$antenna[x2] <- 2
      }

      if (!is.null(r3)) {
        x3 <- which(rd$reader == r3)
        rd$antenna[x3] <- 3
      }

      if (!is.null(r4)) {
        x4 <- which(rd$reader == r4)
        rd$antenna[x4] <- 4
      }

      # Assign new array name as the readers are being combined into one PIT array
      rd$array <- array_name

      # The array name of the rest of the data is defaulted to the reader name
      # unless the function has already been run through and an array column has
      # been created, in which case the array column is left as is
      if ("array" %in% names(rr)) rr$array <- rr$array else rr$array <- rr$reader

      nc <- rbind(rd, rr)

      # Reorder columns
      nc <- nc %>% 
        dplyr::select(array, reader, antenna, det_type, date, time, date_time,
                          time_zone, dur, tag_type, tag_code, consec_det, no_empt_scan_prior) %>% 
        dplyr::mutate(antenna = as.numeric(antenna))

      array_summary(nc)

      return(nc)
    }
  }

  if (configuration == "split") {

    if (is.null(reader_name)) stop("Error: reader name must be specified")

    if (!is.null(reader_name)) {

      if (is.null(new_reader_1_antennas))
        stop("Error: Must specify which antenna(s) will become part of the new reader 1")

      # Select data that comes from reader you want to split in two
      rd <- dplyr::filter(data, reader == reader_name)

      # Select all other data to merge back in later
      rr <- dplyr::filter(data, reader != reader_name)

      # Assign new reader names based on antenna specifications (ao1,a02,and a03
      # get "reader"_1; all others get get "reader"_2)
      if (!is.null(new_reader_1_antennas)) {
        rd$reader <- ifelse(rd$antenna == new_reader_1_antennas,
                            paste(reader_name, "1", sep = "_"),
                            paste(reader_name, "2", sep = "_"))
      }

      # Create an array column if one doesn't exist, if the array column does
      # exist then default to it here
      if ("array" %in% names(rr)) rr$array <- rr$array else rr$array <- rr$reader
      if ("array" %in% names(rd)) rd$array <- rd$array else rd$array <- rd$reader

      nc <- rbind(rd, rr)

      array_summary(nc)

      nc <- nc %>% 
        dplyr::select(array, reader, antenna, det_type, date, time, date_time,
                          time_zone, dur, tag_type, tag_code, consec_det, no_empt_scan_prior) %>% 
        dplyr::mutate(antenna = as.numeric(antenna))

      return(nc)
    }
  }

  if (configuration == "rename_antennas") {

    if (!is.null(reader_name) & !is.null(array_name))
      stop("Error: Only specify one array or one reader with antennas to rename")

    if (is.null(reader_name) & is.null(array_name))
      stop("Error: Must specify a reader or array with antennas to rename")

    if (!is.null(reader_name)) {

      rd <- dplyr::filter(data, reader == reader_name)

      rr <- dplyr::filter(data, reader != reader_name)

    }

    if (!is.null(array_name)) {

      if (!"array" %in% names(data)) stop("Error: No arrays column exists")

      # Select data that comes from array you want to reconfig antennas for
      rd <- dplyr::filter(data, array == array_name)

      # Select all other data to merge back in later
      rr <- dplyr::filter(data, array != array_name)

    }

    # Assign new antenna numbers based on number entries (ao1 -> an1, ao2 ->
    # an2, ao3 -> an3, a04 -> an4). The code is overly complicated because of
    # the possibility of an antenna being numbered "NA" if it was a single reader.
    # I'm allowing for NAs in all steps, but I don't think NA would be present if you
    # are numbering more than 1 antenna (unless there are corrupt multi data)

    # Send a warning if the user is renaming ao2, 3, or 4, but not 1
    if (is.null(ao1)) stop("Error: Always begin renaming by specifying ao1")

    if(!is.null(ao1) & !is.null(ao2) & !is.null(ao3) & !is.null(ao4)) {

      if (as.numeric(an1) > 4 | as.numeric(an2) > 4 | as.numeric(an3) > 4 | as.numeric(an4) > 4)
        stop("Error: Antenna numbers cannot be greater than 4")

      if (ao1 == "NA") temp1 <- subset(rd, is.na(antenna)) else temp1 <- subset(rd, antenna == ao1)
      temp1$antenna <- an1
      if (ao2 == "NA") temp2 <- subset(rd, is.na(antenna)) else temp2 <- subset(rd, antenna == ao2)
      temp2$antenna <- an2
      if (ao3 == "NA") temp3 <- subset(rd, is.na(antenna)) else temp3 <- subset(rd, antenna == ao3)
      temp3$antenna <- an3
      if (ao4 == "NA") temp4 <- subset(rd, is.na(antenna)) else temp4 <- subset(rd, antenna == ao4)
      temp4$antenna <- an4
      # Even with four antennas specified, corrept data may have led to some extra data so we use the if statement here too
      if (ao1 == "NA" | ao2 == "NA" | ao3 == "NA" | ao4 == "NA") other <- subset(rd, !antenna %in% c(ao1, ao2, ao3, ao4) & !is.na(antenna)) else other <- subset(rd, !antenna %in% c(ao1, ao2, ao3, ao4))
      rd <- rbind(temp1, temp2, temp3, temp4, other)
    }

    if(!is.null(ao1) & !is.null(ao2) & !is.null(ao3) & is.null(ao4)) {

      if (as.numeric(an1) > 4 | as.numeric(an2) > 4 | as.numeric(an3) > 4)
        stop("Error: Antenna numbers cannot be greater than 4")

      if (ao1 == "NA") temp1 <- subset(rd, is.na(antenna)) else temp1 <- subset(rd, antenna == ao1)
      temp1$antenna <- an1
      if (ao2 == "NA") temp2 <- subset(rd, is.na(antenna)) else temp2 <- subset(rd, antenna == ao2)
      temp2$antenna <- an2
      if (ao3 == "NA") temp3 <- subset(rd, is.na(antenna)) else temp3 <- subset(rd, antenna == ao3)
      temp3$antenna <- an3
      if (ao1 == "NA" | ao2 == "NA" | ao3 == "NA") other <- subset(rd, !antenna %in% c(ao1, ao2, ao3) & !is.na(antenna)) else other <- subset(rd, !antenna %in% c(ao1, ao2, ao3))
      rd <- rbind(temp1, temp2, temp3, other)
    }

    if(!is.null(ao1) & !is.null(ao2) & is.null(ao3) & is.null(ao4)) {

      if (as.numeric(an1) > 4 | as.numeric(an2) > 4)
        stop("Error: Antenna numbers cannot be greater than 4")

      if (ao1 == "NA") temp1 <- subset(rd, is.na(antenna)) else temp1 <- subset(rd, antenna == ao1)
      temp1$antenna <- an1
      if (ao2 == "NA") temp2 <- subset(rd, is.na(antenna)) else temp2 <- subset(rd, antenna == ao2)
      temp2$antenna <- an2
      if (ao1 == "NA" | ao2 == "NA") other <- subset(rd, !antenna %in% c(ao1, ao2) & !is.na(antenna)) else other <- subset(rd, !antenna %in% c(ao1, ao2, ao3))
      rd <- rbind(temp1, temp2, other)
    }

    if(!is.null(ao1) & is.null(ao2) & is.null(ao3) & is.null(ao4)) {

      if (as.numeric(an1) > 4)
        stop("Error: Antenna numbers cannot be greater than 4")

      if (ao1 == "NA") temp1 <- subset(rd, is.na(antenna)) else temp1 <- subset(rd, antenna == ao1)
      temp1$antenna <- an1
      if (ao1 == "NA") other <- subset(rd, !is.na(antenna)) else other <- subset(rd, !antenna %in% c(ao1))
      rd <- rbind(temp1, other)
    }

    # Create an array column if one doesn't exist, if the array column does
    # exist then default to it here
    if ("array" %in% names(rr)) rr$array <- rr$array else rr$array <- rr$reader
    if ("array" %in% names(rd)) rd$array <- rd$array else rd$array <- rd$reader

    nc <- rbind(rd, rr) %>% 
      dplyr::mutate(antenna = as.numeric(antenna))

    array_summary(nc)

    return(nc)
  }
}

array_summary <- function(x) {
  x1 <- dplyr::select(x, array, reader, antenna)
  x2 <- unique(x1)
  print("Summary of current array, reader, and antenna configuration:")
  print(x2)
}
