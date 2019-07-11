#' @title Collates data from readers in new format (>V5.0)
#'
#' @description Function collates data located in the working directory that
#'   were collected from readers with a firmware version later than 5.0 (April
#'   2014). Users must define the working directory where the PIT files are
#'   located. If data is being collated from different formats, the user must
#'   specify separate working directories for each format and use the applicable
#'   PITR function to collate (see details).
#' @param data filepath of the working directory containing data in new format
#' @param test_tags concatenated list of PIT codes used for testing antenna
#'   performance
#' @param print_to_file export metadata to working directory
#' @param time_zone time zone where data were collected, default is computer time zone
#' @return List of data frames consisting of detection records, event records,
#'   error records from single and multi readers, and character vectors of unique single and multi readers.
#' @details Users can apply the \code{print_to_file} argument to export metadata
#'   to the working directory to confirm that data collation was performed
#'   correctly. Data files must be named in the following format:
#'   \emph{pit_reader_mm_dd_yyyy.txt}. Note that pit_reader is the unique name
#'   of the PIT reader and that mm, dd, and yyyy must be separated by
#'   underscores. If .txt is not at the end of each file, the user must enter
#'   .txt manually to each file prior to reading in the data. Users can choose
#'   to specify the time zone where data were collected in the rare event that
#'   this is different from the computer time zone.
#'
#'   Test tags can be removed from the data using the \code{test_tag} argument.
#'   Users can also include a data frame called study tags in .csv or .txt format
#'   (i.e., study_tags.csv or study_tags.txt) in the working directory that
#'   contains the full list of tags used in a study in one column called
#'   \emph{study_tags}. Tag codes for study tags and test tags must be complete: 900_230000010075 and not
#'   10075, 0000_0000000183783293 and not 183783293. If the study tag data frame
#'   exists, the function will automatically subset the data to retain only the
#'   applicable tags. If no data frame of study tags exists in the working
#'   directory, it is assumed that no tag codes are subsetted out of the
#'   dataset.
#'
#'   There are three versions of ORFID PIT readers that can be collated using
#'   PITR: an older format prior to 2014 (use \code{\link{old_pit}}), a version
#'   from 2015 to 2018 (use \code{\link{new_pit}}), and the most current version
#'   from 2019 (use \code{\link{pit_2019}}). To combine data from multiple
#'   reader versions, separate files into working directories based on version,
#'   collate using the appropriate PITR function, then use
#'   \code{rbind(old_pit$all_det, new_pit$all_det, pit_2019$all_det)} to
#'   concatenate the detection dataframes prior to further analysis. Note:
#'   \code{pit_2019} is a preliminary function and will continue to be updated.
#' @examples
#' # Collate data and print metadata to working directory
#'
#' # Define working directory
#' new <- "./New PIT Data/"
#'
#' # Identify the three test tags used in the study
#' tt <- c("900_230000010075", "900_230000010079", "900_230000010080")
#'
#' # Run new_pit function
#' new_pit(data = new, test_tags = tt, print_to_file = TRUE, time_zone = "America/Vancouver")
#' @importFrom magrittr "%>%"
#' @export

new_pit <- function (data,
                     test_tags = NULL,
                     print_to_file = FALSE,
                     time_zone = NULL,
                     old_format_data = NULL) {

  # Re-structure test data 'oregon_rfid' for use in subsequent functions
  if (data == "oregon_rfid") {

    oregon_rfid <- oregon_rfid %>%
      dplyr::mutate(reader = as.character(oregon_rfid$reader)) %>%
      dplyr::mutate(antenna = as.numeric(oregon_rfid$antenna)) %>%
      dplyr::mutate(det_type = as.character(oregon_rfid$det_type)) %>%
      dplyr::mutate(date = as.character(oregon_rfid$date)) %>%
      dplyr::mutate(time = as.character(oregon_rfid$time)) %>%
      dplyr::mutate(date_time = lubridate::ymd_hm(oregon_rfid$date_time)) %>%
      dplyr::mutate(time_zone = as.character(oregon_rfid$time_zone)) %>%
      dplyr::mutate(dur = as.character(oregon_rfid$dur)) %>%
      dplyr::mutate(tag_type = as.character(oregon_rfid$tag_type)) %>%
      dplyr::mutate(tag_code = as.character(oregon_rfid$tag_code)) %>%
      dplyr::mutate(consec_det = as.character(oregon_rfid$consec_det)) %>%
      dplyr::mutate(no_empt_scan_prior = as.character(oregon_rfid$no_empt_scan_prior))

    return(oregon_rfid)
  }

  counter_paths <- dir(data, full.names = TRUE)
  names(counter_paths) <- basename(counter_paths)

  f1 <- plyr::ldply(counter_paths,
                    read.table,
                    header = FALSE,
                    fill = TRUE,
                    stringsAsFactors = FALSE,
                    skipNul = TRUE,
                    col.names = c("det_type", "date", "time", "dur", "tag_type",
                                  "tag_code", "antenna", "consec_det", "no_empt_scan_prior", rep("na", 20)))

  # Create unique ID for each reader from file names
  f1 <- f1 %>%
    dplyr::mutate(reader = stringr::str_sub(.id, 1, -16)) %>%  # Dependant on correct file naming
    dplyr::select(reader, det_type, date, time, dur, tag_type, tag_code, antenna, consec_det,
           no_empt_scan_prior)

  # Create Event Dataframe --------------------

  event_dat <- dplyr::filter(f1, f1$det_type == "E") %>%
    dplyr::mutate(desc = apply(dplyr::select(., dur:no_empt_scan_prior), 1, paste, collapse = " ")) %>%  # Collapse the columns into one description
    dplyr::mutate(desc = gsub("NA", "", paste(as.character(desc)))) %>% # Remove NA values
    dplyr::mutate(desc = gsub("^\\s+|\\s+$", "", desc)) %>% # Remove space at end of text
    dplyr::select(reader, det_type, date, time, desc) %>%
    dplyr::distinct(reader, date, time, desc, .keep_all = TRUE)

  # Filter voltage events for voltage plots
  v <- dplyr::filter(event_dat, grepl("V", desc))

  if (is.null(time_zone)) {

    v <- v %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time)))
  }

  if (!is.null(time_zone)) { # Manipulate time zone if specified by user

    v <- v %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::mutate(date_time = lubridate::with_tz(date_time, time_zone)) %>%
      dplyr::mutate(time_zone = time_zone)
  }

  # Find the location of the "V" in each row of the data frame
  # I'm assuming V will only occur once in the row.
  v_location = do.call("rbind", stringr::str_locate_all(pattern = "V", string = v$desc))[, 1]

  # Select only voltage numbers for plotting
  # When there are multireaders you can sometimes have different text so I
  # modified this code to search for the V and take the 4 characters in from of it
  # regardless of how many antenna amperages are in the output.
  volt_dat <- v %>%
    dplyr::mutate(volt_location = v_location) %>%
    dplyr::mutate(volt = stringr::str_sub(desc, start = (volt_location - 4), end = (volt_location - 1))) %>%
    #dplyr::mutate(volt = stringr::str_sub(desc, -5, -2)) %>%
    #dplyr::mutate(volt = as.numeric(gsub("^\\s+|\\s+$", "", volt))) %>%
    dplyr::mutate(volt = as.numeric(volt)) %>%
    dplyr::mutate(time = stringr::str_sub(time, 1, 8)) %>%
    dplyr::select(reader, det_type, date, time, desc, date_time, time_zone, volt)

  # Create Detection Dataframe --------------------

  d <- dplyr::filter(f1, f1$det_type == "D")

  if (nrow(d) == 0) stop("Error: Data has no detections") # Force stop if no detections

  # Rbind in old data from "old_pit.R" if old data present
  # This part of the function is no longer in the help documentation but I left it in for now.
  if (! is.null(old_format_data)) {

    print("Warning: old_format_data is no longer supported and will be removed. See instructions for combining multiple PIT formats in help documentation")

    old_data <- old_format_data$all_det
    old_data <- old_data %>%
      dplyr::select(reader, det_type, date, time, dur, tag_type, tag_code,
                    antenna, consec_det, no_empt_scan_prior) %>%
      dplyr::mutate(antenna = paste("A", antenna, sep = "")) %>% # Add back in the A so that the old data filters properly
      dplyr::mutate(date = as.character(date))
    d <- rbind(d, old_data)
  }

  # Turn blanks into NAs (single readers do not have 'antenna' values so last
  # two columns are shifted and sometimes no_empt_scan_prior is blank)
  d[d == ""] <- NA

  # Create data frames of single antenna data and multiplexer data
  sa1 <- dplyr::filter(d, !(antenna %in% c("A1", "A2", "A3", "A4"))
                & !(is.na(antenna)) & is.na(no_empt_scan_prior))
  ma1 <- dplyr::filter(d, antenna %in% c("A1", "A2", "A3", "A4"))

  # Manipulate single antenna data --------------------
  if (nrow(sa1) > 0) {

    sa2 <- sa1 %>% # Rename and manipulate columns to match multireader data
      dplyr::rename(na = no_empt_scan_prior) %>%
      dplyr::rename(no_empt_scan_prior = consec_det) %>%
      dplyr::rename(consec_det = antenna) %>%
      dplyr::mutate(antenna = "NA") %>%
      dplyr::select(reader, det_type, date, time, dur, tag_type, tag_code, antenna,
             consec_det, no_empt_scan_prior) %>%
      dplyr::mutate(consec_det = suppressWarnings(as.numeric(consec_det)))

    if(sum(is.na(sa2$consec_det)) > 0) print("Warning: NAs introduced by coercion in consec_det/count column. Single antenna data may be corrupted. See single_error")

    # Remove duplicate rows
    sa <- sa2 %>%
      dplyr::distinct(reader, date, time, dur, tag_type, tag_code, consec_det,
               no_empt_scan_prior, .keep_all = TRUE)

    # Filter rows that have incorrect values for antenna (user can look over and
    # decide to correct in raw data if important)
    # This is slightly different than the warning above because the column names were changed in sa but
    # here we are looking at d
    corrupt_single <- dplyr::filter(d, is.na(antenna))

    sa_readers <- unique(sa$reader)

    final_single_data <- sa

  }

  # Manipulate Multiplexer Data
  if (nrow(ma1) > 0) {

    ma2 <- dplyr::filter(ma1, complete.cases(no_empt_scan_prior)) # Filter complete cases

    ma2$consec_det <- suppressWarnings(as.numeric(ma2$consec_det))
    if(sum(is.na(ma2$consec_det)) > 0)
      print("Warning: NAs introduced by coercion in consec_det/count column. Multiple antenna data may be corrupted.")

    # Remove duplicate rows
    ma <- ma2 %>%
      dplyr::distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det,
               no_empt_scan_prior, .keep_all = TRUE)

    # Identify corrupt multiplexer data if present
    corrupt_multi <- dplyr::filter(d, !(antenna %in% c("A1", "A2", "A3", "A4"))
                                   & !(is.na(antenna)) & !is.na(no_empt_scan_prior))
    if(nrow(corrupt_multi) > 0) print("Warning: Corrupt multiplexer data were detected and removed. See multi_error")

    ma_readers <- unique(ma$reader)

    ma$antenna <- as.numeric(substr(ma$antenna, 2, 2)) # Remove 'A' from antenna values and convert to numeric
    final_multi_data <- ma
  }

  # Other Dataframe --------------------

  # Pull "other" (corrupt) data from both single and/or multiplexer data and fix
  # and return to final data frames.
  # This is finding "H" in the tag code, which may have been related to a
  # specific bug, or may be general. During testing I could not find testing data
  # with the "H" to show why this might occur.
  other_dat <- f1 %>%
    dplyr::filter(!(det_type %in% c("D", "E"))) %>%
    dplyr::distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det,
             no_empt_scan_prior, .keep_all = TRUE) %>%
    dplyr::filter(grepl("H", tag_type))

  # Look for corruption in multiplexer data (if multi data exists)
  if (nrow(ma1) > 0) {
    if (nrow(other_dat) > 0) { # Remove corruption if it exists
      ma_re <- other_dat %>%
        dplyr::filter(grepl("A", antenna)) %>%
        dplyr::mutate(det_type = "D") %>%
        dplyr::mutate(antenna = as.numeric(substr(antenna, 2, 2))) # Remove 'A' from antenna values and convert to numeric
      final_multi_data <- rbind(final_multi_data, ma_re)
    }
  }

  # Look for corruption in single data (if single data exists)
  if (nrow(sa1) > 0) {
    if (nrow(other_dat) > 0) {

    # Select rows that are from single readers
    sa_re <- other_dat %>%
      dplyr::filter(!(grepl ("A", antenna))) %>%
      dplyr::rename(na = no_empt_scan_prior) %>%
      dplyr::rename(no_empt_scan_prior = consec_det) %>%
      dplyr::rename(consec_det = antenna) %>%
      dplyr::mutate(antenna = "NA") %>%
      dplyr::select(reader, det_type, date, time, dur, tag_type, tag_code, antenna,
             consec_det, no_empt_scan_prior) %>%
      dplyr::mutate(det_type = "D")

    final_single_data <- rbind(final_single_data, sa_re)
    }
  }

  # Combine Single and Mulit --------------------

  if (exists("final_single_data") & exists("final_multi_data")) {
    all_det <- rbind(final_single_data, final_multi_data)

    # Sometimes corrupt multi data becomes single data and it is nearly
    # impossible for PITR to identify it. A red flag would be if the same reader
    # name is employed for both single and multiple readers Create a warning if
    # there is a match between single reader and multi reader names.
    if(sum(unique(final_single_data$reader) %in% unique(final_multi_data$reader)) > 0)
      print("Warning: Single and multi data contain the same reader name. If this does not accurately describe your study, check single_data and mutli_data (and corresponding error outputs) for corrupt data that may have been assigned to the wrong reader type.")

  } else if (exists("final_single_data")) {
    all_det <- final_single_data
  } else if (exists("final_multi_data")) {
    all_det <- final_multi_data
  }

  # Filter Tags --------------------

  # Remove test tags if they exist
  if (!(is.null(test_tags))) {
    all_det <- dplyr::filter(all_det,!(tag_code %in% test_tags))
  }

  # Filter out study tags
  study_tag_path <- counter_paths[grep(pattern = "study_tags", x = counter_paths)]

  if (length(study_tag_path) == 1) {

    if (grepl(pattern = ".csv", x = study_tag_path) == TRUE) study_tag_df <- read.table(study_tag_path, header = TRUE, sep = ",")
    if (grepl(pattern = ".txt", x = study_tag_path) == TRUE) study_tag_df <- read.table(study_tag_path, header = TRUE, sep = "\t")

    study_tags_vector <- study_tag_df$study_tags
    all_det <- dplyr::filter(all_det, tag_code %in% study_tags_vector)

  }

  # Create Datafames to Return --------------------

  # Create new column that combines date and time
  if (is.null(time_zone)) {
    all_det <- all_det %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::mutate(time_zone = Sys.timezone())
  }

  if (!is.null(time_zone)) {
    all_det <- all_det %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::mutate(date_time = lubridate::with_tz(date_time, time_zone)) %>%
      dplyr::mutate(time_zone = time_zone) %>%
      dplyr::mutate(date = lubridate::date(date_time))
  }

  all_det <- all_det %>%
    dplyr::select(reader, antenna, det_type, date, time, date_time, time_zone, dur, tag_type,
           tag_code, consec_det, no_empt_scan_prior) %>%
    dplyr::mutate(antenna = suppressWarnings(as.numeric(antenna)))  # Na introcuded by coercion because there are already nas in the data frame.

  if (exists("final_single_data")) single_data <- final_single_data else single_data <- NA
  if (exists("final_multi_data")) multi_data <- final_multi_data else multi_data <- NA
  if (exists("corrupt_single")) single_error <- corrupt_single else single_error <- NA
  if (exists("corrupt_multi")) multi_error <- corrupt_multi else multi_error <- NA
  if (exists("sa_readers")) single_readers <- sa_readers else single_readers <- NA
  if (exists("ma_readers")) multi_readers <- ma_readers else multi_readers <- NA

  if (print_to_file == TRUE) {
    write.csv(all_det, "all_det.csv", row.names = FALSE)
    write.csv(volt_dat, "volt_dat.csv", row.names = FALSE)
    write.csv(event_dat, "event_dat.csv", row.names = FALSE)
    if (exists("final_single_data"))
      write.csv(single_data, "single_data.csv", row.names = FALSE)
    if (exists("final_multi_data"))
      write.csv(multi_data, "multi_data.csv", row.names = FALSE)
    if (exists("corrupt_single")) write.csv(corrupt_single, "single_error.csv", row.names = FALSE)
    if (exists("corrupt_multi")) write.csv(corrupt_multi, "multi_error.csv", row.names = FALSE)
    if (exists("sa_readers")) write.csv(single_readers, "single_readers.csv", row.names = FALSE)
    if (exists("ma_readers")) write.csv(multi_readers, "multi_readers.csv", row.names = FALSE)
  }

  final_list <- list(all_det = all_det,
                     single_data = single_data,
                     multi_data = multi_data,
                     single_error = single_error,
                     multi_error = multi_error,
                     single_readers = single_readers,
                     multi_readers = multi_readers,
                     volt_dat = volt_dat,
                     event_dat = event_dat)
  return(final_list)

}
