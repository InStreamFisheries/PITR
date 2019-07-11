#' @title Collates data from readers in format (<V5.0) prior to April 2014
#'
#' @description Function collates data located in the working directory that
#'   were collected from readers with a firmware version earlier than 5.0 (April
#'   2014). Users must define the working directory where the PIT files are
#'   located. If data is being collated from different formats, the user must
#'   specify separate working directories for each format and use the applicable
#'   PITR function to collate (see details).
#' @param data filepath of the working directory containing data in old format
#' @param test_tags concatenated list of PIT codes used for testing antenna
#'   performance
#' @param print_to_file export metadata to working directory
#' @param time_zone time zone where data were collected, default is computer time zone
#' @return List of data frames consisting of detection records, event records,
#'   error records, and a character vector of unique readers.
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
#' # Define working directory
#' old <- "./Old PIT Data/"
#'
#' # No test tags, no metadata to print to working directory, data were collected in the time zone "America/Vancouver"
#' old_pit(data = old, test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")
#' @export


old_pit <- function (data,
                     test_tags = NULL,
                     print_to_file = FALSE,
                     time_zone = NULL) {

  counter_paths <- dir(data, full.names = TRUE)
  names(counter_paths) <- basename(counter_paths)

  o_f1 <- plyr::ldply(counter_paths,
                      read.table,
                      header = FALSE,
                      fill = TRUE,
                      stringsAsFactors = FALSE,
                      col.names = c("date", "time", "dur", "tag_type", "tag_code",
                                    "antenna", "consec_det", "no_empt_scan_prior"))

  o_x <- o_f1 %>%
    dplyr::mutate(reader = stringr::str_sub(.id, 1, -16)) %>% # Create unique ID for each reader from file names
    dplyr::select(reader, date, time, dur, tag_type, tag_code, antenna, consec_det,
                  no_empt_scan_prior)

  # Detections --------------------

  # Assign D to detections with tag type "R" or "A", then filter out the detections
  o_x$det_type <- ifelse(o_x$tag_type %in% c("R", "A"), "D", "NA")

  o_d <- dplyr::filter(o_x, det_type == "D")

  if (nrow(o_d) == 0) stop("Error: Data has no detections")

  # Arrange columns to match new data
  o_d <- o_d %>%
    dplyr::select(reader, det_type, date, time, dur, tag_type, tag_code, antenna,
                  consec_det, no_empt_scan_prior) %>%
    dplyr::mutate(date = lubridate::mdy(date))

  # Add a time zone
  if (!is.null(time_zone)) {
    o_d <- o_d %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::mutate(date_time = lubridate::with_tz(date_time, time_zone)) %>%
      dplyr::mutate(time_zone = time_zone) %>%
      dplyr::mutate(date = lubridate::date(date_time))
  }

  if (is.null(time_zone)) {
    o_d <- o_d %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::mutate(time_zone = Sys.timezone())
  }

  # Remove test tags if they exist
  if (!(is.null(test_tags))) {
    o_d <- filter(o_d, !(tag_code %in% test_tags))
  }

  # Filter out study tags if they exist
  study_tag_path <- counter_paths[grep(pattern = "study_tags", x = counter_paths)]

  if (length(study_tag_path) == 1) {

    if (grepl(pattern = ".csv", x = study_tag_path) == TRUE)  study_tag_df <- read.table(study_tag_path, header = TRUE, sep = ",")
    if (grepl(pattern = ".txt", x = study_tag_path) == TRUE)  study_tag_df <- read.table(study_tag_path, header = TRUE, sep = "\t")

    study_tags_vector <- study_tag_df$study_tags
    o_d <- dplyr::filter(o_d, tag_code %in% study_tags_vector)
  }

  # Filter complete cases
  o_d <- dplyr::filter(o_d, complete.cases(no_empt_scan_prior))

  # Remove duplicate rows
  o_d <- o_d %>%
    dplyr::distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det,
                    no_empt_scan_prior, .keep_all = TRUE)

  # Filter rows that have incorrect values for antenna (user can look over and
  # decide to correct in raw data if important)
  o_d_err <- dplyr::filter(o_d, !(antenna %in% c("A1", "A2", "A3", "A4")))

  # Create list of filenames for single antennas for user to double check
  readers <- unique(o_d$reader)

  old_dat <- o_d %>%
    dplyr::mutate(antenna = as.numeric(substr(antenna, 2, 2))) %>%   # Now remove "A" from antenna
    dplyr::select(reader, antenna, det_type, date, time, date_time, time_zone, dur,
                  tag_type, tag_code, consec_det, no_empt_scan_prior)

  # Events --------------------

  # Filter event lines; identify which values are not dates; used to use this to subset "other"
  not_dates <- which(is.na(suppressWarnings(lubridate::mdy(o_x$date))))
  o_e <- dplyr::filter(o_x[!not_dates, ], det_type == "NA")

  if (nrow(o_e) > 0) {
    o_e <- o_e %>%
      dplyr::mutate(det_type = "E") %>%
      dplyr::select(reader, det_type, date, time, dur, tag_type, tag_code, antenna,
                    consec_det, no_empt_scan_prior) %>%
      dplyr::mutate(date = lubridate::mdy(date))
  }

  if (exists("o_e")) old_events <- o_e else old_events <- NA
  if (exists("o_d_err")) old_error <- o_d_err  else old_error <- NA
  if (exists("readers")) old_readers <- readers else old_readers <- NA

  if (print_to_file == TRUE) {
    write.csv(old_dat, "old_dat.csv", row.names = FALSE)
    if (exists("o_e")) write.csv(o_e, "old_all_det.csv", row.names = FALSE)
    if (exists("o_d_err")) write.csv(o_d_err, "old_error.csv", row.names = FALSE)
    if (exists("readers")) write.csv(readers, "old_readers.csv", row.names = FALSE)
  }

  final_list <- list(all_det = old_dat,
                     event_dat = old_events,
                     error = old_error,
                     readers = old_readers)

  return(final_list)

}
