#' @title Collates data from new Oregen RFID readers (>2019)
#'
#' @description Function collates data located in the working directory that
#'   were collected from new readers released in 2019. Users must define the
#'   working directory where the PIT files are located. If data is being
#'   collated from different formats, the user must specify separate working
#'   directories for each format and use the applicable PITR function to collate
#'   (see details).
#' @param data filepath of the working directory containing data in format from
#'   readers released in 2019
#' @param test_tags concatenated list of PIT codes used for testing antenna
#'   performance
#' @param print_to_file export metadata to working directory
#' @param time_zone time zone where data were collected, default is computer time zone
#' @return List of data frames consisting of detection records and event
#'   records, and a character vector of unique readers.
#' @details Data files must be named in the following format:
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
#' data <- "./PIT Data 2019/"
#'
#' # Identify the three test tags used in the study
#' tt <- c("900_230000010075", "900_230000010079", "900_230000010080")
#'
#' # Run new_pit function
#' pit_2019(data = data, test_tags = tt, print_to_file = TRUE, time_zone = "America/Vancouver")
#' @export

pit_2019 <- function(data,
                     test_tags = NULL,
                     print_to_file = FALSE,
                     time_zone = NULL) {
  
  # Create a list of raw data paths
  counter_paths <- dir(data, full.names = TRUE)
  
  # Determine how many lines to skip from each raw data file
  skipLines <- counter_paths %>% 
    # Read in the raw files as a giant character vector
    # Use quietly to store warnings instead of showing them
    purrr::map(purrr::quietly(readLines)) %>% 
    # Pull the results and drop the other list elements
    purrr::map("result") %>% 
    # Determine which lines to skip by selecting the line with DTY (Det Type)
    purrr::map(grep, pattern = "*DTY*") 
  
  # Create a function to read in the data files and skip the file-specific line
  ReadFun <- function(x, y) {
    read.table(x,
               skip = y,
               header = FALSE,
               fill = TRUE,
               stringsAsFactors = FALSE,
               skipNul = TRUE,
               col.names = c("det_type", "date", "time", "dur",
                             "tag_type", "tag_type2", "tag_code",
                             "effective_amps", "unknown")) %>%
      # Pull the reader from the filename
      dplyr::mutate(source = basename(x)) %>% 
      dplyr::mutate(reader = stringr::str_sub(source, 1, -16)) %>%
      # Create antenna column, but all get NA because the new readers only have one antenna
      dplyr::mutate(antenna = NA) %>% 
      dplyr::select(reader, antenna, det_type, date, time, dur, tag_type, tag_type2, tag_code,
                    effective_amps, unknown)
  }
  
  dat0 <- suppressWarnings(mapply(FUN = ReadFun, 
                 counter_paths, 
                 skipLines, 
                 SIMPLIFY = FALSE))
  
  # Turn the list into a data.frame
  dat1 <- dat0 %>% 
    data.table::rbindlist(., fill = TRUE)
  
  # Create Event Dataframe --------------------
  
  event_dat <- dat1 %>%
    dplyr::filter(det_type == "E") %>%
    # Collapse the columns into one description
    dplyr::mutate(desc = apply(dplyr::select(., dur:unknown), 1, paste, collapse = " ")) %>% 
    # Remove NA values
    dplyr::mutate(desc = gsub("NA", "", paste(as.character(desc)))) %>% 
    # Remove space at end of text
    dplyr::mutate(desc = gsub("^\\s+|\\s+$", "", desc)) %>% 
    dplyr::select(reader, det_type, date, time, desc) %>% 
    # Retain only unique lines
    dplyr::distinct()
  
  # Create Detection Dataframe --------------------
  
  d <- dplyr::filter(dat1, det_type %in% c("S", "I"))  # Not sure what "I" is yet
  
  # Force stop if no detections
  if (nrow(d) == 0) stop("Error: Data has no detections") 
  
  # May be temporary, but manipulate to look exactly like new_pit() output
  det_data <- d %>% 
    dplyr::mutate(consec_det = NA) %>%
    dplyr::mutate(consec_det = as.numeric(consec_det)) %>%
    dplyr::mutate(no_empt_scan_prior = NA) %>%
    dplyr::mutate(no_empt_scan_prior = as.numeric(no_empt_scan_prior)) %>%
    dplyr::mutate(antenna = as.numeric(antenna)) %>%
    dplyr::select(reader, antenna, det_type, date, time, dur, tag_type, tag_code, 
                  consec_det, no_empt_scan_prior) %>% 
    # Retain only unique lines
    dplyr::distinct()
  
  # Filter Tags --------------------
  
  # Remove test tags if they exist
  if (!(is.null(test_tags))) {
    det_data <- dplyr::filter(det_data, !(tag_code %in% test_tags))
  }
  
  # Filter out study tags
  study_tag_path <- counter_paths[grep(pattern = "study_tags", x = counter_paths)]
  
  if (length(study_tag_path) == 1) {
    
    if (grepl(pattern = ".csv", x = study_tag_path) == TRUE) study_tag_df <- read.table(study_tag_path, header = TRUE, sep = ",")
    if (grepl(pattern = ".txt", x = study_tag_path) == TRUE) study_tag_df <- read.table(study_tag_path, header = TRUE, sep = "\t")
    
    study_tags_vector <- study_tag_df$study_tags
    det_data <- dplyr::filter(det_data, tag_code %in% study_tags_vector)
  }
  
  # Create Datafames to Return --------------------
  
  # Create new column that combines date and time
  if (is.null(time_zone)) {
    det_data <- det_data %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::mutate(time_zone = Sys.timezone())
  }
  
  if (!is.null(time_zone)) {
    det_data <- det_data %>%
      dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::mutate(date_time = lubridate::with_tz(date_time, time_zone)) %>%
      dplyr::mutate(time_zone = time_zone) %>%
      dplyr::mutate(date = lubridate::date(date_time))
  }
  
  # Create final detection data frame
  all_det <- det_data %>%
    dplyr::select(reader, antenna, det_type, date, time, date_time, time_zone, dur,
                  tag_type, tag_code, consec_det, no_empt_scan_prior)
  
  if (print_to_file == TRUE) {
    write.csv(all_det, "all_det.csv", row.names = FALSE)
    write.csv(event_dat, "event_dat.csv", row.names = FALSE)
    write.csv(unique(all_det$readers), "single_readers.csv", row.names = FALSE)
  }
  
  final_list <- list(all_det = all_det,
                     event_dat = event_dat,
                     readers = unique(all_det$reader))
  
  return(final_list)
  
}
