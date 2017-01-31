#' @title Collates data from readers in old format (<V5.0)
#'
#' @description Function collates data located in the working directory that were collected from readers with a firmware version earlier than 5.0 (April 2014). Users must define the working directory where the PIT files are located. If data is being collated from old and new formats, the user must specify separate working directories containing old and new data.
#' @param path_to_folder filepath of the working directory containing data in old format
#' @param test_tags concatenated list of PIT codes used for testing antenna performance
#' @param print_to_file export metadata to working directory
#' @param time_zone time zone where data were collected, default is time zone of user’s computer
#' @return List of dataframes consisting of detection records, event records, non-detection and non-event records, duplicated detections, error records, and a character vector of unique readers.
#' @details Data files must be in the format ‘pit_reader_mm_dd_yyyy.txt’. Note that ‘pit_reader’ is the unique name of the PIT reader and that mm, dd and yyyy must be separated by underscores. If .txt is not at the end of each file, the user must enter .txt manually to each file prior to uploading the data. Users can apply the \code{print_to_file} argument to export metadata to the working directory to confirm that data collation was performed correctly. Users can choose to specify the time zone where data were collected. Default for time_zone is the time zone of the user’s computer.
#' @examples
#' #collate data located in a Dropbox folder
#' ptf_old <- "~/Dropbox (InStream)/Projects/62 - PIT R and D/5 - Data/Keogh Old"
#'
#' #no test tags, no metadata to print to working director, data were collected in the time zone ‘America/Vancouver’
#' old_pit(path_to_folder = ptf_old, test_tags = NULL, print_to_file = FALSE, time_zone = “America/Vancouver”)
#' @export


old_pit <- function (path_to_folder, test_tags = NULL, print_to_file = FALSE, time_zone = NULL) {

  #require(stringr)
  #require(plyr)
  #require(dplyr)
  #require(lubridate)


  #avoids scientific notation: use "options(scipen=0)" to turn back on
  options(scipen=999)

  #Import PIT txt files: have to specify 9 columns to avoid dropping last column
  #create directory of files in paths to folder
  counter_paths <- dir(path_to_folder, full.names = TRUE)
  names(counter_paths) <- basename(counter_paths)

  o_f1<- plyr::ldply(counter_paths,
                     read.table,
                     header=FALSE,
                     fill=TRUE,
                     stringsAsFactors=FALSE,
                     col.names=c("date", "time", "dur", "tag_type", "tag_code", "antenna", "consec_det", "no_empt_scan_prior"))

  #Create unique ID for each reader from file names, make sure all file names have .txt or .log at end
  o_f1$reader <- str_sub(o_f1$.id,1,-16)

  #Create new data frame dropping .id (filname) and moving reader name to left side
  o_x <- data.frame(o_f1[,c(10,2:9)])

  #Values other than dates in data file (NEED TO UPDATE THIS SO WE CAN SELECT ANYTHING WITH A DATE FORMAT AND AVOID DOING THIS EACH TIME)
  not_date <- c("=~=~=~=~=~=~=~=~=~=~=~=", ">upnew", "upload", "Site", "---------", "0.7A", "0.8A", "1.3A", "1.2A", "1.1A", ">")

  #########################
  ######OLD DETECTIONS#####
  #########################

  #Assign D to detection based on tag type "R" or "A"
  o_x$det_type <- ifelse(o_x$tag_type %in% c("R","A"),"D","NA") # This is what will prevent other data bit located in teh working directory from messing up your df

  #Filter out detections
  o_d <- filter(o_x,det_type=="D")

  if (nrow(o_d) == 0) stop("Error: Data has no detections")

  #Arrange columns to match new data
  o_d1 <- data.frame(o_d[,c(1,10,2:9)])

  #Need to change date format here from MM/DD/YYYY to YYYY-MM-DD
  o_d1$date <- as.character(as.Date(o_d1$date, format="%m/%d/%Y"))

  # Add a time zone
  if (! is.null(time_zone)) {
    o_d1$date_time <- as.POSIXct(paste(o_d1$date, o_d1$time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz = time_zone)
    o_d1$time_zone <- time_zone
  }

  if (is.null(time_zone)) {
    o_d1$date_time <- as.POSIXct(paste(o_d1$date, o_d1$time, sep=" "), format="%Y-%m-%d %H:%M:%S")
    o_d1$time_zone <- Sys.timezone()
  }

  ##############################################
  # Remove test tags if they exist
  if ( !(is.null(test_tags)) ) {
    o_d1 <- filter(o_d1,!(tag_code %in% test_tags))
  }

  # Filter out study tags if they exist
  # Study tags need to be in a file of either txt or csv study_tags needs to be a column and also needs to be the name of the column with tags
  study_tag_path <- counter_paths[grep(pattern = "study_tags", x = counter_paths)]

  if (length(study_tag_path) == 1) {

    if (grepl(pattern = ".csv", x = study_tag_path) == TRUE)   study_tag_df <- read.table(study_tag_path, head = TRUE, sep = ",")
    if (grepl(pattern = ".txt", x = study_tag_path) == TRUE)   study_tag_df <- read.table(study_tag_path, head = TRUE, sep = "\t")

    study_tags_vector <- study_tag_df$study_tags
    o_d1 <- filter(o_d1, tag_code %in% study_tags_vector)
  }

  #
  # Filter complete cases
  o_d1 <- filter(o_d1,complete.cases(no_empt_scan_prior))

  #Remove duplicate rows
  o_d1 <- o_d1 %>% distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

  #Create list of rows that were duplicates
  o_d_dup <- o_d1[duplicated(o_d1[1:10]) | duplicated(o_d1[1:10], fromLast=TRUE),]

  #Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
  o_d_err <- filter(o_d1,!(antenna %in% c("A1","A2", "A3", "A4")))

  #Create list of filenames for single antennas for user to double check
  readers <- unique(o_d1$reader)

  #Turn antenna numeric
  o_d1$antenna <- as.numeric(substr(o_d1$antenna, 2, 2))

  o_d1 <- select(o_d1,reader,antenna,det_type,date,time,date_time,time_zone,dur,tag_type,tag_code,consec_det,no_empt_scan_prior)

  ###############################

  ####################
  ######OLD OTHER#####
  ####################

  #Filter other lines (junk)
  o_o <- filter(o_x, date %in% not_date)

  if (nrow(o_o) > 0) {
    o_o$det_type <- "O"
    o_o1 <- o_o
  }

  #####################
  ######OLD EVENTS#####
  #####################

  #Filter event lines
  o_e <- filter(o_x, !(date %in% not_date) & det_type =="NA")

  if (nrow(o_e) > 0) {
    o_e$det_type <- "E"

    #Arrange columns to match new data
    o_e1 <- data.frame(o_e[,c(1,10,2:9)])

    #Need to change date format here from MM/DD/YYYY to YYYY-MM-DD
    o_e1$date <- as.character(as.Date(o_e1$date, format="%m/%d/%Y"))
  }

  # Set up final data frames and return as list and print to file if requested
  if (exists("o_e1")) old_events <- o_e1 else old_events <- NA
  if (exists("o_o1")) old_other <- o_o1 else old_other <- NA
  if (exists("o_d_dup")) old_duplicate <- o_d_dup else old_duplicate <- NA
  if (exists("o_d_err")) old_error <- o_d_err  else old_error <- NA
  if (exists("readers")) old_readers <- readers else old_readers <- NA

  # Old data should always be present
  old_dat <- o_d1

  # Write files if requested
  if (print_to_file == TRUE) {
    write.csv(old_dat,"old_dat.csv",row.names=FALSE)
    if (exists("o_e1")) write.csv(o_e1,"old_events.csv",row.names=FALSE)
    if (exists("o_o1")) write.csv(o_o1, "old_other.csv", row.names=FALSE)
    if (exists("o_d_dup")) write.csv(o_d_dup,"old_duplicates.csv",row.names=FALSE)
    if (exists("o_d_err")) write.csv(o_d_err,"old_error.csv",row.names=FALSE)
    if (exists("readers")) write.csv(readers,"old_readers.csv",row.names=FALSE)
  }

  final_list <- list(old_dat = old_dat,
                     old_events = old_events,
                     old_other = old_other,
                     old_duplicate = old_duplicate,
                     old_error = old_error,
                     old_readers = old_readers)

  return(final_list)

}
