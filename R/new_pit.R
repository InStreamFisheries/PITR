#' @title Collates data from readers in new format (>V5.0)
#'
#' @description Function collates data located in the working directory that were collected from readers with a firmware version later than 5.0 (April 2014). Users must define the working directory where the PIT files are located. If data is being collated from old and new formats, the user must specify separate working directories containing old and new data. If the function \code{\link{old_pit}} was used to collate old format data, the user must specify the output of the \code{old_pit} function using the \code{old_format_data} argument.
#' @param data filepath of the working directory containing data in new format
#' @param test_tags concatenated list of PIT codes used for testing antenna performance
#' @param print_to_file export metadata to working directory
#' @param time_zone time zone where data were collected, default is time zone of user’s computer
#' @param old_format_data include collated data in old format
#' @return List of dataframes consisting of detection records, event records, non-detection and non-event records, duplicated detections and error records from single and multi readers. Character vectors of unique single and multi readers.
#' @details Data files must be named in the following format ‘pit_reader_mm_dd_yyyy.txt’. Note that pit_reader is the unique name of the PIT reader and that mm, dd and yyyy must be separated by underscores. If .txt is not at the end of each file, the user must enter .txt manually to each file prior to reading in the data. Users can apply the \code{print_to_file} argument to export metadata to the working directory to confirm that data collation was performed correctly. Users can include a dataframe called study tags in .csv or .txt format in the working directory that contains the full list of tags used in a study in one column called ‘tag_code’. Tag codes must be complete: 900_230000010075 and not 10075, 0000_0000000183783293 and not 183783293. If no dataframe of study tags exists in the working directory, it is assumed that no tag codes are subsetted out of the dataset. Users can choose to specify the time zone where data were collected. Default for \code{time_zone} is the time zone of the user’s computer. If \code{old_format_data} is not specified, it is assumed that there are no data in the old format to collate.
#' @examples
#' #collate data located in a Dropbox folder and print metadata to working directory
#'
#' #define working directory
#' new <- "~/Dropbox (InStream)/Projects/62 - PIT R and D/5 - Data/Keogh 2016"
#'
#' #three test tags used in study
#' tt <- c("900_230000010075", "900_230000010079", "900_230000010080")
#'
#' #run pit_data function
#' new_pit(data = new, test_tags = tt, print_to_file = TRUE)
#'
#' #collate old format data with new format data
#'
#' old_data <- old_pit(data = old, test_tags = NULL, print_to_file = FALSE, time_zone = “America/Vancouver”)
#'
#' new_data <- new_pit(data = new, test_tags = tt, print_to_file = FALSE, time_zone = “America/Vancouver”, old_format_data = old_data)
#' @export

new_pit <- function (data, test_tags = NULL, print_to_file = FALSE, time_zone = NULL, old_format_data = NULL) {

  #require(stringr)
  #require(plyr)
  #require(dplyr)
  #require(lubridate)

  #Code to structure test dataset 'oregon_rfid'
  if (data == "oregon_rfid") {

    oregon_rfid$reader <- as.character(oregon_rfid$reader)
    oregon_rfid$antenna <- as.numeric(oregon_rfid$antenna)
    oregon_rfid$det_type <- as.character(oregon_rfid$det_type)
    oregon_rfid$date <- as.character(oregon_rfid$date)
    oregon_rfid$time <- as.character(oregon_rfid$time)
    oregon_rfid$date_time <- as.POSIXct(as.character(oregon_rfid$date_time), format = "%Y-%m-%d %H:%M")
    oregon_rfid$time_zone <- as.character(oregon_rfid$time_zone)
    oregon_rfid$dur <- as.character(oregon_rfid$dur)
    oregon_rfid$tag_type <- as.character(oregon_rfid$tag_type)
    oregon_rfid$tag_code <- as.character(oregon_rfid$tag_code)
    oregon_rfid$consec_det <- as.character(oregon_rfid$consec_det)
    oregon_rfid$no_empt_scan_prior <- as.character(oregon_rfid$no_empt_scan_prior)

    oregon_rfid_data = oregon_rfid

    return(oregon_rfid_data)

  }

  if (data != "oregon_rfid") {

  options(scipen = 999) # avoids scientific notation: use "options(scipen=0)" to turn back on

  #Import PIT txt files: have to specify 9 columns to avoid dropping last column
  #create directory of files in paths to folder
  counter_paths <- dir(data, full.names = TRUE)
  names(counter_paths) <- basename(counter_paths)

  f1 <- plyr::ldply(counter_paths,
                    read.table,
                    header=FALSE,
                    fill=TRUE,
                    stringsAsFactors=FALSE,
                    skipNul = TRUE,
                    col.names=c("det_type", "date", "time", "dur", "tag_type", "tag_code", "antenna", "consec_det", "no_empt_scan_prior"))

  #Create unique ID for each reader fromf file names
  f1$reader <- str_sub(f1$.id,1,-16)

  #Create new data frame dropping .id (filname) and moving reader name to left side
  x <- data.frame(f1[,c(11,2:10)])

  # This data frame contains all data including detections, events, and other
  # The following code needs to separate out the three types of data as well as further separate and manipulate single and multiplexer data
  # becuase x contains all data but some of the columns are mixed up due to the different data types

  #########
  #EVENTS #
  #########

  e1 <- filter(x,x$det_type=="E")

  # Make new object with columns from E that are separated text
  text <- select(e1,5:10)

  # Combine these columns back into matrix of text strings
  desc <- apply(text, 1, paste, collapse=" ")

  # Insert combined text to replace sepearate text columns in E
  e2 <- cbind(e1[,c(1:4)], desc)

  # Convert combined text from factor to character
  e2 <- mutate(e2,desc=as.character(desc))

  # Remove NA and extra spaces at end of combined text
  e2$desc <- gsub("NA", "", paste(e2$desc))
  e2$desc<- gsub("^\\s+|\\s+$", "", e2$desc)

  # Remove duplicate rows
  e <- e2 %>% distinct(reader, date, time, desc, .keep_all=TRUE)

  # Create list of rows that were duplicates
  e_dup <- e2[duplicated(e2[1:5]) | duplicated(e2[1:5], fromLast=TRUE),]

  # Filter voltage events for voltage plots
  v <- filter(e, grepl('V',desc))

  if (is.null(time_zone)) {
    v$date <- as.Date(v$date)
  }

  if (!is.null(time_zone)) {
    v$date <- as.Date(v$date, tz = time_zone)
  }


  # Select only voltage numbers for plotting
  v$volt <- str_sub(v$desc,-5,-2)
  v$volt <- as.numeric(gsub("^\\s+|\\s+$", "", v$volt))

  v$time <- str_sub(v$time,1,8)

  if (is.null(time_zone)) {
    v$date_time <- as.POSIXct(paste(v$date, v$time, sep=" "), format="%Y-%m-%d %H:%M:%S")
  }

  if (!is.null(time_zone)) {
    v$date_time <- as.POSIXct(paste(v$date, v$time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz = time_zone)
  }

  ##################
  # DETECTIONS #####
  ##################

  d <- filter(x,x$det_type=="D")

  if (nrow(d) == 0) stop("Error: Data has no detections")

  # Rbind in old data from "Old PIT data conversion.R" if old data present
  if (! is.null(old_format_data)) {
    old_data <- old_format_data$old_dat
    old_data <- select(old_data,reader,det_type,date,time,dur,tag_type,tag_code,antenna,consec_det,no_empt_scan_prior)
    old_data$antenna <- paste("A",old_data$antenna,sep = "") # Add back in the A so that the old data filters properly
    d <- rbind(d,old_data)
  }


  # Make blank values in last column NA (single antennas do not have 'antenna' values so last two columns are shifted down)
  d[d==""] <- NA

  # Create data frames of single antenna data and multiplexer data and wrap in if statements
  sa1 <- filter(d, !(antenna %in% c("A1","A2", "A3", "A4")) & !(is.na(antenna)))
  ma1 <- filter(d,antenna %in% c("A1", "A2", "A3", "A4"))

  # 1. Only single antenna data
  if ( (nrow(sa1) > 0 ) & (nrow(ma1) == 0) ) {

    #Change column names
    names(sa1) [8]<- c("consec_det")
    names(sa1) [9]<- c("no_empt_scan_prior")

    # Because the single antenna data doesn't have an antenna number a new column of NA must be created
    sa1$antenna <- "NA"
    sa2 <- data.frame(sa1[,c(1:7,11,8,9)]) # Rearrange to match multiplexer column structure

    # consec_det and no_empt_scan_prior columns to numeric
    sa2$consec_det <- as.numeric(sa2$consec_det)

    # Remove duplicate rows
    sa3 <- sa2 %>% distinct(reader, date, time, dur, tag_type, tag_code, consec_det, no_empt_scan_prior, .keep_all=TRUE)

    #Create list of rows that were duplicates
    sa_dup<- sa2[duplicated(sa2[1:10]) | duplicated(sa2[1:10], fromLast=TRUE),]

    #Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
    sa_err <- filter(d,is.na(antenna))

    #Create list of readers for single antennas for user to double check
    sa_readers <- unique(sa1$reader)

    ##########
    # Pull "other" detections and recover any corrupt data
    o1 <- filter(x, !(det_type %in% c("D","E")))

    # Only retain unique rows
    o2 <- o1 %>% distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

    # Select for rows that 'H' in tag_type column (this selects detection that have corrupted data in our test files)
    o3 <- filter(o2, grepl("H", tag_type))

    if ( nrow(o3) > 0) {
      # Change det_type to D to fix corrupt code
      o3$det_type<- "D"

      # Select rows that are from single readers
      sa_re1 <- filter(o3, !(grepl ("A",antenna)))

      # Shift numbers down 2 columns to match sa data
      names(sa_re1) [8] <- c("consec_det")
      names(sa_re1) [9] <- c("no_empt_scan_prior")
      sa_re1$antenna <- "NA"
      sa_re <- data.frame(sa_re1[,c(1:7,11,8,9)])
      # Rbind sa_re to single reader data
      sa <- rbind(sa3,sa_re)
    }

    if (nrow(o3) ==0) {
      sa <- sa3
    }
    xx <- sa

  }


  # 1. Only multiplexer data
  if ( (nrow(ma1) > 0 ) & (nrow(sa1) == 0) ) {

    # Filter complete cases
    ma2 <- filter(ma1,complete.cases(no_empt_scan_prior))

    #Remove duplicate rows
    ma3 <- ma2 %>% distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

    #Create list of rows that were duplicates
    ma_dup <- ma2[duplicated(ma2[1:10]) | duplicated(ma2[1:10], fromLast=TRUE),]

    #Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
    ma_err <- filter(ma1,!(antenna %in% c("A1","A2", "A3", "A4")))

    #Create list of filenames for single antennas for user to double check
    ma_readers <- unique(ma2$reader)

    #############
    # Pull "other" detections and recover any corrupt data
    o1 <- filter(x, !(det_type %in% c("D","E")))

    # Only retain unique rows
    o2 <- o1 %>% distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

    # Select for rows that 'H' in tag_type column (this selects detection that have corrupted data in our test files)
    o3 <- filter(o2, grepl("H", tag_type))

    if ( nrow(o3) > 0) { # i.e., there are corrupt data
      # Change det_type to D to fix corrupt code
      o3$det_type<- "D"
      # Select rows that are from multiplexers
      ma_re<- o3 %>% filter(grepl("A",antenna))

      # Rbind ma_re to multiplexer data
      ma <- rbind(ma3,ma_re)

      # Remove 'A' from antenna values and convert to numeric
      ma$antenna <- as.numeric(substr(ma$antenna, 2, 2))
    }

    if (nrow(o3) ==0) { # i.e., there are no corrupt data
      ma <- ma3
      ma$antenna <- as.numeric(substr(ma$antenna, 2, 2))

    }
    xx <- ma
  }

  # 1. Both single and multiplexer data
  if ( (nrow(ma1) > 0 ) & (nrow(sa1) > 0) ) {

    #Change column names
    names(sa1) [8]<- c("consec_det")
    names(sa1) [9]<- c("no_empt_scan_prior")

    # Because the single antenna data doesn't have an antenna number a new column of NA must be created
    sa1$antenna <- "NA"
    sa2 <- data.frame(sa1[,c(1:7,11,8,9)]) # Rearrange to match multiplexer column structure

    # consec_det and no_empt_scan_prior columns to numeric
    sa2$consec_det <- as.numeric(sa2$consec_det)

    # Remove duplicate rows
    sa3 <- sa2 %>% distinct(reader, date, time, dur, tag_type, tag_code, consec_det, no_empt_scan_prior, .keep_all=TRUE)

    #Create list of rows that were duplicates
    sa_dup<- sa2[duplicated(sa2[1:10]) | duplicated(sa2[1:10], fromLast=TRUE),]

    #Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
    sa_err <- filter(d,is.na(antenna))

    #Create list of readers for single antennas for user to double check
    sa_readers <- unique(sa1$reader)

    # Mulitplexer
    ma2 <- filter(ma1,complete.cases(no_empt_scan_prior))

    #Remove duplicate rows
    ma3 <- ma2 %>% distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

    #Create list of rows that were duplicates
    ma_dup <- ma2[duplicated(ma2[1:10]) | duplicated(ma2[1:10], fromLast=TRUE),]

    #Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
    ma_err <- filter(ma1,!(antenna %in% c("A1","A2", "A3", "A4")))

    #Create list of filenames for single antennas for user to double check
    ma_readers <- unique(ma2$reader)

    # Pull "other" detections and recover any corrupt data
    o1 <- filter(x, !(det_type %in% c("D","E")))

    # Only retain unique rows
    o2 <- o1 %>% distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

    # Select for rows that 'H' in tag_type column (this selects detection that have corrupted data in our test files)
    o3 <- filter(o2, grepl("H", tag_type))

    if ( nrow(o3) > 0) { # i.e., there are corrupt data
      # Change det_type to D to fix corrupt code
      o3$det_type<- "D"

      # Select rows that are from single readers
      sa_re1 <- filter(o3, !(grepl ("A",antenna)))
      ma_re1 <- o3 %>% filter(grepl("A",antenna))

      # A. Corrupt single antenna data
      if ( nrow(sa_re1) > 0 ) { # fix corrupt single antenna data

        # Shift numbers down 2 columns to match sa data
        names(sa_re1) [8] <- c("consec_det")
        names(sa_re1) [9] <- c("no_empt_scan_prior")
        sa_re1$antenna <- "NA"
        sa_re <- data.frame(sa_re1[,c(1:7,11,8,9)])

        # Rbind sa_re to single reader data
        sa <- rbind(sa3,sa_re)
      }

      # B. Corrupt multiplexer data
      if ( nrow(ma_re1) > 0 ) { # Fix corrupt multiplexer data

        # Select rows that are from multiplexers
        ma_re<- o3 %>% filter(grepl("A",antenna))

        # Rbind ma_re to multiplexer data
        ma <- rbind(ma3,ma_re)

        # Remove 'A' from antenna values and convert to numeric
        ma$antenna <- as.numeric(substr(ma$antenna, 2, 2))
      }
    }

    if (nrow(o3) == 0) { # i.e., no corrupt data
      ma <- ma3
      ma$antenna <- as.numeric(substr(ma$antenna, 2, 2))

      sa <- sa3
    }

    xx <- rbind(ma,sa)
  }

  #########################################
  # Final manipulations and printing

  # Filter out proper tag codes
  # Not sure why Joel does this at the very end instead of beginning, but I'll leave it here and also remove the non-study tags
  # Remove test tags if they exist
  if ( !(is.null(test_tags)) ) {
    xx <- filter(xx,!(tag_code %in% test_tags))
  }

  ############################
  # Filter out study tags
  # Study tags need to be in a file of either txt or csv study_tags needs to be a column and also needs to be the name of the column with tags
  study_tag_path <- counter_paths[grep(pattern = "study_tags", x = counter_paths)]

  if (length(study_tag_path) == 1) {

    if (grepl(pattern = ".csv", x = study_tag_path) == TRUE)   study_tag_df <- read.table(study_tag_path, head = TRUE, sep = ",")
    if (grepl(pattern = ".txt", x = study_tag_path) == TRUE)   study_tag_df <- read.table(study_tag_path, head = TRUE, sep = "\t")

    study_tags_vector <- study_tag_df$study_tags
    xx <- filter(xx, tag_code %in% study_tags_vector)
  }
  ###############################

  all_det <- xx

  # Make antenna numeric (note: warning is supressed because the code is changing "NA" into a proper NA
  all_det$antenna <- suppressWarnings(as.numeric(all_det$antenna)) # Na introcuded by coercion because there are already nas in the data frame.

  # Create new column that combines date and time
  if (is.null(time_zone)) {
    all_det$date_time <- as.POSIXct(paste(all_det$date, all_det$time, sep=" "), format="%Y-%m-%d %H:%M:%S")
    all_det$time_zone <- Sys.timezone()
  }

  if (!is.null(time_zone)) {
    all_det$date_time <- as.POSIXct(paste(all_det$date, all_det$time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz = time_zone)
    all_det$time_zone <- time_zone
  }


  # Move date_time to be beside date and time
  all_det <- select(all_det, reader,antenna,det_type,date,time,date_time,time_zone,dur,tag_type,tag_code,consec_det,no_empt_scan_prior)

  # Create a list of objects to return
  # May need to create NULL values for objects that were skipped because of if statements
  if (exists("sa3")) single_data <- sa3                  else single_data <- NA
  if (exists("ma3")) multi_data <- ma3                   else multi_data <- NA
  if (exists("sa_dup")) single_duplicate <- sa_dup       else single_duplicate <- NA
  if (exists("ma_dup")) multi_duplicate <- ma_dup        else multi_duplicate <- NA
  if (exists("sa_err")) single_error <- sa_err           else single_error <- NA
  if (exists("ma_err")) multi_error <- ma_err            else multi_error <- NA
  if (exists("sa_readers")) single_readers <- sa_readers else single_readers <- NA
  if (exists("ma_readers")) multi_readers <- ma_readers  else multi_readers <- NA

  # The following objects should always be present
  volt_dat <- v
  event_dat <- e
  other_dat <- o1

  # Print the objects to file if print_to_file is TRUE
  if (print_to_file == TRUE) {
    write.csv(all_det,"all_det.csv",row.names=FALSE)
    write.csv(volt_dat,"volt_dat.csv",row.names=FALSE)
    write.csv(event_dat,"event_dat.csv",row.names=FALSE)
    write.csv(other_dat,"other_dat.csv",row.names=FALSE)
    if (exists("sa3")) write.csv(sa3,"single_data.csv",row.names=FALSE)
    if (exists("ma3")) write.csv(ma3, "multi_data.csv", row.names=FALSE)
    if (exists("sa_dup")) write.csv(sa_dup, "single_duplicate.csv",row.names=FALSE)
    if (exists("ma_dup")) write.csv(ma_dup, "multi_duplicate.csv", row.names=FALSE)
    if (exists("sa_err")) write.csv(sa_err, "single_error.csv",row.names=FALSE)
    if (exists("ma_err")) write.csv(ma_err, "multi_error.csv",row.names=FALSE)
    if (exists("sa_readers")) write.csv(sa_readers, "single_readers.csv", row.names=FALSE)
    if (exists("ma_readers")) write.csv(ma_readers, "multi_readers.csv", row.names=FALSE)
  }

  final_list <- list(all_det=all_det,
                     single_data=single_data,multi_data=multi_data,
                     single_duplicate=single_duplicate,multi_duplicate=multi_duplicate,
                     single_error=single_error,multi_error=multi_error,
                     single_readers=single_readers,multi_readers=multi_readers,
                     volt_dat=volt_dat,event_dat=event_dat,other_dat=other_dat)
  return(final_list)

  }
}
