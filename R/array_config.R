#' @title Restructure the configuration of arrays
#'
#' @description Function allows users to combine unique readers into an array, split readers with multiple antennas into single antennas, and rename up to four antennas on one reader or one array. Use of this function allows users to manage data for further analysis using \code{\link{det_eff}}, \code{\link{direction}}, \code{\link{direction_total}}, and \code{\link{first_last}} functions.
#' @param dat telemetry dataset created using \code{\link{old_pit}} or \code{\link{new_pit}} function
#' @param configuration either \code{combine}, \code{split} or \code{rename_antennas}
#' @param array_name unique name of an array
#' @param r1 name of reader 1
#' @param r2 name of reader 2
#' @param r3 name of reader 3
#' @param r4 name of reader 4
#' @param reader_name name of reader to split
#' @param new_reader_1_antennas specific antennas to be grouped into reader 1
#' @param ao1 old antenna 1
#' @param ao2 old antenna 2
#' @param ao3 old antenna 3
#' @param ao4 old antenna 4
#' @param an1 new antenna 1
#' @param an2 new antenna 2
#' @param an3 new antenna 3
#' @param an4 new antenna 4
#' @return An updated dataset for further analysis using \code{\link{det_eff}}, \code{\link{direction}}, \code{\link{direction_total}} and \code{\link{first_last}} functions.
#' @details Function is dependent on what the user defines in the \code{configuration} argument. Argument \code{combine} is used to combine up to four single readers into one array. If a user wants to combine a multi reader with another reader (either a multi reader or single reader), the user first must split the multi reader into two or more single readers and then combine them together into an array. Argument \code{split} is used to split multi readers into two or more single readers to allow the combination of single readers into a user-specified array. Argument \code{new_reader_1_antennas} is used to define the antennas to group into reader 1. All other antennas are grouped into reader 2. Argument \code{rename_antennas} is used to either rename antennas part of an array (if \code{array_name} is specified) or antennas part of a reader (if \code{reader_name} is specified). Users can rename up to four antennas for one reader or one array. Arguments with the prefix ‘ao’ correspond to the old (or current) antenna numbering scheme, whereas arguments with the prefix ‘an’ correspond to the new antenna numbering scheme. Old antennas must be specified in order within the \code{array_config} function. Depending on the setup of the study, the user may have to run the \code{array_config} function several times. Note that use of arguments \code{combine}, \code{split} and \code{rename_antennas} must be iterative (see Examples). Ultimately the \code{array_config} function outputs an updated dataset to be used for further analysis.
#' @examples
#' #combine two single readers into an array called ‘bridge_reach34’
#'array_config(new$all_det, configuration = "combine", array_name = "bridge_reach34", r1 = "bridge_reach34_ant1", r2 = "bridge_reach34_ant2")
#'
#' #split the ‘bridge_counter’ multi reader into two single readers; include A2 into reader #1
#' array_config(new$all_det, configuration = "split", reader_name = "bridge_counter", new_reader_1_antennas = c(2))
#' #or
#' array_config(new$all_det, configuration = "split", reader_name = "bridge_counter", new_reader_1_antennas = 2)
#'
#' #within the ‘bridge_counter’ array, rename the old A1 to A2 and the old A2 to A1
#' array_config(new$all_det, configuration = "rename_antennas", array_name = "bridge_counter", ao1 = 2, ao2 = 1, an1 = 1, an2 = 2)
#' #within the ‘bridge_reach34_ant2’ reader, rename the old A2 to A1
#' array_config(new$all_det, configuration = "rename_antennas", reader_name = "bridge_reach34_ant2", ao1 = 2, an1 = 1)
#' @export

array_config <- function (dat, configuration, array_name=NULL, r1=NULL, r2=NULL, r3=NULL, r4=NULL,
                          reader_name=NULL, new_reader_1_antennas=NULL,
                          ao1=NULL, ao2=NULL, ao3=NULL, ao4=NULL, an1=NULL, an2=NULL, an3=NULL, an4=NULL) {

  # Combine up to four readers with single antennas into one array.
  # The readers should be inputed in order so that the new antenna numbers 1,2,3,4 are in order of downstream to upstream

  if (is.null(configuration)) stop("Error: configuration must be specified")

  if (configuration == "combine") {

    if (is.null(arrayname)) stop("Error: arrayname must be specified")

    if (!is.null(arrayname)) {

      # Select data that comes from readers you want to combine
      rd <- filter(dat, reader %in% c(r1,r2,r3,r4))
      # Select all other data to merge back in later
      rr <- filter(dat, !(reader %in% c(r1,r2,r3,r4)))

      # Assign new antenna numbers based on readers (r1, r2, r3 and r4 get antenna 3"s 1,2,3 and 4 respectively)

      # If r1 exists, give new antenna number 1 (so it would be antenna 1 on "new PIT array")
      if (!is.null(r1)) {
        x1 <- which(rd$reader == r1)
        rd$antenna[x1] <- 1
      }

      # If r2 exists, give new antenna number 2 (so it would be antenna 2 on "new PIT array")
      if (!is.null(r2)) {
        x2 <- which(rd$reader == r2)
        rd$antenna[x2] <- 2
      }


      # If r3 exists, give new antenna number 3 (so it would be antenna 3 on "new PIT array")
      if (!is.null(r3)) {
        x3 <- which(rd$reader == r3)
        rd$antenna[x3] <- 3
      }

      # If r4 exists, give new antenna number 4 (so it would be antenna 4 on "new PIT array")
      if (!is.null(r4)) {
        x4 <- which(rd$reader == r4)
        rd$antenna[x4] <- 4
      }

      # All get new array name (becasue it is being combinerd into one PIT array)
      rd$array <- arrayname

      # The array name of the rest of the data is defaulted to the reader name
      # Unless the function has already been run through and an array column has been created, in which case the array column is left as is
      if ("array" %in% names(rr)) rr$array<-rr$array else rr$array<-rr$reader

      nc <- rbind(rd,rr)

      # Reorder columns
      nc <- select(nc,array,reader,antenna,det_type,date,time,date_time,time_zone,dur,tag_type,tag_code,consec_det,no_empt_scan_prior)

      array_summary(nc)

      return(nc)

    }
  }

  if (configuration == "split") {

    if (is.null(readername)) stop("Error: reader name must be specified")

    if (!is.null(readername)) {

      if(is.null(newreader1antennas)) stop("Error: Must specify which antenna(s) will become part of the new reader 1")

      # Select data that comes from reader you want to split in two
      rd <- filter(dat,reader == readername)
      # Select all other data to merge back in later
      rr <- filter(dat, reader != readername)

      # Assign new reader names based on antenna specifications (ao1,a02,and a03 get "reader"_1; all others get get "reader"_2)
      if (!is.null(newreader1antennas)) {
        rd$reader <- ifelse(rd$antenna == newreader1antennas, paste(readername,"1",sep="_"), paste(readername,"2",sep="_"))
      }

      # Create an array column if one doesn't exist, if the array column does exist then default to it here
      if ("array" %in% names(rr)) rr$array<-rr$array else rr$array<-rr$reader
      if ("array" %in% names(rd)) rd$array<-rd$array else rd$array<-rd$reader

      nc<- rbind(rd,rr)

      array_summary(nc)

      nc <- select(nc,array,reader,antenna,det_type,date,time,date_time,time_zone,dur,tag_type,tag_code,consec_det,no_empt_scan_prior)

      return(nc)
    }
  }

  if (configuration == "renameantennas") {

    if (!is.null(readername) & !is.null(arrayname)) stop("Error: Only specify one array or one reader with antennas to rename")

    if (is.null(readername) & is.null(arrayname)) stop("Error: Must specify a reader or array with antennas to rename")


    if (!is.null(readername)) {

      # Select data that comes from reader you want to reconfig antennas for
      rd <- filter(dat, reader == readername)

      # Select all other data to merge back in later
      rr <- filter(dat, reader != readername)

      #Assign new antenna numbers based on number entries (ao1-> an1, ao2-> an2, ao3 -> an3, a04 -> an4)

      # If ao1 exists, give new antenna number an1
      if (!is.null(ao1)) {
        df1 <- rd
        x1 <- which(df1$antenna == ao1)
        df1$antenna[x1] <- an1
      }

      # If ao2 exists, give new antenna number an2
      if (!is.null(ao2)) {
        df2 <- rd
        x2 <- which(df2$antenna == ao2)
        df2$antenna[x2] <- an2
      }

      # If ao3 exists, give new antenna number an3
      if (!is.null(ao3)) {
        df3 <- rd
        x3 <- which(df3$antenna == ao3)
        df3$antenna[x3] <- an3
      }

      #If ao4 exists, give new antenna number an4
      if (!is.null(ao4)) {
        df4 <- rd
        x4 <- which(df4$antenna == ao4)
        df4$antenna[x4] <- an4
      }

      # Combine the data frames
      rd <- df1
      if (exists("df2")) rd <- rbind(df1,df2)
      if (exists("df3")) rd <- rbind(df1,df2,df3)
      if (exists("df4")) rd <- rbind(df1,df2,df3,df4)

      # Create an array column if one doesn't exist, if the array column does exist then default to it here
      if ("array" %in% names(rr)) rr$array<-rr$array else rr$array<-rr$reader
      if ("array" %in% names(rd)) rd$array<-rd$array else rd$array<-rd$reader

      nc<- rbind(rd,rr)

      array_summary(nc)

      return(nc)
    }

    if (!is.null(arrayname)) {

      if(!"array" %in% names(dat)) stop("Error: No arrays column exists")

      # Select data that comes from reader you want to reconfig antennas for
      rd <- filter(dat, array == arrayname)

      # Select all other data to merge back in later
      rr <- filter(dat, array != arrayname)

      #Assign new antenna numbers based on number entries (ao1-> an1, ao2-> an2, ao3 -> an3, a04 -> an4)

      # If ao1 exists, give new antenna number an1
      if (!is.null(ao1)) {
        df1 <- rd
        x1 <- which(df1$antenna == ao1)
        df1$antenna[x1] <- an1
      }

      # If ao2 exists, give new antenna number an2
      if (!is.null(ao2)) {
        df2 <- rd
        x2 <- which(df2$antenna == ao2)
        df2$antenna[x2] <- an2
      }

      # If ao3 exists, give new antenna number an3
      if (!is.null(ao3)) {
        df3 <- rd
        x3 <- which(df3$antenna == ao3)
        df3$antenna[x3] <- an3
      }

      #If ao4 exists, give new antenna number an4
      if (!is.null(ao4)) {
        df4 <- rd
        x4 <- which(df4$antenna == ao4)
        df4$antenna[x4] <- an4
      }

      # Combine the data frames
      rd <- df1
      if (exists("df2")) rd <- rbind(df1,df2)
      if (exists("df3")) rd <- rbind(df1,df2,df3)
      if (exists("df4")) rd <- rbind(df1,df2,df3,df4)

      # Create an array column if one doesn't exist, if the array column does exist then default to it here
      if ("array" %in% names(rr)) rr$array<-rr$array else rr$array<-rr$reader
      if ("array" %in% names(rd)) rd$array<-rd$array else rd$array<-rd$reader

      nc<- rbind(rd,rr)

      array_summary(nc)

      return(nc)
    }
  }
}
