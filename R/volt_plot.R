#' @title Plots voltage of each reader over the study duration
#'
#' @description Function plots the voltage of each reader over the study duration.
#' @param volt_dat voltage dataset created using \code{\link{new_pit}} function
#' @param file_path filepath to save the voltage plot
#' @details Plot is saved in the user-specified working directory.
#' @examples
#' #save voltage plot to a Dropbox folder
#' volt_plot(new_data$volt_dat, "~/Dropbox (InStream)/Projects/62 - PIT R and D/4 – Figures and Tables")
#' @export

volt_plot <- function(volt_dat, file_path = getwd()){
  fig_name <- paste(file_path,"reader voltage",".png")
  png(fig_name, height=1200, width=1200)
  par(mfrow=c(length(unique(volt_dat$reader)),1), mar=c(1.5,1.5,1,1.5), oma=c(4,4,0,0), cex=1.5)
  v2<- dplyr::arrange(volt_dat, date_time)
  d_ply(v2, c("reader"), function(volt_dat){
    plot(volt ~ date_time, data = volt_dat,
         ylim = c(min(volt_dat$volt)-0.5, max(volt_dat$volt)+0.5),
         xlim=c(min(volt_dat$date_time), max(volt_dat$date_time)),
         type = "b",
         las = 1,
         axes = FALSE)
    x_range<- seq(min(as.POSIXct(volt_dat$date)), max(as.POSIXct(volt_dat$date)), by = "days")
    r <- range(volt_dat$date_time)
    axis.POSIXct(1, at = seq(r[1], r[2], by = "weeks"), format = "%b %d", cex.axis = 0.8)
    axis(side=2, col="black", las = 1)
    mtext(volt_dat$reader[1], side=3, adj=0.02, line=-1, cex=1.5)
    mtext("Voltage", 2, cex = 2, line = 2.5)
    box()
  })
  dev.off()
}
