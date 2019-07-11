#' @title Plots voltage of each reader over the study duration
#'
#' @description Function plots the voltage of each reader over the study
#'   duration.
#' @param volt_dat voltage dataset created using \code{\link{new_pit}} function
#' @param file_path filepath to save the voltage plot
#' @details A voltage dataframe is created by \code{\link{new_pit}} only.
#'   Plot is saved in the user-specified working directory.
#' @examples
#' # Save voltage plot to a working directory
#' volt_plot(new_dat$volt_dat)
#' @export

volt_plot <- function(volt_dat, file_path = getwd()) {
  fig_name <- sprintf("%s/reader_voltage.png", file_path)

  v2 <- dplyr::arrange(volt_dat, date_time)

  vplot <- ggplot2::ggplot(v2, ggplot2::aes(x = date_time, y = volt)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%b-%d")) +
    ggplot2::ylab("Voltage") +
    ggplot2::xlab("") +
    ggplot2::facet_wrap(~ reader, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = 16),
                   axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 14))

  ggplot2::ggsave(filename = fig_name, vplot, scale = 2) # Might have to adjust scale depending on testing.
}
