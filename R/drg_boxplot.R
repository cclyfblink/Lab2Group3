#' DRG Boxplot
#'
#' This function will produce a ggplot boxplot of \code{variable} by DRG code.
#'
#' @param data a dataframe with the DRG data
#' @param variable a string name for the variable in the dataframe data
#'
#' @return A boxplot of \code{variable} by DRG code
#' @export
#'
#'
#' @import ggplot2
#' @import stringr

drg_boxplot <- function(data, variable = c("Average Covered Charges",
                                           "Average Total Payments",
                                           "Average Medicare Payments")) {
    if (!(variable %in% c("Average Covered Charges",
                          "Average Total Payments",
                          "Average Medicare Payments"))) {
        stop("Not a valid variable")
    }
    ggplot(data = data, aes(y = get(variable), x = str_extract(get("DRG Definition"), regexp <- "[[:digit:]]+"))) +
        geom_boxplot() +
        labs(x = "DRG Code", y = variable, title = paste(variable, "by DRG Code")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=90, hjust=1))
}
