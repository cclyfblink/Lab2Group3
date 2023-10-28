#' DRG Boxplot (Function 1)
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
    if (!(variable %in% c("Average Covered Charges", # checks for incorrect input
                          "Average Total Payments",
                          "Average Medicare Payments"))) {
        stop("Not a valid variable")
    }
    ggplot(data = data, aes(y = get(variable), # makes boxplot
                            x = str_extract(get("DRG Definition"),
                                            regexp <- "[[:digit:]]+"))) +
        geom_boxplot(outlier.size = 0.8) + # decreases size of outlier points
        labs(x = "DRG Code", # renames x axis
             y = variable, # renames y axis
             title = paste(variable, "by DRG Code")) + # renames title
        theme_bw() + # adds theme bw
        theme(axis.text.x = element_text(angle=90, hjust=1), # makes DRGs vertical
              axis.text = element_text(size = 5.5)) # shrinks DRGs to not overlap
}
