#' Calculate Statistics
#'
#' @param stat_type either the mean, median or standard deviation
#'
#' @return either the mean, median or standard deviation of average
#' @export
#'
#' @importFrom dplyr summarise
#' @import stringr
#' @import knitr
#'
#' @examples
#' DRG <- read_csv("DRG_data.csv")
#'
#' calculate_statistics(DRG, "mean")
#' calculate_statistics(DRG,"median")
#' calculate_statistics(DRG,"std")

calculate_statistics <- function(data, stat_type) {
    if (stat_type %in% c("mean", "median", "std")) {
        result_df <- data %>%
            group_by(`DRG Definition`) %>% # group by DRG
            summarise(
                StatValue = case_when( # calculate statistic
                    stat_type == "mean"   ~ mean(`Average Medicare Payments`),
                    stat_type == "median" ~ median(`Average Medicare Payments`),
                    stat_type == "std"    ~ sd(`Average Medicare Payments`)
                )
            ) %>%
            kable(col.names = c("DRG Definition", str_to_title(stat_type)))
        return(result_df)
    } else { # if incorrect input
        stop("Invalid stat_type. Choose from 'mean', 'median', or 'std'")
    }
}
