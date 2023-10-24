#' Calculate Statistics
#'
#' @param stat_type either the mean, median or standard deviation
#'
#' @return either the mean, median or standard deviation of average
#' @export
#'
#' @importFrom dplyr summarise
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
            group_by(`DRG Definition`) %>%
            summarise(
                StatValue = case_when(
                    stat_type == "mean"   ~ mean(`Average Medicare Payments`),
                    stat_type == "median" ~ median(`Average Medicare Payments`),
                    stat_type == "std"    ~ sd(`Average Medicare Payments`)
                )
            )
        return(result_df)
    } else {
        stop("Invalid stat_type. Choose from 'mean', 'median', or 'std'")
    }
}
