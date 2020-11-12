#' Takes a dataframe with a control and variant and summarizes proportions form user-inputted variables
#'
#' This function allows you to quickly generate summary bayesian statistics, visualize two distributions
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

library("dplyr")

summarize_data <- function(raw_data,variant,numerator,denominator) {

  numerator = enquo(numerator)
  denominator = enquo(denominator)
  variant = enquo(variant)

 data <- raw_data %>%
  group_by(UQ(variant)) %>%
    summarize(obs = n_distinct(id), numerator = n_distinct(id[UQ(numerator)==1]),denominator = n_distinct(id[UQ(denominator)==1])) %>% mutate(rate = numerator/denominator)

 data <- data %>%
  summarise(n_control = max(denominator[sample=='control']),
            n_variant = max(denominator[sample=='variant']),
            prop_control = max(rate[sample=='control']),
            prop_variant = max(rate[sample=='variant']))

 return(data)
}
