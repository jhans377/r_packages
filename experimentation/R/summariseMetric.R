#' Takes a dataframe with a control and variant and summarizes proportions form user-inputted variables
#'
#' This function allows you to quickly generate summary bayesian statistics, visualize two distributions
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

library("dplyr")

summariseMetric <- function(raw_data,sample_variable,control_value,numerator,denominator) {

  numerator = enquo(numerator)
  denominator = enquo(denominator)
  sample_variable = enquo(sample_variable)

  ## take segmentation variable and set to control and variant values
  raw_data <- raw_data %>% mutate(segmentation = ifelse(UQ(sample_variable) == control_value,'control','variant'))

 data <- raw_data %>%
  group_by(segmentation) %>%
    summarize(obs = n_distinct(id),
              numerator = n_distinct(id[UQ(numerator)==1]),
              denominator = n_distinct(id[UQ(denominator)==1])) %>%
                mutate(rate = numerator/denominator)

 data <- data %>%
  summarise(n_control = max(denominator[segmentation=='control']),
            n_variant = max(denominator[segmentation=='variant']),
            prop_control = max(rate[segmentation=='control']),
            prop_variant = max(rate[segmentation=='variant']))

 return(data)
}
