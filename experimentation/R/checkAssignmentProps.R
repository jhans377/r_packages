
#'  Takes raw data from an experiment and tests the observed assignment splits to the hypothetical
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

library("dplyr")

checkAssignmentProps <- function(raw_data,sample_variable,control_value,expected_split) {

  sample_variable = enquo(sample_variable)

  ## take segmentation variable and set to control and variant values
  raw_data <- raw_data %>% mutate(segmentation = ifelse(UQ(sample_variable) == control_value,'control','variant'))

 data <- raw_data %>%
  group_by(segmentation) %>%
    summarize(obs = n_distinct(id)) %>%
      mutate(share = obs/sum(obs))

 data <- data %>%
  summarise(n_control = max(obs[segmentation=='control']),
            n_variant = max(obs[segmentation=='variant']),
            prop_control = max(share[segmentation=='control']),
            prop_variant = max(share[segmentation=='variant']))

 return(data)
}
