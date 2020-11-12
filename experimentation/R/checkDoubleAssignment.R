
#'  Check if one user has multiple observations within the data
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

library("dplyr")

checkDoubleAssignment <- function(raw_data,id_variable,sample_variable,control_value) {

  sample_variable = enquo(sample_variable)
  id_variable = enquo(id_variable)

  ## take segmentation variable and set to control and variant values
 raw_data <- raw_data %>% mutate(segmentation = ifelse(UQ(sample_variable) == control_value,'control','variant'))

 data <- raw_data %>%
  group_by(UQ(id_variable)) %>%
    summarize(obs = n()) %>%
      mutate(double_assignment_status = case_when(obs > 1 ~ 'multiple obs',obs == 1 ~ 'one obs')) %>%
        group_by(double_assignment_status) %>% summarise(obs = n()) %>% mutate(obs_share = obs/sum(obs))


 return(data) ## return duplicate summary
}
