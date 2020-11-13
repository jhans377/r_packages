
#'  Check if one user has multiple assignment events across variations
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' checkDoubleAssignment(raw_test_data,user_id,sample,'is control')

checkDoubleAssignment <- function(raw_data,id_variable,sample_variable,control_value) {

  sample_variable = enquo(sample_variable)
  id_variable = enquo(id_variable)

  ## take segmentation variable and set to control and variant values
 raw_data <- raw_data %>% mutate(segmentation = ifelse(UQ(sample_variable) == control_value,'control','variant'))

 double_assignments <- raw_data %>%
  group_by(UQ(id_variable)) %>%
    summarize(unique_segments = n_distinct(UQ(sample_variable))) %>%
      mutate(double_assignment_status = case_when(unique_segments > 1 ~ 'multiple variations',unique_segments == 1 ~ 'one variation')) %>%
        group_by(double_assignment_status) %>% summarise(obs = n()) %>% mutate(obs_share = obs/sum(obs))


 return(double_assignments) ## return duplicate summary
}
