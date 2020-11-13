
#'  remove any users with double assignment events from experiment data
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' removeDoubleAssignment(raw_test_data,user_id,sample,'is control')

removeDoubleAssignment <- function(raw_data,id_variable,sample_variable,control_value) {

  sample_variable = enquo(sample_variable)
  id_variable = enquo(id_variable)

  ## take segmentation variable and set to control and variant values
 double_assignments <- raw_data %>% mutate(segmentation = ifelse(UQ(sample_variable) == control_value,'control','variant'))

  ## identify unique ids with multiple exposures
 double_assignments <- double_assignments %>%
  group_by(UQ(id_variable)) %>%
    summarize(unique_segments = n_distinct(UQ(sample_variable))) %>%
      filter(unique_segments > 1) %>% select(UQ(id_variable),unique_segments)

 raw_data <- raw_data %>% anti_join(double_assignments,by='user_id') ## anti join to remove ids associated with double assignment

 return(raw_data) ## return raw experiment data with double assignments removed
}
