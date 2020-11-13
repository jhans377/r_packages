
#'  Takes raw data from an experiment and tests the observed assignment splits are as expected
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' checkAssignmentProps(raw_test_data,sample,'is control',.5)

checkAssignmentProps <- function(raw_data,sample_variable,control_value,expected_control_split) {

  sample_variable = enquo(sample_variable)

  ## take segmentation variable and set to control and variant values
  raw_data <- raw_data %>% mutate(segmentation = ifelse(UQ(sample_variable) == control_value,'control','variant'))

 data <- raw_data %>%
  group_by(segmentation) %>%
    summarize(obs = n()) %>%
      mutate(share = obs/sum(obs))

 data <- data %>%
  summarise(n_control = max(obs[segmentation=='control']),
            n_variant = max(obs[segmentation=='variant']),
            prop_control = max(share[segmentation=='control']),
            prop_variant = max(share[segmentation=='variant']))

 pvalue_control <- prop.test(data$n_control*data$prop_control, data$n_control, p = expected_control_split, alternative = "two.sided", correct = TRUE)$p.value
 pvalue_variant <- prop.test(data$n_variant*data$prop_variant, data$n_variant, p = 1-expected_control_split, alternative = "two.sided", correct = TRUE)$p.value

 data <- data.frame(data,pvalue_control,pvalue_variant)

 return(data)
}
