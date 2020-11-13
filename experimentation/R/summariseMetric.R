#' Takes a dataframe with a control and variant and summarizes proportions form user-inputted variables
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summariseMetric(test_metric)

summariseMetric <- function(metric_data) {

 summary <- metric_data %>%
  group_by(segmentation) %>%
    summarize(obs = n_distinct(id),
              numerator = n_distinct(id[numerator==1]),
              denominator = n_distinct(id[denominator==1])) %>% ungroup() %>%
                mutate(rate = numerator/denominator)

 summary <- summary %>% ungroup() %>%
  summarise(n_control = max(denominator[segmentation=='control']),
            n_variant = max(denominator[segmentation=='variant']),
            prop_control = max(rate[segmentation=='control']),
            prop_variant = max(rate[segmentation=='variant']))

 return(summary)
}
