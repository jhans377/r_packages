
#'  Takes raw data from an experiment and tests the observed assignment splits to the hypothetical
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

library("dplyr")

checkAssignmentProps <- function(raw_data,segmentation,expected_split) {

  segmentation = enquo(segmentation)

 data <- raw_data %>%
  group_by(UQ(segmentation)) %>%
    summarize(obs = n_distinct(id)) %>% mutate(share = obs/sum(obs))

 data <- data %>%
  summarise(n_control = max(obs[sample=='control']),
            n_variant = max(obs[sample=='variant']),
            prop_control = max(share[sample=='control']),
            prop_variant = max(share[sample=='variant']))

 return(data)
}
