
#'  Check if one test subject has multiple observations within the data
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

checkUniqueness <- function(raw_data,id_variable) {

 id_variable = enquo(id_variable)

 uniqueness_test <- raw_data %>%
  group_by(UQ(id_variable)) %>%
    summarize(observations = n()) %>%
      mutate(observation_status = case_when(observations > 1 ~ 'multiple observations',observations == 1 ~ 'one observation')) %>%
        group_by(observation_status) %>% summarise(obs = n()) %>% mutate(obs_share = obs/sum(obs))


 return(uniqueness_test) ## return duplicate summary
}
