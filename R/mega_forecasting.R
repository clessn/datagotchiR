#' Simulate Stratified Voting Outcomes
#'
#' This function simulates voting outcomes based on vote and turnout predictions.
#' It generates random samples for each set of inputs, and computes the number of
#' 'rep', 'dem', and 'not_voted' votes.
#'
#' @param vote_prediction A numeric vector of predicted voting intentions.
#' @param vote_sd A numeric vector of standard deviations for vote predictions.
#' @param turnout_prediction A numeric vector of predicted turnout likelihoods.
#' @param turnout_sd A numeric vector of standard deviations for turnout predictions.
#' @param n An integer specifying the number of simulations per prediction set. Default is 10.
#' @param conf_level A numeric value representing the confidence level. Default is 0.999.
#' 
#' @return A data frame with columns 'set_id', 'dem', 'rep', and 'not_voted', representing
#'         the count of Democrat, Republican, and non-voted individuals for each set of inputs.
#'
#' @importFrom stats qnorm rnorm
#' @importFrom dplyr mutate group_by summarize
#' @importFrom tidyr pmin pmax
#' @importFrom magrittr %>%
#'
#' @examples
#' # Example usage:
#' vote_prediction <- c(0.6, 0.4)
#' vote_sd <- c(0.1, 0.1)
#' turnout_prediction <- c(0.7, 0.5)
#' turnout_sd <- c(0.1, 0.1)
#' simulate_strat(vote_prediction, vote_sd, turnout_prediction, turnout_sd, n = 100, conf_level = 0.95)
#'
#' @export
simulate_strat <- function(
  vote_prediction,
  vote_sd,
  turnout_prediction,
  turnout_sd,
  n = 10,
  conf_level = 0.999
){
  conf_factor <- qnorm((1 + conf_level) / 2)
  data <- data.frame(
    set_id = rep(1:length(vote_prediction), each = n),
    vote_intention = rnorm(length(vote_prediction) * n, mean = rep(vote_prediction, each = n), sd = rep(vote_sd, each = n) * conf_factor),
    turnout_likelihood = rnorm(length(turnout_prediction) * n, mean = rep(turnout_prediction, each = n), sd = rep(turnout_sd, each = n) * conf_factor)
  ) %>%
  mutate(
    vote_intention = pmin(pmax(vote_intention, 0), 1),
    turnout_likelihood = pmin(pmax(turnout_likelihood, 0), 1),
    vote_intention = ifelse(vote_intention > 0.5, "rep", "dem"),
    voted = ifelse(turnout_likelihood > 0.5, 1, 0),
    vote_intention = ifelse(voted == 0, "not_voted", vote_intention)
  )
  final_results <- data %>%
    group_by(set_id) %>%
    summarize(
      dem = sum(vote_intention == "dem"),
      rep = sum(vote_intention == "rep"),
      not_voted = sum(vote_intention == "not_voted")
    )
  return(final_results)
}
