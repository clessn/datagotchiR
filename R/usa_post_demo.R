#' Perform Post-Demographic Diagnosis on Models
#'
#' This function performs a post-demographic diagnosis on a set of models by iteratively
#' training and testing them on subsets of the data. It supports both a combined model
#' setup (with "vote" and "undecided" models) and a single model setup.
#'
#' @param models A list of models. For the combined model setup, the list should contain
#' two models with names "vote" and "undecided". For the single model setup, the list
#' should contain one model.
#' @param n_iter The number of iterations to perform. Default is 100.
#'
#' @return A data frame with predictions for each iteration, including the predicted class,
#' predicted probability, and the real class.
#'
#' @importFrom progress progress_bar
#' @importFrom dplyr %>% select arrange mutate bind_rows left_join
#' @importFrom tidyr drop_na
#' @importFrom marginaleffects predictions
#' @importFrom stats update
#'
#' @examples
#' # Example usage:
#' models <- list(vote = vote_model, undecided = undecided_model)
#' result <- post_demo_diagnose(models, n_iter = 100)
#'
#' @export
post_demo_diagnose <- function(
    models,
    n_iter = 100
){
  if (identical(names(models), c("vote", "undecided"))) {
    model_data_vote <- models[["vote"]]$model
    model_data_undecided <- models[["undecided"]]$model
    pb <- progress::progress_bar$new(
      format = "  [:bar] :percent in :elapsed, ETA: :eta",
      total = n_iter, clear = FALSE, width = 60
    )
    for (i in 1:n_iter){
      train_ix <- sample(1:nrow(base_model_data), nrow(base_model_data) * 0.8)
      train_data_vote <- model_data_vote[train_ix, ] %>%
        tidyr::drop_na()
      train_data_undecided <- model_data_undecided[train_ix, ] %>%
        tidyr::drop_na()
      test_data <- base_model_data[-train_ix, ]
      new_model_vote <- update(models[["vote"]], data = train_data_vote)
      preds_vote <- marginaleffects::predictions(new_model_vote,
                                                 newdata = test_data,
                                                 type = "prob") %>%
        dplyr::select(id,
                      predicted_class = group,
                      predicted_probability = estimate) %>%
        dplyr::arrange(id) %>%
        dplyr::mutate(iteration = i)
      new_model_undecided <- update(models[["undecided"]], data = train_data_undecided)
      preds_undecided <- marginaleffects::predictions(new_model_undecided,
                                                      newdata = test_data,
                                                      type = "response") %>%
        dplyr::select(id,
                      predicted_probability = estimate) %>%
        dplyr::arrange(id) %>%
        dplyr::mutate(iteration = i,
                      predicted_class = "undecided") %>%
        select(all_of(names(preds_vote)))
      preds <- preds_vote %>%
        dplyr::left_join(., preds_undecided %>% select(id, undecided_prob = predicted_probability),
                         by = "id") %>%
        dplyr::mutate(predicted_probability = (1 - undecided_prob) * predicted_probability) %>%
        dplyr::select(-undecided_prob) %>%
        dplyr::bind_rows(., preds_undecided) %>%
        dplyr::arrange(id) %>%
        dplyr::left_join(., base_model_data %>% select(id, real_class = vote),
                         by = "id")
      if (i == 1){
        pred_data <- list()
        pred_data[[1]] <- preds
      } else {
        pred_data[[i]] <- preds
      }
      pb$tick()
    }
    pred_data <- bind_rows(pred_data)
    return(pred_data)
  } else {
    model_data <- models$model
    pb <- progress::progress_bar$new(
      format = "  [:bar] :percent in :elapsed, ETA: :eta",
      total = n_iter, clear = FALSE, width = 60
    )
    for (i in 1:n_iter){
      train_ix <- sample(1:nrow(model_data), nrow(model_data) * 0.8)
      train_data <- model_data[train_ix, ]
      test_data <- model_data[-train_ix, ] %>%
        mutate(rowid = 1:nrow(.))
      new_model <- update(models, data = train_data)
      preds <- marginaleffects::predictions(new_model,
                                            newdata = test_data,
                                            type = "prob") %>%
        dplyr::select(rowid,
                      real_class = !!sym(new_model$formula[[2]]),
                      predicted_class = group,
                      predicted_probability = estimate) %>%
        dplyr::arrange(rowid) %>%
        dplyr::mutate(iteration = i)
      if (i == 1){
        pred_data <- list()
        pred_data[[1]] <- preds
      } else {
        pred_data[[i]] <- preds
      }
      pb$tick()
    }
    pred_data <- bind_rows(pred_data)
    return(pred_data)
  }
}
