#' Update and Predict Cumulative Link Model (CLM)
#'
#' This function updates a cumulative link model (CLM) with new training data and generates predictions on test data.
#'
#' @param model An ordinal regression model of class `clm` to be updated.
#' @param new_train_data A data frame containing the new training data to update the model.
#' @param test_data A data frame containing the test data for which predictions are to be made.
#'
#' @return A data frame containing the predictions with columns `id`, `predicted_class`, and `predicted_probability`.
#'
#' @importFrom ordinal clm
#' @importFrom marginaleffects predictions
#' @importFrom dplyr select arrange
#'
#' @examples
#' # Example usage:
#' model <- ordinal::clm(dv_vote ~ ivs_formula, data = train_data, Hess = TRUE)
#' new_train_data <- train_data_sample
#' test_data <- test_data_sample
#' predictions <- update_and_predict_clm_model(model, new_train_data, test_data)
#' print(predictions)
#'
#' @export
update_and_predict_clm_model <- function(model, new_train_data, test_data){
  new_formula <- as.formula(paste(c(model[["terms"]][[2]],
                                    "~",
                                    model[["terms"]][[3]]), collapse = " "))
  new_model <- ordinal::clm(
    formula = new_formula,
    data = new_train_data,
    Hess = TRUE)
  preds <- marginaleffects::predictions(new_model,
                                        newdata = test_data,
                                        type = "prob") %>%
    dplyr::select(id,
                  predicted_class = group,
                  predicted_probability = estimate) %>%
    dplyr::arrange(id)
  return(preds)
}

#' Update and Predict Proportional Odds Logistic Regression Model (polr)
#'
#' This function updates a proportional odds logistic regression model (polr) with new training data and generates predictions on test data.
#'
#' @param model A proportional odds logistic regression model of class `polr` to be updated.
#' @param new_train_data A data frame containing the new training data to update the model.
#' @param test_data A data frame containing the test data for which predictions are to be made.
#'
#' @return A data frame containing the predictions with columns `id`, `predicted_class`, and `predicted_probability`.
#'
#' @importFrom MASS polr
#' @importFrom marginaleffects predictions
#' @importFrom dplyr select arrange
#'
#' @examples
#' # Example usage:
#' model <- MASS::polr(dv_vote ~ ivs_formula, data = train_data, Hess = TRUE)
#' new_train_data <- train_data_sample
#' test_data <- test_data_sample
#' predictions <- update_and_predict_polr_model(model, new_train_data, test_data)
#' print(predictions)
#'
#' @export
update_and_predict_polr_model <- function(model, new_train_data, test_data){
  new_formula <- as.formula(paste(c(model[["terms"]][[2]],
                                    "~",
                                    model[["terms"]][[3]]), collapse = " "))
  new_model <- MASS::polr(
    formula = new_formula,
    data = new_train_data,
    Hess = TRUE)
  preds <- marginaleffects::predictions(new_model,
                                        newdata = test_data,
                                        type = "probs") %>%
    dplyr::select(id,
                  predicted_class = group,
                  predicted_probability = estimate) %>%
    dplyr::arrange(id)
  return(preds)
}



#' Perform Post-Demo Diagnosis on Models
#'
#' This function performs a post-demo diagnosis on a set of models by iteratively
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
    base_model_data = base_model_data,
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
      if (class(models[["vote"]]) == "clm"){
        preds_vote <- update_and_predict_clm_model(models[["vote"]],
                                                   new_train_data = train_data_vote,
                                                   test_data = test_data) %>%
          mutate(iteration = i)
      } else if (class(models[["vote"]]) == "polr") {
        preds_vote <- tryCatch({
          update_and_predict_polr_model(models[["vote"]],
                                        new_train_data = train_data_vote,
                                        test_data = test_data) %>%
            dplyr::mutate(iteration = i)
        }, error = function(e) {
          message("\nSkipping iteration ", i)
          return(NULL)
        })
        # Passer à l'itération suivante si preds_vote est NULL
        if (is.null(preds_vote)) {
          next
        }
      }
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
        mutate(id = 1:nrow(.))
      if (class(models) == "clm"){
        preds <- update_and_predict_clm_model(models,
                                                   new_train_data = train_data,
                                                   test_data = test_data) %>%
          dplyr::mutate(iteration = i)
        preds <- left_join(preds, test_data %>% select(id, real_class = 1),
                           by = "id")
      } else if (class(models) == "polr") {
        preds <- tryCatch({
          update_and_predict_polr_model(models,
                                        new_train_data = train_data,
                                        test_data = test_data) %>%
            dplyr::mutate(iteration = i) %>%
            left_join(., test_data %>% select(id, real_class = 1),
                      by = "id")
        }, error = function(e) {
          message("\nSkipping iteration ", i)
          return(NULL)
        })
        # Passer à l'itération suivante si preds_vote est NULL
        if (is.null(preds)) {
          next
        }
        }
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


#' Create a Density Ridge Plot for Post-Demographic Diagnosis
#'
#' This function creates a density ridge plot for the results of post-demographic diagnosis,
#' showing the distribution of predicted probabilities across different classes.
#'
#' @param data A data frame containing the results of the post-demographic diagnosis. The data frame
#' should have columns `real_class`, `predicted_class`, and `predicted_probability`.
#'
#' @return A ggplot2 object representing the density ridge plot.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggridges
#' @import ggnewscale
#' @importFrom clessnize theme_clean_light
#'
#' @examples
#' # Example usage:
#' data <- data.frame(
#'   real_class = factor(sample(c("totally_biden", "somewhat_biden", "undecided", "somewhat_trump", "totally_trump"), 100, replace = TRUE)),
#'   predicted_class = factor(sample(c("totally_biden", "somewhat_biden", "undecided", "somewhat_trump", "totally_trump"), 100, replace = TRUE)),
#'   predicted_probability = runif(100)
#' )
#' plot <- graph_post_demo_diagnose(data)
#' print(plot)
#'
#' @export
graph_post_demo_diagnose <- function(data, ridges_scale = 1.25){
  library(ggplot2)
  library(dplyr)
  data <- data %>%
    tidyr::drop_na() %>%
    dplyr::mutate(predicted_class = factor(predicted_class,
                                           levels = c("totally_biden", "somewhat_biden", "undecided", "somewhat_trump", "totally_trump"),
                                           ordered = TRUE),
                  wanted_class = ifelse(real_class == predicted_class, 1, 0))
  panels_bg <- data.frame(real_class = levels(data$real_class),
                          predicted_probability = 0, predicted_class = 0) %>%
    mutate(real_class = factor(real_class,
                               levels = c("totally_biden", "somewhat_biden", "undecided", "somewhat_trump", "totally_trump"),
                               ordered = TRUE))
  medians <- data %>%
    group_by(real_class, predicted_class) %>%
    summarise(median = median(predicted_probability)) %>%
    mutate(wanted_class = ifelse(real_class == predicted_class, 1, 0))
  plot <- ggplot(data, aes(x = predicted_probability, y = predicted_class, fill = predicted_class)) +
    geom_rect(data = panels_bg %>% filter(real_class == "totally_biden"),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = colors["totally_biden"],
              alpha = 0.1) +
    geom_rect(data = panels_bg %>% filter(real_class == "somewhat_biden"),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = colors["somewhat_biden"],
              alpha = 0.1) +
    geom_rect(data = panels_bg %>% filter(real_class == "undecided"),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = colors["undecided"],
              alpha = 0.1) +
    geom_rect(data = panels_bg %>% filter(real_class == "somewhat_trump"),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = colors["somewhat_trump"],
              alpha = 0.1) +
    geom_rect(data = panels_bg %>% filter(real_class == "totally_trump"),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = colors["totally_trump"],
              alpha = 0.1) +
    facet_wrap(~real_class, ncol = 1,
               strip.position = "left") +
    geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    scale_y_discrete(breaks = c("totally_biden", "somewhat_biden", "undecided", "somewhat_trump", "totally_trump")) +
    ggridges::geom_density_ridges(aes(y = predicted_class, alpha = wanted_class),
                                  color = NA,
                                  quantile_lines = TRUE, quantiles = 0.5,
                                  bandwidth = 0.025, scale = ridges_scale,
                                  show.legend = FALSE) +
    scale_alpha_continuous(range = c(0.1, 0.485)) +
    ggnewscale::new_scale("alpha") +
    geom_tile(data = medians, aes(x = median, color = predicted_class, alpha = wanted_class),
              width = 0.0025, height = 0.65, show.legend = FALSE) +
    geom_text(data = medians, aes(x = median + 0.005, alpha = wanted_class,
                                  color = predicted_class, label = paste0(round(median * 100), "%")),
              size = 2, hjust = 0, vjust = -0.4,
              fontface = "bold", show.legend = FALSE) +
    scale_alpha_continuous(range = c(0.35, 1)) +
    clessnize::theme_clean_light() +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1),
                       labels = paste0(seq(from = 0, to = 100, by = 10), "%"),
                       limits = c(0, 1)) +
    guides(fill = "none", alpha = "none") +
    theme(strip.placement = "outside",
          strip.text.y = element_text(hjust = 0.5, size = 6),
          strip.background = element_rect(fill = "grey90", color = NA),
          axis.text.y = element_text(vjust = 0))
  return(plot)
}

