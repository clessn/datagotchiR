#' Create Preliminary Model
#'
#' This function creates a preliminary ordinal logistic regression model using
#' randomly selected extra variables and interaction terms. The function will
#' repeatedly attempt to create a model until successful.
#'
#' @param data A data frame containing the dataset to be used for model creation.
#' @param necessary_variables_prefixes A vector of prefixes for necessary variables.
#' @param extra_variables_prefixes A vector of prefixes for extra variables.
#' @param n_vars_app An integer specifying the number of extra variables to use.
#' @param n_vars_model An integer specifying the number of interaction terms to generate.
#'
#' @return An object of class \code{polr} representing the fitted model.
#' @import MASS
#' @importFrom dplyr select starts_with all_of
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' data <- data.frame(vote = sample(c("totally Biden", "somewhat Biden", "neutral", "somewhat Trump", "totally Trump"), 100, replace = TRUE),
#'                    var1 = rnorm(100), var2 = rnorm(100), var3 = rnorm(100), var4 = rnorm(100))
#' necessary_variables_prefixes <- c("var1", "var2")
#' extra_variables_prefixes <- c("var3", "var4")
#' n_vars_app <- 2
#' n_vars_model <- 10
#' model <- create_model_prelim(data, n_vars_app, n_vars_model, necessary_variables_prefixes, extra_variables_prefixes)
#' }
#' @export
create_model_prelim <- function(data,
                                lm = FALSE,
                                n_vars_app = 13,
                                n_vars_model = 10,
                                n_interactions,
                                necessary_variables_prefixes = necessary_variables_prefixes,
                                extra_variables_prefixes = extra_variables_prefixes) {
  repeat {
    tryCatch({
      # Randomly select extra variables
      extra_vars_prefixes <- sample(extra_variables_prefixes, n_vars_app)
      vars <- c(necessary_variables_prefixes, extra_vars_prefixes)

      model_vars <- sample(x = vars, n_vars_model, replace = TRUE)

      data_for_model <- data %>%
        select(vote, starts_with(model_vars))

      necessary_variables <- names(data_for_model)[names(data_for_model) %in% necessary_variables_prefixes]
      extra_vars <- names(data_for_model)[!names(data_for_model) %in% c(necessary_variables_prefixes, "vote")]

      # Generate interaction terms
      interaction_terms_necessary <- sample(necessary_variables, n_interactions, replace = TRUE)
      interaction_terms_extra <- sample(extra_vars, n_interactions, replace = TRUE)
      interaction_terms <- paste0(interaction_terms_necessary, " * ", interaction_terms_extra)

      # Create the formula
      formula <- paste0("vote ~ . + ", paste(interaction_terms, collapse = " + "))

      # Create the model without showing warnings
      ord_model <- suppressWarnings(MASS::polr(as.formula(formula), data = data_for_model,
                                               Hess = TRUE, method = "logistic"))
      if (isTRUE(lm)) {
        data_for_model$vote <- (as.numeric(data_for_model$vote) - 1) / 4
        lm_model <- suppressWarnings(lm(as.formula(formula), data = data_for_model))
        model <- list(ord_model = ord_model, lm_model = lm_model)
      } else {
        model <- ord_model
      }
      model[["n_interactions"]] <- n_interactions
      model[["n_vars_model"]] <- n_vars_model
      # If the model is created successfully, exit the repeat loop
      return(model)
    }, error = function(e) {
      # Continue the loop to retry without showing messages
    })
  }
}

#' Check if the Top Two Predicted Categories are Adjacent
#'
#' This function checks if the two most probable categories (based on their probabilities) are adjacent in an ordinal scale.
#'
#' @param probs A numeric vector of probabilities for each category.
#'
#' @return A logical value indicating whether the two most probable categories are adjacent (TRUE) or not (FALSE).
#'
#' @examples
#' probs <- c(0.1, 0.2, 0.3, 0.25, 0.15)
#' is_adjacent(probs)
#'
#' @export
is_adjacent <- function(probs) {
  sorted_indices <- order(probs, decreasing = TRUE)
  first_pred <- sorted_indices[1]
  second_pred <- sorted_indices[2]
  return(abs(first_pred - second_pred) == 1)
}

#' Count Adjacent Predictions in an Ordinal Model
#'
#' This function takes an ordinal model and returns the number of predictions where the two most probable categories are adjacent.
#'
#' @param model An ordinal model object (e.g., from \code{polr} in the \code{MASS} package).
#'
#' @return An integer representing the number of predictions where the two most probable categories are adjacent.
#'
#' @examples
#' \dontrun{
#' # Assuming ord_model is a fitted ordinal model and newdata is a data frame
#' count_adjacent_predictions(ord_model)
#' }
#' @importFrom MASS polr
#' @export
count_adjacent_predictions <- function(model) {
  ord <- predict(model, type = "probs")
  adjacent_predictions <- apply(ord, 1, is_adjacent)
  return(sum(adjacent_predictions))
}

#' Check if the Top Two Predicted Categories are Extremes
#'
#' This function checks if the two most probable categories (based on their probabilities) are the two extremes in an ordinal scale.
#'
#' @param probs A numeric vector of probabilities for each category.
#'
#' @return A logical value indicating whether the two most probable categories are the two extremes (TRUE) or not (FALSE).
#'
#' @examples
#' probs <- c(0.4, 0.1, 0.1, 0.1, 0.3)
#' is_extreme(probs)
#'
#' @export
is_extreme <- function(probs) {
  sorted_indices <- order(probs, decreasing = TRUE)
  first_pred <- sorted_indices[1]
  second_pred <- sorted_indices[2]
  # Assuming the extremes are the first and last categories
  return((first_pred == 1 && second_pred == length(probs)) ||
           (first_pred == length(probs) && second_pred == 1))
}

#' Count Extreme Predictions in an Ordinal Model
#'
#' This function takes an ordinal model and returns the number of predictions where the two most probable categories are the two extremes.
#'
#' @param model An ordinal model object (e.g., from \code{polr} in the \code{MASS} package).
#' @param newdata A data frame containing the new data for which to make predictions.
#'
#' @return An integer representing the number of predictions where the two most probable categories are the two extremes.
#'
#' @examples
#' \dontrun{
#' # Assuming ord_model is a fitted ordinal model and newdata is a data frame
#' count_extreme_predictions(ord_model, newdata)
#' }
#' @importFrom MASS polr
#' @export
count_extreme_predictions <- function(model) {
  ord <- predict(model, type = "probs")
  extreme_predictions <- apply(ord, 1, is_extreme)
  return(sum(extreme_predictions))
}


#' Diagnose an Ordinal Model
#'
#' This function takes an ordinal model and returns a data frame with various diagnostic metrics.
#'
#' @param model An ordinal model object (e.g., from \code{polr} in the \code{MASS} package).
#'
#' @return A data frame containing diagnostic metrics for the model, including:
#' \item{model_iteration}{The iteration number of the model.}
#' \item{adjacent_predictions_count}{The number of predictions where the two most probable categories are adjacent.}
#' \item{mean_surrogate_res}{The mean of the surrogate residuals.}
#' \item{sd_surrogate_res}{The standard deviation of the surrogate residuals.}
#' \item{aic}{The Akaike Information Criterion of the model.}
#' \item{bic}{The Bayesian Information Criterion of the model.}
#' \item{ivs}{The independent variables in the model.}
#' \item{n_interactions}{The number of interaction terms in the model.}
#' \item{n_vars_model}{The number of variables in the model.}
#'
#' @examples
#' \dontrun{
#' # Assuming ord_model is a fitted ordinal model
#' diagnostics <- diagnose_model(ord_model)
#' print(diagnostics)
#' }
#'
#' @export
diagnose_model <- function(model){
  df <- data.frame(
    model_iteration = model[["iteration"]],
    extreme_predictions_count = count_extreme_predictions(model),
    adjacent_predictions_count = count_adjacent_predictions(model),
    mean_surrogate_res = mean(abs(sure::resids(model))),
    sd_surrogate_res = sd(sure::resids(model)),
    aic = AIC(model),
    bic = BIC(model),
    ivs = attr(model[["terms"]], "term.labels"),
    n_interactions = model[["n_interactions"]],
    n_vars_model = model[["n_vars_model"]]
  )
  return(df)
}


#' Generate a Random Ordinal Regression Model using rstanarm::stan_polr
#'
#' This function randomly selects variables and interaction terms to generate
#' an ordinal regression model using `rstanarm::stan_polr`.
#'
#' @param data The data frame containing the dataset.
#' @param n_vars_app The number of additional variables to sample for the model.
#' @param n_vars_model The total number of variables to include in the model.
#' @param n_interactions The number of interaction terms to include in the model.
#' @param necessary_variables_prefixes A vector of prefixes for the necessary variables.
#' @param extra_variables_prefixes A vector of prefixes for the extra variables.
#'
#' @return A fitted `stan_polr` model object with additional information on the number of interactions and variables used.
#' @export
#'
#' @importFrom rstanarm stan_polr
#' @importFrom dplyr select starts_with
#' @importFrom magrittr %>%
#'
#' @examples
#' # Assuming `data` is your dataset and `necessary_variables_prefixes` and `extra_variables_prefixes` are defined:
#' random_model <- random_stan_polr(data, 13, 27, 5, necessary_variables_prefixes, extra_variables_prefixes)
random_stan_polr <- function(data,
                             n_vars_app = 13,
                             n_vars_model = 27,
                             n_interactions,
                             necessary_variables_prefixes = necessary_variables_prefixes,
                             extra_variables_prefixes = extra_variables_prefixes,
                             refresh = 0) {
  repeat {
    tryCatch({
      # Randomly select extra variables
      extra_vars_prefixes <- sample(extra_variables_prefixes, n_vars_app)
      vars <- c(necessary_variables_prefixes, extra_vars_prefixes)

      model_vars <- sample(x = vars, n_vars_model, replace = TRUE)

      data_for_model <- data %>%
        select(vote, starts_with(model_vars))

      necessary_variables <- names(data_for_model)[names(data_for_model) %in% necessary_variables_prefixes]
      extra_vars <- names(data_for_model)[!names(data_for_model) %in% c(necessary_variables_prefixes, "vote")]

      # Generate interaction terms
      interaction_terms_necessary <- sample(necessary_variables, n_interactions, replace = TRUE)
      interaction_terms_extra <- sample(extra_vars, n_interactions, replace = TRUE)
      interaction_terms <- paste0(interaction_terms_necessary, " * ", interaction_terms_extra)

      # Create the formula
      formula <- paste0("vote ~ . + ", paste(interaction_terms, collapse = " + "))

      # Create the model without showing warnings
      model <- rstanarm::stan_polr(
        formula = formula,
        data = data_for_model,
        method = "logistic",
        prior = rstanarm::R2(0.2, "mean"),  # Example prior
        init_r = 0.1,
        seed = 12345,
        refresh = refresh
      )
      model[["n_interactions"]] <- n_interactions
      model[["n_vars_model"]] <- n_vars_model
      # If the model is created successfully, exit the repeat loop
      return(model)
    }, error = function(e) {
      # Continue the loop to retry without showing messages
      message("    Retrying model")
    })
  }
}
