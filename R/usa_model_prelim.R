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
      n_interactions <- sample(2:(n_vars_model - 5), 1)
      n_interactions <- ifelse(n_interactions > 10, 10, n_interactions)
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
#'
#' @export
count_adjacent_predictions <- function(model) {
  ord <- predict(model, type = "probs")
  adjacent_predictions <- apply(ord, 1, is_adjacent)
  return(sum(adjacent_predictions))
}


