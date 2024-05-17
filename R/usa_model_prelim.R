#' Create Preliminary Model
#'
#' This function creates a preliminary ordinal logistic regression model using
#' randomly selected extra variables and interaction terms. The function will
#' repeatedly attempt to create a model until successful.
#'
#' @param data A data frame containing the dataset to be used for model creation.
#'
#' @return An object of class \code{polr} representing the fitted model.
#' @import MASS
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- data.frame(vote = sample(c("totally Biden", "somewhat Biden", "neutral", "somewhat Trump", "totally Trump"), 100, replace = TRUE),
#'                    var1 = rnorm(100), var2 = rnorm(100), var3 = rnorm(100), var4 = rnorm(100))
#' necessary_variables_prefixes <- c("var1", "var2")
#' extra_variables_prefixes <- c("var3", "var4")
#' n_vars <- 2
#' model <- create_model_prelim(data)
#' }
#' @export
create_model_prelim <- function(data, n_vars_app, n_vars_model,
                                necessary_variables_prefixes,
                                extra_variables_prefixes) {
  repeat {
    tryCatch({
      # Randomly select extra variables
      extra_variables_prefixes <- sample(extra_variables_prefixes, n_vars_app)
      vars <- c(necessary_variables_prefixes, extra_variables_prefixes)

      model_vars <- sample(vars, n_vars_model, replace = TRUE)

      data_for_model <- data %>%
        select(vote, starts_with(model_vars))

      necessary_variables <- names(data_for_model)[names(data_for_model) %in% necessary_variables_prefixes]
      extra_vars <- names(data_for_model)[!names(data_for_model) %in% c(necessary_variables_prefixes, "vote")]

      # Generate interaction terms
      n_interactions <- sample(2:10, 1)
      interaction_terms_necessary <- sample(necessary_variables, n_interactions, replace = TRUE)
      interaction_terms_extra <- sample(extra_vars, n_interactions, replace = TRUE)
      interaction_terms <- paste0(interaction_terms_necessary, " * ", interaction_terms_extra)

      # Create the formula
      formula <- paste0("vote ~ . + ", paste(interaction_terms, collapse = " + "))

      # Create the model without showing warnings
      ord_model <- suppressWarnings(MASS::polr(as.formula(formula), data = data_for_model,
                                           Hess = TRUE, method = "logistic"))
      data_for_model$vote <- (as.numeric(data_for_model$vote) - 1) / 4
      lm_model <- suppressWarnings(lm(as.formula(formula), data = data_for_model))
      model <- list(ord_model = ord_model, lm_model = lm_model)
      model[["n_interactions"]] <- n_interactions
      # If the model is created successfully, exit the repeat loop
      return(model)
    }, error = function(e) {
      # Continue the loop to retry without showing messages
    })
  }
}
