taskcox <- function(train, test = NULL, outcome = NULL, time = NULL, positive = NULL, predictors = NULL){

  outcome    <- srmisc::select_variable(train, outcome)
  time       <- srmisc::select_variable(train, time)
  predictors <- srmisc::select_variable(train, predictors)

  if(length(unique(train[[outcome]])) != 2L){
    stop("The number of levels for the outcome must equal 2 in the train data.", call. = FALSE)
  }

  if(is.null(positive)){
    if(is.numeric(train[[outcome]])){
      positive <- max(train[[outcome]])
    }else if(is.factor(train[[outcome]])){
      positive <- levels(train[[outcome]])[2]
    }else{
      stop("You need to specify the positive event of outcome.", call. = FALSE)
    }
  }

  negative <- setdiff(unique(train[[outcome]]), positive)

  train[[outcome]] <-  ifelse(train[[outcome]] == positive, 1, 0)

  if(!is.null(test)){
    srmisc::check_name(test, outcome)
    srmisc::check_name(test, time)
    srmisc::check_name(test, predictors)

    if(length(unique(test[[outcome]])) != 2L){
      stop("The number of levels for the outcome must equal 2 in the test data.", call. = FALSE)
    }

    test[[outcome]] <- ifelse(test[[outcome]] == positive, 1, 0)
  }

  if(is.null(predictors)){
    predictors <- names(train)
  }

  predictors <- setdiff(predictors, outcome)
  predictors <- setdiff(predictors, time)

  out <- list(train = train,
              test  = test,
              outcome = outcome,
              time = time,
              positive   = positive,
              negative   = negative,
              predictors = predictors)

  class(out) <- c("taskcox", "list")

  out
}
