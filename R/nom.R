nom <- function(task,
                points.label = "Points",
                total.points.label = "Total points",
                fun.label = "Risk",
                maxscale = 100,
                xfrac = 0.35,
                font.size = 12,
                fun.time = NULL,
                fun.at = NULL,
                show.points = FALSE,
                show.model = FALSE,
                show.explain = TRUE, ...){
  UseMethod("nom")
}


nom.taskcox <- function(task,
                        points.label = "Points",
                        total.points.label = "Total points",
                        maxscale = 100,
                        xfrac = 0.35,
                        font.size = 12,
                        fun.label = "Risk",
                        fun.time = NULL,
                        fun.at = NULL,
                        show.points = FALSE,
                        show.model = FALSE,
                        show.explain = TRUE, ...){

  train <- task$train
  outcome <- task$outcome
  time <- task$time
  predictors <- task$predictors

  pos <- 1
  envir = as.environment(pos)
  assign("dddd", rms::datadist(train), envir = envir)
  options(datadist = "dddd")

  frm <- paste(predictors, collapse = " + ")
  frm <- sprintf("survival::Surv(%s, %s) ~ %s", time, outcome, frm)
  frm <- stats::as.formula(frm)

  model <- rms::cph(formula = frm, data = train, x = TRUE, y = TRUE, surv = TRUE)

  surv <- rms::Survival(model)

  funtimelist <- lapply(fun.time, \(i){
    function(x){
      surv(i,lp = x)
    }
  })

  funlabel<- sapply(fun.time, \(x){
    sprintf("%d survival probability", x)
  })

  if(is.null(fun.at)){
    nom <- rms::nomogram(model,
                         fun= funtimelist,
                         lp= F,
                         funlabel = funlabel,
                         maxscale = maxscale,
                         ...)
  }else{
    nom <- rms::nomogram(model,
                         fun= funtimelist,
                         lp= F,
                         funlabel = funlabel,
                         maxscale = maxscale,
                         fun.at = fun.at)
  }

  if(exists("dddd")){
    rm("dddd", inherits = TRUE, envir = envir)
  }
  options(datadist = NULL)

  font.size <- font.size * 0.0834

  if(show.model){
    print(model)
  }

  if(show.points){
    suppressWarnings(print(nom))
  }

  attr(nom, "xfrac") <- xfrac
  attr(nom, "points.label") <- points.label
  attr(nom, "total.points.label") <- total.points.label
  attr(nom, "cex.var") <- font.size
  attr(nom, "cex.axis") <- font.size
  attr(nom, "show.explain") <- show.explain
  attr(nom, "outcome") <- outcome

  plot(nom)
}
