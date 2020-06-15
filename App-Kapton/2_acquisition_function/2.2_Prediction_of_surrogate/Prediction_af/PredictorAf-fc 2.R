# this files are needed in order to create the object (not every function is used, but anyway)
# TO DO: all in one file 
source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/utils-iml-molnar.R")
source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/inferTask-iml-molnar.R")
source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/find_y-iml-molnar.R")
source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/Data-iml-molnar.R")

#' @title Predictor object that predicts the value of the AF in the MBO process.
#'
#' @description
#' A `Predictor` object holds any machine learning model (`mlr`, `caret`,
#' `randomForest`, ...) and the data to be used for analyzing the model. The
#' interpretation methods in the `iml` package need the machine learning model
#' to be wrapped in a `Predictor` object. `PredictorAf` is a extension of `Predictor`
#' (inherits its behavior), which holds a `mlr` model and can be seen as a transformation
#' of it. It can be used to understand the behavior of the Af within a MBO process
#' using the tools provided in the `iml` package. PredictorAf is built on iml version ‘0.10.0’, 
#' Due to inherit, if latest versions of iml are released PredictorAf might generate errors.
#'
#' @importFrom prediction find_data
#'
#' @details
#' A Predictor object is a container for the prediction model and the data.
#' This ensures that the machine learning model can be analyzed in a robust way.
#'
#' Note:
#' -In case of classification, the model should return one column per class
#' with the class probability.
#' -`PredictorAf` has been created for regression tasks. For classfication taks,
#' we do not know how & if it works
#' @export
PredictorAf = R6::R6Class("PredictorAf",
  inherit = iml::Predictor,
  public = list(
    
    #' @description Create a PredictorAf object
    #' @param model\cr
    #'   A machine learning model from `mlr` package, used as surrogate model in the 
    #'   MBO process, and needed in order to measure the Infill criteria 
    #' @param data [data.frame]\cr
    #'   The data to be used in the entire MBO process. Allowed column
    #'   classes are: [numeric], [factor], [integer], [ordered] and [character]
    #'   For some models the data can be extracted automatically. The data argument
    #'   includes the initial data and the proposed points (target value included, but
    #'   not the value of the infill criteria)
    #'   `Predictor$new()` throws an error when it can't extract the data
    #'   automatically.
    #' @param y `character(1)` | [numeric] | [factor]\cr The target vector or
    #'   (preferably) the name of the target column in the `data` argument.
    #'   Predictor tries to infer the target automatically from the model
    #' @param batch.size `numeric(1)`\cr
    #' The maximum number of rows to be input the model for prediction at once.
    #' Currently only respected for [FeatureImp], [Partial] and [Interaction].
    #' @param res.mbo\cr
    #' @param design [data.frame]\cr
    #' The design is a subset of data, for iter i it includes the initial data and
    #' the proposed points until iter i-1.
    #' @param iter\cr
    #' @param progress\cr
    #' @param attributes\cr
    #' @param iter.mbo\cr
    initialize = function(
      # some arguments of the iml::Predictor 
      model = NULL, data = NULL, y = NULL, batch.size = 1000,
      # arguments needed for the Predictor of the Af
      res.mbo = NULL, design = NULL, iter = NULL, attributes = FALSE
    ) {
      checkmate::assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
      checkmate::assert_number(batch.size, lower = 1)
      if (is.null(model)) {
        stop("Provide a model!")
      }
      if (is.null(data)) {
        tryCatch(
          {
            data = find_data(model)
          },
          error = function(e) stop("Can't extract data from model, please provide via data=")
        )
      }
      if (is.null(y)) {
        y = find_y(model)
        # y not always needed, so ignore when not in data
        if (is.character(y) && !(y %in% names(data))) {
          y = NULL
        }
      }
      self$data = Data$new(data, y = y)
      #self$class = class
      self$model = list(model)
      self$task = inferTaskFromModel(model)
      self$batch.size = batch.size
      # begin edited:
      self$res.mbo = res.mbo
      self$par.set = res.mbo$opt.path$par.set
      self$control = res.mbo$control
      self$design = list(design)
      self$iter = iter
      if (res.mbo$control$infill.crit$id == "adacb") {
        self$progress = getProgressAdaCB(res.mbo, iter)
      } else {
        self$progress = NULL
      }
      self$attributes = attributes
      # the prdiction function is extracted from res.mbo, do not need an argument
      self$prediction.function = res.mbo$control$infill.crit$fun
      # end edited
    },
    #' @description Predict new data with the machine learning model.
    #' @template newdata
    predict = function(newdata) {
      checkmate::assert_data_frame(newdata)
      # Makes sure it's not a data.table
      newdata = as.data.frame(newdata)
      # make sure only features are used
      newdata = newdata[, intersect(
        self$data$feature.names,
        colnames(newdata)
      ), drop = FALSE]
      # begin edited:
      prediction = self$prediction.function(
        points = newdata,
        models = self$model,
        control = self$control,
        par.set = self$par.set,
        designs = self$design,
        iter = self$iter,
        progress = self$progress,
        attributes = FALSE
      )
      prediction = data.frame(prediction)
      # end edited:
      if (!private$predictionChecked) {
        checkPrediction(prediction, newdata)
        private$predictionChecked = TRUE
      }
      # If S3 or function, we can only infer the task
      # once we see the predictions
      if (is.null(self$task)) {
        self$task = inferTaskFromPrediction(prediction)
      }
      rownames(prediction) = NULL
      data.frame(prediction, check.names = FALSE)
    },
    #' @description Print the PredictorAf object.
    print = function() {
      cat(
        "Prediction task of surrogate model:", self$task,
        ", Infill criteria:", self$res.mbo$control$infill.crit$id, "\n"
      )
    },
    
    #' @field data [data.frame]\cr
    #' Data object with the data for the model interpretation.
    data = NULL,
    
    #' @field model (any)\cr
    #'   The machine learning model form mlr package.
    model = NULL,
    
    #' @field batch.size `numeric(1)`\cr
    #' The number of rows to be input the model for prediction at once.
    batch.size = NULL,
   
    #' @field task `character(1)`\cr
    #'   The inferred prediction task: `"classification"` or `"regression"`.
    task = NULL,
   
    # begin edited:
    #' @field res.mbo
    #' the results of the MBO process
    res.mbo = NULL,
   
    #' @field prediction.function
    #' The acquisition function of the MBO process
    prediction.function = NULL,
   
    #' @field 
    #' the parameter set of the MBO process
    par.set = NULL,
   
    #' @field 
    #' the control object of the MBO process
    control = NULL,
   
    #' @field 
    #' the design used in the the specific iteration of the MBO process
    design = NULL,
   
    #' @field  iter `integer(1)`\cr
    #' Current iteration of the MBO process. Is used for internal purposes, helps
    #' to select the right Surrogate model and design for measuring the AF throughout
    #' the process.
    iter = NULL,
   
    #' @field progress `numeric(1)`\cr
    #' A value between 0 and 1 indicating the progress of the optimization. Only needed in case of AdaCB,
    #' or other custom Adaptive Infill Criteria. For more information on progress is computed, see 
    # #' [\code{\link{MBOSingleObjResult}}]
    progress = NULL,
   
    #' @field `logical(1)`\cr
    #' Are there attributes appended to the return value that should be added to the OptPath?
    attributes = FALSE
    # end edited
  ),
  private = list(
    predictionChecked = FALSE
  )
)

##################
getProgressAdaCB = function(res.mbo, iter) {
  opdf.mbo = as.data.frame(res.mbo$opt.path)
  lambda.start = res.mbo$control$infill.crit$params$cb.lambda.start
  lambda.end = res.mbo$control$infill.crit$params$cb.lambda.end
  lambda = opdf.mbo[which(opdf.mbo$dob == iter), "lambda"]
  
  progress = (lambda - lambda.start) / (lambda.end - lambda.start)
  
}
