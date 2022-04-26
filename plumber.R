#
# This is a Plumber API. It loads a model and predicts on new data
# and allows you to run a report which renders a html
#
# 
#
#    
#

library(parsnip)

#* @apiTitle Plumber Model API
#* 
#* 


#* Returns predictions
#* @param path_to_data The path to new data you want to generate predictions for. Client side
#* @param target the target you defined in MakeModel. Defines the model name. Server side
#* @get /predictions
PredictData <- function(path_to_data, target) {
  
  newData   <- read.csv(path_to_data)
  model_fit <- readRDS(file = file.path("models", paste0(target, "_model")))
  preds     <- predict(model_fit, new_data = newData)
  
  return(preds)
  
}

#* Define a function that generates model and report
#* @param inputFile the path of the .Rmd file you want to render. Server side
#* @param inputData your input data. Client side
#* @param target the target column
#* @param tune_params tune_params? FALSE is default.
#* @serializer html
#* @get /report

MakeModel <- function(inputFile, inputData = NULL, target, tune_params = FALSE){
  #generate temp file
  tmp <- tempfile(fileext = ".html")
  
  #render report and write to temp file
  rmarkdown::render(
    input = inputFile, 
    output_format = "html_document",
    output_file = tmp,
    params = list(input_data = inputData,
                  target = target,
                  tune_params = tune_params)
  )
  #read binary files from tmp file 
  return(readBin(tmp, "raw", n = file.info(tmp)$size))
  
}