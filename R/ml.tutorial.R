#' Function to start a tutorial accompanying the package "machinelearning" and the book "Künstliche Intelligenz und Machine Learning mit R".
#'
#' @param name is the name of the tutorial
#' @keywords tutorial
#' @example  ms.tutorial(name = "ml.syntax")
#' @export

ml.tutorial <-  function(name){
  learnr::run_tutorial(name, package = "machinelearning")
}  
