#' predict_pH : function to predict pH from standard curve.
#'
#' @param standard_data data frame containing standard curve data. Has to have a column named IR and column named pH
#' @param prediction_data data frame with the prediction data, must contain the IR. Must have a column named IR.
#'
#' @return a data frame with predicted pH values
#' @export
#'
#' @examples
#'
#' standard_data = data.frame(IR = c(1,1.2,1.3,1.5,1.8,2.5,3.5,4.2,4.7,4.8,6),
#' pH = seq(from =4, to = 9,by = 0.5))
#' prediction_data = data.frame(time = 1:10, IR = c(5,5,4,4,3,2,2,1.1,4,5))
#'
#' predicted_pH_data = predict_pH(standard_data,prediction_data)
#'
predict_pH = function(standard_data,prediction_data){

    model = dr4pl::dr4pl(standard_data,dose = pH,response = IR,method.init = "logistic")

    # Extracting all the model coefficents
    coefficients <- coef(model)
    names(coefficients) <- c("d","c","b","a")
    d <- coefficients[1]
    c <- coefficients[2]
    b <- coefficients[3]
    a <- coefficients[4]


    # Fitting model
    output = dplyr::mutate(prediction_data,predicted_pH = c*((a-d)/(IR-d)-1)^(1/b))

    return(output)
}
