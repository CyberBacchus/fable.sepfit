#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#' @importFrom stats predict
train_dcmp_sep_fit <- function(.data, specials, ...){
  # 使用 X13 分解时间序列
  decomposed <- x13binary::seas(.data)
  
  # 获取分解结果中的趋势和季节部分
  trend <- decomposed$trend
  seasonal <- decomposed$seasonal
  
  # 对趋势部分拟合 ARIMA 模型
  trend_model <- fable::ARIMA(trend)
  
  # 对季节部分拟合 SNAIVE 模型
  seasonal_model <- fable::SNAIVE(seasonal)
  
  # 返回一个包含两个模型的结构体
  structure(
    list(
      trend_model = trend_model,
      seasonal_model = seasonal_model,
      decomposed = decomposed
    ),
    class = "fbl_dcmp_sep_fit"
  )
}

specials_dcmp_sep_fit <- new_specials(
  xreg = function(..., prior_scale = NULL, standardize = "auto", type = NULL){
    model_formula <- new_formula(
      lhs = NULL,
      rhs = reduce(c(0, enexprs(...)), function(.x, .y) call2("+", .x, .y))
    )
    list(
      xreg = model.matrix(model_formula, self$data),
      prior_scale = prior_scale,
      standardize = standardize,
      mode = type
    )
  }
)

#' DcmpSepFit: Decompose time series and fit models to trend and seasonality
#'
#' This function decomposes a time series into trend and seasonal components using X13,
#' and fits an ARIMA model to the trend component and a SNAIVE model to the seasonal component.
#'
#' @param formula A symbolic description of the model to be fitted.
#' @param ... Additional arguments passed to the model fitting process.
#'
#' @return A model object of class "dcmp_sep_fit".
#' @export
DcmpSepFit <- function(formula, ...){
  dcmp_sep_fit <- new_model_class("dcmp_sep_fit", train_dcmp_sep_fit, specials = NULL)
  new_model_definition(dcmp_sep_fit, !!rlang::enquo(formula), ...)
}

#' Generate forecasts from a DcmpSepFit model
#'
#' @param object A DcmpSepFit model object.
#' @param new_data New data to generate forecasts for.
#' @param specials Not used, included for compatibility.
#' @param ... Additional arguments passed to the forecasting method.
#'
#' @return A forecasted time series object.
#' @export
forecast.fbl_dcmp_sep_fit <- function(object, new_data, specials = NULL, ...){
  # Combine trend and seasonal model forecasts
  trend_model <- object$trend_model
  seasonal_model <- object$seasonal_model
  
  trend_forecast <- fable::forecast(trend_model, new_data)
  seasonal_forecast <- fable::forecast(seasonal_model, new_data)
  
  trend_forecast + seasonal_forecast
}

#' Obtain fitted values from a DcmpSepFit model
#'
#' @param object A DcmpSepFit model object.
#' @param ... Additional arguments passed to the fitted method.
#'
#' @return A vector of fitted values.
#' @export
fitted.fbl_dcmp_sep_fit <- function(object, ...){
  trend_model <- object$trend_model
  seasonal_model <- object$seasonal_model
  
  fitted_values <- fable::fitted(trend_model) + fable::fitted(seasonal_model)
  fitted_values
}

dist_symmetric_percentile <- function(x, percentile) {
  distributional::new_dist(x = x, percentile = percentile, class = c("dist_symmetric_percentile", "dist_percentile"))
}

#' @export
mean.dist_symmetric_percentile <- function(x, ...){
  median(x, ...)
}

#' @export
model_sum.fbl_dcmp_sep_fit <- function(x){
  "DcmpSepFit Model"
}

#' @export
format.fbl_dcmp_sep_fit <- function(x, ...){
  "DcmpSepFit Model"
}
