#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#' 
train_sep_fit <- function(.data, ...){
  # 检查数据结构，确保其为tsibble
  model_data <- .data |> tsibble::as_tsibble()

  # 进行X13分解
  x13_fit <- model_data |> 
    fabletools::model(feasts::X_13ARIMA_SEATS()) |>
    components()
    
  # 创建包含趋势和季节性的新数据集
  decomposed_data <- x13_fit |>
    dplyr::select(trend, seasonal)

  # 对X13分解后的数据进行建模
  trend_model <- fable::ARIMA(trend)
  seasonal_model <- fable::SNAIVE(seasonal)

  # 训练模型
  trend_fit <- fabletools::model(decomposed_data, trend_model)
  seasonal_fit <- fabletools::model(decomposed_data, seasonal_model)
  
  # 返回一个包含两个模型的结构体
  structure(
    list(
      trend_model = trend_fit,
      seasonal_model = seasonal_fit,
      decomposed = x13_fit
    ),
    class = "sep_fit"
  )
}

specials_sep_fit <- new_specials(
  trend = function(type = c("linear", "ARIMA"), ...){
    type <- match.arg(type)
    list(type = type, ...)
  },
  season = function(method = c("SNAIVE", "TBATS"), ...){
    method <- match.arg(method)
    list(method = method, ...)
  }
)


#' sepfit: Decompose time series and fit models to trend and seasonality
#'
#' This function decomposes a time series into trend and seasonal components using X13,
#' and fits an ARIMA model to the trend component and a SNAIVE model to the seasonal component.
#'
#' @param formula A symbolic description of the model to be fitted.
#' @param ... Additional arguments passed to the model fitting process.
#'
#' @return A model object of class "sep_fit".
#' @export
SepFit <- function(formula, ...){
  sep_fit <- new_model_class("sep_fit", train_sep_fit, specials_sep_fit)
  new_model_definition(sep_fit, !!rlang::enquo(formula), ...)
}

#' Forecast method for SepFit models
#'
#' This function generates forecasts for SepFit models by separately forecasting
#' the trend and seasonal components and then combining them.
#'
#' @param object An object of class "sep_fit", typically the result of calling `SepFit()`
#' @param new_data A tsibble of future time points to forecast
#' @param specials Special arguments passed to underlying models (unused in this implementation)
#' @param ... Additional arguments passed to the forecasting process
#'
#' @return A fable containing the forecasts
#' @export
forecast.sep_fit <- function(object, new_data, specials = NULL, ...) {
  # 提取趋势和季节性模型
  trend_model <- object$trend_model
  seasonal_model <- object$seasonal_model
  
  # 对趋势和季节性分别进行预测
  trend_forecast <- fabletools::forecast(trend_model, new_data = new_data, ...)
  seasonal_forecast <- fabletools::forecast(seasonal_model, new_data = new_data, ...)
  
  # 提取预测值
  trend_values <- trend_forecast$.mean
  seasonal_values <- seasonal_forecast$.mean
  
  # 计算最终预测值（趋势 * 季节性）
  final_forecast <- trend_values * seasonal_values
  
  # 返回分布对象
  distributional::dist_degenerate(final_forecast)
}

#' Obtain fitted values from a sepfit model
#'
#' @param object A sepfit model object.
#' @param ... Additional arguments passed to the fitted method.
#'
#' @return A vector of fitted values.
#' @export
fitted.sep_fit <- function(object, ...){
  trend_model <- object$trend_model
  seasonal_model <- object$seasonal_model
  
  fitted_values <- fable::fitted(trend_model) * fable::fitted(seasonal_model)
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
model_sum.sep_fit <- function(x){
  "sepfit Model"
}

#' @export
format.sep_fit <- function(x, ...){
  "sepfit Model"
}
