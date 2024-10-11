#' @docType package
#' @keywords package
#' @import rlang
"_PACKAGE"

globalVariables("self")

#' 
train_sep_fit <- function(.data, formula, ...){
  # 提取要建模的变量
  response_var <- all.vars(formula)[1]
  
  # 转换为tsibble
  model_data <- .data |> tsibble::as_tsibble()
  
  # 提取指定的变量
  model_data <- model_data |> dplyr::select(!!sym(response_var))
  
  # 进行X13分解
  x13_fit <- model_data |> 
    fabletools::model(feasts::X_13ARIMA_SEATS(!!sym(response_var))) |>
    components()
    
  trend <- x13_fit$trend
  seasonal <- x13_fit$seasonal
  
  # 建立趋势和季节性模型
  trend_model <- fable::ARIMA(trend)
  seasonal_model <- fable::SNAIVE(seasonal)
  
  # 训练模型
  trend_fit <- fabletools::model(tsibble::as_tsibble(trend), trend_model)
  seasonal_fit <- fabletools::model(tsibble::as_tsibble(seasonal), seasonal_model)
  
  # 返回结构体
  structure(
    list(
      trend_model = trend_fit,
      seasonal_model = seasonal_fit,
      decomposed = x13_fit
    ),
    class = "sep_fit"
  )
}

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
SepFit <- function(formula, ...) {
  # 使用fabletools::new_model来定义模型类
  sep_fit <- fabletools::new_model_class("sep_fit", train = train_sep_fit)
  
  # 使用fabletools::new_model_definition来创建模型定义
  fabletools::new_model_definition(sep_fit, !!rlang::enquo(formula), ...)
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
  
  # 创建退化分布
  forecast_dist <- distributional::dist_degenerate(final_forecast)
  
  # 返回fable对象，将final_forecast作为.mean列
  fabletools::new_data_frame(list(
    index = new_data[[tsibble::index_var(new_data)]],  # 未来时间点
    value = forecast_dist,  # 分布对象
    .mean = final_forecast  # 预测的均值
  ), class = "fable", response = "value")
}

#' Obtain fitted values from a sepfit model
#'
#' @param object A sepfit model object.
#' @param ... Additional arguments passed to the fitted method.
#'
#' @return A vector of fitted values.
#' @export
fitted.sep_fit <- function(object, ...){
  if(missing(formula)) {
    stop("formula must be provided")
  }
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
