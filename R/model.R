#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#' 训练SepFit模型，组装结果
train_sep_fit <- function(.data, specials, ...){
  # 获得预测变量
  mv <- tsibble::measured_vars(.data)

  # 检查数据结构，确保其为tsibble
  model_data <- .data |> tsibble::as_tsibble() |> dplyr::select(!!sym(mv))

  # 提取含指定变量的tsibble转为ts
  model_data_ts <- model_data |> as.ts()
  
  # 进行X13分解
  x13_fit <- seas(model_data_ts)  # 修改这一行
  
  # 创建包含趋势和季节性的新数据集
  trend <- x13_fit |> series("seats.trend")
  seasonal <- x13_fit |> series("seats.seasonal")

  decomposed_data <- model_data |> mutate(
    trend = trend,
    seasonal = seasonal
  )

  # 对X13分解后的数据进行建模
  trend_fit <- decomposed_data |> fabletools::model(fable::ARIMA(trend))
  seasonal_fit <- decomposed_data |> fabletools::model(fable::ARIMA(seasonal))

  trend_model_fitted <- trend_fit |> fitted() |> dplyr::pull(.fitted)
  seasonal_model_fitted <- seasonal_fit |> fitted() |> dplyr::pull(.fitted)
  seasonal_model_fitted[is.na(seasonal_model_fitted)] <- 1
  fitted_values <- trend_model_fitted * seasonal_model_fitted

  true_values <- model_data |> dplyr::pull(!!sym(mv))
  residuals <- true_values - fitted_values
  
  # 返回一个包含两个模型的结构体
  structure(
    list(
      trend_model = trend_fit,
      seasonal_model = seasonal_fit,
      decomposed = x13_fit,
      fitted_values = fitted_values,
      residuals = residuals
    ),
    class = "sep_fit"
  )
}

# 这个specials暂时没有任何功能
specials_sep_fit <- new_specials(
  season = function(period = NULL) {
    stop("不支持`")
  },
  xreg = function(...) {
    stop("不支持")
  }
)


#' SepFit: 分解时间序列后对子序列分别拟合
#'
#' 这一函数使用X-13ARIMA-SEATS方法分解时间序列为趋势、季节性、残差子序列
#' 使用ARIMA和SNAIVE等方式拟合子序列并组合预测结果
#'
#' @param formula 暂时用不到
#' @param ... 暂时不支持
#'
#' @return 模型类"sep_fit".
#' @export
SepFit <- function(formula, ...){
  sep_fit <- new_model_class("sep_fit", train_sep_fit, specials_sep_fit)
  new_model_definition(sep_fit, !!rlang::enquo(formula), ...)
}

#' SepFit模型的预测方法
#'
#' 这一预测方法基于传递的trend_model、seasonal_model得到预测
#' 因为X-13ARIMA-SEATS方法是乘法分解，所以这一方法相乘得到序列整体预测结果
#'
#' @param object "sep_fit"类, 是调用`SepFit()`得到的结果
#' @param new_data tsibble格式的未来数据
#' @param specials 暂时用不到
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
  object$fitted_values
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
  "分解-组合模型"
}

#' @export
format.sep_fit <- function(x, ...){
  "sepfit Model"
}

#' @export
residuals.sep_fit <- function(object, ...){
  object$residuals
}