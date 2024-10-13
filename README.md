## 注意 Attention

这是为项目开发的fable适配模型包，还处在不稳定的起步阶段。这一模型的目的是使用X-13ARIMA-SEAT方法分解时间序列后分解对趋势和季节性拟合ARIMA（等）模型，预测并进行组合。

This is a custom fable model package developed for a project, which is still in an unstable early stage. The model aims to decompose time series using the X-13ARIMA-SEATS method, fit ARIMA (and other) models to the trend and seasonal components, forecast, and combine the results.

我是第一次尝试R包开发和fable模型适配，代码还存在很多问题。

This is my first attempt at R package development and fable model adaptation, and the code still has many issues.

开发本包目的是项目使用，泛用性还需要后续更新。

The package is being developed for project use, it still needs subsequent updates for usebility.

## 安装方法 Installation

```r
# install.packages("devtools")
devtools::install_github("cyberbacchus/fable.sepfit")
```
