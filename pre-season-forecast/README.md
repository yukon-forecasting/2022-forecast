2022 Pre-Season Forecast
================

``` r
library(knitr)
library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)
library(ggtext)
```

``` r
forecast_year <- 2022
suppressWarnings({
  dir.create("./figures")
  dir.create("./output")
})
theme_set(theme_bw())
```

## Data

``` r
environment <- read_csv("https://raw.githubusercontent.com/yukon-forecasting/data/5b14d42c0fea20b42478e2fd0a6d390a498b7e4f/data/environment/environment.csv")
```

    ## Rows: 62 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (4): year, amatc, pice, msstc
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
cpue <- read_csv("https://raw.githubusercontent.com/yukon-forecasting/data/5b14d42c0fea20b42478e2fd0a6d390a498b7e4f/data/cpue/cpue.csv")
```

    ## Rows: 61 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): source
    ## dbl (4): year, fifdj, qdj, mdj
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
yukon <- left_join(environment, cpue)
```

    ## Joining, by = "year"

## Figures

### MDJ vs. AMATC

``` r
p_amatc <- ggplot(yukon, aes(amatc, mdj)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = yukon[which(yukon$year == forecast_year), "amatc"][[1]]) +
  labs(x = expression("AMATC," * ~ degree * "C"), y = "MDJ (June)")

p_amatc
```

![](README_files/figure-gfm/amatcfigure-1.png)<!-- -->

``` r
ggsave("./figures/mdj_against_amatc.png", width = 4, height = 4)
```

### MDJ vs. MSSTC

``` r
p_msstc <- ggplot(yukon, aes(msstc, mdj)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = yukon[which(yukon$year == forecast_year), "msstc"][[1]]) +
  labs(x = expression("MSSTC," * ~ degree * "C"), y = NULL)

p_msstc
```

![](README_files/figure-gfm/msstcfigure-1.png)<!-- -->

``` r
ggsave("./figures/mdj_against_msstc.png", width = 4, height = 4)
```

### MDJ vs. PICE

``` r
p_pice <- ggplot(yukon, aes(pice, mdj)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = yukon[which(yukon$year == forecast_year), "pice"][[1]]) +
  scale_x_continuous(limits = c(0, 1.0)) +
  labs(
    x = "PICE",
    y = NULL)

ggsave("./figures/mdj_against_pice.png", width = 4, height = 4)

p_all <- p_amatc + p_msstc + p_pice
p_all
```

![](README_files/figure-gfm/picefigure-1.png)<!-- -->

``` r
ggsave("./figures/three_panel.png", width = 9, height = 3)
```

### Time series of AMATC, MSSTC, PICE

``` r
p1 <- ggplot(yukon, aes(year, amatc)) +
  geom_line() +
  geom_point(data = subset(yukon, year == forecast_year)) +
  geom_hline(yintercept = mean(yukon[yukon$year < forecast_year, "amatc"][[1]])) +
  labs(y = expression("AMATC," * ~ degree * "C")) +
  theme(axis.title.x = element_blank())

p2 <- ggplot(yukon, aes(year, msstc)) +
  geom_line() +
  geom_point(data = subset(yukon, year == forecast_year)) +
  geom_hline(yintercept = mean(yukon[yukon$year < forecast_year, "msstc"][[1]])) +
  labs(y = expression("MSSTC," * ~ degree * "C")) +
  theme(axis.title.x = element_blank())

p3 <- ggplot(yukon, aes(year, pice)) +
  geom_line() +
  geom_point(data = subset(yukon, year == forecast_year)) +
  geom_hline(yintercept = mean(yukon[yukon$year < forecast_year, "pice"][[1]], na.rm = TRUE)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Year",
    y = "PICE"
  )
timeseries_3p <- p1 / p2 / p3
timeseries_3p
```

![](README_files/figure-gfm/timeseries-1.png)<!-- -->

``` r
ggsave("./figures/timseries_3p.png", timeseries_3p, width = 8, height = 6)
```

## Modeling

### Model Selection

``` r
models <- c(
  "mdj ~ amatc",
  "mdj ~ msstc",
  "mdj ~ pice",
  "mdj ~ amatc + msstc",
  "mdj ~ amatc + pice",
  "mdj ~ msstc + pice",
  "mdj ~ amatc + msstc + pice"
)
models
```

    ## [1] "mdj ~ amatc"                "mdj ~ msstc"
    ## [3] "mdj ~ pice"                 "mdj ~ amatc + msstc"
    ## [5] "mdj ~ amatc + pice"         "mdj ~ msstc + pice"
    ## [7] "mdj ~ amatc + msstc + pice"

# Set up selection

``` r
hindcast_window <- 15 # last n years
hindcast_years <- seq(forecast_year - hindcast_window, forecast_year - 1)
round_method <- floor # Floor predictions

hindcast_year <- function(data, model, forecast_year) {
  training_data <- data[data$year < forecast_year, ]
  training_model <- lm(formula(model), training_data)

  new_data <- data[data$year == forecast_year, ]
  prediction <- predict(training_model, newdata = new_data, se.fit = TRUE)
  prediction_fit <- round_method(prediction$fit[[1]])
  prediction_interval <- prediction_fit + c(-2, 2) * qnorm(0.975) *
    prediction$se.fit[[1]]

  # Extract response
  response_var = dimnames(attr(terms(as.formula(model)), "factors"))[[1]][1]
  actual <- new_data[[response_var]]

  in_interval <- actual >= round_method(prediction_interval[1]) &&
    actual <= round_method(prediction_interval[2])

  data.frame(
    "formula" = model,
    "year" = forecast_year,
    "predicted" = (prediction_fit),
    "observed" = actual,
    "diff" = prediction_fit - actual,
    "predict_se" = prediction$se.fit[[1]],
    "in_interval" = in_interval,
    "int_lower" = prediction_interval[1],
    "int_upper" = prediction_interval[2],
    "int_width" = prediction_interval[2] -
      prediction_interval[1]
  )
}

hindcast_model <- function(data, model, years, summarize = TRUE) {
  result <- lapply(years, function(year) {
    hindcast_year(data, model, year)
  })

  model_result <- do.call(rbind, result)

  if (!summarize) {
    return(model_result)
  }

  data.frame(
    model = model,
    "MAPE" = round(mean(abs(model_result$predicted - model_result$observed)), 2),
    "SDMAPE" = round(sd(abs(model_result$predicted - model_result$observed)), 2),
    "width" = round(mean(model_result$int_width), 2),
    "p.in" = round(sum(model_result$in_interval) / length(model_result$in_interval), 2),
    "absmax" = max(abs(model_result$predicted - model_result$observed)),
    "meanbias" = round(mean(model_result$predicted - model_result$observed), 2)
  )
}

hindcast_models <- function(data, models, years) {
  result <- lapply(models, function(model) {
    hindcast_model(data, model, years)
  })

  do.call(rbind, result)
}

model_selection_result <- hindcast_models(yukon, models, hindcast_years)
knitr::kable(model_selection_result)
```

| model                       | MAPE | SDMAPE | width | p.in | absmax | meanbias |
|:----------------------------|-----:|-------:|------:|-----:|-------:|---------:|
| mdj \~ amatc                | 4.60 |   2.72 |  6.09 | 0.33 |     12 |    -4.60 |
| mdj \~ msstc                | 1.67 |   1.54 |  4.47 | 0.67 |      5 |    -0.87 |
| mdj \~ pice                 | 3.53 |   3.52 |  6.52 | 0.60 |     11 |    -3.40 |
| mdj \~ amatc + msstc        | 2.40 |   1.72 |  5.95 | 0.67 |      7 |    -2.00 |
| mdj \~ amatc + pice         | 4.00 |   3.05 |  8.19 | 0.47 |     10 |    -3.87 |
| mdj \~ msstc + pice         | 2.07 |   2.25 |  6.89 | 0.80 |      7 |    -1.40 |
| mdj \~ amatc + msstc + pice | 2.20 |   1.93 |  8.21 | 0.87 |      6 |    -1.80 |

``` r
write.csv(model_selection_result, file = "./output/model_select.csv")
```

### 15%

``` r
model_fifdj <- lm(fifdj ~ amatc + msstc + pice, data = subset(yukon, year < forecast_year))
summary(model_fifdj)
```

    ##
    ## Call:
    ## lm(formula = fifdj ~ amatc + msstc + pice, data = subset(yukon,
    ##     year < forecast_year))
    ##
    ## Residuals:
    ##    Min     1Q Median     3Q    Max
    ## -9.501 -1.809  0.470  2.070  6.843
    ##
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   9.6389     1.7835   5.405 2.01e-06 ***
    ## amatc        -0.6342     0.1753  -3.618 0.000712 ***
    ## msstc        -1.4217     0.3734  -3.808 0.000398 ***
    ## pice         -0.9905     4.0428  -0.245 0.807496
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##
    ## Residual standard error: 3.381 on 48 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.552,  Adjusted R-squared:  0.524
    ## F-statistic: 19.71 on 3 and 48 DF,  p-value: 1.813e-08

``` r
prediction_fifdj <- floor(predict(model_fifdj, newdata = yukon[yukon$year == forecast_year, ]))
```

### 25%

``` r
model_qdj <- lm(qdj ~ amatc + msstc + pice, data = subset(yukon, year < forecast_year))
summary(model_qdj)
```

    ##
    ## Call:
    ## lm(formula = qdj ~ amatc + msstc + pice, data = subset(yukon,
    ##     year < forecast_year))
    ##
    ## Residuals:
    ##    Min     1Q Median     3Q    Max
    ## -8.337 -1.710  0.170  1.576  6.358
    ##
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  12.3528     1.6693   7.400 1.80e-09 ***
    ## amatc        -0.5304     0.1641  -3.233  0.00222 **
    ## msstc        -1.6284     0.3495  -4.659 2.54e-05 ***
    ## pice         -0.3449     3.7840  -0.091  0.92776
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##
    ## Residual standard error: 3.165 on 48 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.5915, Adjusted R-squared:  0.5659
    ## F-statistic: 23.17 on 3 and 48 DF,  p-value: 2.043e-09

``` r
prediction_qdj <- floor(predict(model_qdj, newdata = yukon[yukon$year == forecast_year, ]))
```

### 50%

``` r
model_mdj <- lm(mdj ~ amatc + msstc + pice, data = subset(yukon, year < forecast_year))
summary(model_mdj)
```

    ##
    ## Call:
    ## lm(formula = mdj ~ amatc + msstc + pice, data = subset(yukon,
    ##     year < forecast_year))
    ##
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max
    ## -8.7769 -1.3147  0.2846  1.5585  6.7477
    ##
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  18.2899     1.5792  11.582 1.67e-15 ***
    ## amatc        -0.2903     0.1552  -1.871   0.0675 .
    ## msstc        -1.8658     0.3306  -5.644 8.74e-07 ***
    ## pice          0.7915     3.5796   0.221   0.8259
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##
    ## Residual standard error: 2.994 on 48 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.5993, Adjusted R-squared:  0.5742
    ## F-statistic: 23.93 on 3 and 48 DF,  p-value: 1.294e-09

``` r
prediction_mdj <- floor(predict(model_mdj, newdata = yukon[yukon$year == forecast_year, ]))
```

``` r
predictions <- data.frame(
  percentile = c("fifdj", "qdj", "mdj"),
  prediction = as.integer(c(
    prediction_fifdj,
    prediction_qdj,
    prediction_mdj
  ))
)
write_csv(predictions, file = "./output/predictions.csv")
kable(predictions)
```

| percentile | prediction |
|:-----------|-----------:|
| fifdj      |         10 |
| qdj        |         13 |
| mdj        |         18 |

# Historical Comparisons

## Long Term Means

``` r
long_term_means <- data.frame(
  variable = c("AMATC", "MSSTC", "PICE"),
  current_year_value = c(
    mean(yukon$amatc[which(yukon$year == forecast_year)]),
    mean(yukon$msstc[which(yukon$year == forecast_year)]),
    mean(yukon$pice[which(yukon$year == forecast_year)], na.rm = TRUE)
  ),
  long_term_mean = c(
    mean(yukon$amatc[which(yukon$year < forecast_year)]),
    mean(yukon$msstc[which(yukon$year < forecast_year)]),
    mean(yukon$pice[which(yukon$year < forecast_year)], na.rm = TRUE)
  )
)
long_term_means$cur_minus_ltm <- long_term_means$current_year_value - long_term_means$long_term_mean
long_term_means$range <- c(
  paste(range(yukon$amatc[which(yukon$year < forecast_year)]), collapse = " to "),
  paste(range(yukon$msstc[which(yukon$year < forecast_year)]), collapse = " to "),
  paste(range(yukon$pice[which(yukon$year < forecast_year)], na.rm = TRUE), collapse = " to ")
)
kable(long_term_means)
```

| variable | current_year_value | long_term_mean | cur_minus_ltm | range          |
|:---------|-------------------:|---------------:|--------------:|:---------------|
| AMATC    |             -2.330 |     -6.6050820 |     4.2750820 | -17.1 to 1.3   |
| MSSTC    |              0.173 |     -0.4724590 |     0.6454590 | -3.8 to 2.8    |
| PICE     |              0.268 |      0.5439615 |    -0.2759615 | 0.078 to 0.784 |

## Hindcast all three models

``` r
hindcast_fifdj <- hindcast_model(yukon, "fifdj ~ amatc + msstc + pice", hindcast_years)
hindcast_qdj <- hindcast_model(yukon, "qdj ~ amatc + msstc + pice", hindcast_years)
hindcast_mdj <- hindcast_model(yukon, "mdj ~ amatc + msstc + pice", hindcast_years)

hindcast_all_percentiles <- rbind(
  hindcast_fifdj,
  hindcast_qdj,
  hindcast_mdj)
write_csv(hindcast_all_percentiles, "output/hindcast_all_models.csv")
kable(hindcast_all_percentiles)
```

| model                         | MAPE | SDMAPE | width | p.in | absmax | meanbias |
|:------------------------------|-----:|-------:|------:|-----:|-------:|---------:|
| fifdj \~ amatc + msstc + pice | 2.93 |   2.74 |  8.24 | 0.87 |      9 |    -0.93 |
| qdj \~ amatc + msstc + pice   | 2.60 |   2.87 |  7.71 | 0.73 |      9 |    -1.00 |
| mdj \~ amatc + msstc + pice   | 2.20 |   1.93 |  8.21 | 0.87 |      6 |    -1.80 |

``` r
hindcast_models <- c(
  "fifdj ~ amatc + msstc + pice",
  "qdj ~ amatc + msstc + pice",
  "mdj ~ amatc + msstc + pice"
)
hindcast <- do.call(rbind, lapply(hindcast_models, function(model) {
  hindcast_model(yukon, model, hindcast_years, summarize = FALSE)

}))

hindcast$formula <- toupper(hindcast$formula)
hindcast$formula <- ordered(hindcast$formula, c(
  "FIFDJ ~ AMATC + MSSTC + PICE",
  "QDJ ~ AMATC + MSSTC + PICE",
  "MDJ ~ AMATC + MSSTC + PICE"
))
predicted_vs_observed <- ggplot(hindcast, aes(observed, predicted)) +
  geom_point(shape = 1) +
  scale_shape_manual(values = c(1, 19)) +
  facet_wrap(~ formula) +
  annotate(
    geom = "segment",
    x = min(c(hindcast$observed, hindcast$predicted)),
    y = min(c(hindcast$observed, hindcast$predicted)),
    xend = max(c(hindcast$observed, hindcast$predicted)),
    yend = max(c(hindcast$observed, hindcast$predicted))
  ) +
  labs(
    x = "Observed (June)",
    y = "Predicted (June)"  ) +
  theme(strip.background = element_rect(fill=NA, colour=NA),
        strip.text = element_text(hjust = 0))

ggsave("figures/predicted_vs_observed.png",
  predicted_vs_observed,
  width = 8,
  height = 3
)
predicted_vs_observed
```

![](README_files/figure-gfm/predicted_vs_observed-1.png)<!-- -->

``` r
forecast_timeseries <- ggplot(hindcast, aes(year, diff)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ formula, ncol = 1) +
  annotate(geom = "segment", x = min(hindcast$year), xend = max(hindcast$year), y = 0, yend = 0) +
  labs(
    x = "Year",
    y = "Predicted - Observed"
  )
forecast_timeseries
```

![](README_files/figure-gfm/forecast_timeseries-1.png)<!-- -->
