ts_model_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("ts_model"), height = "650px"))
}

ts_model_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["ts_model"]] <- renderEcharts4r({
      ts_yr <- state[["ts_yr"]]
      ts_var <- gsub("_", "\\.", make_clean_names(state[["ts_var"]]))
      ts_int <- make_clean_names(state[["ts_int"]])
      loc <- make_clean_names(state[["map_onclick"]])
      req(loc %in% state[["data"]][["location"]])
      req(ts_var %in% names(state[["data"]]))

      data <- state[["data"]] %>%
        filter(with(state, between(year(datetime), ts_yr[1], ts_yr[2]))) %>%
        mutate(t = as_date(datetime), y_t = !!sym(ts_var)) %>%
        as_tibble() %>%
        group_by(t) %>%
        summarise(y_t = mean(y_t, na.rm = TRUE)) %>%
        mutate(
          t = as.numeric(t),
          y_t = case_when(y_t <= 0 ~ NA_real_, TRUE ~ case_when(
            state[["ts_geomean"]] ~ log(y_t), TRUE ~ y_t
          ))
        )

      req(mean(is.na(data[["y_t"]])) < .2)

      if (state[["ts_trend"]] == "Null") {
        trend <- "y_t ~ 1"
      } else {
        trend <- "y_t ~ t"
      }
      if (state[["ts_autocor"]] == "AR(1)") {
        trend <- paste(gsub("t", "t[-1]", trend), "+ head(y_t, -1)")
      }
      trend_fit <- lm(as.formula(trend), data, na.action = na.exclude)
      pred <- predict(trend_fit, as.list(data), interval = ts_int)
      if (state[["ts_autocor"]] == "AR(1)") pred <- rbind(NA, pred)
      model <- list(
        fitted = pred[, 1],
        seof = (pred[, 1] - pred[, 2]) / qt(.975, df.residual(trend_fit)),
        r = residuals(trend_fit)
      )
      if (state[["ts_autocor"]] == "AR(1)") model[["r"]] <- c(NA, model[["r"]])
      model[["seof_sig"]] <- model[["seof"]] / sigma(trend_fit)

      if (state[["ts_vov"]] == "GARCH(1,1)") {
        model[["r"]] <- with(model, replace_na(r, mean(r, na.rm = TRUE)))
        garch_fit <- garch(model[["r"]], trace = FALSE, grad = "numerical")
        model[["seof"]] <- fitted(garch_fit)[, 1] * model[["seof_sig"]]
      }

      df <- nrow(data) - sum(is.na(data[["y_t"]])) - 2 -
        (state[["ts_autocor"]] == "ARMA(1,1)") * 2 -
        (state[["ts_vov"]] == "GARCH(1,1)") * 3

      exp_if_geo <- function(x, to_be_exp = state[["ts_geomean"]]) {
        case_when(to_be_exp ~ exp(x), TRUE ~ x)
      }

      label <- "Average" %>%
        paste(state[["ts_var"]]) %>%
        paste("in", state[["map_onclick"]]) %>%
        paste(ifelse(coef(trend_fit)[2] >= 0, "up", "down"), "by") %>%
        paste(round(abs(ifelse(
          state[["ts_geomean"]],
          (exp(coef(trend_fit)[2] * 365) - 1) * 100,
          coef(trend_fit)[2] * 365
        )), 2)) %>%
        paste0(ifelse(state[["ts_geomean"]], "%", "")) %>%
        paste("per year") %>%
        paste(ifelse(diff(state[["ts_yr"]]) == 0, "in", "from")) %>%
        paste(ifelse(
          diff(state[["ts_yr"]]) == 0,
          state[["ts_yr"]][1],
          paste(state[["ts_yr"]], collapse = " to ")
        ))

      pval <- summary(trend_fit)[["coefficients"]][2, 4]

      p_trend <- ifelse(
        state[["ts_trend"]] == "Linear",
        p_trend <- "p-value of trend:" %>%
          paste(ifelse(round(pval, 4) == 0, "< 0.001", sprintf("%.4f", pval))),
        ""
      )

      data %>%
        mutate(
          y_t = exp_if_geo(y_t),
          y_lwr = exp_if_geo(with(model, fitted - qt(.975, df) * seof)),
          y_upr = exp_if_geo(with(model, fitted + qt(.975, df) * seof)),
          y_hat = exp_if_geo(model[["fitted"]]),
          t = as_date(t)
        ) %>%
        e_charts(t) %>%
        e_line(y_t, symbol = "none", name = "Observed Values") %>%
        e_band(y_lwr, y_upr, areaStyle = list(
          list(opacity = 0), list(color = "black", opacity = .4)
        )) %>%
        e_line(y_hat, symbol = "none", name = "Fitted Values", lineStyle = list(
          width = 1, opacity = .8
        )) %>%
        e_title(label, p_trend) %>%
        e_legend(top = "10%")
    }) %>%
      bindCache(
        state[["ts_int"]], state[["ts_yr"]], state[["ts_var"]],
        state[["ts_geomean"]], state[["ts_trend"]], state[["ts_autocor"]],
        state[["ts_vov"]], state[["map_onclick"]]
      )
  }

  moduleServer(id, module)
}
