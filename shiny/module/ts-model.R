ts_model_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(tagList(echarts4rOutput(ns("ts_model"), height = "400px"))),
    fluidRow(
      column(tagList(echarts4rOutput(ns("acf_Residual Series"))), width = 6),
      column(tagList(echarts4rOutput(ns("pacf_Residual Series"))), width = 6)
    ),
    fluidRow(
      column(tagList(echarts4rOutput(ns("acf_Observed Series"))), width = 6),
      column(tagList(echarts4rOutput(ns("pacf_Observed Series"))), width = 6)
    )
  )
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
      req(!all(state[["ts_vov"]] != "Null", ts_int == "null"))

      data <- state[["data"]] %>%
        filter(
          with(state, between(year(datetime), ts_yr[1], ts_yr[2])),
          location == loc
        ) %>%
        mutate(t = as_date(datetime), y_t = !!sym(ts_var)) %>%
        as_tibble() %>%
        group_by(t) %>%
        summarise(y_t = mean(y_t, na.rm = TRUE)) %>%
        mutate(
          t = as.numeric(t),
          y_t = case_when(
            state[["ts_geomean"]] ~ log(y_t),
            TRUE ~ y_t
          )
        )

      req(mean(is.na(data[["y_t"]])) < .2)

      if (state[["ts_trend"]] == "Null") {
        trend <- "y_t ~ 1"
      } else {
        trend <- "y_t ~ t"
      }
      if (substr(state[["ts_autocor"]], 1, 2) == "AR") {
        ar_ord <- as.numeric(gsub("\\D", "", state[["ts_autocor"]]))
        lags <- paste0("tail(lag(y_t, ", seq_len(3), "), -", ar_ord, ")")
        trend <- paste(
          gsub("t", paste0("t[-seq_len(", ar_ord, ")]"), trend),
          "+", paste(lags[seq_len(ar_ord)], collapse = " + ")
        )
      } else {
        ar_ord <- 0
      }
      trend_fit <- lm(as.formula(trend), data, na.action = na.exclude)
      pred <- predict(trend_fit,
        interval = ifelse(ts_int == "null", "confidence", ts_int)
      )
      if (substr(state[["ts_autocor"]], 1, 2) == "AR") {
        pred <- rbind(matrix(NA, ar_ord, 3), pred)
      }
      model <- list(
        fitted = pred[, 1],
        seof = (pred[, 1] - pred[, 2]) / qt(.975, df.residual(trend_fit)),
        r = c(rep(NA, ar_ord), residuals(trend_fit))
      )
      model[["seof_sig"]] <- model[["seof"]] / sigma(trend_fit)

      if (state[["ts_vov"]] == "GARCH(1,1)") {
        model[["r"]] <- with(model, replace_na(r, mean(r, na.rm = TRUE)))
        garch_fit <- garch(model[["r"]], trace = FALSE, grad = "numerical")
        model[["seof"]] <- fitted(garch_fit)[, 1] * model[["seof_sig"]]
      }

      df <- nrow(data) - sum(is.na(data[["y_t"]])) - 2 -
        ar_ord * 2 - (state[["ts_vov"]] == "GARCH(1,1)") * 3

      exp_if_geo <- function(x, to_be_exp = state[["ts_geomean"]]) {
        case_when(to_be_exp ~ exp(x), TRUE ~ x)
      }

      label <- ifelse(
        state[["ts_trend"]] == "Linear",
        "Average" %>%
          paste(state[["ts_var"]]) %>%
          paste("in", state[["map_onclick"]]) %>%
          paste(ifelse(coef(trend_fit)[2] >= 0, "up", "down"), "by") %>%
          paste(round(abs(ifelse(
            state[["ts_geomean"]],
            (exp(coef(trend_fit)[2] * 365) - 1) * 100,
            coef(trend_fit)[2] * 365
          )), 2)) %>%
          paste0(ifelse(state[["ts_geomean"]], "%", "")) %>%
          paste("per annum") %>%
          paste(ifelse(diff(state[["ts_yr"]]) == 0, "in", "from")) %>%
          paste(ifelse(
            diff(state[["ts_yr"]]) == 0,
            state[["ts_yr"]][1],
            paste(state[["ts_yr"]], collapse = " to ")
          )), ""
      )

      pval <- rbind(summary(trend_fit)[["coefficients"]], NA)[2, 4]

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
        e_line(y_t, symbol = "none", name = "Observed Series") %>%
        e_band(y_lwr, y_upr, areaStyle = list(
          list(opacity = 0), list(color = "black", opacity = .4 * (ts_int != "null"))
        )) %>%
        e_line(y_hat, symbol = "none", name = "Fitted Series", lineStyle = list(
          width = 1, opacity = .8
        )) %>%
        e_title(label, p_trend) %>%
        e_legend(top = "10%")
    })

    e_acf <- function(data, method, ser, loc, ts_yr, ts_var,
                      ts_geomean, ts_trend, ts_autocor) {
      loc <- make_clean_names(state[["map_onclick"]])
      ts_var <- gsub("_", "\\.", make_clean_names(ts_var))
      req(loc %in% data[["location"]])
      req(ts_var %in% names(data))

      data <- data %>%
        filter(
          between(year(datetime), ts_yr[1], ts_yr[2]),
          location == loc
        ) %>%
        mutate(t = as_date(datetime), y_t = !!sym(ts_var)) %>%
        as_tibble() %>%
        group_by(t) %>%
        summarise(y_t = mean(y_t, na.rm = TRUE)) %>%
        mutate(
          t = as.numeric(t),
          y_t = case_when(y_t <= 0 ~ NA_real_, TRUE ~ case_when(
            ts_geomean ~ log(y_t), TRUE ~ y_t
          ))
        )

      req(mean(is.na(data[["y_t"]])) < .2)

      if (ser == "Residual Series") {
        if (ts_trend == "Null") {
          trend <- "y_t ~ 1"
        } else {
          trend <- "y_t ~ t"
        }
        if (substr(ts_autocor, 1, 2) == "AR") {
          ar_ord <- as.numeric(gsub("\\D", "", ts_autocor))
          lags <- paste0("tail(lag(y_t, ", seq_len(3), "), -", ar_ord, ")")
          trend <- paste(
            gsub("t", paste0("t[-seq_len(", ar_ord, ")]"), trend),
            "+", paste(lags[seq_len(ar_ord)], collapse = " + ")
          )
        }
        trend_fit <- lm(as.formula(trend), data, na.action = na.exclude)
        r <- residuals(trend_fit) %>% replace_na(0)
      } else {
        r <- with(data, replace_na(y_t, mean(y_t, na.rm = TRUE)))
      }

      f <- eval(sym(method))

      acf_data <- tibble(
        t = 0:min(28, length(r) - 1),
        lag_t = c(0, tail(f(r, 28, plot = FALSE)[["acf"]][, 1, 1], length(t) - 1))
      )

      acf_data %>%
        e_charts(t) %>%
        e_bar(lag_t, barWidth = 2) %>%
        e_mark_line(
          data = list(
            yAxis = qt(.975, length(r) - 1) / sqrt(length(r)),
            lineStyle = list(color = "red"),
            label = list(formatter = "5% Sig.")
          ),
          name = "mark 1",
          symbol = "none"
        ) %>%
        e_mark_line(
          data = list(
            yAxis = -qt(.975, length(r) - 1) / sqrt(length(r)),
            lineStyle = list(color = "red"),
            label = list(formatter = "5% Sig.")
          ),
          name = "mark 2",
          symbol = "none"
        ) %>%
        e_title(paste(toupper(method), ser, sep = ", ")) %>%
        e_x_axis(
          type = "category",
          nameLocation = "middle",
          axisLabel = list(interval = 6),
          axisTick = list(alignWithLabel = TRUE),
          name = "Lag"
        ) %>%
        e_legend(show = FALSE)
    }

    map(
      list(
        c("acf", "Observed Series"), c("pacf", "Observed Series"),
        c("acf", "Residual Series"), c("pacf", "Residual Series")
      ),
      function(type) {
        output[[paste(type, collapse = "_")]] <- renderEcharts4r({
          e_acf(
            state[["data"]], type[1], type[2], state[["map_onclick"]],
            state[["ts_yr"]], state[["ts_var"]], state[["ts_geomean"]],
            state[["ts_trend"]], state[["ts_autocor"]]
          )
        })
      }
    )
  }

  moduleServer(id, module)
}
