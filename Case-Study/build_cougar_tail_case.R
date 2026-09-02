# Rebuild the three case-study charts without rendering the R Markdown slides.
# Requires ggplot2. Run: Rscript "Case-Study/build_cougar_tail_case.R"
# Every date, schedule, sale, weather reading, and rating below is invented.
# No BYU records were used. Price ($8) and capacity (64,000) are exercise assumptions.

make_case_data <- function(noise_sd = 750, rebalance = TRUE) {
  set.seed(411)
  weeks <- seq(1, 11, by = 2)
  games <- expand.grid(season_week = weeks, season = 2016:2025)
  n <- nrow(games)
  difficulty <- unlist(lapply(2016:2025, function(year) sample(rep(1:3, 2))))
  # Unequal category sizes without changing the weather simulation's RNG state.
  if (rebalance) difficulty[head(which(difficulty == 3), 6)] <- 1
  september_first <- as.Date(paste0(games$season, "-09-01"))
  first_saturday <- september_first + (6 - as.POSIXlt(september_first)$wday + 7) %% 7
  games$date <- first_saturday + 7 * (games$season_week - 1)
  games$byu_win_pct <- c(85, 55, 25)[difficulty]
  games$temperature_f <- round(pmin(92, pmax(40,
    75 - 0.8 * games$season_week + rnorm(n, 0, 12))))
  games$attendance <- round(pmin(63000, pmax(45000,
    56000 + 1200 * (difficulty - 2) + rnorm(n, 0, 2500))), -2)
  # Extra week-to-week variation keeps the seasonal averages from looking
  # perfectly linear; this is a designed teaching example, not evidence.
  calendar_bump <- rep(c(-200, 450, -500, 700, -450, 250), 10)
  games$units <- round(10000 - 350 * (games$season_week - 6) -
    110 * (games$temperature_f - 65) + 1100 * (difficulty - 2) +
    0.18 * (games$attendance - 56000) + calendar_bump + rnorm(n, 0, noise_sd))
  games$revenue <- games$units * 8
  games$difficulty <- factor(ifelse(games$byu_win_pct >= 70, "Low",
    ifelse(games$byu_win_pct >= 40, "Medium", "High")),
    levels = c("Low", "Medium", "High"))

  # A raw import with three deliberately planted problems. The full clean data
  # stands in for verified sales, weather, and attendance source records.
  raw <- games[, setdiff(names(games), "difficulty")]
  raw$temperature_f[raw$season == 2025 & raw$season_week == 3] <- NA
  raw$attendance[raw$season == 2025 & raw$season_week == 9] <- 90000
  raw <- raw[!(raw$season == 2025 & raw$season_week == 7), ]
  schedule <- games[, c("season", "season_week", "date")]
  list(clean = games, raw = raw, schedule = schedule)
}

fit_case_model <- function(games) {
  # Week, temperature (degrees F), and attendance (people) are continuous.
  # Only opponent difficulty is categorical; Low is the reference category.
  lm(units ~ season_week + difficulty + temperature_f + attendance, data = games)
}

build_case_study <- function(output_dir) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to rebuild the case-study figures.")
  }
  library(ggplot2)
  data <- make_case_data()
  games <- data$clean
  model <- fit_case_model(games)
  means_difficulty <- aggregate(units ~ difficulty, games, mean)
  means_week <- aggregate(units ~ season_week, games, mean)
  counts <- table(games$difficulty)
  number <- function(x) format(round(x), big.mark = ",", trim = TRUE)
  previous_games <- make_case_data(noise_sd = 1000, rebalance = FALSE)$clean
  previous_scatter_sd <- summary(lm(units ~ temperature_f, previous_games))$sigma
  scatter_sd <- summary(lm(units ~ temperature_f, games))$sigma

  # One illustrative upcoming game connects the audience-specific decisions.
  upcoming_game <- data.frame(season_week = 3,
    difficulty = factor("High", levels = levels(games$difficulty)),
    temperature_f = 70, attendance = 60000)
  forecast <- predict(model, upcoming_game, interval = "prediction", level = 0.80)
  # Match the upper prediction limit shown on the slide, rounded to 100 units.
  production_target <- round(forecast[1, "upr"] / 100) * 100
  baker_capacity <- 1200 # units per baker in the 12-hour pre-game window
  bakers_for_tails <- ceiling(production_target / baker_capacity)
  # Compare complete held-out seasons, not randomly mixed game rows.
  train <- games[games$season < 2025, ]
  test <- games[games$season == 2025, ]
  test_forecast <- predict(fit_case_model(train), test)
  holdout_mae <- mean(abs(test$units - test_forecast))
  baseline_mae <- mean(abs(test$units - mean(train$units)))

  # Guard the intended teaching patterns and the underlying arithmetic.
  stopifnot(nrow(games) == 60, nrow(data$raw) == 59,
    !anyDuplicated(games$date), all(games$attendance <= 64000),
    all(games$units > 0), all(games$revenue == games$units * 8),
    sum(is.na(data$raw$temperature_f)) == 1,
    sum(data$raw$attendance > 64000) == 1,
    cor(games$temperature_f, games$units) < -0.2,
    cor(means_week$season_week, means_week$units) < -0.5,
    all(diff(means_difficulty$units) > 0), length(unique(as.numeric(counts))) == 3,
    scatter_sd < previous_scatter_sd,
    is.numeric(games$temperature_f), is.numeric(games$attendance),
    coef(model)["season_week"] < 0, coef(model)["temperature_f"] < 0,
    coef(model)["difficultyHigh"] > coef(model)["difficultyMedium"],
    coef(model)["difficultyMedium"] > 0, coef(model)["attendance"] > 0,
    (bakers_for_tails - 1) * baker_capacity < production_target,
    bakers_for_tails * baker_capacity >= production_target)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  navy <- "#002E5D"
  blue <- "#2168A6"
  gray <- "#DCE3EB"
  ink <- "#263342"
  chart_theme <- theme_minimal(base_size = 17, base_family = "sans") +
    theme(panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = gray, linewidth = 0.3),
      axis.text = element_text(color = ink),
      axis.title = element_text(color = ink),
      axis.title.x = element_text(margin = margin(t = 14)),
      axis.title.y = element_text(margin = margin(r = 14)),
      plot.margin = margin(15, 22, 12, 12))
  save_chart <- function(chart, filename) {
    stopifnot(inherits(chart, "ggplot"))
    ggsave(file.path(output_dir, filename), plot = chart,
      width = 10, height = 850 / 180, units = "in", dpi = 180,
      bg = "white", device = "png")
  }

  temperature_chart <- ggplot(games, aes(temperature_f, units)) +
    geom_point(color = blue, size = 2.5, alpha = 0.85) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
      color = navy, linewidth = 1) +
    scale_y_continuous(labels = number) +
    labs(x = "Game temperature (degrees F)", y = "Cougar tails sold (units / game)") +
    chart_theme
  save_chart(temperature_chart, "cougar_tail_temperature.png")

  difficulty_labels <- setNames(paste0(names(counts), "\n(n = ", counts, ")"), names(counts))
  opponent_chart <- ggplot(means_difficulty, aes(difficulty, units)) +
    geom_col(fill = blue, width = 0.72) +
    geom_text(aes(label = number(units)), vjust = -0.5, size = 6, color = ink) +
    scale_x_discrete(labels = difficulty_labels) +
    scale_y_continuous(labels = number, limits = c(0, NA),
      expand = expansion(mult = c(0, 0.16))) +
    labs(x = "Opponent difficulty (pre-game rating)", y = "Mean sales (units / game)") +
    chart_theme
  save_chart(opponent_chart, "cougar_tail_opponent.png")

  # Position local-low labels below their points to avoid crossing the line.
  means_week$label_y <- means_week$units + c(230, 230, -230, 230, -230, 230)
  week_chart <- ggplot(means_week, aes(season_week, units)) +
    geom_line(color = blue, linewidth = 1) +
    geom_point(color = blue, size = 3) +
    geom_text(aes(y = label_y, label = number(units)), size = 5.5, color = ink) +
    scale_x_continuous(breaks = means_week$season_week) +
    scale_y_continuous(labels = number, expand = expansion(mult = c(0.1, 0.12))) +
    labs(x = "Week within the football season", y = "Mean sales (units / game)") +
    chart_theme
  save_chart(week_chart, "cougar_tail_season_week.png")

  cat("\nRAW IMPORT EXCERPT (2025):\n")
  print(data$raw[data$raw$season == 2025, ], row.names = FALSE)
  cat("\nVERIFIED SOURCE RECORDS (2025):\n")
  print(games[games$season == 2025, ], row.names = FALSE)
  cat("\nAVERAGES BY DIFFICULTY:\n")
  print(means_difficulty, row.names = FALSE)
  cat("\nAVERAGES BY SEASON WEEK (10 games at each week):\n")
  print(means_week, row.names = FALSE)
  cat("\nCATEGORY SIZES:\n")
  print(counts)
  cat("\nOLS MODEL: continuous week, temperature F, attendance people; Low difficulty baseline\n")
  print(round(coef(summary(model)), 3))
  cat("n =", nobs(model), "; R-squared =", round(summary(model)$r.squared, 3), "\n")
  cat("Temperature / sales correlation:", round(cor(games$temperature_f, games$units), 3), "\n")
  cat("Week / average sales correlation:", round(cor(means_week$season_week, means_week$units), 3), "\n")
  cat("Scatter residual SD, previous / updated:", round(previous_scatter_sd), "/", round(scatter_sd), "\n")
  cat("\nUPCOMING GAME AND 80% PREDICTION INTERVAL:\n")
  print(upcoming_game)
  print(round(forecast))
  cat("Production target from upper prediction limit, rounded to 100:", production_target, "\n")
  cat("Bakers at 1,200 tails per 12-hour window:", bakers_for_tails, "\n")
  cat("Held-out 2025 MAE, model / training-mean baseline:", round(holdout_mae), "/", round(baseline_mae), "\n")
  invisible(c(data, list(model = model, forecast = forecast,
    production_target = production_target, bakers_for_tails = bakers_for_tails,
    holdout_mae = holdout_mae, baseline_mae = baseline_mae)))
}

if (sys.nframe() == 0L) {
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1])))
  build_case_study(file.path(dirname(script_dir), "Figures"))
}
