# 校验长格式数据，并确保所有统计量使用同一组完整被试。
.prepare_repeated <- function(data, subject_col, session_col, value_col,
                              trial_col = NULL, method = "mean", sessions = NULL) {
  cols <- c(subject_col, session_col, value_col, trial_col)
  if (!is.data.frame(data) || !all(cols %in% names(data))) stop("\u6570\u636e\u6846\u7f3a\u5c11\u6307\u5b9a\u5217\u3002")
  if (!is.numeric(data[[value_col]])) stop("\u6307\u6807\u5217\u5fc5\u987b\u4e3a\u6570\u503c\u3002")
  if (anyNA(data[c(subject_col, session_col)])) stop("\u88ab\u8bd5\u548c\u4f1a\u8bdd\u6807\u8bc6\u4e0d\u80fd\u7f3a\u5931\u3002")
  if (any(is.infinite(data[[value_col]]))) stop("\u6307\u6807\u4e0d\u80fd\u5305\u542b\u65e0\u7a77\u503c\u3002")
  method <- match.arg(method, c("mean", "max", "min", "median"))
  observed <- unique(as.character(data[[session_col]]))
  if (is.null(sessions)) sessions <- if (is.factor(data[[session_col]])) {
    intersect(levels(data[[session_col]]), observed)
  } else sort(observed)
  if (length(sessions) < 2L) stop("\u81f3\u5c11\u9700\u8981\u4e24\u4e2a\u4f1a\u8bdd\u3002")
  d <- data.frame(id = as.character(data[[subject_col]]),
                  session = as.character(data[[session_col]]), value = data[[value_col]])
  if (is.null(trial_col) && anyDuplicated(d[c("id", "session")])) {
    stop("\u5b58\u5728\u91cd\u590d\u7684\u88ab\u8bd5-\u4f1a\u8bdd\u8bb0\u5f55\uff1b\u8bf7\u6307\u5b9a trial_col \u8fdb\u884c\u805a\u5408\u3002")
  }
  if (!is.null(trial_col)) {
    if (anyNA(data[[trial_col]]) || anyDuplicated(data[c(subject_col, session_col, trial_col)])) {
      stop("\u8bd5\u6b21\u6807\u8bc6\u7f3a\u5931\u6216\u5b58\u5728\u91cd\u590d\u7684\u88ab\u8bd5-\u4f1a\u8bdd-\u8bd5\u6b21\u8bb0\u5f55\u3002")
    }
    fun <- switch(method, mean = base::mean, max = base::max, min = base::min, median = stats::median)
    d <- dplyr::summarise(dplyr::group_by(d, .data$id, .data$session),
                         value = if (all(is.na(.data$value))) NA_real_ else fun(.data$value, na.rm = TRUE),
                         .groups = "drop")
  }
  wide <- tidyr::pivot_wider(d, id_cols = "id", names_from = "session", values_from = "value")
  x <- as.matrix(wide[, sessions, drop = FALSE])
  keep <- stats::complete.cases(x)
  if (any(!keep)) warning(sprintf("\u5df2\u6392\u9664 %d \u540d\u4f1a\u8bdd\u6570\u636e\u4e0d\u5b8c\u6574\u7684\u88ab\u8bd5\u3002", sum(!keep)), call. = FALSE)
  x <- x[keep, , drop = FALSE]
  if (nrow(x) < 3L) stop("\u81f3\u5c11\u9700\u8981\u4e09\u540d\u5b8c\u6574\u914d\u5bf9\u7684\u88ab\u8bd5\u3002")
  x
}

# 双因素残差扣除被试效应和会话效应；两会话时等于差值方差的一半。
.error_components <- function(x) {
  n <- nrow(x); k <- ncol(x)
  residual <- sweep(sweep(x, 1L, rowMeans(x), "-"), 2L, colMeans(x), "-") + mean(x)
  mse <- sum(residual^2) / ((n - 1L) * (k - 1L))
  ms_session <- n * sum((colMeans(x) - mean(x))^2) / (k - 1L)
  ms_subject <- k * stats::var(rowMeans(x))
  ms_within <- sum(sweep(x, 1L, rowMeans(x), "-")^2) / (n * (k - 1L))
  list(mse = mse, ms_session = ms_session, ms_subject = ms_subject, ms_within = ms_within)
}

# SD 与 ICC 单位配套：单分数使用全部观测分数的 SD，均值使用被试均值的 SD。
# 直接 ANOVA 和旧版模型匹配算法作为明确选择的备选项保留。
.reliability_metrics <- function(x, model, type, unit, cv_method, swc_threshold,
                                 sem_method = "sd_icc", icc_value = NULL) {
  e <- .error_components(x)
  k <- ncol(x); n <- nrow(x)
  sem_var <- if (model == "oneway") e$ms_within else if (type == "consistency") {
    e$mse
  } else e$mse + (e$ms_session - e$mse) / n
  sem_sd <- if (unit == "average") stats::sd(rowMeans(x)) else stats::sd(as.vector(x))
  sem <- if (sem_method == "sd_icc") {
    if (is.null(icc_value)) icc_value <- irr::icc(x, model = model, type = type, unit = unit)$value
    if (is.finite(icc_value) && icc_value <= 1) sem_sd * sqrt(1 - icc_value) else NA_real_
  } else if (sem_method == "weir") {
    sqrt(if (model == "oneway") e$ms_within else e$mse)
  } else if (sem_var >= 0) sqrt(sem_var / if (unit == "average") k else 1) else NA_real_
  te <- sqrt(e$mse)
  cv <- if (cv_method == "log") {
    if (any(x <= 0)) stop("\u5bf9\u6570 CV \u8981\u6c42\u6240\u6709\u805a\u5408\u540e\u7684\u6570\u503c\u4e25\u683c\u5927\u4e8e\u96f6\u3002")
    100 * expm1(sqrt(.error_components(log(x))$mse))
  } else if (mean(x) > 0) 100 * (if (cv_method == "sem") sem else te) / mean(x) else {
    warning("\u5747\u503c\u975e\u6b63\uff0c\u539f\u59cb CV \u65e0\u5b9a\u4e49\uff0c\u8fd4\u56de NA\u3002", call. = FALSE)
    NA_real_
  }
  pooled_sd <- sqrt(mean(apply(x, 2L, stats::var)))
  swc <- if (is.null(swc_threshold)) NA_real_ else swc_threshold * pooled_sd
  usefulness <- if (is.na(swc)) NA_character_ else if (te < swc) "Good" else if (te == swc) "OK" else "Marginal"
  list(sem = sem, sem_sd = sem_sd, te = te, mse = e$mse, cv = cv, swc = swc, usefulness = usefulness)
}

# NULL 表示不计算 SWC，保留结果行并返回缺失值。
.validate_reliability_options <- function(value_cols, var_names, swc_threshold) {
  if (!is.character(value_cols) || !length(value_cols) || anyNA(value_cols) || anyDuplicated(value_cols)) stop("value_cols \u5fc5\u987b\u4e3a\u975e\u7a7a\u4e14\u65e0\u91cd\u590d\u7684\u5217\u540d\u5411\u91cf\u3002")
  if (!is.null(var_names) && length(var_names) != length(value_cols)) stop("var_names \u5fc5\u987b\u4e0e value_cols \u7b49\u957f\u3002")
  if (!is.null(swc_threshold) && (!is.numeric(swc_threshold) || length(swc_threshold) != 1L || !is.finite(swc_threshold) || swc_threshold < 0)) stop("swc_threshold \u5fc5\u987b\u4e3a NULL \u6216\u975e\u8d1f\u6709\u9650\u6570\u503c\u3002")
}

# 统一处理零差值方差，避免完全一致的数据使配对检验报错。
.paired_stats <- function(x, y) {
  diff <- y - x
  s <- stats::sd(diff)
  p <- if (s == 0) if (mean(diff) == 0) 1 else 0 else stats::t.test(y, x, paired = TRUE)$p.value
  d <- if (s == 0) if (mean(diff) == 0) 0 else sign(mean(diff)) * Inf else mean(diff) / s
  list(p = p, d = d, magnitude = if (abs(d) < 0.2) "negligible" else if (abs(d) < 0.5) "small" else if (abs(d) < 0.8) "moderate" else "large")
}
