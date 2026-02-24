# sportreandva Package: Core Analysis Script (Long-Format-Only Edition)
# 文件名: reliability_and_validity.R
# 描述: 包含sportreandva包的所有核心函数。所有函数均要求输入长格式数据。


# 函数1：描述性统计summarize_trials----
#' @title 批量计算描述性统计和正态性
#' @description 对长格式数据中的一个或多个数值列，按会话(session)和/或试次(trial)进行分组，
#'   计算描述性统计，并可选进行Shapiro-Wilk正态性检验。
#'
#' @param data 一个长格式的数据框(data.frame)。
#' @param value_cols 包含需要分析的指标数值的列名。**可以是单个字符串或字符串向量**。
#' @param group_cols 一个字符向量，包含用于分组的列名 (例如 c("Session", "Trial"))。建议提前将这些列设为因子类型。
#' @param normality 一个逻辑值 (TRUE/FALSE)，指示是否执行正态性检验。默认为 FALSE。
#'
#' @return 返回一个tidy的数据框，第一列为变量名，包含了每个组的描述性统计和可选的正态性检验结果。
#' @importFrom stats sd shapiro.test
#' @importFrom dplyr %>% group_by summarise n mutate left_join
#' @importFrom rlang sym syms
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' # 对单个变量进行描述性统计
#' summarize_trials(
#'   data = descriptive_data,
#'   value_cols = "JumpHeight",
#'   group_cols = c("Session", "Trial")
#' )
#'
#' # 批量对多个变量进行描述性统计
#' # (为了演示，我们假设 descriptive_data 还有一列叫 'Power')
#' # descriptive_data$Power <- descriptive_data$JumpHeight * 100
#' # summarize_trials(
#' #   data = descriptive_data,
#' #   value_cols = c("JumpHeight", "Power"),
#' #   group_cols = "Session"
#' # )
summarize_trials <- function(data, value_cols, group_cols, normality = FALSE) {

  # --- 核心修改：使用 map_dfr 循环处理所有 value_cols ---
  all_results_df <- purrr::map_dfr(
    .x = value_cols,
    .f = function(current_value_col) {

      # --- 以下为原函数的主体逻辑，现在被包含在循环中 ---

      local_data <- data # 使用本地副本以确保安全

      for (col in group_cols) {
        if (!is.factor(local_data[[col]])) {
          warning(paste0("提醒: 分组列 '", col, "' 不是因子类型，已自动转换为因子。\n",
                         "为确保顺序正确，建议您在函数外部预先设置因子水平。"),
                  call. = FALSE)
          local_data[[col]] <- as.factor(local_data[[col]])
        }
      }

      value_sym <- rlang::sym(current_value_col)
      group_syms <- rlang::syms(group_cols)

      desc_stats <- local_data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(
          n = sum(!is.na(!!value_sym)),
          mean = mean(!!value_sym, na.rm = TRUE),
          sd = stats::sd(!!value_sym, na.rm = TRUE),
          min = min(!!value_sym, na.rm = TRUE),
          max = max(!!value_sym, na.rm = TRUE),
          .groups = 'drop'
        )

      if (normality) {
        normality_results <- local_data %>%
          dplyr::group_by(!!!group_syms) %>%
          dplyr::summarise(
            Shapiro_W = if(sum(!is.na(!!value_sym)) > 2) stats::shapiro.test(!!value_sym)$statistic else NA_real_,
            Shapiro_p = if(sum(!is.na(!!value_sym)) > 2) stats::shapiro.test(!!value_sym)$p.value else NA_real_,
            .groups = 'drop'
          )
        desc_stats <- dplyr::left_join(desc_stats, normality_results, by = group_cols)
      }

      # 新增步骤：在结果表格的最前面加上变量名，以便区分
      desc_stats <- desc_stats %>%
        dplyr::mutate(Variable = current_value_col, .before = 1)

      return(desc_stats)
    }
  )

  return(all_results_df)
}

# 函数2：两次重复测量信度----
#' @title 分析两个会话的重测信度 (t检验, 支持批量处理)
#' @description 专为比较两个会话(test-retest)的场景设计。可对单个或多个因变量进行分析。
#'   使用配对t检验和Cohen's d评估系统误差。
#'
#' @param data 一个长格式的数据框。
#' @param subject_col 包含被试ID的列名。
#' @param session_col 包含测试会话/时间点的列名。此列必须恰好包含两个水平。
#' @param value_cols 包含需要分析的指标数值的列名。**可以是单个字符串，也可以是包含多个字符串的向量**。
#' @param trial_type 自定义的ICC类型（single/average）。
#' @param trial_col (可选) 包含试次编号的列名(如使用平均测试成绩)。
#' @param method 聚合会话内多项试次的方法。
#' @param icc_model (可选) 传递给`ICC`函数的模型（oneway/twoway）。
#' @param icc_type (可选) 传递给`ICC`函数的类型（agreement/consistency）。
#' @param var_names  (可选) 一个与 `value_cols` 一一对应的字符向量，用于在结果中重命名变量。
#'   如果提供，其长度必须与 `value_cols` 相同。
#'
#' @return 返回一个数据框，包含一个或多个变量的信度分析结果，并附有t检验和Cohen's d统计量。
#' @importFrom dplyr %>% group_by summarise ungroup n_distinct select case_when
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang sym
#' @importFrom stats aov t.test as.formula sd
#' @importFrom irr icc
#' @importFrom rstatix cohens_d
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' # 筛选出前两次会话 (Week1 vs Week2) 进行 t检验信度分析
#' reliability_subset_ttest <- reliability_data[reliability_data$Session %in% c("Week1", "Week2"), ]
#' analyze_reliability_ttest(
#'   data = reliability_subset_ttest,
#'   subject_col = "Subject",
#'   session_col = "Session",
#'   value_cols = "Score",
#'   trial_col = "Trial",
#'   method = "mean"
#' )

analyze_reliability_ttest <- function(data,
                                      subject_col,
                                      session_col,
                                      value_cols, # <--- 已修改参数名
                                      trial_type = c("single", "average"),
                                      trial_col = NULL,
                                      method = "mean",
                                      icc_model = c("twoway", "oneway"),
                                      icc_type = c("agreement", "consistency"),
                                      swc_threshold = NULL,
                                      var_names = NULL) {

  # --- 参数预处理 ---
  # 检查session列是否恰好有两个水平
  if (dplyr::n_distinct(data[[session_col]]) != 2) {
    stop("错误: 'session_col' 必须恰好包含两个不同的会话水平才能使用此函数。")
  }

  final_icc_model <- match.arg(icc_model)
  final_icc_type <- match.arg(icc_type)
  initial_trial_type <- match.arg(trial_type)

  if (!is.null(var_names) && length(var_names) != length(value_cols)) {
    stop("错误: 'var_names' 的长度必须与 'value_cols' 的长度完全相同。")
  }

  all_results_df <- purrr::imap_dfr(
    .x = value_cols,
    .f = function(current_value_col, index) {

      final_var_name <- if (!is.null(var_names)) var_names[index] else current_value_col

      final_trial_type <- initial_trial_type
      temp_trial_col <- trial_col

      if (!is.null(temp_trial_col) && dplyr::n_distinct(data[[temp_trial_col]]) == 1) {
        final_trial_type <- "single"
        temp_trial_col <- NULL
      }
      if (!is.null(temp_trial_col) && method == "mean") {
        final_trial_type <- "average"
      }

      local_data <- data
      if (!is.factor(local_data[[session_col]])) {
        local_data[[session_col]] <- as.factor(local_data[[session_col]])
      }

      subject_sym <- rlang::sym(subject_col)
      session_sym <- rlang::sym(session_col)
      value_sym <- rlang::sym(current_value_col) # <--- 使用当前循环的变量

      if (!is.null(temp_trial_col)) {
        agg_fun <- switch(method,
                          mean = function(x) mean(x, na.rm = TRUE),
                          max = function(x) max(x, na.rm = TRUE),
                          min = function(x) min(x, na.rm = TRUE),
                          median = function(x) median(x, na.rm = TRUE),
                          stop("错误: method 必须是 'mean', 'max', 'min', 或 'median' 之一。")
        )
        processed_data <- local_data %>%
          dplyr::group_by(!!subject_sym, !!session_sym) %>%
          dplyr::summarise(agg_value = agg_fun(!!value_sym), .groups = 'drop')
      } else {
        processed_data <- local_data %>%
          dplyr::select(!!subject_sym, !!session_sym, agg_value = !!value_sym)
      }

      stats_summary <- processed_data %>%
        dplyr::group_by(!!session_sym) %>%
        dplyr::summarise(
          mean_sd = paste0(round(mean(agg_value, na.rm = TRUE), 2), " ± ",
                           round(stats::sd(agg_value, na.rm = TRUE), 2)),
          .groups = 'drop'
        ) %>%
        tidyr::pivot_wider(names_from = !!session_sym, values_from = mean_sd)

      wide_data <- processed_data %>%
        tidyr::pivot_wider(id_cols = !!subject_sym, names_from = !!session_sym, values_from = agg_value)
      measure_cols <- levels(local_data[[session_col]])
      icc_results <- irr::icc(
        wide_data[, measure_cols],
        model = final_icc_model,
        type = final_icc_type,
        unit = final_trial_type
      )
      chosen_icc <- icc_results

      long_for_calc <- wide_data %>%
        dplyr::mutate(subject_id = dplyr::row_number()) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(measure_cols), names_to = "trial", values_to = "value") %>%
        dplyr::mutate(trial = factor(.data$trial, levels = measure_cols))

      subject_means <- long_for_calc %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::summarise(mean_value = mean(.data$value, na.rm = TRUE), .groups = 'drop')
      between_subject_sd <- stats::sd(subject_means$mean_value, na.rm = TRUE)
      sem <- between_subject_sd * sqrt(1 - chosen_icc$value)

      diffs <- wide_data[[measure_cols[2]]] - wide_data[[measure_cols[1]]]
      te <- stats::sd(diffs, na.rm = TRUE) / sqrt(2)
      grand_mean <- mean(c(wide_data[[measure_cols[1]]], wide_data[[measure_cols[2]]]), na.rm = TRUE)
      cv_percent <- (te / grand_mean) * 100
      swc <- swc_threshold * between_subject_sd

      ttest_result <- stats::t.test(wide_data[[measure_cols[1]]], wide_data[[measure_cols[2]]], paired = TRUE)
      formula_for_d <- stats::as.formula(paste("agg_value ~", session_col))
      cohens_d_result <- rstatix::cohens_d(processed_data, formula_for_d, paired = TRUE)

      icc_value <- chosen_icc$value
      icc_magnitude <- dplyr::case_when(
        icc_value < 0.1 ~ "Trivial", icc_value < 0.3 ~ "Low",
        icc_value < 0.5 ~ "Moderate", icc_value < 0.7 ~ "High",
        icc_value < 0.9 ~ "Very High", icc_value <= 1.0 ~ "Nearly Perfect",
        TRUE ~ "Error"
      )
      usefulness <- ifelse(round(te, 2) < swc, "Good", ifelse(round(te, 2) == swc, "OK", "Marginal"))

      # --- 结果汇总 ---
      results_df <- data.frame(
        Variable = final_var_name,
        stats_summary,
        "ICC (95% CI)" = paste0(round(chosen_icc$value, 2), ' (', round(chosen_icc$lbound, 2), ',', round(chosen_icc$ubound, 2), ')'),
        'p (ICC)' = round(chosen_icc$p, 3),
        'ICC Magnitude' = icc_magnitude,
        'CV%' = round(cv_percent, 2),
        SEM = round(sem, 2),
        TE = round(te, 2),
        SWC = round(swc, 2),
        Usefulness = usefulness,
        'p (t)' = round(ttest_result$p.value, 3),
        'ES (d)' = round(cohens_d_result$effsize, 2),
        'ES Magnitude' = cohens_d_result$magnitude,
        check.names = FALSE
      )
      return(results_df)
    }
  )
  return(all_results_df)
}


# 函数3：多次重复测量信度 (支持批量处理)----
#' @title 从长格式数据分析重测信度 (支持单个或多个变量)
#' @description 为长格式数据设计。可对单个或多个因变量进行分析。
#'   函数会自动聚合一个“会话(Session)”内的多次“尝试(Trial)”，
#'   或将一个“会话(Session)”作为独立个体，然后计算“会话(Session)”之间的信度。
#'
#' @param data 一个长格式的数据框。
#' @param subject_col 包含被试ID的列名。
#' @param session_col 包含测试会话/时间点的列名。建议此列为因子类型以保证顺序正确。
#' @param value_cols 包含需要分析的指标数值的列名。可以是单个字符串，也可以是包含多个字符串的向量。
#' @param trial_type 用户选择的ICC类型。当 trial_col 被使用且 method = 'mean' 时，此参数会被自动设为 "average"。
#' @param trial_col (可选) 包含试次编号的列名。如果提供，函数会聚合这些试次。
#' @param method 聚合会话内多次尝试的方法。
#' @param icc_model (可选) 传递给`ICC`函数的模型。默认为 "twoway"。可选 "oneway" 或 "twoway"。
#' @param icc_type (可选) 传递给`ICC`函数的类型。默认为 "agreement"。可选 "agreement" 或 "consistency"。
#' @param var_names (可选) 一个与 `value_cols` 一一对应的字符向量，用于在结果中重命名变量。
#'   如果提供，其长度必须与 `value_cols` 相同。
#'
#' @return 返回一个数据框，包含一个或多个变量的会话间信度分析结果。
#' @importFrom dplyr %>% group_by summarise ungroup n_distinct select case_when
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang sym syms
#' @importFrom irr icc
#' @importFrom stats aov sd
#' @importFrom purrr map_dfr
#' @importFrom rstatix anova_test get_anova_table
#' @export
#' @examples
#' # 使用全部三个会话进行 ANOVA 信度分析
#' analyze_reliability_ANOVA(
#'   data = reliability_data,
#'   subject_col = "Subject",
#'   session_col = "Session",
#'   value_cols = "Score",
#'   trial_col = "Trial",
#'   method = "mean"
#' )

analyze_reliability_ANOVA <- function(data,
                                      subject_col,
                                      session_col,
                                      value_cols,
                                      trial_type = c("single", "average"),
                                      trial_col = NULL,
                                      method = "mean",
                                      icc_model = c("twoway", "oneway"),
                                      icc_type = c("agreement", "consistency"),
                                      swc_threshold = NULL,
                                      var_names = NULL) {

  final_icc_model <- match.arg(icc_model)
  final_icc_type <- match.arg(icc_type)

  initial_trial_type <- match.arg(trial_type)

  if (!is.null(var_names) && length(var_names) != length(value_cols)) {
    stop("错误: 'var_names' 的长度必须与 'value_cols' 的长度完全相同。")
  }

  all_results_df <- purrr::imap_dfr(
    .x = value_cols,
    .f = function(current_value_col, index) {

      final_var_name <- if (!is.null(var_names)) var_names[index] else current_value_col
      final_trial_type <- initial_trial_type
      temp_trial_col <- trial_col

      if (!is.null(temp_trial_col) && dplyr::n_distinct(data[[temp_trial_col]]) == 1) {
        message(paste0("(", current_value_col, ") 提醒: trial_col 中只发现一个试次，无法进行聚合。trial_type 已自动设为 'single'。"))
        final_trial_type <- "single"
        temp_trial_col <- NULL
      }

      if (!is.null(temp_trial_col) && method == "mean") {
        if (final_trial_type != "average") {
          message(paste0("(", current_value_col, ") 提醒: 由于 method = 'mean', trial_type 已自动设为 'average' 以计算均值的信度 (ICC,k)。"))
        }
        final_trial_type <- "average"
      }

      local_data <- data
      if (!is.factor(local_data[[session_col]])) {
        local_data[[session_col]] <- as.factor(local_data[[session_col]])
      }

      subject_sym <- rlang::sym(subject_col)
      session_sym <- rlang::sym(session_col)
      value_sym <- rlang::sym(current_value_col)

      if (!is.null(temp_trial_col)) {
        agg_fun <- switch(method,
                          mean = function(x) mean(x, na.rm = TRUE),
                          max = function(x) max(x, na.rm = TRUE),
                          min = function(x) min(x, na.rm = TRUE),
                          median = function(x) median(x, na.rm = TRUE),
                          stop("错误: method 必须是 'mean', 'max', 'min', 或 'median' 之一。")
        )
        processed_data <- local_data %>%
          dplyr::group_by(!!subject_sym, !!session_sym) %>%
          dplyr::summarise(agg_value = agg_fun(!!value_sym), .groups = 'drop')
      } else {
        processed_data <- local_data %>%
          dplyr::select(!!subject_sym, !!session_sym, agg_value = !!value_sym)
      }

      stats_summary <- processed_data %>%
        dplyr::group_by(!!session_sym) %>%
        dplyr::summarise(
          mean_sd = paste0(round(mean(agg_value, na.rm = TRUE), 2), " ± ",
                           round(stats::sd(agg_value, na.rm = TRUE), 2)),
          .groups = 'drop'
        ) %>%
        tidyr::pivot_wider(names_from = !!session_sym, values_from = mean_sd)

      wide_data <- processed_data %>%
        tidyr::pivot_wider(id_cols = !!subject_sym, names_from = !!session_sym, values_from = agg_value)

      measure_cols <- levels(local_data[[session_col]])
      icc_results <- irr::icc(
        wide_data[, measure_cols],
        model = final_icc_model,
        type = final_icc_type,
        unit = final_trial_type
      )

      chosen_icc <- icc_results

      icc_value <- chosen_icc$value
      icc_magnitude <- dplyr::case_when(
        icc_value < 0.1 ~ "Trivial", icc_value < 0.3 ~ "Low",
        icc_value < 0.5 ~ "Moderate", icc_value < 0.7 ~ "High",
        icc_value < 0.9 ~ "Very High", icc_value <= 1.0 ~ "Nearly Perfect",
        TRUE ~ "Error"
      )

      long_for_calc <- wide_data %>%
        dplyr::mutate(subject_id = dplyr::row_number()) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(measure_cols), names_to = "trial", values_to = "value") %>%
        dplyr::mutate(trial = factor(.data$trial, levels = measure_cols))

      subject_means <- long_for_calc %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::summarise(mean_value = mean(.data$value, na.rm = TRUE), .groups = 'drop')

      between_subject_sd <- stats::sd(subject_means$mean_value, na.rm = TRUE)
      sem <- between_subject_sd * sqrt(1 - chosen_icc$value)

      aov_for_te <- stats::aov(value ~ as.factor(subject_id), data = long_for_calc)
      mse <- summary(aov_for_te)[[1]]["Residuals", "Mean Sq"]
      te <- sqrt(mse)
      grand_mean <- mean(long_for_calc$value, na.rm = TRUE)
      cv_percent <- (te / grand_mean) * 100
      swc <- swc_threshold * between_subject_sd

      rm_anova_results <- rstatix::anova_test(
        data = long_for_calc, dv = value, wid = subject_id, within = trial, effect.size = "pes"
      )
      anova_table <- rstatix::get_anova_table(rm_anova_results)

      pes_interpretation <- dplyr::case_when(
        anova_table$pes < 0.01 ~ "Trivial", anova_table$pes < 0.06 ~ "Small",
        anova_table$pes < 0.14 ~ "Moderate", TRUE ~ "Large"
      )

      usefulness <- ifelse(round(te, 2) < swc, "Good", ifelse(round(te, 2) == swc, "OK", "Marginal"))

      results_df <- data.frame(
        Variable = final_var_name,
        stats_summary,
        "ICC (95% CI)" = paste0(round(chosen_icc$value, 2), ' (', round(chosen_icc$lbound, 2), ',', round(chosen_icc$ubound, 2), ')'),
        'p (ICC)' = round(chosen_icc$p, 3),
        'ICC Magnitude' = icc_magnitude,
        'CV%' = round(cv_percent, 2),
        SEM = round(sem, 2),
        MSE = round(mse,2),
        TE = round(te, 2),
        SWC = round(swc, 2),
        Usefulness = usefulness,
        'p (ANOVA)' = round(anova_table$p, 3),
        'ES (η²ₚ)' = round(anova_table$pes, 2),
        'ES Magnitude' = pes_interpretation,
        check.names = FALSE
      )

      return(results_df)
    }
  )
  return(all_results_df)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


# 函数4：效度分析----
#' @title 批量分析效度
#' @description (推荐) 用于效度分析，通过明确指定金标准和测试方法，进行全面的比较...
# ... (roxygen comments remain the same) ...
#' @param criterion_method 一个字符串，指定 `method_col` 中代表**金标准**的水平名称。
#' @param test_method 一个字符串，指定 `method_col` 中代表**测试方法**的水平名称。
# ... (other params remain the same) ...
#' @export
#' @examples
#' # 使用内置的 validity_data 数据集进行效度分析
#' analysis_validity(
#'   data = validity_data,
#'   subject_col = "Subject",
#'   method_col = "Device",
#'   criterion_method = "GasAnalyzer",
#'   test_method = "PortableDevice",
#'   value_cols = "VO2max"
#' )

analysis_validity <- function(data,
                              subject_col,
                              method_col,
                              criterion_method, # <--- Updated parameter name
                              test_method,
                              value_cols,
                              trial_col = NULL,
                              method = "mean",
                              swc_threshold = NULL,
                              var_names = NULL) {

  # --- Parameter checks ---
  valid_data <- data[data[[method_col]] %in% c(criterion_method, test_method), ]
  if (dplyr::n_distinct(valid_data[[method_col]]) != 2) {
    stop("错误: 在数据中找不到您指定的两种方法。请检查 'criterion_method' 和 'test_method' 的名称是否正确。")
  }
  if (!is.null(var_names) && length(var_names) != length(value_cols)) {
    stop("错误: 'var_names' 的长度必须与 'value_cols' 的长度完全相同。")
  }

  all_results_df <- purrr::imap_dfr(
    .x = value_cols,
    .f = function(current_value_col, index) {

      final_var_name <- if (!is.null(var_names)) var_names[index] else current_value_col

      local_data <- valid_data; subject_sym <- rlang::sym(subject_col); method_sym <- rlang::sym(method_col); value_sym <- rlang::sym(current_value_col)
      if (!is.null(trial_col)) {
        agg_fun <- switch(method, mean = function(x) mean(x, na.rm = TRUE), max = function(x) max(x, na.rm = TRUE), min = function(x) min(x, na.rm = TRUE), median = function(x) stats::median(x, na.rm = TRUE), stop("错误: method 必须是 'mean', 'max', 'min', 或 'median' 之一。"))
        processed_data <- local_data %>% dplyr::group_by(!!subject_sym, !!method_sym) %>% dplyr::summarise(agg_value = agg_fun(!!value_sym), .groups = 'drop')
      } else {
        processed_data <- local_data %>% dplyr::select(!!subject_sym, !!method_sym, agg_value = !!value_sym)
      }

      stats_summary <- processed_data %>%
        dplyr::group_by(!!method_sym) %>%
        dplyr::summarise(
          mean_sd = paste0(round(mean(agg_value, na.rm = TRUE), 2), " ± ",
                           round(stats::sd(agg_value, na.rm = TRUE), 2)),
          .groups = 'drop'
        ) %>%
        tidyr::pivot_wider(names_from = !!method_sym, values_from = mean_sd)

      wide_data <- processed_data %>% tidyr::pivot_wider(id_cols = !!subject_sym, names_from = !!method_sym, values_from = agg_value)
      if (!all(c(criterion_method, test_method) %in% names(wide_data))) { stop("处理数据后出错，无法找到指定的金标准或测试方法列。") }
      criterion_measure <- wide_data[[criterion_method]]; test_measure <- wide_data[[test_method]]


      ttest_result <- stats::t.test(test_measure, criterion_measure, paired = TRUE)
      diff_conf_int <- ttest_result$conf.int

      formula_for_d <- stats::as.formula(paste("agg_value ~", method_col))
      cohens_d_result <- rstatix::cohens_d(processed_data, formula_for_d, paired = TRUE) # <--- FIXED LINE

      mean_diff <- mean(test_measure - criterion_measure, na.rm = TRUE)
      corr_test_result <- stats::cor.test(test_measure, criterion_measure)
      pearson_r <- corr_test_result$estimate;
      r_conf_int <- corr_test_result$conf.int;
      r_p_value <- corr_test_result$p.value
      r_magnitude <- dplyr::case_when(abs(pearson_r) < 0.1 ~ "Trivial", abs(pearson_r) < 0.3 ~ "Small", abs(pearson_r) < 0.5 ~ "Moderate", abs(pearson_r) < 0.7 ~ "Large", abs(pearson_r) < 0.9 ~ "Very Large", TRUE ~ "Nearly Perfect")
      sd_criterion <- stats::sd(criterion_measure, na.rm = TRUE)
      see <- sd_criterion * sqrt(1 - pearson_r^2)
      swc <- swc_threshold * sd_criterion
      mbi_inference <- dplyr::case_when(diff_conf_int[2] < -swc ~ "Substantially Lower", diff_conf_int[1] > swc ~ "Substantially Higher", diff_conf_int[1] > -swc & diff_conf_int[2] < swc ~ "Trivial", TRUE ~ "Unclear")
      se_diff <- (diff_conf_int[2] - diff_conf_int[1]) / (2 * qt(0.975, df = ttest_result$parameter))
      prob_lower <- round(pt((-swc - mean_diff) / se_diff, df = ttest_result$parameter) * 100)
      prob_higher <- round((1 - pt((swc - mean_diff) / se_diff, df = ttest_result$parameter)) * 100)
      prob_trivial <- 100 - prob_lower - prob_higher
      mbi_probs <- paste0(prob_lower, "/", prob_trivial, "/", prob_higher)

      # --- Final results table ---
      results_df <- data.frame(
        "Variable" = final_var_name,
        stats_summary,
        "Mean Diff (95% CI)" = paste0(round(mean_diff, 2), " (", round(diff_conf_int[1], 2), ", ", round(diff_conf_int[2], 2), ")"),
        "p (t)" = round(ttest_result$p.value, 3),
        "ES (d)" = round(cohens_d_result$effsize, 2),
        "ES Magnitude" = cohens_d_result$magnitude,
        "Pearson's r (95% CI)" = paste0(round(pearson_r, 2), " (", round(r_conf_int[1], 2), ", ", round(r_conf_int[2], 2), ")"),
        'p (r)' = round(r_p_value,3),
        "R Magnitude" = r_magnitude,
        "SEE" = round(see, 2),
        "MBI" = mbi_inference,
        "MBI Probs (L/T/H %)" = mbi_probs,
        check.names = FALSE
      )
      return(results_df)
    }
  )
  return(all_results_df)
}


# 函数5：两个会话间的B-A分析----
#' @title 批量计算Bland-Altman分析统计量
#' @description 专为比较两个会话(test-retest)的场景设计，或用于效度分析，可对单个或多个因变量进行分析，
#'   并计算Bland-Altman分析所需的核心统计量。
#'
#' @param data 一个长格式的数据框。
#' @param subject_col 包含被试ID的列名。
#' @param session_col 包含测试会话/时间点的列名。此列必须恰好包含两个水平。
#' @param value_cols 包含需要分析的指标数值的列名。可以是单个字符串或字符串向量。
#' @param trial_col (可选) 包含试次编号的列名。
#' @param method 聚合会话内多次尝试的方法。
#' @param var_names (可选) 一个与 `value_cols` 一一对应的字符向量，用于在结果中重命名变量。
#'
#' @return 返回一个数据框，包含一个或多个变量的Bland-Altman分析核心统计量。
#' @importFrom dplyr %>% group_by summarise ungroup n_distinct select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#' @importFrom stats sd median
#' @importFrom purrr imap_dfr
#' @export
analyze_B_A <- function(data,
                        subject_col,
                        session_col,
                        value_cols,
                        trial_col = NULL,
                        method = "mean",
                        var_names = NULL) {

  # --- 参数预处理和检查 ---
  if (dplyr::n_distinct(data[[session_col]]) != 2) {
    stop("错误: 'session_col' 必须恰好包含两个不同的会话水平才能使用此函数。")
  }
  if (!is.null(var_names) && length(var_names) != length(value_cols)) {
    stop("错误: 'var_names' 的长度必须与 'value_cols' 的长度完全相同。")
  }

  list_of_results <- purrr::imap(
    .x = value_cols,
    .f = function(current_value_col, index) {

      # --- 确定最终的变量名 ---
      final_var_name <- if (!is.null(var_names)) var_names[index] else current_value_col

      # --- 以下为原函数的主体逻辑 ---
      local_data <- data
      if (!is.factor(local_data[[session_col]])) {
        local_data[[session_col]] <- as.factor(local_data[[session_col]])
      }

      subject_sym <- rlang::sym(subject_col)
      session_sym <- rlang::sym(session_col)
      value_sym <- rlang::sym(current_value_col)

      if (!is.null(trial_col)) {
        agg_fun <- switch(method,
                          mean = function(x) mean(x, na.rm = TRUE),
                          max = function(x) max(x, na.rm = TRUE),
                          min = function(x) min(x, na.rm = TRUE),
                          median = function(x) stats::median(x, na.rm = TRUE),
                          stop("错误: method 必须是 'mean', 'max', 'min', 或 'median' 之一。")
        )
        processed_data <- local_data %>%
          dplyr::group_by(!!subject_sym, !!session_sym) %>%
          dplyr::summarise(agg_value = agg_fun(!!value_sym), .groups = 'drop')
      } else {
        processed_data <- local_data %>%
          dplyr::select(!!subject_sym, !!session_sym, agg_value = !!value_sym)
      }
      wide_data <- processed_data %>%
        tidyr::pivot_wider(id_cols = !!subject_sym, names_from = !!session_sym, values_from = agg_value)
      measure_cols <- levels(local_data[[session_col]])

      diffs <- wide_data[[measure_cols[2]]] - wide_data[[measure_cols[1]]]
      bias <- mean(diffs, na.rm = TRUE)
      sd_diff <- stats::sd(diffs, na.rm = TRUE)
      upper_loa <- bias + 1.96 * sd_diff
      lower_loa <- bias - 1.96 * sd_diff
      n_subjects <- sum(!is.na(diffs))
      se_diff <- sd_diff / sqrt(n_subjects)
      bias_ci_lower <- bias - 1.96 * se_diff
      bias_ci_upper <- bias + 1.96 * se_diff

      # --- 结果汇总 (使用更适合发表的列名) ---
      table_df  <- data.frame(
        "Variable" = final_var_name,
        "Bias (95%CI)" = paste0(round(bias,2),' (',round(bias_ci_lower,2),',',round(bias_ci_upper,2),')'),
        "SD of Difference" = sd_diff,
        "Upper LoA" = upper_loa,
        "Lower LoA" = lower_loa,
        check.names = FALSE
      )

      params_df <- data.frame(
        Variable = final_var_name,
        Bias = bias,
        Upper_LoA = upper_loa,
        Lower_LoA = lower_loa,
        Bias_CI_Lower = bias_ci_lower,
        Bias_CI_Upper = bias_ci_upper
      )

      return(list(table = table_df, params = params_df))
    }
  )
  final_table <- purrr::map_dfr(list_of_results, ~.x$table)
  final_params <- purrr::map_dfr(list_of_results, ~.x$params)

  return(list(
    results = final_table,
    params = final_params)
  )
}


# 函数6：多次重复测量云雨图/差异对比----
#' @title 从长格式数据可视化多次重复测量差异
#' @description 创建一个或多个“云雨图”(Raincloud Plot)，可自动添加各组间的两两比较显著性，
#'   并可选择性地隐藏不显著（ns）的标记和连接线。
#'
#' @param data 一个长格式的数据框(data.frame)。
#' @param subject_col 包含被试ID的列名。
#' @param session_col 包含将在X轴上显示的会话/时间点的列名。
#' @param value_cols 包含将在Y轴上显示的指标数值的列名。可以是单个字符串或字符串向量。
#' @param var_names (可选) 一个与 `value_cols` 一一对应的字符向量，用于设定每个图形的Y轴标题。
#' @param custom_colors (可选) 一个包含颜色的字符向量，用于自定义每个会話的颜色。
#' @param add_significance (可选) 一个逻辑值 (`TRUE`/`FALSE`)，决定是否添加两两对比的显著性检验。默认为 `FALSE`。
#' @param hide_ns (可选) 一个逻辑值 (`TRUE`/`FALSE`)。如果为 `TRUE` (默认)，将只显示显著（p < 0.05）的对比连接线。
#' @param comparison_method (可选) 一个字符串，指定进行显著性检验的统计方法。默认为 "t.test"。
#' @param paired (可选) 一个逻辑值 (`TRUE`/`FALSE`)，指定是否进行配对检验。对于重复测量数据，应设为 `TRUE` (默认)。
#'
#' @return 返回一个包含一个或多个 ggplot 对象的列表。
#' @import ggplot2
#' @import gghalves
#' @importFrom dplyr %>% filter
#' @importFrom rlang sym
#' @importFrom purrr imap
#' @importFrom ggpubr stat_compare_means stat_pvalue_manual
#' @importFrom rstatix pairwise_t_test add_xy_position
#' @importFrom utils combn
#' @export
plot_reliability <- function(data,
                             subject_col,
                             session_col,
                             value_cols,
                             var_names = NULL,
                             custom_colors = NULL,
                             add_significance = TRUE,
                             hide_ns = TRUE,
                             comparison_method = "t.test",
                             paired = TRUE) {

  # --- 参数检查 ---
  if (!is.null(var_names) && length(var_names) != length(value_cols)) {
    stop("错误: 'var_names' 的长度必须与 'value_cols' 的长度完全相同。")
  }

  if (!is.factor(data[[session_col]])) {
    data[[session_col]] <- as.factor(data[[session_col]])
  }

  plots_list <- purrr::imap(
    .x = value_cols,
    .f = function(current_value_col, index) {

      y_axis_title <- if (!is.null(var_names)) var_names[index] else current_value_col

      # --- 基础绘图 ---
      p <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data[[session_col]],
                                        y = .data[[current_value_col]],
                                        fill = .data[[session_col]])) +
        gghalves::geom_half_point(side = "l", shape = 21, alpha = 0.6, size = 2, position = ggplot2::position_jitter(width = 0.1, seed = 1)) +
        gghalves::geom_half_violin(side = "r", alpha = 0.6, trim = FALSE) +
        ggplot2::geom_boxplot(width = 0.15, alpha = 0.7, outlier.shape = NA) +
        ggplot2::geom_line(ggplot2::aes(group = .data[[subject_col]]), color = "grey60", alpha = 0.4) + # 修正了 .data 的用法
        ggplot2::labs(x = session_col, y = y_axis_title, fill = session_col) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::theme(legend.position = "none", axis.title = ggplot2::element_text(face = "bold"), axis.text = ggplot2::element_text(color = "black"))

      # --- 应用自定义颜色 ---
      if (!is.null(custom_colors)) {
        p <- p + ggplot2::scale_fill_manual(values = custom_colors)
      } else {
        p <- p + ggplot2::scale_fill_viridis_d(option = "C")
      }

      # --- 核心修改：添加显著性检验 ---
      if (add_significance) {
        if (length(levels(data[[session_col]])) >= 2) {

          formula <- as.formula(paste(current_value_col, "~", session_col))

          if (hide_ns) {
            # 1. 先进行统计检验
            stat_test <- rstatix::pairwise_t_test(data, formula, paired = paired) %>%
              rstatix::add_xy_position(x = session_col)

            # 2. 只筛选出显著的结果
            significant_stats <- stat_test %>% dplyr::filter(p.adj < 0.05) # 使用调整后的p值

            # 3. 只将显著的结果添加到图中
            if (nrow(significant_stats) > 0) {
              p <- p + ggpubr::stat_pvalue_manual(
                significant_stats,
                label = "p.adj.signif", # 使用调整后p值的显著性标记
                tip.length = 0.01
              )
            }
          } else {
            # 如果不隐藏ns，使用之前的方法，显示所有对比
            comparisons_list <- utils::combn(levels(data[[session_col]]), 2, simplify = FALSE)
            p <- p + ggpubr::stat_compare_means(
              comparisons = comparisons_list,
              method = comparison_method,
              paired = paired,
              label = "p.signif"
            )
          }
        } else {
          warning("提醒: 至少需要两个组才能进行显著性比较。")
        }
      }

      return(p)
    }
  )

  # --- 返回结果 ---
  if (length(plots_list) == 1) {
    return(plots_list[[1]])
  } else {
    names(plots_list) <- value_cols
    return(plots_list)
  }
}


# 函数7：两个会话的B-A图/效度B-A----
#' @title 批量绘制带边缘密度图的Bland-Altman图
#' @description 专为比较两个会话(test-retest)或进行效度分析的场景设计。
#'   此函数将Bland-Altman散点图与一个展示差值分布的边缘密度图（山峦图）结合在一起。
#'
#' @param data 一个长格式的数据框。
#' @param subject_col 包含被试ID的列名。
#' @param session_col 包含测试会话/时间点的列名。此列必须恰好包含两个水平。
#' @param value_cols 包含需要分析的指标数值的列名。可以是单个字符串或字符串向量。
#' @param trial_col (可选) 包含试次编号的列名。
#' @param method 聚合会话内多次尝试的方法。
#' @param var_names (可选) 一个与 `value_cols` 一一对应的字符向量，用于设定每个图形的标题。
#' @param point_fill (可选) 散点的填充颜色。
#' @param density_fill (可选) 右侧密度图的填充颜色。
#' @param bias_color (可选) Bias线的颜色。
#' @param loa_color (可选) LoA线的颜色。
#' @param ci_fill (可选) Bias置信区间矩形的填充颜色。
#'
#' @return 返回一个包含一个或多个组合图（ggplot对象）的列表。
#' @import ggplot2
#' @importFrom dplyr %>% group_by summarise ungroup n_distinct select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym syms
#' @importFrom purrr imap
#' @importFrom cowplot plot_grid
#' @export
plot_B_A <- function(data,
                     subject_col,
                     session_col,
                     value_cols,
                     trial_col = NULL,
                     method = "mean",
                     var_names = NULL,
                     point_fill = "#D25D5D",
                     density_fill = "#E7D3D3",
                     density_color = "#722323",
                     bias_color = "#722323",
                     loa_color = "#B9375D",
                     ci_fill = "#E7D3D3") {

  # --- 参数检查 ---
  if (dplyr::n_distinct(data[[session_col]]) != 2) { stop("错误: 'session_col' 必须恰好包含两个不同的会话水平才能使用此函数。") }
  if (!is.null(var_names) && length(var_names) != length(value_cols)) { stop("错误: 'var_names' 的长度必须与 'value_cols' 的长度完全相同。") }

  plots_list <- purrr::imap(
    .x = value_cols,
    .f = function(current_value_col, index) {

      plot_title <- if (!is.null(var_names)) var_names[index] else current_value_col

      local_data <- data; if (!is.factor(local_data[[session_col]])) { local_data[[session_col]] <- as.factor(local_data[[session_col]]) }
      subject_sym <- rlang::sym(subject_col); session_sym <- rlang::sym(session_col); value_sym <- rlang::sym(current_value_col)
      if (!is.null(trial_col)) {
        agg_fun <- switch(method, mean = function(x) mean(x, na.rm = TRUE), max = function(x) max(x, na.rm = TRUE), min = function(x) min(x, na.rm = TRUE), median = function(x) stats::median(x, na.rm = TRUE), stop("错误: method 必须是 'mean', 'max', 'min', 或 'median' 之一。"))
        processed_data <- local_data %>% dplyr::group_by(!!subject_sym, !!session_sym) %>% dplyr::summarise(agg_value = agg_fun(!!value_sym), .groups = 'drop')
      } else {
        processed_data <- local_data %>% dplyr::select(!!subject_sym, !!session_sym, agg_value = !!value_sym)
      }
      wide_data <- processed_data %>% tidyr::pivot_wider(id_cols = !!subject_sym, names_from = !!session_sym, values_from = agg_value)
      measure_cols <- levels(local_data[[session_col]])
      plot_data <- data.frame(mean = (wide_data[[measure_cols[1]]] + wide_data[[measure_cols[2]]]) / 2, diff = wide_data[[measure_cols[2]]] - wide_data[[measure_cols[1]]])

      analysis_results <- analyze_B_A(data = data, subject_col = subject_col, session_col = session_col, value_cols = current_value_col, trial_col = trial_col, method = method)
      stats_for_plot <- analysis_results$params

      # 图1: 主B-A散点图 (Main Plot)
      main_plot <- ggplot(plot_data, aes(x = .data$mean, y = .data$diff)) +
        geom_rect(data = stats_for_plot, aes(xmin = -Inf, xmax = Inf, ymin = .data$Bias_CI_Lower, ymax = .data$Bias_CI_Upper), fill = ci_fill, alpha = 0.5, inherit.aes = FALSE) +
        geom_point(shape = 21, size = 3, fill = point_fill, alpha = 0.7) +
        geom_hline(yintercept = stats_for_plot$Bias, color = bias_color, linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = stats_for_plot$Upper_LoA, color = loa_color, linetype = "dotdash", linewidth = 1) +
        geom_hline(yintercept = stats_for_plot$Lower_LoA, color = loa_color, linetype = "dotdash", linewidth = 1) +
        annotate("text", x = Inf, y = stats_for_plot$Bias, label = paste("Bias =", round(stats_for_plot$Bias, 2)), hjust = 1.05, vjust = -0.5, color = bias_color, size = 4, fontface = "bold") +
        annotate("text", x = Inf, y = stats_for_plot$Upper_LoA, label = paste("+1.96 SD =", round(stats_for_plot$Upper_LoA, 2)), hjust = 1.05, vjust = -0.5, color = loa_color, size = 4, fontface = "bold") +
        annotate("text", x = Inf, y = stats_for_plot$Lower_LoA, label = paste("-1.96 SD =", round(stats_for_plot$Lower_LoA, 2)), hjust = 1.05, vjust = -0.5, color = loa_color, size = 4, fontface = "bold") +
        labs(title = paste("Bland-Altman Plot for", plot_title), x = "Mean", y = "Difference") +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_text(face = "bold"), axis.text = element_text(color = "black", face = "bold"))

      # 图2: 右侧边缘密度图 ("山峦图")
      density_plot <- ggplot(plot_data, aes(x = .data$diff)) +
        geom_density(alpha = 0.7, fill = density_fill,color = density_color) +
        coord_flip() + # 关键：将密度图旋转90度
        theme_void()   # 移除所有背景、坐标轴和标签

      combined_plot <- cowplot::plot_grid(
        main_plot,
        density_plot,
        ncol = 2,
        align = "h", # 水平对齐
        rel_widths = c(4, 1) # 主图占3.5份宽度，密度图占1份
      )

      return(combined_plot)
    }
  )

  # --- 返回结果 ---
  if (length(plots_list) == 1) { return(plots_list[[1]]) } else { names(plots_list) <- value_cols; return(plots_list) }
}



# 函数8：效度分析相关性图----
#' @title 绘制效度相关性图
#' @description 用于效度分析的可视化，绘制“金标准方法”(Y轴)相对于“测试方法”(X轴)的散点图。
#'   图中自动包含 y=x 一致性线、线性回归线、回归方程和R²。可自定义所有颜色元素，并选择性地添加边缘密度图。
#'
#' @param data 一个长格式的数据框。
#' @param subject_col 包含被试ID的列名。
#' @param method_col 包含测量方法名称的列名。
#' @param criterion_method 一个字符串，指定 `method_col` 中代表金标准的水平名称 (将显示在Y轴)。
#' @param test_method 一个字符串，指定 `method_col` 中代表测试方法的水平名称 (将显示在X轴)。
#' @param value_cols 包含需要分析的指标数值的列名。可以是单个字符串或字符串向量。
#' @param var_names (可选) 一个与 `value_cols` 一一对应的字符向量，用于设定每个图形的总标题。
#' @param marginal_plot (可选) 一个逻辑值 (`TRUE`/`FALSE`)，决定是否添加边缘密度图。默认为 `TRUE`。
#' @param point_fill (可选) 散点的填充颜色。
#' @param line_color (可选) 回归线的颜色。
#' @param ci_fill (可选) 回归线置信区间的填充颜色。
#' @param density_fill (可选) 边缘密度图的填充颜色。
#'
#' @return 返回一个包含一个或多个 ggplot 对象的列表。
#' @import ggplot2
#' @importFrom dplyr %>% select all_of
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#' @importFrom purrr imap
#' @importFrom ggpmisc stat_poly_eq
#' @importFrom ggExtra ggMarginal
#' @export
plot_validity_correlation <- function(data,
                                      subject_col,
                                      method_col,
                                      criterion_method,
                                      test_method,
                                      value_cols,
                                      var_names = NULL,
                                      marginal_plot = TRUE,
                                      point_fill = "#D25D5D",
                                      line_color = "#B9375D",
                                      ci_fill = "#E7D3D3",
                                      density_fill = "#E7D3D3",
                                      density_color = "#B9375D") {

  if (!is.null(var_names) && length(var_names) != length(value_cols)) {
    stop("错误: 'var_names' 的长度必须与 'value_cols' 的长度完全相同。")
  }

  plots_list <- purrr::imap(
    .x = value_cols,
    .f = function(current_value_col, index) {

      plot_title <- if (!is.null(var_names)) var_names[index] else current_value_col
      x_axis_label <- paste(current_value_col, "-", test_method)
      y_axis_label <- paste(current_value_col, "-", criterion_method)

      valid_data <- data[data[[method_col]] %in% c(criterion_method, test_method), ]
      wide_data <- valid_data %>%
        tidyr::pivot_wider(
          id_cols = all_of(subject_col),
          names_from = all_of(method_col),
          values_from = all_of(current_value_col)
        )
      if (!all(c(criterion_method, test_method) %in% names(wide_data))) {
        stop("处理数据后出错，无法找到指定的金标准或测试方法列。")
      }

      p <- ggplot(wide_data, aes(x = .data[[test_method]], y = .data[[criterion_method]])) +
        geom_abline(linetype = "dashed", color = "grey80", linewidth = 1) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = line_color, fill = ci_fill, alpha = 0.5) +
        geom_point(size = 3, alpha = 0.8, shape = 21, fill = point_fill, color = "#D25D5D") +
        ggpmisc::stat_poly_eq(
          aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
          formula = y ~ x, parse = TRUE, label.x = "left", label.y = "top", size = 4.5,color = 'black',family = "Times New Roman"
        ) +
        coord_equal(ratio = 1) +
        labs(
          title = plot_title,
          x = x_axis_label,
          y = y_axis_label
        ) +
        theme_classic(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size=16),
          axis.title = element_text(face = "bold", size=14),
          axis.text = element_text(color = "black", face = "bold", size=12)
        )

      if (marginal_plot) {
        p <- ggExtra::ggMarginal(p, type = 'density', margins = 'both', fill = density_fill,color = density_color, alpha = 0.4)
      }

      return(p)
    }
  )

  if (length(plots_list) == 1) {
    return(plots_list[[1]])
  } else {
    names(plots_list) <- value_cols
    return(plots_list)
  }
}


# 函数9：自动导出三线表----
#' @title 将分析结果导出为论文级三线表
#' @description 一个通用的导出函数，可以将任何数据框以“三线表”格式导出到 Word (.docx) 文件中。
#'   可以精确控制三线表中上、中、下三条线的宽度。
#'
#' @param result_df 需要导出的数据框。
#' @param table_title 表格的标题。
#' @param filename 导出的 Word 文件的路径和名称。
#' @param auto_map_title (可选) 逻辑值 (`TRUE`/`FALSE`)。如果为 `TRUE` (默认)，函数将尝试自动映射标题。
#' @param top_border_width (可选) 表格顶部线条的宽度（磅）。默认为 1.5。
#' @param middle_border_width (可选) 表格中间（表头下）线条的宽度（磅）。默认为 1.0。
#' @param bottom_border_width (可选) 表格底部线条的宽度（磅）。默认为 1.5。
#'
#' @return 该函数没有返回值，它会将表格直接保存为文件。
#' @import flextable
#' @export
export_table <- function(result_df,
                         table_title = "Table: Analysis Results",
                         filename = "Reliability_and_Validity_results.docx",
                         auto_map_title = TRUE,
                         top_border_width = 1.5,
                         middle_border_width = 1.0,
                         bottom_border_width = 1.5) {

  if (is.list(result_df) && "results_for_publication" %in% names(result_df)) {
    result_df <- result_df$results_for_publication
  }

  if (!is.data.frame(result_df)) {
    stop("错误: 'result_df' 必须是一个数据框或一个包含 'results_for_publication' 的列表。")
  }

  final_title <- table_title

  if (auto_map_title) {
    col_names <- names(result_df)
    if (any(c("Shapiro_p", "mean", "sd") %in% col_names)) {
      final_title <- "Table 1: Descriptive statistics"
    } else if ("ICC (95% CI)" %in% col_names) {
      final_title <- "Table 2：Realiability Analysis Results"
    } else if ("Pearson's r (95% CI)" %in% col_names) {
      final_title <- "Table 3: Validity Analysis Results"
    } else if (any(grepl("Bias", col_names))) { # 更稳健的B-A检测
      final_title <- "Table 4：Bland-Altman Analysis Results"
    } else {
      warning("无法自动匹配标题，将使用默认或用户指定的标题。")
    }
  }

  border_top <- officer::fp_border(width = top_border_width)
  border_middle <- officer::fp_border(width = middle_border_width)
  border_bottom <- officer::fp_border(width = bottom_border_width)

  ft <- flextable::flextable(result_df) %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = border_top, part = "header") %>%
    flextable::hline_bottom(border = border_middle, part = "header") %>%
    flextable::hline_bottom(border = border_bottom, part = "body") %>%
    flextable::set_caption(caption = final_title) %>%
    flextable::italic(italic = FALSE, part = "all") %>%
    flextable::autofit() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all")

  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, value = ft)
  print(doc, target = filename)
  message(paste("表格已成功导出到:", filename))
}
