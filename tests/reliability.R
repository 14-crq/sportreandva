library(sportreandva)

# 独立矩阵计算验证公式，包含明显会话偏移。
x <- cbind(c(10, 14, 18, 21, 25, 28), c(15, 20, 22, 29, 30, 36), c(23, 24, 31, 33, 39, 40))
d <- data.frame(id = rep(1:6, 3), session = rep(c("A", "B", "C"), each = 6), value = as.vector(x))
calc <- function(d, ...) analyze_reliability_ttest(d, "id", "session", "value", ...)
two <- subset(d, session != "C")
expected_te <- sd(x[, 2] - x[, 1]) / sqrt(2)
r <- calc(two)
stopifnot(nrow(r) == 1L, is.na(r$SWC), is.na(r$Usefulness), r$TE == round(expected_te, 2),
          r[["CV%"]] == round(100 * expected_te / mean(x[, 1:2]), 2))
log_te <- sd(log(x[, 2]) - log(x[, 1])) / sqrt(2)
stopifnot(calc(two, cv_method = "log")[["CV%"]] == round(100 * expm1(log_te), 2))

# SEM 与 ICC 模型及单位相匹配。
residual <- x[, 1:2] - rowMeans(x[, 1:2]) - rep(colMeans(x[, 1:2]), each = 6) + mean(x[, 1:2])
mse <- sum(residual^2) / 5
ms_session <- 6 * sum((colMeans(x[, 1:2]) - mean(x[, 1:2]))^2)
stopifnot(r$SEM == round(sqrt(mse + (ms_session - mse) / 6), 2),
          calc(two, icc_type = "consistency")$SEM == round(sqrt(mse), 2),
          calc(two, trial_type = "average")$SEM == round(sqrt((mse + (ms_session - mse) / 6) / 2), 2))
stopifnot(calc(two, icc_model = "oneway")$SEM == round(sqrt(sum((x[, 1:2] - rowMeans(x[, 1:2]))^2) / 6), 2))

# 行排序和会话内重复试次不改变估计单位。
stopifnot(isTRUE(all.equal(r, calc(two[c(8, 1, 12, 5, 3, 7, 2, 10, 6, 9, 4, 11), ]))))
trials <- rbind(transform(two, trial = 1), transform(two, trial = 2))
stopifnot(isTRUE(all.equal(r, calc(trials, trial_col = "trial"))))

# 多会话 TE 扣除会话效应，两个入口在两会话数据上保持一致。
ra <- analyze_reliability_ANOVA(two, "id", "session", "value")
stopifnot(ra$TE == r$TE, ra$SEM == r$SEM)
residual3 <- x - rowMeans(x) - rep(colMeans(x), each = 6) + mean(x)
ra3 <- analyze_reliability_ANOVA(d, "id", "session", "value")
stopifnot(ra3$TE == round(sqrt(sum(residual3^2) / 10), 2))
shifted <- d
shifted$value[shifted$session == "C"] <- shifted$value[shifted$session == "C"] + 100
stopifnot(analyze_reliability_ANOVA(shifted, "id", "session", "value")$TE == ra3$TE)

# 缺失值按完整被试统一排除，重复记录和非法对数输入必须报错。
missing <- two; missing$value[1] <- NA
stopifnot(isTRUE(all.equal(suppressWarnings(calc(missing)), calc(subset(two, id != 1)))))
fails <- function(expr) inherits(tryCatch(expr, error = identity), "error")
stopifnot(fails(calc(rbind(two, two[1, ]))), fails(calc(two, swc_threshold = -1)))
zero <- two; zero$value[1] <- 0
stopifnot(fails(calc(zero, cv_method = "log")))
unused <- two; unused$session <- factor(unused$session, levels = c("A", "B", "unused"))
stopifnot(isTRUE(all.equal(r, calc(unused))))

# 默认效度分析和内置示例均可运行。
v <- data.frame(id = rep(1:6, 2), device = rep(c("criterion", "test"), each = 6), value = as.vector(x[, 1:2]))
stopifnot(nrow(analysis_validity(v, "id", "device", "criterion", "test", "value")) == 1L)
