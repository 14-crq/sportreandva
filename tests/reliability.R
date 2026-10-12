library(sportreandva)

# 独立矩阵计算验证公式，包含明显会话偏移。
x <- cbind(c(10, 14, 18, 21, 25, 28), c(15, 20, 22, 29, 30, 36), c(23, 24, 31, 33, 39, 40))
d <- data.frame(id = rep(1:6, 3), session = rep(c("A", "B", "C"), each = 6), value = as.vector(x))
calc <- function(d, sem_method = "weir", cv_method = "raw", ...) {
  analyze_reliability_ttest(d, "id", "session", "value", sem_method = sem_method, cv_method = cv_method, ...)
}
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
stopifnot(calc(two, sem_method = "icc")$SEM == round(sqrt(mse + (ms_session - mse) / 6), 2),
          calc(two, icc_type = "consistency")$SEM == round(sqrt(mse), 2),
          calc(two, trial_type = "average", sem_method = "icc")$SEM == round(sqrt((mse + (ms_session - mse) / 6) / 2), 2))
stopifnot(calc(two, icc_model = "oneway")$SEM == round(sqrt(sum((x[, 1:2] - rowMeans(x[, 1:2]))^2) / 6), 2))

# 行排序和会话内重复试次不改变估计单位。
stopifnot(isTRUE(all.equal(r, calc(two[c(8, 1, 12, 5, 3, 7, 2, 10, 6, 9, 4, 11), ]))))
trials <- rbind(transform(two, trial = 1), transform(two, trial = 2))
stopifnot(isTRUE(all.equal(r, calc(trials, trial_col = "trial"))))

# 多会话 TE 扣除会话效应，两个入口在两会话数据上保持一致。
ra <- analyze_reliability_ANOVA(two, "id", "session", "value", sem_method = "weir", cv_method = "raw")
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

# Weir 表 1 和表 6 的算例：异质性不改变随机 SEM，偏移不混入随机误差。
a1 <- c(146, 148, 170, 90, 157, 156, 176, 205)
a2 <- c(140, 152, 152, 99, 145, 153, 167, 218)
b1 <- c(166, 168, 160, 150, 147, 146, 156, 155)
b2 <- c(160, 172, 142, 159, 135, 143, 147, 168)
c2 <- c(161, 162, 189, 100, 175, 171, 195, 219)
weir_data <- function(x, y) data.frame(id = rep(1:8, 2), session = rep(c("A", "B"), each = 8), value = c(x, y))
wa <- calc(weir_data(a1, a2))
wb <- calc(weir_data(b1, b2))
wc <- calc(weir_data(a1, c2), trial_type = "average")
stopifnot(wa$SEM == round(sqrt(399.75 / 7), 2), wa$SEM == wb$SEM,
          wa$SEM == wa$TE, wc$SEM == round(sqrt(33 / 7), 2), wc$SEM == wc$TE,
          wc[["ICC Unit"]] == "average", wc[["SEM Unit"]] == "single")
stopifnot(calc(two)$SEM == round(expected_te, 2), calc(two, trial_type = "average")$SEM == r$SEM)

# 默认 SD-ICC 算法使用与分数单位配套的 SD，CV 随 SEM 单位。
single_icc <- irr::icc(x[, 1:2], model = "twoway", type = "agreement", unit = "single")$value
average_icc <- irr::icc(x[, 1:2], model = "twoway", type = "agreement", unit = "average")$value
single_sem <- sd(as.vector(x[, 1:2])) * sqrt(1 - single_icc)
average_sem <- sd(rowMeans(x[, 1:2])) * sqrt(1 - average_icc)
rs <- analyze_reliability_ttest(two, "id", "session", "value")
rk <- analyze_reliability_ttest(two, "id", "session", "value", trial_type = "average")
stopifnot(rs$SEM == round(single_sem, 2), rk$SEM == round(average_sem, 2),
          rs[["CV%"]] == round(100 * single_sem / mean(x[, 1:2]), 2),
          rk[["CV%"]] == round(100 * average_sem / mean(x[, 1:2]), 2),
          rk[["SD for SEM"]] == sd(rowMeans(x[, 1:2])), rk[["SEM Unit"]] == "average")
