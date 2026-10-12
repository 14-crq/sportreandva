# sportreandva

用于体育科学长格式重复测量数据的信度、效度和 Bland–Altman 分析。

```r
remotes::install_github("14-crq/sportreandva")
library(sportreandva)

# 会话内先按试次取均值，再估计一个会话分数的重测信度
analyze_reliability_ttest(
  reliability_data[reliability_data$Session %in% c("Week1", "Week2"), ],
  subject_col = "Participant", session_col = "Session", value_cols = "Score",
  trial_col = "Trial", method = "mean", trial_type = "single",
  cv_method = "log", swc_threshold = 0.2
)
```

## Weir 文献校对（2.0.2）

按用户指定，默认 `sem_method="sd_icc"` 使用 `SEM=SD*sqrt(1-ICC)`，不使用 MSE 直接计算 SEM。
single 的 SD 为全部被试、全部会话观测分数的样本 SD；average 的 SD 为每名被试跨会话均值的样本 SD。
这是明确指定 SD 取法的经典估计，不保证与直接 ANOVA SEM 恒等。差值 SD 不可代入此式。
默认 `cv_method="sem"` 为 `100*SEM/均值`；`cv_method="raw"` 保留单分数 TE-based CV；`log` 保留参考表对数 TE 回变换。
`sem_method="weir"` 作为备选：双因素 `SEM=sqrt(MSE)`，两次测试时等于 `TE=SD(差值)/sqrt(2)`，独立于 ICC 单位。
`sem_method="icc"` 保留 2.0.1 的模型匹配 SEM。
结果列 `SEM Method`、`SEM Unit`、`SD for SEM` 和 `CV Method` 标明取法。
Weir 原文第 237–239 页同时讨论 SD–ICC 法与直接 ANOVA 法；后者更便于不同研究比较，但本研究按用户选择使用前者。

## SEM、TE 和 CV（SEM 行为对应 sem_method="icc"）

| 指标 | 2.0.1 计算口径 |
|---|---|
| TE | 双因素模型（被试 + 会话）残差标准差；两个会话时等于 `SD(第二次−第一次)/sqrt(2)` |
| CV，`cv_method="raw"` | `100*TE/总均值`，可选的 TE-based CV；均值非正时返回 NA |
| CV，`cv_method="log"` | `100*(exp(TE_log)-1)`；先对聚合后的会话分数取自然对数，要求所有值为正 |
| SEM，双因素 consistency | `sqrt(MSE)` |
| SEM，双因素 agreement | `sqrt(MSE+(MS_session−MSE)/N)`；保留方差分量估计，负误差方差返回 NA |
| SEM，oneway | 被试内均方的平方根，包括会话效应 |
| SEM，average 单位 | 对应 single 的 SEM 除以 `sqrt(会话数)` |
| SWC | 指定系数 × 各会话 SD 的合并值；`NULL` 返回 NA，不进行 usefulness 分类 |

在 `sem_method="icc"` 时，SEM 与所选 ICC 的模型和单位一致。TE/CV 描述一个会话分数的随机误差，扣除会话系统偏移。
会话内多个试次取平均，不等于跨会话平均值 ICC；`trial_type` 由调用者明确选择，不再自动改为 average。
因此新版本的 SEM、ICC 和多会话 TE 可能与旧版不同。

两会话对数 CV 与用户提供的 `Reliability_CMJ.xls` 公式核对通过：
MV：TE = 0.028810714486302895，CV = 6.368819132570039%；
PV：TE = 0.7355888179485253，CV = 1.8222983380721445%。
包的展示表保留两位小数。SEM 是独立的模型估计；参考表没有单列 SEM。
参考表的偏差校正 ICC 与 `irr::icc` 的 ANOVA ICC 不同，本包继续使用后者。
多会话 TE 使用完整被试的双因素残差，而非参考表相邻会话 TE 的合并算法。

所有统计量统一使用完整被试；缺失会话会发出排除人数提示。无试次列时，重复的被试–会话记录会报错。
配对效应量为 `d_z=mean(第二次−第一次)/SD(差值)`，不受输入行顺序影响。

计算定义参考 [Hopkins 的信度计算说明](https://sportscience.sportsci.org/resource/stats/relycalc.html)。

## 验证

`tests/reliability.R` 用独立公式验证 SEM、TE、CV、ICC 单位、会话偏移、行顺序、缺失值和非法输入。
运行 `R CMD build sportreandva`，再运行 `R CMD check --no-manual sportreandva_2.0.2.tar.gz`。
