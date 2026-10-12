# sportreandva 2.0.2

- 根据 Weir (2005) 与用户指定，默认 SEM 使用 SD*sqrt(1-ICC)，明确区分 single 和 average 的 SD 取法。
- 默认 CV 使用 100*SEM/均值；保留 raw TE-based CV 和参考表 log CV。
- 备选 sem_method="weir" 为单分数 ANOVA 误差标准差，icc 保留 2.0.1 算法。
- 结果标明 SEM Method、SEM Unit、SD for SEM 与 CV Method，增加 Weir 算例与公式回归验证。

# sportreandva 2.0.1

- 统一信度分析数据准备，按完整被试配对，校验重复记录和非法输入。
- 修正 SEM 的 ICC 模型/单位匹配、多会话 TE 的会话效应混入、会话内平均误触发跨会话 average ICC。
- 新增 `cv_method="log"`，与参考表的对数 CV 回变换公式一致；保留 raw 默认值。
- 修复默认 SWC 为 NULL 导致结果表构造失败；采用未四舍五入的 TE 判断 usefulness。
- 效度分析按被试配对，SEE 使用回归残差标准差；Bland–Altman 偏差区间使用 t 分布。
- 修复正态性检验的常量和样本量边界、内置示例及参数文档。
- 图形界面新增 CV 方式和 SWC 系数选择；增加公式回归测试。
