#' 描述性统计示例数据集
#'
#' 模拟10名运动员在2个不同日期，分别进行3次垂直纵跳测试的数据。
#'
#' @format 一个包含 60 行和 4 列的数据框:
#' \describe{
#'   \item{Subject}{运动员ID}
#'   \item{Session}{测试日期, 包含 "Day1" 和 "Day2" 两个水平}
#'   \item{Trial}{试次编号 (1, 2, 3)}
#'   \item{JumpHeight}{垂直纵跳高度 (cm)}
#' }
"descriptive_data"


#' 信度分析示例数据集
#'
#' 模拟15名参与者在3个不同时间点 (Week1, Week2, Week4)，
#' 分别进行2次力量测试的数据。
#'
#' @format 一个包含 90 行和 4 列的数据框:
#' \describe{
#'   \item{Subject}{参与者ID}
#'   \item{Session}{测试时间点, 包含 "Week1", "Week2", "Week4" 三个水平}
#'   \item{Trial}{试次编号 (1, 2)}
#'   \item{Score}{力量测试得分 (N)}
#' }
"reliability_data"


#' 效度分析示例数据集
#'
#' 模拟20名个体分别使用金标准设备 (气体分析仪) 和一个便携式新设备
#' 测量最大摄氧量 (VO2max) 的数据。每个测量被视为单次试次。
#'
#' @format 一个包含 40 行和 4 列的数据框:
#' \describe{
#'   \item{Subject}{个体ID}
#'   \item{Device}{测量设备名称, "GasAnalyzer" (金标准) 和 "PortableDevice" (测试设备)}
#'   \item{Trial}{试次编号 (此数据集中恒为1)}
#'   \item{VO2max}{最大摄氧量 (ml/kg/min)}
#' }
"validity_data"
