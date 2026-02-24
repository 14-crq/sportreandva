
if (!require("remotes")) {
  install.packages("remotes", repos = "http://cran.rstudio.com/")
}

# 检查并从GitHub安装您的 sportreandva 包
if (!require("sportreandva")) {
  remotes::install_github("14-crq/sportreandva")
}

# --- 后续代码不变 ---
library(shiny)
library(shinythemes)
# ... 您的其他所有代码 ...

library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(sportreandva)
library(readxl)
library(vroom)
library(ggplot2)
library(colourpicker)
library(scales)      # 用于 hue_pal()

# =================================================================
# ---- 2. UI (用户界面) ----
# =================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),

  # --- UI 修改处: 添加标题和Logo ---
  fluidRow(
    # 将标题放在一个10/12宽度的列中
    column(10,
           titlePanel("运动科学信效度分析平台 (sportreandva包 GUI)")
    ),
    # 将Logo放在一个2/12宽度的列中，并让它靠右对齐
    column(2,
           # 确保您的Logo文件名为'bplogo.png'，并已放入'www'文件夹
           tags$img(src = "bplogo.png", height = "65px",
                    style = "float: right; padding-top: 5px; padding-right: 20px;")
    )
  ),
  hr(), # 添加一条水平线以示美观
  # --- 修改结束 ---

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. 获取数据"),
      fileInput("uploaded_file", "上传您的长格式数据 (CSV/XLSX)", accept = c(".csv", ".xlsx")),
      selectInput("example_data", "或加载内置示例数据:",
                  choices = c("请选择..." = "", "信度分析" = "reliability",
                              "效度分析" = "validity", "描述性统计" = "descriptive")),
      hr(),
      h4("2. 数据筛选 (可选)"),
      uiOutput("filter_col_ui"),
      uiOutput("filter_val_ui"),
      hr(),
      h4("3. 定义核心变量"),
      selectInput("subject_col", "受试者/被试 ID 列:", choices = NULL),
      selectInput("value_cols", "要分析的指标列 (可多选):", choices = NULL, multiple = TRUE),
      hr(),
      h4("4. 定义分组变量"),
      selectInput("session_col", "会话/日期/设备 列:", choices = NULL),
      selectInput("trial_col", "试次 (Trial) 列:", choices = NULL)
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        type = "pills",
        tabPanel("欢迎使用", icon = icon("home"),
                 h3("欢迎使用 sportreandva 分析平台！"),
                 p("请在左侧上传或选择示例数据，并逐步完成分析。"),
                 hr(),
                 h4("当前数据预览 (前10行)"),
                 DT::dataTableOutput("data_preview_table")
        ),

        tabPanel("描述性统计", icon = icon("table"),
                 h3("描述性统计分析"),
                 actionButton("run_desc_btn", "开始计算", icon = icon("calculator")),
                 hr(),
                 DT::dataTableOutput("desc_table")
        ),

        tabPanel("信度分析", icon = icon("chart-line"),
                 h3("重测信度分析"),
                 wellPanel(
                   radioButtons("reliability_type", "选择信度分析类型:",
                                choices = c("日间信度 (比较不同 Session)" = "inter",
                                            "日内信度 (比较特定 Session 内的 Trial)" = "intra"),
                                inline = TRUE),
                   uiOutput("intra_day_selector_ui"),
                   uiOutput("inter_day_agg_ui")
                 ),
                 actionButton("run_rel_btn", "开始信度分析", icon = icon("cogs")),
                 hr(),
                 h4("信度分析结果"),
                 DT::dataTableOutput("reliability_table"),
                 hr(),
                 h4("数据可视化 (云雨图)"),
                 wellPanel(
                   h5("颜色自定义"),
                   uiOutput("rel_plot_color_ui")
                 ),
                 uiOutput("reliability_plot_selector_ui"),
                 plotOutput("reliability_plot"),
                 wellPanel(
                   fluidRow(
                     column(8, downloadButton("download_rel_plot_btn", "下载该图")),
                     column(4, selectInput("rel_plot_format", "格式:",
                                           choices = c("png", "jpeg", "pdf"), width = "100px"))
                   )
                 )
        ),

        tabPanel("效度分析", icon = icon("check-double"),
                 h3("效度与一致性分析"),
                 wellPanel(
                   p("请在下方明确指定哪个是金标准，哪个是测试方法。"),
                   radioButtons("validity_agg_method", "Session内Trial的处理方式:",
                                choices = c("使用平均值" = "mean",
                                            "使用最大值" = "max",
                                            "使用最小值" = "min"),
                                inline = TRUE),
                   uiOutput("validity_selectors_ui")
                 ),
                 actionButton("run_val_btn", "开始效度分析", icon = icon("cogs")),
                 hr(),
                 h4("综合效度分析结果"),
                 DT::dataTableOutput("validity_table"),
                 hr(),
                 h4("Bland-Altman 分析结果"),
                 DT::dataTableOutput("ba_table"),
                 hr(),
                 h4("数据可视化"),
                 wellPanel(
                   h5("颜色自定义"),
                   fluidRow(
                     column(6, h6("相关性图颜色"),
                            colourInput("corr_point_fill", "散点颜色:", value = "#D25D5D"),
                            colourInput("corr_line_color", "回归线/密度图边框颜色:", value = "#B9375D"),
                            colourInput("corr_ci_fill", "置信区间/密度图填充颜色:", value = "#E7D3D3")
                     ),
                     column(6, h6("Bland-Altman图颜色"),
                            colourInput("ba_point_fill", "散点颜色:", value = "#D25D5D"),
                            colourInput("ba_bias_color", "Bias线/密度图边框颜色:", value = "#722323"),
                            colourInput("ba_loa_color", "LoA线颜色:", value = "#B9375D"),
                            colourInput("ba_ci_fill", "CI/密度图填充颜色:", value = "#E7D3D3")
                     )
                   )
                 ),
                 fluidRow(
                   column(6,
                          uiOutput("validity_corr_plot_selector_ui"),
                          plotOutput("validity_corr_plot"),
                          wellPanel(
                            fluidRow(
                              column(8, downloadButton("download_corr_plot_btn", "下载该图")),
                              column(4, selectInput("corr_plot_format", "格式:",
                                                    choices = c("png", "jpeg", "pdf"), width = "100px"))
                            )
                          )
                   ),
                   column(6,
                          uiOutput("validity_ba_plot_selector_ui"),
                          plotOutput("validity_ba_plot"),
                          wellPanel(
                            fluidRow(
                              column(8, downloadButton("download_ba_plot_btn", "下载该图")),
                              column(4, selectInput("ba_plot_format", "格式:",
                                                    choices = c("png", "jpeg", "pdf"), width = "100px"))
                            )
                          )
                   )
                 )
        )
      )
    )
  )
)


# =================================================================
# ---- 3. Server ----
# =================================================================
server <- function(input, output, session) {

  # ---- 数据加载 ----
  uploaded_data <- reactiveVal(NULL)

  observeEvent(input$uploaded_file, {
    req(input$uploaded_file)
    updateSelectInput(session, "example_data", selected = "")

    ext <- tools::file_ext(input$uploaded_file$name)
    df <- switch(ext,
                 csv = vroom(input$uploaded_file$datapath, delim = ","),
                 xlsx = read_excel(input$uploaded_file$datapath),
                 {
                   validate("无效的文件类型。")
                   NULL
                 })
    uploaded_data(df)
  })

  raw_data <- reactive({
    if (!is.null(input$example_data) && input$example_data != "") {
      return(get(paste0(input$example_data, "_data")))
    }
    if (!is.null(uploaded_data())) {
      return(uploaded_data())
    }
    return(NULL)
  })

  # ---- 数据筛选与预览 ----
  output$filter_col_ui <- renderUI({
    req(raw_data())
    selectInput("filter_col", "选择筛选列 (可选):",
                choices = c("不过滤" = "",
                            names(raw_data())[!sapply(raw_data(), is.numeric)]))
  })

  output$filter_val_ui <- renderUI({
    req(input$filter_col != "")
    selectInput("filter_val", "选择要保留的值:",
                choices = unique(raw_data()[[input$filter_col]]))
  })

  user_data <- reactive({
    df <- raw_data()
    req(df)
    if (!is.null(input$filter_col) && input$filter_col != "" &&
        !is.null(input$filter_val)) {
      df <- df %>% filter(.data[[input$filter_col]] == input$filter_val)
    }
    return(df)
  })

  observe({
    req(user_data())
    col_names <- names(user_data())
    updateSelectInput(session, "subject_col", choices = col_names,
                      selected = intersect(c("Subject", "subject", "SubjectID"), col_names)[1])
    updateSelectInput(session, "session_col", choices = col_names,
                      selected = intersect(c("Session", "session", "Device"), col_names)[1])
    updateSelectInput(session, "trial_col", choices = col_names,
                      selected = intersect(c("Trial", "trial"), col_names)[1])
    updateSelectInput(session, "value_cols", choices = col_names[sapply(user_data(), is.numeric)])
  })

  output$data_preview_table <- DT::renderDataTable({
    req(user_data())
    DT::datatable(head(user_data(), 10), options = list(dom = 't', scrollX = TRUE))
  })

  # ---- 动态UI ----
  output$intra_day_selector_ui <- renderUI({
    if(input$reliability_type == "intra") {
      req(input$session_col, user_data())
      selectInput("intra_day_choice", "请选择要分析的 Session:",
                  choices = unique(user_data()[[input$session_col]]))
    }
  })

  output$inter_day_agg_ui <- renderUI({
    if(input$reliability_type == "inter") {
      radioButtons("inter_day_agg_method", "Session内Trial的处理方式:",
                   choices = c("使用平均值" = "mean",
                               "使用最大值" = "max",
                               "使用最小值" = "min"),
                   inline = TRUE)
    }
  })

  output$validity_selectors_ui <- renderUI({
    req(input$session_col, user_data())
    method_levels <- unique(user_data()[[input$session_col]])
    tagList(
      selectInput("criterion_method", "金标准方法:", choices = method_levels),
      selectInput("test_method", "测试方法:", choices = method_levels,
                  selected = if(length(method_levels)>1) method_levels[2] else NULL)
    )
  })

  # =================================================================
  # --- 模块一：描述性统计 ---
  # =================================================================
  desc_results <- eventReactive(input$run_desc_btn, {
    req(user_data(), input$value_cols, input$session_col, input$trial_col)
    summarize_trials(data = user_data(),
                     value_cols = input$value_cols,
                     group_cols = c(input$session_col, input$trial_col))
  })

  output$desc_table <- DT::renderDataTable({
    DT::datatable(desc_results(), extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel'),
                                 scrollX = TRUE))
  })

  # =================================================================
  # --- 模块二：信度分析 ---
  # =================================================================
  reliability_analysis_results <- eventReactive(input$run_rel_btn, {
    req(user_data(), input$subject_col, input$value_cols)

    if (input$reliability_type == "inter") {
      req(input$inter_day_agg_method, input$session_col, input$trial_col)
      n_sessions <- n_distinct(user_data()[[input$session_col]])

      if (n_sessions == 2) {
        analyze_reliability_ttest(
          data = user_data(),
          subject_col = input$subject_col,
          session_col = input$session_col,
          value_cols = input$value_cols,
          trial_col = input$trial_col,
          method = input$inter_day_agg_method
        )
      } else {
        analyze_reliability_ANOVA(
          data = user_data(),
          subject_col = input$subject_col,
          session_col = input$session_col,
          value_cols = input$value_cols,
          trial_col = input$trial_col,
          method = input$inter_day_agg_method
        )
      }
    } else {
      req(input$intra_day_choice, input$session_col, input$trial_col)
      intra_data <- user_data() %>%
        filter(.data[[input$session_col]] == input$intra_day_choice)

      analyze_reliability_ANOVA(
        data = intra_data,
        subject_col = input$subject_col,
        session_col = input$trial_col,
        value_cols = input$value_cols,
        trial_col = NULL,
        trial_type = 'single'
      )
    }
  })

  output$reliability_table <- DT::renderDataTable({
    DT::datatable(reliability_analysis_results(),
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel'),
                                 scrollX = TRUE))
  })

  output$reliability_plot_selector_ui <- renderUI({
    req(input$value_cols)
    selectInput("rel_plot_var", "选择要显示的指标:", choices = input$value_cols)
  })

  # --- SERVER 修正处: 简化信度图的颜色选择逻辑 ---
  rel_plot_session_levels <- reactive({
    req(user_data())
    grouping_col <- if (isolate(input$reliability_type) == "intra") {
      isolate(input$trial_col)
    } else {
      isolate(input$session_col)
    }
    req(grouping_col)

    df <- if (isolate(input$reliability_type) == "intra") {
      req(isolate(input$intra_day_choice))
      user_data() %>% filter(.data[[isolate(input$session_col)]] == isolate(input$intra_day_choice))
    } else {
      user_data()
    }
    sort(unique(df[[grouping_col]]))
  })

  output$rel_plot_color_ui <- renderUI({
    levels <- rel_plot_session_levels()
    req(levels)
    default_colors <- hue_pal()(length(levels))

    lapply(seq_along(levels), function(i) {
      colourInput(
        inputId = paste0("rel_plot_color_", make.names(levels[i])),
        label = paste("分组颜色:", levels[i]),
        value = default_colors[i]
      )
    })
  })

  rel_colors <- reactive({
    levels <- rel_plot_session_levels()
    req(levels)
    sapply(levels, function(level) {
      input_id <- paste0("rel_plot_color_", make.names(level))
      req(input[[input_id]])
      input[[input_id]]
    }, USE.NAMES = FALSE)
  })

  current_rel_plot <- reactive({
    req(reliability_analysis_results(), input$rel_plot_var, rel_colors())

    if (input$reliability_type == "inter") {
      req(input$inter_day_agg_method)
      data_for_plot <- user_data() %>%
        group_by(across(all_of(c(input$subject_col, input$session_col)))) %>%
        summarise(
          aggregated_value = get(input$inter_day_agg_method)(.data[[input$rel_plot_var]], na.rm = TRUE),
          .groups = 'drop'
        )

      plot_reliability(
        data = data_for_plot,
        subject_col = input$subject_col,
        session_col = input$session_col,
        value_cols = "aggregated_value",
        var_names = input$rel_plot_var,
        add_significance = TRUE,
        custom_colors = rel_colors()
      )
    } else {
      req(input$intra_day_choice)
      intra_data <- user_data() %>%
        filter(.data[[input$session_col]] == input$intra_day_choice)

      plot_reliability(
        data = intra_data,
        subject_col = input$subject_col,
        session_col = input$trial_col,
        value_cols = input$rel_plot_var,
        add_significance = TRUE,
        custom_colors = rel_colors()
      )
    }
  })

  output$reliability_plot <- renderPlot({ current_rel_plot() })

  output$download_rel_plot_btn <- downloadHandler(
    filename = function() {
      paste0("reliability_plot_", input$rel_plot_var, ".", input$rel_plot_format)
    },
    content = function(file) {
      ggsave(file, plot = current_rel_plot(), device = input$rel_plot_format,
             width = 8, height = 6)
    }
  )

  # =================================================================
  # --- 模块三：效度分析 ---
  # =================================================================
  validity_analysis_results <- eventReactive(input$run_val_btn, {
    req(user_data(), input$subject_col, input$session_col,
        input$criterion_method, input$test_method,
        input$value_cols, input$validity_agg_method)

    showModal(modalDialog("正在聚合数据并全力计算中...", footer = NULL))

    agg_data <- user_data() %>%
      group_by(.data[[input$subject_col]], .data[[input$session_col]]) %>%
      summarise(across(all_of(input$value_cols),
                       .fns = get(input$validity_agg_method), na.rm = TRUE),
                .groups = 'drop')

    data_for_analysis <- agg_data %>%
      filter(.data[[input$session_col]] %in% c(input$criterion_method, input$test_method))

    val_res <- analysis_validity(
      data = data_for_analysis,
      subject_col = input$subject_col,
      method_col = input$session_col,
      criterion_method = input$criterion_method,
      test_method = input$test_method,
      value_cols = input$value_cols
    )

    ba_res <- analyze_B_A(
      data = data_for_analysis,
      subject_col = input$subject_col,
      session_col = input$session_col,
      value_cols = input$value_cols
    )

    removeModal()
    list(validity = val_res, ba = ba_res$results_for_publication,
         data_for_plots = data_for_analysis)
  })

  output$validity_table <- DT::renderDataTable({
    DT::datatable(validity_analysis_results()$validity, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel'),
                                 scrollX = TRUE))
  })

  output$ba_table <- DT::renderDataTable({
    DT::datatable(validity_analysis_results()$ba, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel'),
                                 scrollX = TRUE))
  })

  output$validity_corr_plot_selector_ui <- renderUI({
    req(input$value_cols)
    selectInput("corr_plot_var", "选择要显示的指标:", choices = input$value_cols)
  })

  output$validity_ba_plot_selector_ui <- renderUI({
    req(input$value_cols)
    selectInput("ba_plot_var", "选择要显示的指标:", choices = input$value_cols)
  })

  current_corr_plot <- reactive({
    req(validity_analysis_results(), input$corr_plot_var)
    plot_validity_correlation(
      data = validity_analysis_results()$data_for_plots,
      subject_col = input$subject_col,
      method_col = input$session_col,
      criterion_method = input$criterion_method,
      test_method = input$test_method,
      value_cols = input$corr_plot_var,
      point_fill = input$corr_point_fill,
      line_color = input$corr_line_color,
      ci_fill = input$corr_ci_fill,
      density_fill = input$corr_ci_fill,
      density_color = input$corr_line_color
    )
  })

  current_ba_plot <- reactive({
    req(validity_analysis_results(), input$ba_plot_var)
    plot_B_A(
      data = validity_analysis_results()$data_for_plots,
      subject_col = input$subject_col,
      session_col = input$session_col,
      value_cols = input$ba_plot_var,
      point_fill = input$ba_point_fill,
      bias_color = input$ba_bias_color,
      loa_color = input$ba_loa_color,
      ci_fill = input$ba_ci_fill,
      density_fill = input$ba_ci_fill,
      density_color = input$ba_bias_color
    )
  })

  output$validity_corr_plot <- renderPlot({ current_corr_plot() })
  output$validity_ba_plot <- renderPlot({ current_ba_plot() })

  output$download_corr_plot_btn <- downloadHandler(
    filename = function() {
      paste0("validity_correlation_plot_", input$corr_plot_var, ".", input$corr_plot_format)
    },
    content = function(file) {
      ggsave(file, plot = current_corr_plot(), device = input$corr_plot_format,
             width = 7, height = 7)
    }
  )

  output$download_ba_plot_btn <- downloadHandler(
    filename = function() {
      paste0("bland_altman_plot_", input$ba_plot_var, ".", input$ba_plot_format)
    },
    content = function(file) {
      ggsave(file, plot = current_ba_plot(), device = input$ba_plot_format,
             width = 8, height = 6)
    }
  )
}

shinyApp(ui = ui, server = server)
