#' 启动EcoModelVis Shiny应用
#'
#' 该函数启动一个交互式Shiny应用，提供图形用户界面来使用包的所有功能。
#'
#' @return 无返回值，启动Shiny应用
#' @import shiny
#' @importFrom tools file_ext
#' @import ggplot2
#' @import ROCR
#' @import matrixStats
#' @export
run_ecovis_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("需要安装'shiny'包: install.packages('shiny')")
  }

  # 定义ROC绘图函数
  plot_roc_curve <- function(data_train, auc_mean, title, xlab, ylab, title_size, xlab_size, ylab_size) {
    roc_plot <- ggplot2::ggplot(data = data_train, ggplot2::aes(x = tpr.mean)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = fpr.mean - fpr.sd,
                     ymax = fpr.mean + fpr.sd),
        fill = "blue",
        alpha = 1,
        color = NA
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = fpr.mean),
        color = "red",
        size = 0.8
      ) +
      ggplot2::geom_line(
        data = data.frame(x = c(0, 1), y = c(0, 1)),
        ggplot2::aes(x = x, y = y),
        color = "black",
        size = 0.8,
        linetype = "solid"
      ) +
      ggplot2::geom_point(
        data = data.frame(
          color = c("red", "blue", "black")
        ),
        ggplot2::aes(x = -Inf, y = -Inf, color = color),
        size = 0,
        alpha = 0
      ) +
      ggplot2::scale_color_manual(
        name = "",
        values = c(
          "red" = "red",
          "blue" = "blue",
          "black" = "black"
        ),
        labels = c(
          paste0("Mean (AUC=", round(auc_mean, 3), ")"),
          "Random Prediction",
          "Mean+/-one std dev"
        )
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(
        name = xlab,
        breaks = seq(0, 1, 0.2),
        limits = c(0, 1)
      ) +
      ggplot2::scale_y_continuous(
        name = ylab,
        breaks = seq(0, 1, 0.2),
        limits = c(0, 1)
      ) +
      ggplot2::labs(title = title) +
      ggplot2::theme(
        text = ggplot2::element_text(family = "Times New Roman"),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = title_size,
          face = "bold"
        ),
        axis.title.x = ggplot2::element_text(size = xlab_size),
        axis.title.y = ggplot2::element_text(size = ylab_size),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.direction = "vertical",
        legend.key = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(1.5, "lines"),
        legend.background = ggplot2::element_blank(),
        legend.box.background = ggplot2::element_blank(),
        legend.box.margin = ggplot2::margin(2, 2, 2, 2),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          colour = "black",
          fill = NA,
          linewidth = 1,
          inherit.blank = FALSE
        )
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = 15,
            size = 4,
            alpha = 1
          )
        )
      )

    return(roc_plot)
  }

  # UI定义
  ui <- shiny::fluidPage(
    shiny::titlePanel(
      shiny::div(
        style = "font-family: 'Times New Roman', sans-serif;",
        "生态模型可视化工具"
      )
    ),
    shiny::tabsetPanel(
      id = "tabs",
      shiny::tabPanel(
        "响应曲线",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            shiny::textInput("species", "物种名称", "Fargesia_nitida"),
            shiny::textInput("variable", "环境变量", "bio11"),
            shiny::fileInput("resp_files", "选择响应曲线文件 (.dat格式)",
                             multiple = TRUE,
                             accept = ".dat"),
            shiny::numericInput("xmin", "X轴最小值", value = -25, min = -1000, max = 7000),
            shiny::numericInput("xmax", "X轴最大值", value = 25, min = -1000, max = 7000),
            shiny::sliderInput("ylim", "Y轴范围", min = 0, max = 1, value = c(0, 0.8), step = 0.1),
            shiny::textAreaInput("resp_title", "图标题", value = "响应曲线", rows = 2),
            shiny::textAreaInput("resp_xlab", "X轴标题", value = "", rows = 2),
            shiny::textAreaInput("resp_ylab", "Y轴标题", value = "", rows = 2),
            shiny::numericInput("resp_title_size", "图标题字号", value = 14, min = 1, max = 30),
            shiny::numericInput("resp_xlab_size", "X轴标题字号", value = 12, min = 1, max = 30),
            shiny::numericInput("resp_ylab_size", "Y轴标题字号", value = 12, min = 1, max = 30),
            shiny::numericInput("resp_dpi", "图片分辨率 (DPI)", value = 600, min = 300, max = 1200),
            shiny::actionButton("run_resp", "生成响应曲线", class = "btn-primary"),
            shiny::downloadButton("save_response_tiff", "保存为TIFF", class = "btn-download"),
            shiny::downloadButton("save_response_pdf", "保存为PDF", class = "btn-download")
          ),
          shiny::mainPanel(
            width = 9,
            shiny::plotOutput("response_plot", height = "600px")
          )
        )),
      shiny::tabPanel(
        "刀切法",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            shiny::fileInput("knife_file", "上传刀切法CSV文件",
                             accept = c(".csv")),
            shiny::textAreaInput("knife_title", "图表标题", "刀切法", rows = 2),
            shiny::textAreaInput("knife_xlab", "X轴标题", value = "", rows = 2),
            shiny::textAreaInput("knife_ylab", "Y轴标题", value = "", rows = 2),
            shiny::numericInput("knife_title_size", "图标题字号", value = 14, min = 1, max = 30),
            shiny::numericInput("knife_xlab_size", "X轴标题字号", value = 12, min = 1, max = 30),
            shiny::numericInput("knife_ylab_size", "Y轴标题字号", value = 12, min = 1, max = 30),
            shiny::numericInput("knife_dpi", "图片分辨率 (DPI)", value = 600, min = 300, max = 1200),
            shiny::actionButton("run_knife", "生成刀切法图", class = "btn-primary"),
            shiny::downloadButton("save_knife_tiff", "保存为TIFF", class = "btn-download"),
            shiny::downloadButton("save_knife_pdf", "保存为PDF", class = "btn-download")
          ),
          shiny::mainPanel(
            width = 9,
            shiny::plotOutput("knife_plot", height = "600px")
          )
        )),
      shiny::tabPanel(
        "ROC曲线",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            shiny::fileInput("presence_file", "主要样本预测文件",
                             accept = c(".csv")),
            shiny::fileInput("sample_files", "重复样本预测文件",
                             multiple = TRUE,
                             accept = c(".csv")),
            shiny::fileInput("bg_files", "重复背景预测文件",
                             multiple = TRUE,
                             accept = c(".csv")),
            shiny::textAreaInput("roc_title", "图表标题", "ROC曲线", rows = 2),
            shiny::textAreaInput("roc_xlab", "X轴标题", value = "1 - Specificity", rows = 2),
            shiny::textAreaInput("roc_ylab", "Y轴标题", value = "Sensitivity", rows = 2),
            shiny::numericInput("roc_title_size", "图标题字号", value = 14, min = 1, max = 30),
            shiny::numericInput("roc_xlab_size", "X轴标题字号", value = 12, min = 1, max = 30),
            shiny::numericInput("roc_ylab_size", "Y轴标题字号", value = 12, min = 1, max = 30),
            shiny::numericInput("roc_dpi", "图片分辨率 (DPI)", value = 600, min = 300, max = 1200),
            shiny::actionButton("run_roc", "生成ROC曲线", class = "btn-primary"),
            shiny::downloadButton("save_roc_tiff", "保存为TIFF", class = "btn-download"),
            shiny::downloadButton("save_roc_pdf", "保存为PDF", class = "btn-download")
          ),
          shiny::mainPanel(
            width = 9,
            shiny::verbatimTextOutput("auc_text"),
            shiny::plotOutput("roc_plot", height = "500px")
          )
        ))
    ),
    shiny::tags$style(HTML("
    /* 按钮样式 */
    .btn-primary {
      background-color: #4CAF50;
      color: white;
      padding: 10px 20px;
      font-size: 16px;
      border: none;
      border-radius: 4px;
      cursor: pointer;
      width: 100%;
      margin-bottom: 10px;
      transition: background-color 0.3s;
    }
    .btn-primary:hover {
      background-color: #45a049;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
    }

    .btn-download {
      background-color: #2196F3;
      color: white;
      padding: 10px 20px;
      font-size: 16px;
      border: none;
      border-radius: 4px;
      cursor: pointer;
      width: 100%;
      margin-top: 10px;
      transition: background-color 0.3s;
    }
    .btn-download:hover {
      background-color: #0b7dda;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
    }

    .shiny-input-container {
      margin-bottom: 15px;
    }

    .well {
      background-color: #f9f9f9;
      border-radius: 5px;
      padding: 15px;
      margin-bottom: 15px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    }

    .nav-tabs > li > a {
      color: #555;
      font-weight: bold;
      font-family: 'Times New Roman', sans-serif;
    }
    .nav-tabs > li.active > a {
      color: #2196F3;
      border-bottom: 2px solid #2196F3;
    }

    .panel-title {
      font-size: 18px;
      font-weight: bold;
      margin-bottom: 15px;
      color: #333;
      font-family: 'Times New Roman', sans-serif;
    }

    body, .btn, .shiny-input-container, .shiny-html-output {
      font-family: 'Times New Roman', sans-serif;
    }
  "))
  )

  # 服务器逻辑
  server <- function(input, output, session) {
    # 响应曲线
    resp_plot <- shiny::eventReactive(input$run_resp, {
      req(input$resp_files)

      # 验证文件格式
      files <- input$resp_files

      # 检查文件扩展名
      invalid_ext <- !grepl("\\.dat$", files$name, ignore.case = TRUE)
      if (any(invalid_ext)) {
        invalid_names <- paste(files$name[invalid_ext], collapse = ", ")
        shiny::showNotification(
          paste("以下文件不是.dat格式:", invalid_names),
          type = "error", duration = 10
        )
        return(NULL)
      }

      # 验证文件名格式
      pattern <- "^[a-zA-Z0-9_]+_[0-9]+_bio[0-9]{1,2}_only\\.dat$"
      invalid_names <- !grepl(pattern, files$name, ignore.case = TRUE)
      if (any(invalid_names)) {
        invalid_files <- paste(files$name[invalid_names], collapse = ", ")
        shiny::showNotification(
          paste("文件名格式不正确，应为 '物种名_[数字]_bio[气候变量编号]_only.dat':", invalid_files),
          type = "error", duration = 10
        )
        return(NULL)
      }

      tryCatch({
        plot <- response_curve(
          file_paths = files$datapath,
          species_name = input$species,
          variable_name = input$variable,
          x_limit = c(input$xmin, input$xmax),
          y_limit = input$ylim
        )

        # 添加自定义标签
        plot <- plot +
          ggplot2::labs(
            title = input$resp_title,
            x = input$resp_xlab,
            y = input$resp_ylab
          ) +
          ggplot2::theme(
            text = ggplot2::element_text(family = "Times New Roman"),
            plot.title = ggplot2::element_text(size = input$resp_title_size),
            axis.title.x = ggplot2::element_text(size = input$resp_xlab_size),
            axis.title.y = ggplot2::element_text(size = input$resp_ylab_size),
            axis.text.x = ggplot2::element_text(size = input$resp_xlab_size),
            axis.text.y = ggplot2::element_text(size = input$resp_ylab_size)
          )

      }, error = function(e) {
        shiny::showNotification(paste("生成响应曲线错误:", e$message), type = "error", duration = 10)
        NULL
      })
    })

    output$response_plot <- shiny::renderPlot({
      plot <- resp_plot()
      if (is.null(plot)) {
        ggplot2::ggplot() +
          ggplot2::labs(title = "等待上传响应曲线数据") +
          ggplot2::theme_void() +
          ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman"))
      } else {
        plot
      }
    })

    # 响应曲线保存为TIFF
    output$save_response_tiff <- shiny::downloadHandler(
      filename = function() {
        paste0("response_curve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tif")
      },
      content = function(file) {
        plot <- resp_plot()
        if (is.null(plot)) return()

        ggplot2::ggsave(
          filename = file,
          plot = plot,
          device = "tiff",
          dpi = input$resp_dpi,
          compression = "lzw",
          width = 7,
          height = 5,
          units = "in"
        )
      }
    )

    # 响应曲线保存为PDF
    output$save_response_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("response_curve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        plot <- resp_plot()
        if (is.null(plot)) return()

        ggplot2::ggsave(
          filename = file,
          plot = plot,
          device = "pdf",
          width = 7,
          height = 5,
          units = "in"
        )
      }
    )

    # 刀切法图
    knife_plot_obj <- shiny::eventReactive(input$run_knife, {
      req(input$knife_file)

      # 获取文件路径
      file_path <- input$knife_file$datapath

      # 验证文件扩展名
      if (!grepl("\\.csv$", input$knife_file$name, ignore.case = TRUE)) {
        shiny::showNotification("请上传CSV文件", type = "error", duration = 10)
        return(NULL)
      }

      # 尝试生成图表
      tryCatch({
        plot <- knife_plot(
          file_path = file_path,
          title = input$knife_title
        )

        # 添加自定义标签和字号 - 使用Times New Roman
        plot <- plot +
          ggplot2::labs(
            title = input$knife_title,
            x = input$knife_xlab,
            y = input$knife_ylab
          ) +
          ggplot2::theme(
            # 所有文本元素使用Times New Roman
            text = ggplot2::element_text(family = "Times New Roman"),

            # 主标题
            plot.title = ggplot2::element_text(
              size = input$knife_title_size,
              face = "bold"
            ),

            # 坐标轴标题
            axis.title.x = ggplot2::element_text(
              size = input$knife_xlab_size
            ),
            axis.title.y = ggplot2::element_text(
              size = input$knife_ylab_size
            ),

            # 坐标轴刻度
            axis.text = ggplot2::element_text(
              size = 10
            )
          )

        # 存储DPI设置
        knife_values$dpi <- input$knife_dpi
        knife_values$plot <- plot

        return(plot)

      }, error = function(e) {
        error_msg <- paste("生成刀切法图错误:", e$message)
        shiny::showNotification(error_msg, type = "error", duration = 10)

        # 创建错误信息图
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste("错误:", e$message),
                            size = 6, color = "red",
                            family = "Times New Roman") +
          ggplot2::theme_void()
      })
    })

    # 存储刀切法相关值
    knife_values <- shiny::reactiveValues(plot = NULL, dpi = 600)

    output$knife_plot <- shiny::renderPlot({
      plot <- knife_plot_obj()
      if (is.null(plot)) {
        # 创建空图
        ggplot2::ggplot() +
          ggplot2::labs(title = "等待上传刀切法数据") +
          ggplot2::theme_void() +
          ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman"))
      } else {
        plot
      }
    })

    # 刀切法图保存为TIFF
    output$save_knife_tiff <- shiny::downloadHandler(
      filename = function() {
        paste0("knife_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tif")
      },
      content = function(file) {
        req(knife_values$plot, knife_values$dpi)

        # 创建临时文件路径
        temp_file <- tempfile(fileext = ".tif")

        # 保存图片
        ggplot2::ggsave(
          filename = temp_file,
          plot = knife_values$plot,
          device = "tiff",
          dpi = knife_values$dpi,
          compression = "lzw",
          width = 7,
          height = 5,
          units = "in"
        )

        # 复制文件到下载位置
        file.copy(temp_file, file)
      }
    )

    # 刀切法图保存为PDF
    output$save_knife_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("knife_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        req(knife_values$plot)

        # 保存为PDF
        ggplot2::ggsave(
          filename = file,
          plot = knife_values$plot,
          device = "pdf",
          width = 7,
          height = 5,
          units = "in"
        )
      }
    )


    # ROC曲线
    roc_data <- shiny::eventReactive(input$run_roc, {
      req(input$presence_file, input$sample_files, input$bg_files)

      tryCatch({
        # 1. 读取主要样本文件
        presence <- read.csv(input$presence_file$datapath)

        # 检查必要列
        if (!"Test.or.train" %in% colnames(presence)) {
          stop("主要样本文件必须包含 'Test.or.train' 列")
        }

        # 2. 读取样本预测文件列表
        sample_files <- input$sample_files
        data_list.pre <- lapply(sample_files$datapath, function(file) {
          read.csv(file)
        })

        # 3. 读取背景预测文件列表
        bg_files <- input$bg_files
        data_list.bac <- lapply(bg_files$datapath, function(file) {
          read.csv(file)
        })

        # 检查文件数量一致
        n_folds <- length(sample_files$name)
        if (length(data_list.pre) != n_folds || length(data_list.bac) != n_folds) {
          stop(sprintf("文件数量不一致: 样本文件%d个, 背景文件%d个",
                       length(data_list.pre), length(data_list.bac)))
        }

        # 合并文件
        merged_pre <- do.call(cbind, data_list.pre)
        merged_bac <- do.call(cbind, data_list.bac)

        # 确定列索引
        pre_col_indexes <- seq(6, 6 * n_folds, by = 6)
        if (max(pre_col_indexes) > ncol(merged_pre)) {
          stop(sprintf("样本文件列数不足。需要至少%d列，实际%d列",
                       max(pre_col_indexes), ncol(merged_pre)))
        }
        pre <- merged_pre[, pre_col_indexes, drop = FALSE]

        bac_col_indexes <- seq(5, 5 * n_folds, by = 5)
        if (max(bac_col_indexes) > ncol(merged_bac)) {
          stop(sprintf("背景文件列数不足。需要至少%d列，实际%d列",
                       max(bac_col_indexes), ncol(merged_bac)))
        }
        bac <- merged_bac[, bac_col_indexes, drop = FALSE]

        # 计算AUC和ROC
        auc_list <- numeric(n_folds)
        tpr_list <- list()
        fpr_list <- list()

        for (i in 1:n_folds) {
          pp <- pre[, i]
          bb <- bac[, i]

          # 仅使用训练数据
          train_idx <- presence$Test.or.train == "train"
          if (sum(train_idx) == 0) {
            stop("没有找到训练数据，请检查 'Test.or.train' 列")
          }

          trainpp <- pp[train_idx]
          combined <- c(trainpp, bb)
          label <- c(rep(1, length(trainpp)), rep(0, length(bb)))

          pred <- ROCR::prediction(combined, label)
          perf <- ROCR::performance(pred, "tpr", "fpr")
          auc_list[i] <- ROCR::performance(pred, "auc")@y.values[[1]]
          tpr_list[[i]] <- perf@x.values[[1]]
          fpr_list[[i]] <- perf@y.values[[1]]
        }

        # 准备绘图数据
        tpr.dat <- do.call(cbind, tpr_list)
        fpr.dat <- do.call(cbind, fpr_list)

        tpr.dat.plot <- data.frame(tpr.mean = rowMeans(tpr.dat))
        fpr.dat.plot <- data.frame(
          fpr.mean = rowMeans(fpr.dat),
          fpr.sd = matrixStats::rowSds(fpr.dat, na.rm = TRUE)
        )

        data_train <- cbind(tpr.dat.plot, fpr.dat.plot)
        n <- nrow(data_train)

        # 处理边界值
        first_one_index <- which(data_train$fpr.mean == 1)[1]
        if (!is.na(first_one_index)) {
          data_train[first_one_index:n, 1:2] <- 1
          data_train[first_one_index:n, 3] <- 0
        }

        # 确保auc_list存在
        if (length(auc_list) == 0) {
          stop("无法计算平均AUC值，auc_list为空")
        }

        auc_mean <- mean(auc_list, na.rm = TRUE)

        # 使用新函数创建ROC曲线图
        roc_plot <- plot_roc_curve(
          data_train = data_train,
          auc_mean = auc_mean,
          title = input$roc_title,
          xlab = input$roc_xlab,
          ylab = input$roc_ylab,
          title_size = input$roc_title_size,
          xlab_size = input$roc_xlab_size,
          ylab_size = input$roc_ylab_size
        )

        return(list(auc = auc_list, plot = roc_plot))

      }, error = function(e) {
        shiny::showNotification(paste("生成ROC曲线错误:", e$message), type = "error", duration = 10)
        NULL
      })
    })

    # 显示ROC曲线
    output$roc_plot <- shiny::renderPlot({
      data <- roc_data()
      if (is.null(data)) return()
      data$plot
    })

    # 显示AUC值
    output$auc_text <- shiny::renderPrint({
      data <- roc_data()
      if (is.null(data)) {
        cat("等待计算AUC值...")
      } else {
        cat("平均AUC值:", round(mean(data$auc), 4), "\n")
        cat("各折AUC值:\n")
        print(round(data$auc, 4))
      }
    })

    # ROC曲线保存为TIFF
    output$save_roc_tiff <- shiny::downloadHandler(
      filename = function() {
        paste0("roc_curve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tif")
      },
      content = function(file) {
        data <- roc_data()
        if (is.null(data)) return()

        ggplot2::ggsave(
          filename = file,
          plot = data$plot,
          device = "tiff",
          dpi = input$roc_dpi,
          compression = "lzw",
          width = 8,
          height = 6,
          units = "in"
        )
      }
    )
    # ROC曲线保存为PDF
    output$save_roc_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("roc_curve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        data <- roc_data()
        if (is.null(data)) return()

        ggplot2::ggsave(
          filename = file,
          plot = data$plot,
          device = "pdf",
          width = 8,
          height = 6,
          units = "in"
        )
      }
    )
  }

  # 运行应用
  shiny::shinyApp(ui, server)
}
