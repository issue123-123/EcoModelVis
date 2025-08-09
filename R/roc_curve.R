#'@title 生成ROC曲线和AUC值
#'
#'@description 该函数读取样本和背景预测文件，计算AUC值并创建ROC曲线图。
#'
#' @param sample_path 样本预测文件路径
#' @param background_dir 背景预测文件目录
#' @param plot_title 图表标题（默认为"ROC Curve"）
#'
#' @return 包含AUC值和ROC曲线的列表
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot geom_smooth geom_line scale_color_manual scale_fill_manual scale_x_continuous scale_y_continuous labs theme_bw theme element_text element_rect
#' @importFrom ROCR prediction performance
#' @importFrom matrixStats rowSds
#' @export



roc_curve <- function(sample_path, background_paths, plot_title = "ROC Curve",
                      save_path = NULL, dpi = 600) {

  # 验证包依赖
  required_pkgs <- c("dplyr", "purrr", "ggplot2", "matrixStats", "ROCR","extrafont")
  check_packages(required_pkgs)


    # 读取样本数据
    presence <- tryCatch({
      read.csv(sample_path)
    }, error = function(e) {
      stop("读取样本文件失败: ", sample_path, "\n错误信息: ", e$message)
    })

    # 检查必要列
    if (!"Test.or.train" %in% colnames(presence)) {
      stop("样本文件必须包含 'Test.or.train' 列")
    }

    # 获取背景和样本预测文件名
    file_names.bac <- list.files(pattern = "^Antigone_antigone_\\d+_backgroundPredictions.csv$")
    file_names.pre <- list.files(pattern = "^Antigone_antigone_\\d+_samplePredictions.csv$")

    # 读取背景和样本预测数据
    data_list.bac <- lapply(file_names.bac, function(file) {
      tryCatch({
        read.table(file, header = TRUE, sep = ",")
      }, error = function(e) {
        stop("读取背景文件失败: ", file, "\n错误信息: ", e$message)
      })
    })
    data_list.pre <- lapply(file_names.pre, function(file) {
      tryCatch({
        read.table(file, header = TRUE, sep = ",")
      }, error = function(e) {
        stop("读取样本文件失败: ", file, "\n错误信息: ", e$message)
      })
    })

    # 合并背景和样本预测数据
    merged_bac <- do.call(cbind, data_list.bac)
    merged_pre <- do.call(cbind, data_list.pre)

    # 提取预测列
    pre <- merged_pre[, seq(6, 60, 6)]
    bac <- merged_bac[, seq(5, 50, 5)]

    # 初始化列表和向量
    tpr_list <- list()
    fpr_list <- list()
    auc_list <- numeric(10)

    # 计算AUC和ROC
    for (i in 1:10) {
      pp <- pre[, i]
      bb <- bac[, i]
      trainpp <- pp[presence$Test.or.train == "train"]
      combined <- c(trainpp, bb)
      label <- c(rep(1, length(trainpp)), rep(0, length(bb)))

      pred <- prediction(combined, label)
      perf <- performance(pred, "tpr", "fpr")

      # 计算并保存 AUC 值
      auc_list[i] <- performance(pred, "auc")@y.values[[1]]
      cat("Iteration", i, "AUC:", auc_list[i], "\n")

      tpr_train <- perf@x.values[[1]]
      fpr_train <- perf@y.values[[1]]
      tpr_list[[i]] <- tpr_train
      fpr_list[[i]] <- fpr_train
    }

    # 将列表转换为数据框
    tpr.dat <- do.call(cbind, tpr_list)
    fpr.dat <- do.call(cbind, fpr_list)

    tpr.dat.plot <- data.frame(tpr.mean = rowMeans(tpr.dat))
    fpr.dat.plot <- data.frame(fpr.mean = rowMeans(fpr.dat),
                               fpr.sd = apply(fpr.dat, 1, sd))

    data_train <- cbind(tpr.dat.plot, fpr.dat.plot)

    # 找到第二列'fpr.mean'中第一个值为1的行索引
    first_one_index <- which(data_train$fpr.mean == 1)[1]

    # 检查是否找到符合条件的行
    if (!is.na(first_one_index)) {
      # 获取数据框总行数
      n_rows <- nrow(data_train)

      # 修改目标区域的值
      data_train[first_one_index:n_rows, 1:2] <- 1  # 第1-2列设为1
      data_train[first_one_index:n_rows, 3] <- 0     # 第3列设为0
    }
    line <- data.frame(x = c(0, 1), y = c(0, 1))

    # 创建ROC曲线图
    roc_plot <- ggplot2::ggplot(data = data_train, ggplot2::aes(x = tpr.mean)) +
      # 1. 绘制带状区域（蓝色，不透明）
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = fpr.mean - fpr.sd,
                     ymax = fpr.mean + fpr.sd),
        fill = "blue",  # 直接指定颜色
        alpha = 1,
        color = "black"
      ) +
      # 2. 绘制均值线（红色）
      ggplot2::geom_line(
        ggplot2::aes(y = fpr.mean),
        color = "red",  # 直接指定颜色
        size = 0.8
      ) +
      # 3. 绘制随机预测线（黑色实线）
      ggplot2::geom_line(
        data = data.frame(x = c(0, 1), y = c(0, 1)),
        ggplot2::aes(x = x, y = y),
        color = "black",  # 直接指定颜色
        size = 0.8,
        linetype = "solid"
      ) +
      # 添加图例映射
      ggplot2::geom_point(
        data = data.frame(
          color = c("red", "blue", "black")  # 直接使用颜色值作为名称
        ),
        ggplot2::aes(x = -Inf, y = -Inf, color = color),
        size = 0,  # 隐藏实际点
        alpha = 0   # 完全透明
      ) +
      # 设置图例颜色和形状
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
        name = input$roc_xlab,
        breaks = seq(0, 1, 0.2),
        limits = c(0, 1)
      ) +
      ggplot2::scale_y_continuous(
        name = input$roc_ylab,
        breaks = seq(0, 1, 0.2),
        limits = c(0, 1)
      ) +
      ggplot2::labs(title = input$roc_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = input$roc_title_size,
          family = "SimSun",  # 使用extrafont加载的字体
          face = "bold"
        ),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "black", size = 0.8),
        axis.title = ggplot2::element_text(size = 12, family = "SimSun"),
        axis.title.x = ggplot2::element_text(
          size = input$roc_xlab_size,
          family = "SimSun"
        ),
        axis.title.y = ggplot2::element_text(
          size = input$roc_ylab_size,
          family = "SimSun"
        ),
        axis.text = ggplot2::element_text(
          size = 8,
          family = "Times New Roman"
        ),
        # 图例设置 - 去掉所有框线
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.95, 0.05),  # 右下角位置
        legend.justification = c(1, 0),    # 对齐到右下角
        legend.direction = "vertical",      # 垂直方向
        legend.text = ggplot2::element_text(
          family = "Times New Roman",  # 图例文字用Times New Roman
          size = 10
        ),
        legend.key = ggplot2::element_rect(
          fill = "white",
          color = NA,         # 去掉图例色块外面的线
          size = 0
        ),
        legend.key.size = ggplot2::unit(1.5, "lines"),
        legend.background = ggplot2::element_blank(),  # 完全去掉图例背景
        legend.box.background = ggplot2::element_blank(),  # 去掉图例框背景
        legend.box.margin = ggplot2::margin(2, 2, 2, 2)  # 适当边距
      ) +
      # 覆盖图例显示为实心方块
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(15, 15, 15),  # 实心方块形状
            size = 4,               # 方块大小
            linetype = 0,           # 隐藏线条
            alpha = 1               # 不透明
          ),
          label.theme = ggplot2::element_text(
            family = "Times New Roman
            ",
            size = 10
          )
        )
      ) +
      # 确保中文显示
      ggplot2::theme(text = ggplot2::element_text(family = "SimSun"))
    return(list(auc = auc_list, plot = last_plot()))
}
