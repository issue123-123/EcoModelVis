#' @title 生成物种响应曲线
#'
#' @description 该函数读取多个数据文件，计算平均值和绝对偏差，并创建物种对环境变量的响应曲线。
#'
#' @param file_paths 包含数据文件的文件路径向量
#' @param species_name 物种名称（用于标题）
#' @param variable_name 环境变量名称（用于标题和x轴标签）
#' @param x_limit x轴范围（默认为c(-25,25)）
#' @param y_limit y轴范围（默认为c(0,0.8)）
#' @param save_path 保存图片路径（可选）
#' @param dpi 图片分辨率（默认为600）
#'
#' @return ggplot对象
#' @importFrom dplyr group_by summarise mutate filter
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv cols col_character col_double
#' @importFrom stringr str_extract
#' @importFrom ggplot2 ggplot geom_ribbon geom_line scale_x_continuous scale_y_continuous labs theme element_line element_text element_rect element_blank ggsave
#' @export

response_curve <- function(file_paths, species_name, variable_name,
                           x_limit = c(-25, 25), y_limit = c(0, 0.8),
                           save_path = NULL, dpi = 600) {

  # 验证包依赖
  required_pkgs <- c("dplyr", "purrr", "readr", "stringr", "ggplot2")
  check_packages(required_pkgs)

  # 检查文件是否存在
  missing_files <- file_paths[!file.exists(file_paths)]
  if (length(missing_files) > 0) {
    stop("找不到以下文件:\n", paste(missing_files, collapse = "\n"))
  }

  # 读取并处理数据 - 使用readr::read_csv
  combined_data <- purrr::map_dfr(file_paths, function(.x) {
    tryCatch({
      df <- readr::read_csv(
        .x,
        col_names = c("variable", "x", "y"),
        col_types = readr::cols(
          variable = readr::col_character(),
          x = readr::col_double(),
          y = readr::col_double()
        )
      )

      # 提取文件ID - 如果需要
      file_id <- as.integer(stringr::str_extract(basename(.x), "\\d"))

      df %>%
        dplyr::mutate(file_id = file_id, .before = 1) %>%
        dplyr::filter(!is.na(y))
    }, error = function(e) {
      stop("读取文件失败: ", .x, "\n错误信息: ", e$message)
    })
  })

  # 检查数据是否为空
  if (nrow(combined_data) == 0) {
    stop("没有读取到有效数据，请检查文件格式和内容")
  }

  # 计算统计量 - 使用原始成功的方法
  summary_data <- combined_data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(
      mean_y = mean(y, na.rm = TRUE),
      abs_deviation = mean(abs(y - mean_y), na.rm = TRUE),
      .groups = "drop"
    )

  # 创建响应曲线图
  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_y - abs_deviation,
                                      ymax = mean_y + abs_deviation),
                         fill = "#0000fe") +
    ggplot2::geom_line(ggplot2::aes(y = mean_y),
                       color = "#CB181D",
                       linewidth = 0.8) +
    ggplot2::scale_x_continuous(limits = x_limit) +
    ggplot2::scale_y_continuous(limits = y_limit) +
    ggplot2::labs(title = paste("Response of", species_name, "to", variable_name),
                  x = variable_name,
                  y = "Logistic output") +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(color = "black", size = 10),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "plain"),
      panel.grid = ggplot2::element_blank(),  # 修复这里
      panel.background = ggplot2::element_rect(fill = "white")
    )

  # 保存图片
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(
      save_path,
      plot = p,
      device = "tiff",
      dpi = dpi,
      width = 8,
      height = 6,
      units = "in",
      compression = "lzw"
    )
    message("响应曲线已保存至: ", save_path)
  }

  return(p)
}
