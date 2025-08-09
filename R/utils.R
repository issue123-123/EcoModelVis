#' @title 辅助函数：检查并安装缺失包
#'
#' @param pkgs
#'
#' @export
check_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing_pkgs) > 0) {
    message("安装缺失包: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs)
  }
  # 尝试加载所有包
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("无法加载包: ", pkg, "\n请手动安装")
    }
  }
}
