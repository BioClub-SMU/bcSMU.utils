#' @param model 一个已拟合的coxph对象，参考组应为第1类
#' @param exposure_var 暴露变量的名称（字符型），如 "PsRS"
#' @param data 数据框，必须包含暴露变量
#'
#' @return 返回PFP%的数值
#' 
calculate_pfp <- function(model, exposure_var, data) {
  
  data[[exposure_var]] <- factor(data[[exposure_var]], levels = c("1", "2", "3"))
  
  coefs <- summary(model)$coefficients
  confint_mat <- summary(model)$conf.int
  
  # 提取HR及上下限
  coefs <- summary(model)$conf.int
  HR2 <- coefs[grep("2", rownames(coefs)), "exp(coef)"]
  HR3 <- coefs[grep("3", rownames(coefs)), "exp(coef)"]
  HR2_lower <- coefs[grep("2", rownames(coefs)), "lower .95"]
  HR2_upper <- coefs[grep("2", rownames(coefs)), "upper .95"]
  HR3_lower <- coefs[grep("3", rownames(coefs)), "lower .95"]
  HR3_upper <- coefs[grep("3", rownames(coefs)), "upper .95"]
  
  # 计算暴露人群比例
  P2 <- mean(data[[exposure_var]] == "2")
  P3 <- mean(data[[exposure_var]] == "3")
  
  # PFP计算函数
  compute_pfp <- function(p2, p3, hr2, hr3){
    num <- p2 * (hr2 - 1) + p3 * (hr3 - 1)
    denom <- num + 1
    PFP_percent <- num / denom * 100
    return(PFP_percent)
  }
  
  # 主估计值
  pfp_point <- compute_pfp(P2, P3, HR2, HR3)
  # 下限估计
  pfp_lower <- compute_pfp(P2, P3, HR2_lower, HR3_lower)
  # 上限估计
  pfp_upper <- compute_pfp(P2, P3, HR2_upper, HR3_upper)
  
  # 输出字符串
  result <- paste0(
    round(pfp_point, 2), "% (95% CI: ",
    round(pfp_lower, 2), "% – ",
    round(pfp_upper, 2), "%)"
  )
  
  return(result)
}
