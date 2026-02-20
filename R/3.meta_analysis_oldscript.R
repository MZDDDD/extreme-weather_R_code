run_meta <- function(meta_m, disease_name = "FLU", type = "RDs") {

  library(dplyr)
  library(mixmeta)

  if (is.null(meta_m) || nrow(meta_m) == 0) {
    stop("meta_m is empty. Check Poisson step output.")
  }

  meta_m <- as.data.frame(meta_m)

  # 兼容你 meta_m 没列名的情况：强制前两列为 Beta / var
  if (!all(c("Beta","var") %in% names(meta_m))) {
    names(meta_m)[1:2] <- c("Beta", "var")
  }


  meta_m$Beta <- as.numeric(meta_m$Beta)
  meta_m$var  <- as.numeric(meta_m$var)

 
  S <- diag(meta_m$var)

  # random-effects meta（REML）
  model_m_coef <- tryCatch(
    mixmeta(Beta ~ 1, S = S, data = meta_m, method = "reml"),
    error = function(e) { message("mixmeta error: ", e$message); return(NULL) }
  )
  if (is.null(model_m_coef)) return(NULL)

  sm <- summary(model_m_coef)

  # pooled beta / se / p
  beta <- sm$coefficients[1, 1]
  se   <- sm$coefficients[1, 2]
  pval <- sm$coefficients[1, 4]

  # 95% CI (beta scale -> RR)
  rr      <- exp(beta)
  rr_low  <- exp(beta - 1.96 * se)
  rr_high <- exp(beta + 1.96 * se)

  meta_res <- data.frame(
    RRall     = rr,
    RRalllow  = rr_low,
    RRallhigh = rr_high,
    p_value   = pval,
    type      = type,
    name      = disease_name
  )

  return(meta_res)
}
