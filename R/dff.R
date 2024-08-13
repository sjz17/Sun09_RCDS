RCDS=function(data){
  CalRiskScore <- function(fit, new_data, type = "lp"){
    new_data <- new_data[, fit$subFeature]
    RS <- quiet(switch(
      EXPR = class(fit)[1],
      "coxnet"      = predict(fit, type = 'link', as.matrix(new_data)),
      "coxph"       = predict(fit, type = 'lp', as.data.frame(new_data)),
      "survivalsvm" = predict(fit, as.data.frame(new_data))$predicted,
      "CoxBoost"    = predict(fit, type = "lp", as.data.frame(new_data)),
      "superpc"     = superpc.predict(object = fit,
                                      data = fit$data,
                                      newdata = list(x = t(scale(as.matrix(new_data)))),
                                      threshold = fit$threshold,
                                      n.components = 1)$v.pred,
      "plsRcoxmodel" = predict(fit, type = "lp", as.data.frame(new_data)),
      "rfsrc"        = predict(fit, as.data.frame(new_data))$predicted,
      "gbm"          = predict(fit, type = 'link', as.data.frame(new_data))
    ))
    RS = as.vector(RS)
    names(RS) = rownames(new_data)
    return(RS)
  }
  int=intersect(colnames(data),fit$subFeature)
  data=data[,fit$subFeature]
  data=scale(data)
  score<- CalRiskScore(fit =fit, new_data =data,type = "lp")
}
