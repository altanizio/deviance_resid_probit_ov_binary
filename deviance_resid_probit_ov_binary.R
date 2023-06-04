library(tidyverse)
library(lavaan)

model <- '
y ~ x + d + n
j ~ o + e
l ~ a + b

# free the intercept and fix threshold to 0
y | 0*t1
y ~ NA*1
j | 0*t1
j ~ NA*1
l | 0*t1
l ~ NA*1
'
fit  <-  sem(model, data = data, group = "group", meanstructure=TRUE,
                          estimator='WLSMV', ordered = TRUE,link='probit')
                          
deviance_resid_probit_ov_binary <- function(fit) {
  model_estimates = parameterestimates(fit) %>% data.frame()
  n_groups = unique(model_estimates$group)
  ov_y = lavNames(fit, "ov.nox")
  
  result_final = list()
  for (groups_for in n_groups) {
    data_model = inspect(fit, "data")[[groups_for]] |> as.data.frame()
    model_estimates_group = model_estimates %>% filter(group == groups_for)
    result_final_dataframe = data.frame(id = seq(1,nrow(data_model)))
    for (ov_y_for in ov_y) {
      model_estimates_ov = model_estimates_group %>% filter(lhs == ov_y_for)
      model_estimates_inters = model_estimates_ov %>% filter(op == "~1")
      model_estimates_coef = model_estimates_ov %>% filter(op == "~")
      ov = 0
      for (i in seq(1,nrow(model_estimates_coef))) {
        model_estimates_rhs = model_estimates_coef[i,"rhs"]
        model_estimates_est = model_estimates_coef[i,"est"]
        ov = ov + data_model[,model_estimates_rhs] * model_estimates_est
      }
      ov = ov + model_estimates_inters[1,"est"]
      ov <- pnorm(ov)
      
      ov_resid = c()
      
      for (i in seq(1,length((data_model[,ov_y_for] - 1)))) {
        if ((data_model[,ov_y_for] - 1)[i] == 1 ) {
          ov_resid[i] = sqrt(-2 * log(ov[i]))  
        }else {
          ov_resid[i] = -sqrt(-2 * log(1-ov[i]))  
        }
      }
      data_result = tibble(ov_resid)
      colnames(data_result) = ov_y_for
      result_final_dataframe = bind_cols(result_final_dataframe, data_result)
    }
    result_final[groups_for] = list(result_final_dataframe)
  }
  return(result_final)
}

resids <- deviance_resid_probit_ov_binary(fit)
