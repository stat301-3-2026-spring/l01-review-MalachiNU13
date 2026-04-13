log_model <- logistic_reg() %>%
  set_engine("glm")

wf_log <- workflow() %>%
  add_model(log_model) %>%
  add_recipe(rec)

set.seed(3012)

log_fit <- fit(wf_log, data = train_data)

# Validation predictions
log_preds <- predict(log_fit, val_data, type = "prob") %>%
  bind_cols(val_data)

roc_auc(log_preds, truth = host_is_superhost, .pred_1)