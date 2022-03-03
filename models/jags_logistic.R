cast_dm_controls <- jags_logistic(data_set = "dm_controls", main = ".", control_files = files_control(), control_runjags = runjags_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_dm_controls, main = ".", quiet = FALSE, arg_checks = TRUE);
