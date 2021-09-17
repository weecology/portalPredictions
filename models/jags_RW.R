cast_all <- jags_RW(data_set = "all", main = ".", control_files = files_control(), control_runjags = runjags_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_all, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_controls <- jags_RW(data_set = "controls", main = ".", control_files = files_control(), control_runjags = runjags_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_controls, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_exclosures <- jags_RW(data_set = "exclosures", main = ".", control_files = files_control(), control_runjags = runjags_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_exclosures, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_dm_controls <- jags_RW(data_set = "dm_controls", main = ".", control_files = files_control(), control_runjags = runjags_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_dm_controls, main = ".", quiet = FALSE, arg_checks = TRUE);
