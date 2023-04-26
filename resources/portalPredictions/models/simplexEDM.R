cast_all_interp <- simplexEDM(data_set = "all_interp", main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_all_interp, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_controls_interp <- simplexEDM(data_set = "controls_interp", main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_controls_interp, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_exclosures_interp <- simplexEDM(data_set = "exclosures_interp", main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_exclosures_interp, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_dm_controls_interp <- simplexEDM(data_set = "dm_controls_interp", main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_dm_controls_interp, main = ".", quiet = FALSE, arg_checks = TRUE);
