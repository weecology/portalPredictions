cast_all_interp <- pevGARCH(data_set = "all_interp", lag = 6, main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_all_interp, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_controls_interp <- pevGARCH(data_set = "controls_interp", lag = 6, main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_controls_interp, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_exclosures_interp <- pevGARCH(data_set = "exclosures_interp", lag = 6, main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_exclosures_interp, main = ".", quiet = FALSE, arg_checks = TRUE);
