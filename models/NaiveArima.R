cast_all <- NaiveArima(data_set = "all", main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_all, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_controls <- NaiveArima(data_set = "controls", main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_controls, main = ".", quiet = FALSE, arg_checks = TRUE);
cast_exclosures <- NaiveArima(data_set = "exclosures", main = ".", control_files = files_control(), quiet = FALSE, verbose = TRUE, arg_checks = TRUE);
save_cast_output(cast_exclosures, main = ".", quiet = FALSE, arg_checks = TRUE);
