cast_all_interp <- pevGARCH(dataset = "all_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_all_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls_interp <- pevGARCH(dataset = "controls_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_controls_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures_interp <- pevGARCH(dataset = "exclosures_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_exclosures_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_dm_controls_interp <- pevGARCH(dataset = "dm_controls_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_dm_controls_interp, main = ".", settings = directory_settings(), quiet = FALSE);
