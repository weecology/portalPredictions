cast_all_interp <- nbGARCH(dataset = "all_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_all_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls_interp <- nbGARCH(dataset = "controls_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_controls_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures_interp <- nbGARCH(dataset = "exclosures_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_exclosures_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_dm_controls_interp <- nbGARCH(dataset = "dm_controls_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_dm_controls_interp, main = ".", settings = directory_settings(), quiet = FALSE);
