cast_all_interp <- nbsGARCH(dataset = "all_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_all_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls_interp <- nbsGARCH(dataset = "controls_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_controls_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures_interp <- nbsGARCH(dataset = "exclosures_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_exclosures_interp, main = ".", settings = directory_settings(), quiet = FALSE);
cast_dm_controls_interp <- nbsGARCH(dataset = "dm_controls_interp", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_dm_controls_interp, main = ".", settings = directory_settings(), quiet = FALSE);
