cast_all <- pevGARCH(dataset = "all", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_all, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls <- pevGARCH(dataset = "controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_controls, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures <- pevGARCH(dataset = "exclosures", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_exclosures, main = ".", settings = directory_settings(), quiet = FALSE);
cast_dm_controls <- pevGARCH(dataset = "dm_controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, lag = 6);
save_cast_output(cast_dm_controls, main = ".", settings = directory_settings(), quiet = FALSE);
