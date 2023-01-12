cast_all <- AutoArima(dataset = "all", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_all, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls <- AutoArima(dataset = "controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_controls, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures <- AutoArima(dataset = "exclosures", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_exclosures, main = ".", settings = directory_settings(), quiet = FALSE);
