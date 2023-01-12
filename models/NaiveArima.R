cast_all <- NaiveArima(dataset = "all", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_all, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls <- NaiveArima(dataset = "controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_controls, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures <- NaiveArima(dataset = "exclosures", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_exclosures, main = ".", settings = directory_settings(), quiet = FALSE);
