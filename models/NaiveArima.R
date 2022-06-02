cast_all <- NaiveArima(dataset = "all", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, order = c(0, 1, 0));
save_cast_output(cast_all, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls <- NaiveArima(dataset = "controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, order = c(0, 1, 0));
save_cast_output(cast_controls, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures <- NaiveArima(dataset = "exclosures", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, order = c(0, 1, 0));
save_cast_output(cast_exclosures, main = ".", settings = directory_settings(), quiet = FALSE);
cast_dm_controls <- NaiveArima(dataset = "dm_controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE, order = c(0, 1, 0));
save_cast_output(cast_dm_controls, main = ".", settings = directory_settings(), quiet = FALSE);
