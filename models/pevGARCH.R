cast_all <- pevGARCH(dataset = "all", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_all, main = "~/te", settings = directory_settings(), quiet = FALSE);
cast_controls <- pevGARCH(dataset = "controls", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_controls, main = "~/te", settings = directory_settings(), quiet = FALSE);
cast_exclosures <- pevGARCH(dataset = "exclosures", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_exclosures, main = "~/te", settings = directory_settings(), quiet = FALSE);
cast_dm_controls <- pevGARCH(dataset = "dm_controls", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_dm_controls, main = "~/te", settings = directory_settings(), quiet = FALSE);
