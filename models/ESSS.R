cast_all <- ESSS(dataset = "all", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_all, main = "~/te", settings = directory_settings(), quiet = FALSE);
cast_controls <- ESSS(dataset = "controls", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_controls, main = "~/te", settings = directory_settings(), quiet = FALSE);
cast_exclosures <- ESSS(dataset = "exclosures", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_exclosures, main = "~/te", settings = directory_settings(), quiet = FALSE);
cast_dm_controls <- ESSS(dataset = "dm_controls", main = "~/te", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_dm_controls, main = "~/te", settings = directory_settings(), quiet = FALSE);
