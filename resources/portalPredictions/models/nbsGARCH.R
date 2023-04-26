cast_all <- nbsGARCH(dataset = "all", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_all, main = ".", settings = directory_settings(), quiet = FALSE);
cast_controls <- nbsGARCH(dataset = "controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_controls, main = ".", settings = directory_settings(), quiet = FALSE);
cast_exclosures <- nbsGARCH(dataset = "exclosures", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_exclosures, main = ".", settings = directory_settings(), quiet = FALSE);
