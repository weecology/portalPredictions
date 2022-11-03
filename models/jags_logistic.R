cast_dm_controls <- jags_logistic(dataset = "dm_controls", main = ".", settings = directory_settings(), quiet = FALSE, verbose = FALSE);
save_cast_output(cast_dm_controls, main = ".", settings = directory_settings(), quiet = FALSE);
