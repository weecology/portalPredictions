tree <- dirtree(".", "", subdirs());
all <- read_data(tree, "all");
controls <-read_data(tree, "controls");
covariates <- read_data(tree, "covariates"); 
metadata <- read_data(tree, "metadata");
f_a <- pevGARCH(all, covariates, metadata, lag = 6, quiet = FALSE);
f_c <- pevGARCH(controls, covariates, metadata, level = "Controls", lag = 6, quiet = FALSE);
save_forecast_output(f_a, f_c, "pevGARCH", metadata, sub_path(tree, "tmp"))
