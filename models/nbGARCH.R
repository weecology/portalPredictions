tree <- dirtree(".", "", subdirs());
all <- read_data(tree, "all");
controls <-read_data(tree, "controls");
metadata <- read_data(tree, "metadata");
f_a <- nbGARCH(all, metadata, quiet = FALSE);
f_c <- nbGARCH(controls, metadata, level = "Controls", quiet = FALSE);
save_forecast_output(f_a, f_c, "nbGARCH", metadata, sub_path(tree, "tmp"))
