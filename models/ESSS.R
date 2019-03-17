tree <- dirtree(".", "", subdirs());
f_a <- ESSS(tree, level = "All", quiet = FALSE);
f_c <- ESSS(tree, level = "Controls", quiet = FALSE);
save_forecast_output(f_a, f_c, "", tree)
