tree <- dirtree(".", "", subdirs());
f_a <- AutoArima(tree, level = "All", quiet = FALSE);
f_c <- AutoArima(tree, level = "Controls", quiet = FALSE);
save_forecast_output(f_a, f_c, "AutoArima", tree)
