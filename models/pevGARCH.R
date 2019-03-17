tree <- dirtree(".", "", subdirs());
f_a <- pevGARCH(tree, level = "All", lag = 6, quiet = FALSE);
f_c <- pevGARCH(tree, level = "Controls", lag = 6, quiet = FALSE);
save_forecast_output(f_a, f_c, "", tree)
