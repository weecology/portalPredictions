tree <- dirtree(".", "", subdirs());
f_a <- nbGARCH(tree, level = "All", quiet = FALSE);
f_c <- nbGARCH(tree, level = "Controls", quiet = FALSE);
save_forecast_output(f_a, f_c, "nbGARCH", tree)
