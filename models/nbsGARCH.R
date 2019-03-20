tree <- dirtree(".", "", subdirs());
f_a <- nbsGARCH(tree, level = "All", quiet = FALSE);
f_c <- nbsGARCH(tree, level = "Controls", quiet = FALSE);
save_forecast_output(f_a, f_c, "nbsGARCH", tree)
