tree <- dirtree("~", "forecasting", c("predictions", "models", "PortalData", "data", "tmp"));
all <- read.csv(file_path(tree, "data/all.csv"));
controls <- read.csv(file_path(tree, "data/controls.csv"));
metadata <- yaml::yaml.load_file(file_path(tree, "data/metadata.yaml"));
f_a <- nbGARCH(all, metadata);
f_c <- nbGARCH(controls, metadata, level = "Controls");
save_forecast_output(f_a, f_c, "nbGARCH", metadata, sub_path(tree, "tmp"))
