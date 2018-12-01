tree <- dirtree("~", "forecasting", c("predictions", "models", "PortalData", "data", "tmp"));
all <- read.csv(file_path(tree, "data/all.csv"));
controls <- read.csv(file_path(tree, "data/controls.csv"));
metadata <- yaml::yaml.load_file(file_path(tree, "data/metadata.yaml"));
f_a <- AutoArima(all, metadata);
f_c <- AutoArima(controls, metadata, level = "Controls");
save_forecast_output(f_a, f_c, "AutoArima", metadata, sub_path(tree, "tmp"))
