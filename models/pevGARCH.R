tree <- dirtree(".", "", c("predictions", "models", "PortalData", "data", "tmp"));
all <- read.csv(file_path(tree, "data/all.csv"));
controls <- read.csv(file_path(tree, "data/controls.csv"));
covariates <- read.csv(file_path(tree, "data/covariates.csv")); 
metadata <- yaml::yaml.load_file(file_path(tree, "data/metadata.yaml"));
f_a <- pevGARCH(all, covariates, metadata, lag = 6);
f_c <- pevGARCH(controls, covariates, metadata, level = "Controls", lag = 6);
save_forecast_output(f_a, f_c, "pevGARCH", metadata, sub_path(tree, "tmp"))
