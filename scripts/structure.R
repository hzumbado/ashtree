folders <- c("data/raw",
             "data/processed",
             "shapefiles",
             "scripts",
             "output/figures",
             "output/tables",
             "manuscript")

sapply(folders,
       FUN = dir.create,
       recursive = TRUE)
