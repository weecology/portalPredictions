main_exists <- any(ls() == "main")

old_main <- dynGet("main")

main   <- ".."

global <- global_list(main = main)

onStop(function() {
  rm(main, inherits = TRUE)
  if (main_exists) main <<- old_main
})