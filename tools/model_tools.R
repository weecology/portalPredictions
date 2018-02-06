#' Interpolate missing (rodent) abundance data
#' 
#' @param abundance data table with new moon column
#' @return data table of interpolation-inclusive counts for each species and 
#'  total

interpolate_abundance <- function(abundance){

      moons <- (min(abundance$newmoonnumber)):(max(abundance$newmoonnumber))
      nmoons <- length(moons)

      species <- c("BA", "DM", "DO", "DS", "NA.", "OL", "OT", "PB", "PE", 
                   "PF", "PH", "PL", "PM", "PP", "RF", "RM", "RO", "SF",
                   "SH", "SO")
      nspecies <- length(species)

      abunds <- matrix(NA, nrow = nmoons, ncol = nspecies)

      for(i in 1:nmoons){
        if(length(which(abundance$newmoonnumber == moons[i])) > 0){
          temp <- abundance[which(abundance$newmoonnumber == moons[i]),
                            which(colnames(abundance) %in% species)]
          abunds[i, ] <- as.numeric(temp)
        }
      }

      interpolated_abunds <- abunds
      colnames(interpolated_abunds) <- species

      for(j in 1:nspecies){
        interpolated_abunds[ , j] <- round(na.interp(abunds[ , j]))
      }

      interpolated_total <- apply(interpolated_abunds, 1, sum)

    out <- data.frame(moons, interpolated_abunds, total = interpolated_total)

    return(out)
}
