require(palmerpenguins)

#Calculate Mean

aggregate(penguins$flipper_length_mm, 
          list(penguins$species),
          FUN = mean, na.rm = TRUE
          )

#Calculate Mean (esier code)

aggregate(penguins$flipper_length_mm ~ species, 
          data = penguins,
          FUN = mean, 
          na.rm = TRUE)

boxplot(penguins$flipper_length_mm ~ species, 
        data = penguins)
