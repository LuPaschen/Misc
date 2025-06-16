####################
# This script reads .normtimef0 files created by ProsodyPro and creates normalized pitch plots
# The files should be named according to this template:
# word_repetition_speaker_syllableStructure.normtimef0
# Ludger Paschen, June 2025
####################

# Load required libraries
library(tidyverse)
library(tools)

# Directory where the data is located
directory <- "..."

# Extension
extension <- "normtimef0"

# Get file names in directory
files <- list.files(path = directory, pattern = paste0("\\.", extension, "$"), full.names = TRUE)

# Function to read files into one dataframe 
df_all <- bind_rows(lapply(files, function(file) {
  df <- read.delim(file, stringsAsFactors = FALSE)
  df$filename <- tools::file_path_sans_ext(basename(file))
  return(df)
}))

# Read files and normalize F0 to semitones
normalize <- df_all %>%
  separate(filename,
           into = c("word", "repetition", "speaker", "syllable_structure"),
           sep = "_",
           remove = FALSE) %>%
  group_by(filename, rowLabel) %>%
  mutate(F0_base = first(F0),
         pitch_st = 12 * log2(F0 / F0_base))

# Make a selection, e.g. to just include a certain syllable type
selection <- normalize %>%
  filter(syllable_structure == "CVCV")

# Calculate mean pitch values for each point on the normalized time scale
summary_df <- selection %>%
  group_by(NormalizedTime) %>%
  summarize(
    mean_pitch_st = mean(pitch_st, na.rm = TRUE),
    sd_pitch_st = sd(pitch_st, na.rm = TRUE)
  )

# Anciallary calculations for the plots
n_points <- nrow(summary_df)
mid_index <- ceiling(n_points / 2)
mid_x <- summary_df$NormalizedTime[mid_index]

# Create a plot showing the normalized contour for the 
p <- ggplot(summary_df, aes(x = NormalizedTime, y = mean_pitch_st, group = 1)) +
  geom_line(color = "blue") +  # Connect mean points
  geom_point() +
## Error bars - useful for raw pitch plotting but not so much for semitones anchored to the first time point
#  geom_errorbar(aes(ymin = mean_pitch_st - sd_pitch_st,
#                    ymax = mean_pitch_st + sd_pitch_st),
#                width = 0.2) +
  geom_vline(xintercept = mid_x, linetype = "dashed", color = "red") +
  annotate("text", x = mid_index / 2, y = max(summary_df$mean_pitch_st, na.rm = TRUE) + 1,
           label = "V1", size = 5, fontface = "bold") +
  annotate("text", x = mid_index + (n_points - mid_index) / 2, 
           y = max(summary_df$mean_pitch_st, na.rm = TRUE) + 1,
           label = "V2", size = 5, fontface = "bold") +
  theme_minimal() +
  labs(x = "Normalized Time", y = "Normalized Pitch (st)", 
       title = "My title")

# Show plot
p

# Optionally, save plot (adjust values as necessary)
ggsave("my_plot.png", plot = p, path = directory, width = 6, height = 4, dpi = 300)

## Note: To plot pitch contours for words with fewer or more vowels, you will have to adjust the code above
## For example, if you have three vowels, you will probably want to plot two vertical lines and three annotations:
# geom_vline(xintercept = mid_index * 0.67, linetype = "dashed", color = "red") +
# geom_vline(xintercept = mid_index * 1.33, linetype = "dashed", color = "red") +
#  annotate("text", x = mid_index * 0.33, 
#           y = max(summary_df_CVCVCV$mean_pitch_st, na.rm = TRUE) + 0.5,
#           label = "V1", size = 5, fontface = "bold") +
#  annotate("text", x = mid_index,
#           y = max(summary_df_CVCVCV$mean_pitch_st, na.rm = TRUE) + 0.5,
#           label = "V2", size = 5, fontface = "bold") +
#  annotate("text", x = mid_index * 1.67,
#           y = max(summary_df_CVCVCV$mean_pitch_st, na.rm = TRUE) + 0.5,
#           label = "V3", size = 5, fontface = "bold") +






