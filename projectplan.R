
library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)
library(corrplot)
library(lubridate)
library(gridExtra)
library(grid)
library(fmsb)
library(tidyr)

# Read the CSV file into a data frame
spotify_2023 <- read.csv("spotify-2023.csv")

# View the structure of the data frame
str(spotify_2023)

# View the first few rows of the data frame
head(spotify_2023)




# Drop row at index 5
spotify_2023 <- spotify_2023[-573, ]

# Re-index the rows
rownames(spotify_2023) <- NULL



# Convert streams column to integer
spotify_2023$streams <- as.integer(spotify_2023$streams)

# Replace NAs with 0 in the streams column
spotify_2023$streams[is.na(spotify_2023$streams)] <- 0

any(is.na(spotify_2023$streams))

spotify_2023$in_deezer_playlists <- as.integer(spotify_2023$in_deezer_playlists)
spotify_2023$in_deezer_playlists[is.na(spotify_2023$in_deezer_playlists)] <- 0

spotify_2023$in_shazam_charts <- as.integer(spotify_2023$in_shazam_charts)
spotify_2023$in_shazam_charts[is.na(spotify_2023$in_shazam_charts)] <- 0


# Convert 'released_month' to a factor with proper ordering
spotify_2023$released_month <- factor(spotify_2023$released_month, levels = 1:12, labels = month.abb)





# Distribution of Audio Features


# Create a data frame with the audio features
audio_features <- data.frame(
  Feature = rep(c("BPM", "Danceability", "Energy", "Acousticness", "Speechiness"), each = nrow(spotify_2023)),
  Value = c(spotify_2023$bpm, spotify_2023$danceability, spotify_2023$energy,
            spotify_2023$acousticness, spotify_2023$speechiness)
)

# Plot histograms using ggplot2
ggplot(audio_features, aes(x = Value, fill = Feature)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  facet_wrap(~Feature, scales = "free") +
  labs(title = "Distribution of Audio Features",
       x = "Value", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12))




# Determine the platform with the highest value for each song
spotify_2023$max_platform <- apply(spotify_2023[, c("in_spotify_charts", "in_apple_charts", "in_deezer_charts", "in_shazam_charts")], 1, function(x) {
  platforms <- c("Spotify", "Apple Music", "Deezer", "Shazam")
  platforms[which.max(x)]
})

# Count the songs under the platform with the highest value
platform_counts <- table(spotify_2023$max_platform)

# Plot the bar chart
barplot(platform_counts, 
        main = "Platform Presence of Songs",
        xlab = "Platform",
        ylab = "Number of Songs",
        col = "lightblue")

grid()






# Create a data frame with artist count, mode, and frequency
artist_mode_counts <- table(spotify_2023$artist_count, spotify_2023$mode)
artist_mode_counts <- as.data.frame(artist_mode_counts)
colnames(artist_mode_counts) <- c("artist_count", "mode", "frequency")


# Plot clustered bar chart
ggplot(artist_mode_counts, aes(x = factor(artist_count), y = frequency, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Artist Counts by Mode",
       x = "Number of Artists",
       y = "Number of Songs",
       fill = "Mode") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center the title and make it bold



# Group by released_year and sum the streams
yearly_streams <- spotify_2023 %>%
  group_by(released_year) %>%
  summarise(total_streams = sum(streams))

# Filter the data to include years from 2000 onwards
yearly_streams <- yearly_streams %>% 
  filter(released_year >= 2000)

# Create the line plot with enhanced aesthetics
ggplot(yearly_streams, aes(x = released_year, y = total_streams)) +
  geom_line(color = "#0072B2", size = 1) +  # Custom line color and size
  labs(title = "Trends in Total Streams by Year",
       x = "Released Year",
       y = "Total Streams") +    # Add caption
  scale_x_continuous(breaks = seq(2000, max(yearly_streams$released_year), by = 1)) +  # Set breaks for x-axis
  scale_y_continuous(labels = scales::comma_format()) +  # Use comma format for y-axis labels
  theme_minimal() +                            # Minimal theme
  theme(plot.title = element_text(size = 18, face = "bold"),  # Title font size and style
        axis.title = element_text(size = 14, face = "bold"),  # Axis label font size and style
        axis.text = element_text(size = 12),                 # Axis tick font size
        plot.caption = element_text(size = 10),              # Caption font size
        panel.background = element_rect(fill = "white"),     # Set background color to white
        panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Gridline color and size
        panel.grid.minor = element_blank(),                 # Remove minor gridlines
        panel.border = element_blank(),                     # Remove panel border
        plot.margin = margin(20, 20, 20, 20)) +             # Adjust plot margins
   annotate("text", x = 2019, y = 3e7, label = "Sharp Increase", color = "red", size = 5, fontface = "bold")      # Add annotation







# Create a table of counts for each month
monthly_counts <- table(spotify_2023$released_month)

# Plotting the time series chart
barplot(monthly_counts, 
        main = "Number of Songs Released Each Month",
        xlab = "Month",
        ylab = "Number of Songs",
        col = "skyblue",
        border = "white",
        ylim = c(0, max(monthly_counts) * 1.1),  # Set y-axis limits with a buffer
        names.arg = levels(spotify_2023$released_month))  # Set x-axis labels to month abbreviations

grid()




# Most-streamed 10 artists

spotify_2023_long <- spotify_2023 %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  group_by(artist.s._name) %>%
  summarize(total_streams = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(total_streams))

top_artists_by_streams <- head(spotify_2023_long, 10)

# Display the top artists by streams
print(top_artists_by_streams)

# Custom function to convert numbers to dominant billions format
convert_to_dominant_billions <- function(x) {
  ifelse(x >= 1e9, paste0(round(x / 1e9, 1), "B"),
         ifelse(x >= 1e6, paste0(round(x / 1e6, 2), "M"), as.character(x)))
}

# Create a horizontal bar chart with customized colors and theme
ggplot(top_artists_by_streams, aes(x = reorder(artist.s._name, total_streams), y = total_streams)) +
  geom_bar(stat = "identity", fill = "#1db954", width = 0.6) +  # Adjust width of bars
  geom_text(aes(label = convert_to_dominant_billions(total_streams)), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 5, 
            fontface = "bold") +
  coord_flip() +
  labs(title = "Top 10 Artists", 
       x = "Total Streams",  # Adjust x-axis label
       y = "Artist") +  # Adjust y-axis label
  theme_minimal() +
  theme(
    text = element_text(color = "black", face = "bold", family = "Arial", size = 5),  # Adjust text size
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(color = "black", face = "bold", family = "Arial", size = 15),  # Adjust y-axis text size
    plot.title = element_text(size = 20, hjust = 0.5)
  )





# Most-streamed 10 songs
sorted_spotify_2023 <- spotify_2023 %>%
  arrange(desc(streams))
top_10_songs <- head(sorted_spotify_2023, 10)
top_10_songs$track_name

# Create a bar chart for the top 10 songs by streams
ggplot(top_10_songs, aes(x = reorder(track_name, streams), y = streams)) +
  geom_bar(stat = "identity", fill = "#1db954", width = 0.6) +  # Set bar color to green
  geom_text(aes(label = convert_to_dominant_billions(streams)),  # Apply custom function to convert stream numbers
            position = position_stack(vjust = 0.5), 
            color = "white",  # Set text color to white
            size = 5,  # Adjust position and appearance of text
            fontface = "bold") +  # Make text on bars bold
  coord_flip() +
  labs(title = "Top 10 Songs",  # Title without bold
       x = "Song",  # x-axis label without bold
       y = "Total Streams") +  # y-axis label without bold
  theme_minimal() +
  theme(  # Customize theme settings
    text = element_text(color = "black", face = "bold", family = "Arial"),  # Set text color to white, make it bold, and use Arial font
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text = element_text(color = "black", face = "bold", family = "Arial", size = 15),  # Set axis text color to white, make it bold, use Arial font, and increase size
    plot.title = element_text(size = 20, hjust = 0.5)  # Increase size of heading and align center
  )


# Total songs
unique_songs <- spotify_2023 %>%
  distinct()
total_unique_songs <- nrow(unique_songs)
total_unique_songs

# Retrieve the total number of unique songs
unique_songs <- spotify_2023 %>%
  distinct()
total_unique_songs <- nrow(unique_songs)

# Display the total number of unique songs with styled text
total_unique_songs_text <- paste("Total Unique Songs\n", total_unique_songs)

# Create a plot to display the total number of unique songs with styled text
ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = total_unique_songs, 
           size = 10, color = "#1db954", fontface = "bold", family = "Arial") +
  annotate("text", x = 0.5, y = 0.6, label = "Total songs", 
           size = 10, color = "white", fontface = "bold", family = "Arial") +
  annotate("text", x = 0.5, y = 0, label = total_unique_songs, 
           size = 10, color = "green", fontface = "bold", family = "Arial") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white", face = "bold", family = "Arial")  # Set text color to white, make it bold, and use Arial font
  )







# Total artists
all_artists <- spotify_2023$artist.s._name

# Get unique artists
unique_artists <- unique(all_artists)

# Total number of unique artists
total_unique_artists <- length(unique_artists)


total_unique_artists


ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = total_unique_artists, 
           size = 10, color = "#1db954", fontface = "bold", family = "Arial") +
  annotate("text", x = 0.5, y = 0.6, label = "Total artists", 
           size = 10, color = "white", fontface = "bold", family = "Arial") +
  annotate("text", x = 0.5, y = 0, label = total_unique_artists, 
           size = 10, color = "green", fontface = "bold", family = "Arial") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white", face = "bold", family = "Arial")  # Set text color to white, make it bold, and use Arial font
  )

# Total streams
total_streams <- sum(spotify_2023$streams)
total_streams


ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = convert_to_dominant_billions(total_streams), 
           size = 10, color = "#1db954", fontface = "bold", family = "Arial") +
  annotate("text", x = 0.5, y = 0.6, label = "Total streams", 
           size = 10, color = "white", fontface = "bold", family = "Arial") +
  annotate("text", x = 0.5, y = 0, label = total_unique_artists, 
           size = 10, color = "green", fontface = "bold", family = "Arial") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white", face = "bold", family = "Arial")  # Set text color to white, make it bold, and use Arial font
  )







# Create data with fixed values for song attributes
data <- data.frame(
  Danceability = c(100, 1, mean(spotify_2023$danceability_.)), 
  Valence = c(100, 1, mean(spotify_2023$valence_.)), 
  Energy = c(100, 1, mean(spotify_2023$energy_.)), 
  Acousticness = c(100, 1, mean(spotify_2023$acousticness_.)), 
  Instrumentalness = c(100, 1, mean(spotify_2023$instrumentalness_.)), 
  Liveness = c(100, 1, mean(spotify_2023$liveness_.)), 
  Speechiness = c(100, 1, mean(spotify_2023$speechiness_.))
)

# Check your data
print(head(data))
par(bg = "white")
# Create the radar chart with white text and green fill
radarchart(
  data,
  axistype = 1,
  pcol = "#1db954",         # Green line color
  pfcol = "#1db954",        # Green fill color
  plwd = 2,                 # Line width
  cglcol = "black",         # Color for grid lines
  cglty = 1,                # Type for grid lines
  cglwd = 0.8,              # Width for grid lines
  axislabcol = "white",     # Color for axis labels
  vlcex = 1,
  calcex = 1,               # Size for axis labels (0 to hide them)
  title = "Song Attributes Radar Chart" # Chart title
)







# Streams by decade of release
spotify_2023$decade <- 10 * (spotify_2023$released_year %/% 10)

# Aggregate streams by decade
streams_by_decade <- aggregate(streams ~ decade, data = spotify_2023, sum)

# Check the aggregated data
print(streams_by_decade)



# Plot donut chart
ggplot(streams_by_decade, aes(x = "", y = streams, fill = factor(decade))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove axis and grid lines
  scale_fill_manual(values = c("#a50026","#d73027", "#f46d43", "#fdae61", "#fee08b","#ffffbf","#d9ef8b", "#a6d96a","#66bd63","#1a9850")) +  # Custom colors
  theme(legend.position = "right",  # Adjust legend position
        legend.text = element_text(color = "black"),  # Set legend text color to white
        legend.title = element_text(color = "black"),  # Set legend title color to white
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black")  # Title appearance
  ) +
  labs(fill = "Decade") +  # Legend title
  ggtitle("Streams by Decade of Release")  # Chart title





# Streams by decade of release
spotify_2023$decade <- 10 * (spotify_2023$released_year %/% 10)

# Aggregate streams by decade
streams_by_decade <- aggregate(streams ~ decade, data = spotify_2023, sum)

# Calculate percentage of streams for each decade
streams_by_decade$percentage <- streams_by_decade$streams / sum(streams_by_decade$streams) * 100

# Check the aggregated data with percentages
print(streams_by_decade)



# Plot donut chart
ggplot(streams_by_decade, aes(x = "", y = streams, fill = factor(decade))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove axis and grid lines
  scale_fill_manual(values = c("#a50026","#d73027", "#f46d43", "#fdae61", "#fee08b","#ffffbf","#d9ef8b", "#a6d96a","#66bd63","#1a9850")) +  # Custom colors
  theme(legend.position = "right",  # Adjust legend position
        plot.background = element_rect(fill = "black"),  # Set background color to black
        panel.background = element_rect(fill = "black"),  # Set panel background color to black
        legend.text = element_text(color = "white", size = 14),  # Set legend text color to white
        legend.title = element_text(color = "white", size = 14),  # Set legend title color to white
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "white")  # Title appearance
  ) +
  labs(fill = "Decade") +  # Legend title
  ggtitle("Streams by Decade of Release", vjust = 0.5) # Chart title









# Function to create a green music symbol on a black background
draw_music_symbol <- function(size = 10, symbol_color = "#1db954", bg_color = "black") {
  grid.newpage()
  grid.rect(gp = gpar(fill = bg_color))
  grid.draw(
    grobTree(
      textGrob("\U266B", x = 0.5, y = 0.5, gp = gpar(col = symbol_color, fontsize = size, fontface = "bold"))
    )
  )
}

# Draw the music symbol on black background
draw_music_symbol(size = 100, symbol_color = "#1db954", bg_color = "black")









# Set up the plot area
plot.new()
par(bg = "black")
plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)

# Draw the triangle (play symbol)
polygon(c(0.3, 0.3, 0.8), c(0.2, 0.8, 0.5), col = "#1db954", border = NA)

# Draw the circle (center of play symbol)
symbols(x = 0.6, y = 0.5, circles = 0.3, inches = FALSE, add = TRUE, bg = "#1db954", fg = "#1db954", lwd = 2)

# Add some finishing touches (optional)
title(main = "Play Button", col.main = "white", font.main = 1, cex.main = 2)



# Release statistics

# Create a numeric month column from the month name
spotify_2023$month_num <- match(spotify_2023$released_month, month.abb)

# Combine year, month, and day into a Date object
spotify_2023$release_date <- as.Date(paste(spotify_2023$released_year, spotify_2023$month_num, spotify_2023$released_day, sep = "-"), format = "%Y-%m-%d")


# Extract month and day from release_date
spotify_2023$month <- month(spotify_2023$release_date, label = TRUE, abbr = TRUE)
spotify_2023$day <- day(spotify_2023$release_date)

# Aggregate the number of releases by month and day
release_count <- aggregate(release_date ~ month + day, data = spotify_2023, FUN = length)
colnames(release_count) <- c("Month", "Day", "Count")

# Create a heatmap
ggplot(release_count, aes(x = Day, y = Month, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Release Date Heatmap", x = "Day of Month", y = "Month", fill = "Number of Releases") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )














# Subset the data frame to include only the desired columns
selected_columns <- spotify_2023[, c("danceability_.","valence_.","energy_.", 
                                     "acousticness_.", "instrumentalness_.", "liveness_.", "speechiness_.")]

# Compute the correlation matrix for the selected columns
cor_matrix <- cor(selected_columns)

# View the correlation matrix
print(cor_matrix)






# Plot the correlation matrix heatmap using corrplot
corrplot(cor_matrix, method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black", # Add correlation coefficients on the heatmap
         col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "Correlation Matrix of Selected Musical Attributes and Streaming Statistics",
         mar = c(0, 0, 1, 0)) # Add title margin



# Melt the correlation matrix for plotting
melted_cor_matrix <- melt(cor_matrix)

# Plotting the correlation matrix with ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "lightgreen", high = "darkgreen", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),  # Adjust x-axis text
        axis.text.y = element_text(size = 12),  # Increase y-axis text size
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Title size and bold
        plot.background = element_rect(fill = "white", color = NA),  # Black background without border
        panel.background = element_rect(fill = "white"),  # Black panel background
        panel.border = element_blank(),  # Remove panel border
        legend.text = element_text(color = "black"),  # Legend text color
        legend.title = element_text(color = "black")  # Legend title color
  ) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  labs(title = "Correlation Matrix of Musical Attributes and Streaming Statistics",
       x = "", y = "")





# Convert counts to a data frame for plotting
counts_df <- data.frame(type = factor(names(counts), levels = c("0", "1")),
                        count = as.numeric(counts))

# Plotting the bar chart
ggplot(counts_df, aes(x = type, y = count, fill = type)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Solo vs. Collaborations", x = "Track Type", y = "Count") +
  scale_fill_manual(values = c("#1f78b4", "#33a02c"), guide = FALSE) +  # Custom fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered title
    axis.text = element_text(size = 14),  # Adjust axis text size
    axis.title = element_text(size = 16, face = "bold")  # Adjust axis title size
  )






all_artists <- spotify_2023$artist.s._name
# Load necessary libraries
library(dplyr)
library(tidyr)

# Create a function to determine if a song is solo or collaboration
is_collaboration <- function(artists) {
  if (grepl(",", artists)) {
    return("collab")
  } else {
    return("solo")
  }
}

# Apply the function to each row of the dataset
song_counts <- spotify_2023 %>%
  mutate(song_type = sapply(artist.s._name, is_collaboration)) %>%
  group_by(song_type) %>%
  summarise(count = n())

# Print the table
print(song_counts)

# Plotting the bar chart with green bars for solo and collab, and adjusting size and theme
ggplot(song_counts, aes(x = song_type, y = count, fill = song_type)) +
  geom_bar(stat = "identity", color = "black", width = 0.3) +  # Adjust width of bars (example: 0.8)
  scale_fill_manual(values = c("solo" = "#1db954", "collab" = "#1db954")) +  # Set green color
  labs(
    title = "Solo vs. Collaboration Songs",
    x = "Song Type",
    y = "Count",
    fill = "Song Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank()      # Remove panel border
  )





# Count the number of 'Major' and 'Minor' songs
mode_counts <- spotify_2023 %>%
  count(mode)

# Plotting the pie chart
ggplot(mode_counts, aes(x = "", y = n, fill = mode)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Major" = "#1db954", "Minor" = "#a50026")) +  # Custom colors
  theme_void() +  # Remove axis and grid lines
  labs(fill = "Mode") +  # Legend title
  ggtitle("Distribution of Major and Minor Songs") +  # Chart title
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "right",  # Adjust legend position
    legend.text = element_text(size = 14, color = "black"),  # Legend text appearance
    legend.title = element_text(size = 14, face = "bold", color = "black")  # Legend title appearance
  )






# Counting songs by key and mode
key_mode_counts <- spotify_2023 %>%
  count(key, mode) %>%
  arrange(key, mode)

# Plotting the stacked bar chart
ggplot(key_mode_counts, aes(x = key, y = n, fill = mode)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("#1db954", "darkgreen")) +  # Green for Major, Red for Minor
  labs(
    title = "Count of Songs by Key and Mode",
    x = "Key",
    y = "Count",
    fill = "Mode"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top"
  )














# Replace missing or blank key values with 'unknown'
spotify_2023$key[spotify_2023$key == "" | is.na(spotify_2023$key)] <- "unknown"

# Counting songs by key and mode
key_mode_counts <- spotify_2023 %>%
  count(key, mode) %>%
  arrange(key, mode)

# Sum counts by key to reorder in decreasing order
key_counts <- key_mode_counts %>%
  group_by(key) %>%
  summarize(total_count = sum(n)) %>%
  arrange(desc(total_count))

# Reorder key factor based on total count
key_mode_counts$key <- factor(key_mode_counts$key, levels = key_counts$key)

# Plotting the stacked bar chart
ggplot(key_mode_counts, aes(x = key, y = n, fill = mode)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("#1db954", "darkgreen")) +  # Green for Major, Dark Green for Minor
  labs(
    title = "Count of Songs by Key and Mode",
    x = "Key",
    y = "Count",
    fill = "Mode"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top"
  )







# Create a subset of relevant columns
data_subset <- spotify_2023[, c("energy_.", "danceability_.", "valence_.", "released_year", "released_month", "released_day")]

# Create scatter plot matrix
ggpairs(data_subset, 
        columns = c("energy_.", "danceability_.", "valence_.", "released_year", "released_month", "released_day"),
        lower = list(continuous = "points"),  # Display lower triangle as scatter plots
        diag = list(continuous = "barDiag"))  # Display diagonal as bar plots (for release date)


