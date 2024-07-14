# Load the necessary library
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(corrplot)
library(broom)
library(car)
library(ggfortify)


# Read the data
spotify_songs <- read.csv("./spotify_songs.csv")


# Calculate the average track popularity
avg_popularity <- round(mean(spotify_songs$track_popularity), digits = 2) 

# Calculate the percentiles
p25 <- quantile(spotify_songs$track_popularity, 0.25)
p50 <- quantile(spotify_songs$track_popularity, 0.50)
p75 <- quantile(spotify_songs$track_popularity, 0.75)



# Plot the histogram
ggplot(spotify_songs, aes(x=track_popularity)) +
  geom_histogram(binwidth=5, breaks = seq(from = -0.1, to = 100.1, by = 5), right = TRUE, fill="#4E94C4", color="#e9ecef", alpha=0.9) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  # Add density
  geom_density(aes(y=5*..count..), size=0.5, color = "#4C6E62") +
  # Add data label
  stat_bin(binwidth = 5, geom = "text", aes(label = scales::comma(..count..)), vjust = -1, size = 4, breaks = seq(from = -0.1, to = 105, by = 5)) +
  # Add mean to graph
  geom_vline(aes(xintercept=avg_popularity), color="#4E94C4") +
  annotate("text", label = paste("Average popularity:", avg_popularity, sep = " "), x = avg_popularity-17, y  = 4000, size = 3, hjust = 0) +
  # Add percentile to graph
  geom_vline(aes(xintercept=p25), color="#4E94C4", linetype="dashed", size=0.5) +
  annotate("text", x=p25-1, y=3200, label="25 percentile", size = 3.5, angle=90) +
  geom_vline(aes(xintercept=p50), color="#4E94C4", linetype="dashed", size=0.5) +
  annotate("text", x=p50-1, y=3200, label="50 percentile", size = 3.5, angle=90) +
  geom_vline(aes(xintercept=p75), color="#4E94C4", linetype="dashed", size=0.5) +
  annotate("text", x=p75-1, y=3200, label="75 percentile", size = 3.5, angle=90) +
  labs(title="Track Popularity Distribution",
       x="Track Popularity",
       y="Count of Tracks") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) # Center the title



# Calculate the average track popularity by playlist genre
avg_popularity_by_genre <- spotify_songs %>%
  group_by(playlist_genre) %>%
  summarise(avg_popularity_by_genre = mean(track_popularity, na.rm = TRUE))

# Plot the histogram with data labels
ggplot(avg_popularity_by_genre, aes(x=playlist_genre, y=avg_popularity_by_genre, fill=playlist_genre)) +
  geom_bar(stat="identity", color="#e9ecef", alpha=0.9) +
  geom_text(aes(label=round(avg_popularity_by_genre, 2)), vjust=-0.5, color="black", size=5) +
  scale_fill_brewer(palette="Spectral") +
  labs(title="Histogram of Average Track Popularity by Playlist Genre",
       x="",
       y="Average Track Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) # Center the title


# Extract the year from the release date
spotify_songs$year <- year(as.Date(spotify_songs$track_album_release_date))


# Calculate the average popularity by genre and year
avg_popularity_by_genre_and_year <- spotify_songs %>%
  group_by(playlist_genre, year) %>%
  summarise(avg_popularity_by_genre_and_year = mean(track_popularity, na.rm = TRUE))

# Define the colors for each genre
colors <- c("edm" = "#D95463", "latin" = "#FC996D", "pop" = "#FFE398", "r&b" = "#E8F6A3", "rap" = "#A2D89D", "rock" = "#4E94C4")

# Plot the stacked area chart
ggplot(avg_popularity_by_genre_and_year, aes(x=year, y=avg_popularity_by_genre_and_year, fill=playlist_genre)) +
  geom_area() +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(from = 1960, to = 2020, by = 5)) +
  labs(title="Average Popularity of Each Genre Over Years",
       x="",
       y="Stacked Genre Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) # Center the title

# Calculate correlations
correlations <- cor(spotify_songs[,c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')], use="complete.obs")

# View the correlations with 'danceability'
correlations['danceability',]


# Create a correlation matrix plot
corrplot(correlations, method = "color", type = "upper",  
         addCoef.col = "black", # Add correlation coefficients on the heatmap
         tl.col = "black", # Text label color
         tl.srt = 45, # Text label rotation
         diag = FALSE) # Don't show self-correlation



model1 <- lm(danceability ~ energy, data = spotify_songs)
model2 <- lm(danceability ~ key, data = spotify_songs)
model3 <- lm(danceability ~ loudness, data = spotify_songs)
model4 <- lm(danceability ~ mode, data = spotify_songs)
model5 <- lm(danceability ~ speechiness, data = spotify_songs)
model6 <- lm(danceability ~ acousticness, data = spotify_songs)
model7 <- lm(danceability ~ instrumentalness, data = spotify_songs)
model8 <- lm(danceability ~ liveness, data = spotify_songs)
model9 <- lm(danceability ~ valence, data = spotify_songs)
model10 <- lm(danceability ~ tempo, data = spotify_songs)
model11 <- lm(danceability ~ duration_ms, data = spotify_songs)




popularity_predict <- lm(track_popularity~danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+factor(playlist_genre), data = spotify_songs)
# Extract coefficients from the model summary
coef_df <- as.data.frame(summary(popularity_predict)$coefficients)
# Sort the coefficients by the absolute value of the Estimate column in descending order
sorted_coef <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]
# Get the top 3 coefficients(beside  Intercept)
print(head(sorted_coef, 4))

# Print the top 3 coefficients
print(top_3_coef)


plot(popularity_predict)

# extract fitted values and residuals
fitted.values <- fitted(popularity_predict)
residuals <- resid(popularity_predict)

# create a data frame
df <- data.frame(fitted.values, residuals)

# create the plot
ggplot(df, aes(x=fitted.values, y=residuals)) +
  geom_point(alpha=0.5, aes(col=fitted.values)) + # set color of points
  geom_smooth(se=FALSE, color="red", method="loess") + # add a loess smoothed line
  theme_minimal() + # set theme
  labs(x="Fitted values", y="Residuals", title="Residuals vs Fitted values") # add labels

# extract residuals
residuals <- resid(popularity_predict)

# create a data frame
df <- data.frame(residuals)

# create the Q-Q plot
ggplot(df, aes(sample=residuals)) +
  stat_qq(colour="blue") + # set color of points
  stat_qq_line(colour="red") + # set color of line
  theme_minimal() + # set theme
  labs(title="Q-Q Plot of Residuals", x="Theoretical Quantiles", y="Sample Quantiles") # add labels

# calculate VIF
vif_values <- vif(popularity_predict)

# calculate leverage
influence_measures <- influence.measures(popularity_predict)
leverage <- influence_measures$hat

# calculate standardized residuals
standardized_residuals <- rstandard(popularity_predict)

# create a data frame
df <- data.frame(standardized_residuals, leverage)

# create the plot
ggplot(df, aes(x=leverage, y=standardized_residuals, color=leverage)) +
  geom_point(alpha=0.5) + # set transparency of points
  theme_minimal() + # set theme
  labs(x="Leverage", y="Standardized Residuals", color="Leverage", title="Residuals vs Leverage") # add labels



