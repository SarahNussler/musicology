library(tidyverse)

crisis <- read_csv("crisis-muziek.csv")
pre <- read_csv("pre-crisis_muziek.csv")

# Add period labels and combine
crisis <- crisis %>%
  mutate(period = "Crisis (2008–2010)")

pre <- pre %>%
  mutate(period = "Pre-crisis (2004–2007)")

songs <- bind_rows(pre, crisis)

songs <- songs %>%
  rename(
    track_name = `Track Name`,
    artist = `Artist Name(s)`,
    danceability = Danceability,
    energy = Energy,
    valence = Valence,
    tempo = Tempo,
    loudness = Loudness,
    speechiness = Speechiness,
    acousticness = Acousticness,
    liveness = Liveness
  ) %>%
  mutate(
    year = as.numeric(substr(`Release Date`, 1, 4))
  )

songs_long <- songs %>%
  select(track_name, artist, period,
         danceability, energy, valence, tempo,
         loudness, speechiness, acousticness, liveness) %>%
  pivot_longer(
    cols = c(danceability, energy, valence, tempo,
             loudness, speechiness, acousticness, liveness),
    names_to = "feature",
    values_to = "value"
  ) %>%
  mutate(
    feature = recode(feature,
                     danceability = "Danceability",
                     energy = "Energy",
                     valence = "Valence",
                     tempo = "Tempo",
                     loudness = "Loudness",
                     speechiness = "Speechiness",
                     acousticness = "Acousticness",
                     liveness = "Liveness"
    )
  )

ggplot(songs_long, aes(x = value, fill = period)) +
  geom_density(alpha = 0.45, color = NA) +
  facet_wrap(~ feature, scales = "free", ncol = 2) +
  labs(
    title = "Musical characteristics before and during the financial crisis",
    subtitle = "Comparing Spotify audio features (2004–2010)",
    x = NULL,
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )