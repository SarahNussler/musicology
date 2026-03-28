library(tidyverse)
library(compmus)

american_idiot_original <- read_csv("Desktop/musicology/greenday1.csv")
american_idiot_anniversary <- read_csv("Desktop/musicology/greenday2.csv")

compmus_long_distance(
  american_idiot_original |> 
    compmus_wrangle_chroma() |> 
    mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |> 
    filter(row_number() %% 50L == 0L),
  american_idiot_anniversary |> 
    compmus_wrangle_chroma() |> 
    mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |> 
    filter(row_number() %% 50L == 0L),
  feature = pitches,
  method = "euclidean"
) |>
  filter(!is.nan(d)) |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_equal() +
  labs(x = "American Idiot, original version", y = "American Idiot, 20th anniversary edition") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)