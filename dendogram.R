library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}  

crisis_muziek <- read_csv("crisis-muziek.csv")

set.seed(123)

crisis_sample <- crisis_muziek |>
  slice_sample(n = 50) |>
  mutate(`Track Name` = make.unique(str_trunc(`Track Name`, 36)))

crisis_juice <-
  recipe(
    `Track Name` ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = crisis_sample
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  prep() |>
  juice() |>
  column_to_rownames("Track Name")

crisis_dist <- dist(crisis_juice, method = "euclidean")

crisis_dist |> 
  hclust(method = "complete") |> 
  dendro_data() |>
  ggdendrogram() +
  labs(title = "Music from the crisisperiod (2008–2011)") +
  theme(plot.title = element_text(hjust = 0.5))