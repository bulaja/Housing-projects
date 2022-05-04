library(tidyverse)
library(rsample)

df <- read.csv("00_data/Stanovi.csv")
# faktorisanje

cols <- c("Lodja", "Lift", "Terasa", "broj_soba")


df <- df %>% mutate_each_(funs(factor(.)),cols)


# Train/Test split ----
df_ml <- df %>% select(-cena)
set.seed(11)
split <- initial_split(df_ml, prop = 0.8, strata = cena_po_m2)
train_data <- split %>% training()
test_data <- split %>% testing()

