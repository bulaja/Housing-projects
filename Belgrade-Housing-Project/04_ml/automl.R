library(h2o)
src("01_functions/load_data.R")


# Initialize H2O JVM
h2o.init()
# Split data into Train/Validation/Test Sets
df_h2o <- as.h2o(df_ml)

split_h2o <- h2o.splitFrame(df_h2o, c(0.7, 0.15), seed = 12 )

train_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 70%
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 15%
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 15%

# Set names for h2o
y <- "cena_po_m2"
x <- setdiff(names(train_h2o), y)

# Run the automated machine learning 
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = valid_h2o,
  max_runtime_secs  = 300
)

leaderboard <- h2o.get_leaderboard(automl_models_h2o)
leaderboard