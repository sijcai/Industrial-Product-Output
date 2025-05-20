
# Importing R packages ----------------------------------------------------
library(tidyverse)
library(missRanger)
library(readxl)
library(writexl)

# Missforest Interpolation Function --------------------------------------------------------------
interpolate_data <- function(train_data, test_data, unobserved_data, industry_name) {
  imputed_train <- missRanger(train_data, returnOOB = TRUE)
  oob_train <- attr(imputed_train, "oob")
  test_combined = rbind(imputed_train, test_data)
  imputed_test <- missRanger(test_combined, returnOOB = TRUE)
  oob_test <- attr(imputed_test, "oob")
  unobserved_combined <- rbind(imputed_test, unobserved_data)
  imputed_unobserved <- missRanger(unobserved_combined, returnOOB = TRUE)
  oob_unobserved <- attr(imputed_unobserved, "oob")
  oob_errors <- list(
    train_oob = oob_train,
    test_oob = oob_test,
    unobserved_oob = oob_unobserved
  )
  return(list(
    train_imp = train_imp,
    test_imp = test_imp,
    unobserved_imp = unobserved_imp,
    oob_errors = oob_errors  # 包含OOB误差的部分
  ))
}

# Apply the interpolation function ----------------------------------------
mineral_results <- interpolate_data(mineral_train, mineral_test, mineral_unobserved, "mineral")
chemical_results <- interpolate_data(chemical_train, chemical_test, chemical_unobserved, "chemical")
ferrous_metal_results <- interpolate_data(ferrous_metal_train, ferrous_metal_test, ferrous_metal_unobserved, "ferrous_metal")
nonferrous_metal_results <- interpolate_data(nonferrous_metal_train, nonferrous_metal_test, nonferrous_metal_unobserved, "nonferrous_metal")

