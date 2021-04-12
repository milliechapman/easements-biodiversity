library(tidyverse)
library(sf)

fee_exp <- st_read("data/GEE/fee_valid.shp") %>%
  select(Date_Est, GAP_Sts, d_State_Nm, State_Nm, Category, Mang_Type)  %>%
  mutate(num = seq(1:178936))

fee_exp1 <- fee_exp %>%
  dplyr::mutate(valid = st_is_valid(fee_exp)) %>%
  dplyr::filter(valid == "TRUE")

st_write(fee_exp1, "data/GEE/feeexp.shp", append = TRUE)


fee_exp1 <- st_read("data/GEE/feeexp.shp") %>%
  mutate(num = seq(1:178936))

st_write(fee_exp1, "data/GEE/feeexpnum.shp", append = TRUE)
