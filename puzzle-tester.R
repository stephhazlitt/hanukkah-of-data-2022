library(DBI)
library(dplyr)
library(elevatr)
library(lubridate)
library(stringr)
library(tidyr)

noahsmarket <- dbConnect(RSQLite::SQLite(), "data/noahsmarket.db")
dbListTables(noahsmarket)

## A. Sarah wants to call the oldest member of the Mile-High Club.
## What’s their phone number?

customers_df <- tbl(noahsmarket, "customers") |> collect()

#get USGS elevation data using elevatr
elevation <- customers_df |>
  select(x = long, y = lat, customerid) |>
  data.frame()

prj_dd <- "EPSG:4326"

customers_elev <- get_elev_point(elevation, prj = prj_dd, src = "epqs")

#join elevation data to customers table
customers_elev_df <- data.frame(customers_elev) |>
  left_join(customers, by = "customerid") |>
  mutate(birthday = as_date(birthdate))

#find phone # of oldest customer living 1 mile (1609m) or higher
customers_elev_df |>
  filter(elevation >= 1609) |>
  filter(birthday == min(birthday)) |>
  pull(phone)
# [1] "970-290-3653"

## B. It costs $10 to ship anything smaller than 100 cubic inches (plus the
## cost of the item itself of course). How much will it cost to send a dreidel
## to every member of the Mile-High Club?

#find # of customers living 1 mile (1609m) or higher

#all addresses
count_highmile_all <- customers_elev_df |>
  filter(elevation >= 1500) |>
  count() |>
  pull()
#[1] 77

#rm dupe addresses
count_highmile <- customers_elev_df |>
  filter(elevation >= 1609) |>
  distinct(address) |>
  count() |>
  pull()
#[1] 64

#inventory of dreidles
inventory_df <- tbl(noahsmarket, "inventory") |> collect()
orderitems_df <- tbl(noahsmarket, "orderitems") |>
  collect() |>
  left_join(products, by ="sku")
orders_df <- tbl(noahsmarket, "orders") |> collect()

#cost of a dreidle + shipping
products_df <- tbl(noahsmarket, "products") |>
  collect() |>
  left_join(inventory_df, by = "sku")

#length(cm) × width(cm) × height(cm) ÷ 16.387064 = cubic inches
dreidles <- products_df |>
  filter(str_detect(desc, "Dreidel")) |>
  separate(dims_cm, c("l", "w", "h"), sep = "x") |>
  mutate(cubcm = as.numeric(l) * as.numeric(w) * as.numeric(h),
         cubinch = cubcm / 16.387064) |>
  filter(cubinch < 100) |>
  filter(cost == min(cost)) |>
  pull(cost)

foo <- (dreidles * count_highmile_all) + (10 * count_highmile_all)
foo

count_one <- dreidles |>
  filter(cum_total <= count_highmile) |>
  summarise(sum(total_avail)) |>
  pull()

set_one <- dreidles |>
  filter(cum_total <= count_highmile) |>
  mutate(total_cost = unit_ship_cost * total_avail) |>
  summarise(total_cost = sum(total_cost)) |>
  pull(total_cost)

count_two <- count_highmile - count_one

set_two <- dreidles |>
  filter(cum_total > count_highmile) |>
  slice_head(n = 1) |>
  mutate(total_cost = unit_ship_cost * count_two) |>
  pull(total_cost)

final_cost <- set_one + set_two





