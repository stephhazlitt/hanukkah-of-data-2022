library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(phonenumber)
library(lubridate)


## PUZZLE 0
## They still have the same Manhattan storefront, and they’re still running on
## the same database your cousin Alex set up at the start of 2017.
## “Alex set up the backups to be password-protected. I can never remember the password itself,
## but it’s just the year in the Hebrew calendar when Alex set up the database.”
## What’s the password to open the .zip files on the USB drive?
## 5777


##USB DRIVE
customers_raw <- read_csv("data/noahs-csv/noahs-customers.csv") |>
  distinct()
orders_raw <- read_csv("data/noahs-csv/noahs-orders.csv")
orders_items_raw <-
  read_csv("data/noahs-csv/noahs-orders_items.csv")
products_raw <- read_csv("data/noahs-csv/noahs-products.csv")


## PUZZLE 1
customers_raw |>
  mutate(
    lastname = str_remove_all(name, " Jr."),
    lastname = str_remove_all(lastname, " III"),
    lastname = str_remove_all(lastname, " II"),
    lastname = str_remove_all(lastname, " IV"),
    lastname = str_split_i(lastname, " ",-1),
    lastname_length = str_count(lastname)
  ) |>
  filter(lastname_length == 10) |>
  mutate(number = lapply(lastname, letterToNumber),
         phone_raw = str_remove_all(phone, "-")) |>
  filter(phone_raw == number) |>
  pull(phone)
## [1] "488-836-2374"

## PUZZLE 2
orders_raw |>
  left_join(customers_raw, by = "customerid") |>
  mutate(year = year(ordered)) |>
  filter(year == 2017) |>
  mutate(
    tidyname = str_remove_all(name, " Jr."),
    tidyname = str_remove_all(tidyname, " III"),
    tidyname = str_remove_all(tidyname, " II"),
    tidyname = str_remove_all(tidyname, " IV"),
    lastname = str_split_i(tidyname, " ", -1),
    firstname = str_split_i(tidyname, " ", 1),
    firstinitial = str_extract(firstname, "^.{1}"),
    lastinitial = str_extract(lastname, "^.{1}"),
    initials = str_c(firstinitial, lastinitial)
  ) |>
  filter(initials == "JD") |>
  left_join(orders_items_raw, by = "orderid") |>
  left_join(products_raw, by = "sku") |>
  filter(desc == "Coffee, Drip") |>
  pull(phone)
## [1] "212-771-8924"

