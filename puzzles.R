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
## Rug given to cleaner 2017-04-05 12:49:41


## PUZZLE 3
#Aries == March 21 – April 19
#Year of the Dog ==  c(2018, 2006, 1994, 1982, 1970, 1958, 1946)
#Neighbourhood == 134-10 Foch Blvd, South Ozone Park, NY 11420

dog <- c(2006, 1994, 1982, 1970, 1958, 1946, 1934)

customers_raw |>
  mutate(year = year(birthdate),
         month = month(birthdate),
         day = day(birthdate),
         monthday = format(birthdate, "%m-%d"),
         dogyear = case_when(year %in% dog ~ TRUE,
                             TRUE ~ FALSE)) |>
  filter(dogyear == TRUE,
         month %in% c(3,4),
         monthday > "03-20",
         monthday < "04-20") |>
  filter(str_detect(citystatezip, "South Ozone Park")) |>
  pull(phone)
## [1] "516-636-7397"


## PUZZLE 4
orders_raw |>
  left_join(orders_items_raw, by = "orderid") |>
  left_join(products_raw, by = "sku") |>
  filter(ordered == shipped) |>
  filter(str_detect(desc, "Puff")) |>
  left_join(customers_raw, by = "customerid") |>
  mutate(ordertime = hour(ordered),
         year = year(ordered)) |>
  filter(ordertime == 4) |>
  pull(phone)
## [1] "718-649-9036"


## PUZZLE 5
orders_raw |>
  left_join(orders_items_raw, by = "orderid") |>
  left_join(products_raw, by = "sku") |>
  left_join(customers_raw, by = "customerid") |>
  filter(str_detect(desc, "Cat Food")) |>
  filter(str_detect(citystatezip, "Queens Village")) |>
  filter(name == "Anita Koch") |>
  slice_head() |>
  pull(phone)
## [1] "315-492-7411"


## PUZZLE 6
orders_items_raw |>
  left_join(products_raw, by = "sku") |>
  mutate(cost_price = wholesale_cost * qty,
         sell_price = unit_price * qty) |>
  group_by(orderid) |>
  summarise(total_cost = sum(cost_price),
            total_sell = sum(sell_price)) |>
  ungroup() |>
  mutate(profit = total_sell - total_cost) |>
  filter(profit < 0 ) |>
  left_join(orders_raw, by = "orderid") |>
  left_join(customers_raw, by = "customerid") |>
  group_by(name, phone) |>
  count() |>
  ungroup() |>
  filter(n == max(n)) |>
  pull(phone)
## [1] "914-868-0316"


## PUZZLE 7
emily <- orders_raw |>
  left_join(orders_items_raw, by = "orderid") |>
  left_join(products_raw, by = "sku") |>
  left_join(customers_raw, by = "customerid") |>
  filter(name == "Emily Randolph") |>
  filter(str_detect(desc, ".\\)")) |>
  mutate(date = as_date(shipped),
         item = str_remove(desc, "\\(.*" ))

emily_dates <- emily |> pull(date)
emily_items <- emily |> pull(item)

boyfriend <- orders_raw |>
  left_join(orders_items_raw, by = "orderid") |>
  left_join(products_raw, by = "sku") |>
  left_join(customers_raw, by = "customerid") |>
  mutate(date = as_date(shipped),
         item = str_remove(desc, "\\(.*" )) |>
  filter(date %in% emily_dates,
         item %in% emily_items,
         name != "Emily Randolph")

emily |>
  left_join(boyfriend, by = c("date", "item")) |>
  select(date, item, desc.x, desc.y, birthdate.x, name.x, name.y, birthdate.y, phone.y) |>
  mutate(age_diff_years = year(birthdate.x) - year(birthdate.y)) |>
  filter(name.y == "Jonathan Adams") |> #date, item and age diff all reasonable
  pull(phone.y)
## [1] "315-618-5263"


## PUZZLE 8
collectables <- orders_raw |>
  left_join(orders_items_raw, by = "orderid") |>
  left_join(products_raw, by = "sku") |>
  filter(str_detect(desc, "^Noah's")) |>
  mutate(items = str_remove(desc, "\\(.*" )) |>
  distinct(desc) |>
  pull(desc)

orders_raw |>
  left_join(orders_items_raw, by = "orderid") |>
  left_join(products_raw, by = "sku") |>
  left_join(customers_raw, by = "customerid") |>
  group_by(customerid, name, phone) |>
  filter(desc %in% collectables) |>
  count() |>
  ungroup() |>
  filter(n == max(n)) |>
  pull(phone)
## [1] "929-906-5980"


