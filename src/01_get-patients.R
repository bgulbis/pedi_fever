library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"
tz <- "US/Central"

dirr::gzip_files()

# MBO query folder: Resident Projects/pedi_fever

# 01_find-patients -------------------------------------
#   * Facility (Curr): HC Childrens
#   * Date and Time - Admit: 1/1/2016 - 12/31/2017
#   * Mnemonic (Primary Generic) FILTER ON: acetaminophen;ibuprofen
#   * Date and Time - Original (Placed): 1/1/2016 - 12/31/2017

pts <- read_data(dir_raw, "find-patients", FALSE) %>%
    as.patients() %>%
    filter(discharge.datetime <= mdy("12/31/2017", tz = tz))

mbo_pts <- concat_encounters(pts$millennium.id)

# 02_order-details -------------------------------------
#   * Mnemonic (Primary Generic) FILTER ON: acetaminophen;ibuprofen

orders <- read_data(dir_raw, "order-details", FALSE) %>%
    as.order_detail(
        extras = c("order.dc.datetime" = "Date and Time - Discontinue Effective")
    ) %>%
    filter(
        prn == "PRN",
        !is.na(order.dc.datetime)
    ) %>%
    format_dates("order.dc.datetime")

apap <- orders %>%
    filter(order == "acetaminophen") %>%
    select(
        millennium.id,
        apap = order,
        apap.start = order.datetime,
        apap.stop = order.dc.datetime
    ) 

ibup <- orders %>%
    filter(order == "ibuprofen") %>%
    select(
        millennium.id,
        ibup = order,
        ibup.start = order.datetime,
        ibup.stop = order.dc.datetime
    ) 

overlap <- apap %>%
    inner_join(ibup, by = "millennium.id") %>%
    mutate(
        apap.interval = interval(apap.start, apap.stop),
        ibup.interval = interval(ibup.start, ibup.stop),
        overlap = int_overlaps(apap.interval, ibup.interval)
    ) %>%
    select(
        -apap.interval,
        -ibup.interval
    ) %>%
    filter(overlap) 

mbo_overlap <- concat_encounters(overlap$millennium.id)

# 03_locations -----------------------------------------

locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations()

icu <- locations %>%
    filter(
        unit.name %in% c(
            "HC A8N4",
            "HC A8NH",
            "HC NICE",
            "HC NICW",
            "HC PICU"
        )
    )

excl_icu <- overlap %>%
    inner_join(icu, by = "millennium.id") %>%
    mutate(
        apap.interval = interval(apap.start, apap.stop),
        ibup.interval = interval(ibup.start, ibup.stop),
        icu.interval = interval(arrive.datetime, depart.datetime),
        apap.overlap = int_overlaps(apap.interval, icu.interval),
        ibup.overlap = int_overlaps(ibup.interval, icu.interval),
        icu.overlap = apap.overlap | ibup.overlap
    ) %>%
    select(-contains("interval")) %>%
    filter(icu.overlap) 

# exclude scheduled doses ------------------------------

sched_meds <- read_data(dir_raw, "order-details", FALSE) %>%
    as.order_detail(
        extras = c("order.dc.datetime" = "Date and Time - Discontinue Effective")
    ) %>%
    filter(
        prn != "PRN",
        !is.na(order.dc.datetime)
    ) %>%
    format_dates("order.dc.datetime")

sched_apap <- sched_meds %>%
    filter(order == "acetaminophen") %>%
    select(
        millennium.id,
        apap = order,
        apap.start = order.datetime,
        apap.stop = order.dc.datetime
    ) 

sched_ibup <- sched_meds %>%
    filter(order == "ibuprofen") %>%
    select(
        millennium.id,
        ibup = order,
        ibup.start = order.datetime,
        ibup.stop = order.dc.datetime
    ) 

excl_sched <- sched_apap %>%
    inner_join(sched_ibup, by = "millennium.id") %>%
    mutate(
        apap.interval = interval(apap.start, apap.stop),
        ibup.interval = interval(ibup.start, ibup.stop),
        overlap = int_overlaps(apap.interval, ibup.interval)
    ) %>%
    select(
        -apap.interval,
        -ibup.interval
    ) %>%
    filter(overlap) 

# include patients -------------------------------------

include <- overlap %>%
    anti_join(excl_icu, by = "millennium.id") %>%
    anti_join(excl_sched, by = "millennium.id") %>%
    left_join(pts, by = "millennium.id") %>%
    filter(age < 18) %>%
    distinct()

mbo_include <- concat_encounters(include$millennium.id)

# x <- distinct(include, millennium.id)
