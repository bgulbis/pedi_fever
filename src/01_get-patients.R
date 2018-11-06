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

# vals <- list(
#         "order.id" = "Order Id",
#         "order.datetime" = "Date and Time - Original (Placed)",
#         "order" = "Mnemonic (Primary Generic) FILTER ON",
#         "route" = "Order Route",
#         "freq" = "Frequency",
#         "prn" = "PRN Indicator",
#         "order.provider" = "Ordering Provider LIMITS",
#         "order.provider.position" = "Ordering Provider Position LIMITS",
#         "not.in.list" = "Not In List"
#     )
# 
# y <- vals %in% colnames(orders)
# t <- rename(orders, !!!vals[y]) 
    
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

set.seed(77123)
include <- overlap %>%
    anti_join(excl_icu, by = "millennium.id") %>%
    anti_join(excl_sched, by = "millennium.id") %>%
    left_join(pts, by = "millennium.id") %>%
    filter(age < 18) %>%
    distinct() %>%
    sample_n(300)

mbo_include <- concat_encounters(include$millennium.id)
mbo_include
# x <- distinct(include, millennium.id)

# 04_demographics --------------------------------------

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

# 05_labs ----------------------------------------------

labs <- read_data(dir_raw, "labs", FALSE) %>%
    as.labs() %>%
    tidy_data() %>%
    filter(
        !is.na(lab.result),
        lab %in% c(
            "ast",
            "alt",
            "bili total",
            "bili direct",
            "creatinine lvl",
            "bun",
            "egfr"
        )
    )

overlap_include <- include %>%
    semi_join(demog, by = "millennium.id") %>%
    rowwise() %>%
    mutate(
        overlap.start = max(apap.start, ibup.start),
        overlap.stop = min(apap.stop, ibup.stop)
    ) %>%
    select(millennium.id, overlap.start, overlap.stop)

labs_range <- labs %>%
    left_join(overlap_include, by = "millennium.id") %>%
    filter(
        lab.datetime >= overlap.start - days(1),
        lab.datetime <= overlap.stop
    ) %>%
    select(millennium.id:lab.result, overlap.start) %>%
    distinct() %>%
    spread(lab, lab.result)
                
# 06_medications ---------------------------------------

meds <- read_data(dir_raw, "medications", FALSE) %>%
    as.meds_inpt() %>%
    mutate(orig.order.id = order.parent.id) %>%
    mutate_at("orig.order.id", na_if, y = 0) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id)))

meds_prn <- meds %>%
    rename(route.admin = route) %>%
    inner_join(
        orders, 
        by = c("millennium.id", "orig.order.id" = "order.id")
    )

# 07_measures ------------------------------------------

measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE)

# 08_temperatures --------------------------------------

temps <- read_data(dir_raw, "temp", FALSE) %>%
    as.vitals() %>%
    left_join(overlap_include, by = "millennium.id") %>%
    filter(
        vital.datetime >= overlap.start,
        vital.datetime <= overlap.stop + days(5)
    ) 

# urine_output -----------------------------------------

# run EDW query
#   * Identifiers - by Millennium Encounter ID

id <- read_data(dir_raw, "identifiers") %>%
    as.id()

edw_id <- concat_encounters(id$pie.id)
edw_id

# run EDW query
#   * Urine Output

uop <- read_data(dir_raw, "uop") %>%
    as.uop() %>%
    left_join(id[c("pie.id", "millennium.id")], by = "pie.id") %>%
    left_join(overlap_include, by = "millennium.id") %>%
    filter(
        uop.datetime >= overlap.start - days(1),
        uop.datetime <= overlap.stop
    ) %>%
    select(millennium.id, everything(), -pie.id) %>%
    filter(uop != "Urine Count")
