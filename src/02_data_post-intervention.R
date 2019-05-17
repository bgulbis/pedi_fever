library(tidyverse)
library(lubridate)
library(edwr)
library(openxlsx)

dir_raw <- "data/raw/post"
tz <- "US/Central"

dirr::gzip_files(dir_raw)

# MBO query folder: Resident Projects/pedi_fever

# 01_find-patients -------------------------------------
#   * Facility (Curr): HC Childrens
#   * Date and Time - Admit: 3/1/2019 - 5/7/2019
#   * Mnemonic (Primary Generic) FILTER ON: acetaminophen;ibuprofen
#   * Date and Time - Original (Placed): 3/1/2019 - 5/7/2019

pts <- read_data2(dir_raw, "find-patients", FALSE) 
    # as.patients() %>%
    # filter(discharge.datetime <= mdy("12/31/2017", tz = tz))

mbo_pts <- concat_encounters(pts$millennium.id)

# 02_order-details -------------------------------------
#   * Mnemonic (Primary Generic) FILTER ON: acetaminophen;ibuprofen

orders <- read_data2(dir_raw, "order-details", FALSE) %>%
    filter(
        prn == "PRN",
        !is.na(order.dc.datetime)
    ) 

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

locations <- read_data2(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    filter(
        depart.datetime <= today(tzone = tz),
        arrive.datetime != depart.datetime
    ) %>%
    tidy_data() %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, arrive.datetime) %>%
    mutate(wrong.depart = depart.datetime != lead(arrive.datetime))

icu <- locations %>%
    filter(
        location %in% c(
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

sched_meds <- read_data2(dir_raw, "order-details", FALSE) %>%
    filter(
        prn != "PRN",
        !is.na(order.dc.datetime)
    ) 

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

# set.seed(77123)
include <- overlap %>%
    anti_join(excl_icu, by = "millennium.id") %>%
    anti_join(excl_sched, by = "millennium.id") %>%
    left_join(pts, by = "millennium.id") %>%
    filter(age < 18) %>%
    distinct() 
    # sample_n(300)

mbo_include <- concat_encounters(include$millennium.id)
mbo_include
# x <- distinct(include, millennium.id)

# 04_demographics --------------------------------------

demog <- read_data2(dir_raw, "demographics", FALSE) 

# 05_labs ----------------------------------------------

mbo_labs <- concat_encounters(include$millennium.id, 250)
mbo_labs

labs <- read_data2(dir_raw, "labs", FALSE) %>%
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
    select(millennium.id, overlap.start, overlap.stop) %>%
    distinct(millennium.id, .keep_all = TRUE)

labs_range <- labs %>%
    left_join(overlap_include, by = "millennium.id") %>%
    filter(
        lab.datetime >= overlap.start - days(1),
        lab.datetime <= overlap.stop
        # lab.datetime <= overlap.stop
    ) %>%
    select(millennium.id:lab.result, overlap.start) %>%
    distinct(millennium.id, lab.datetime, lab, .keep_all = TRUE) %>%
    spread(lab, lab.result)
                
# 06_medications ---------------------------------------

meds <- read_data2(dir_raw, "medications", FALSE) %>%
    # as.meds_inpt() %>%
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

measures <- read_data2(dir_raw, "measures", FALSE) %>%
    arrange(millennium.id, event, desc(event.datetime)) %>%
    distinct(millennium.id, event, .keep_all = TRUE) %>%
    select(millennium.id, event, event.result) %>%
    spread(event, event.result)
    
# 08_temperatures --------------------------------------

temps <- read_data2(dir_raw, "temp", FALSE) %>%
    # as.vitals() %>%
    left_join(overlap_include, by = "millennium.id") %>%
    filter(
        event.datetime >= overlap.start,
        event.datetime <= overlap.stop + days(5)
    ) 

# urine_output -----------------------------------------

# run EDW query
#   * Identifiers - by Millennium Encounter ID

id <- read_data2(dir_raw, "identifiers") 
    # as.id()

edw_id <- concat_encounters(id$pie.id)
edw_id

# run EDW query
#   * Urine Output

uop <- read_data2(dir_raw, "uop") %>%
    left_join(id[c("pie.id", "millennium.id")], by = "pie.id") %>%
    left_join(overlap_include, by = "millennium.id") %>%
    filter(
        event.datetime >= overlap.start - days(1),
        event.datetime <= overlap.stop
    ) %>%
    select(millennium.id, everything(), -pie.id) %>%
    filter(event != "Urine Count")

# location ---------------------------------------------

order_location <- locations %>%
    left_join(overlap_include, by = "millennium.id") %>%
    filter(
        overlap.start >= arrive.datetime,
        overlap.start <= depart.datetime,
        (!wrong.depart | is.na(wrong.depart))
    ) %>%
    select(millennium.id, location) %>%
    ungroup()

# order comments ---------------------------------------

orders_prn <- orders %>%
    semi_join(include, by = "millennium.id") %>%
    anti_join(sched_meds, by = c("millennium.id", "order.id"))

mbo_orders <- concat_encounters(orders_prn$order.id) 
mbo_orders

orders_comments <- read_data2(dir_raw, "comments") %>%
    rename(order.comment = `Order Comment`)
    
df <- orders_comments %>%
    count(order.comment) %>%
    filter(n >= 10)

df_comments <- orders_comments %>%
    anti_join(df, by = "order.comment")

# select patients --------------------------------------

set.seed(77123)
pts_labs <- labs_range %>%
    # full_join(demog["millennium.id"], by = "millennium.id") %>%
    # inner_join(id, by = "millennium.id") %>%
    # select(fin, lab.datetime, alt:egfr) %>%
    filter(
        !is.na(ast),
        !is.na(`bili total`),
        !is.na(`creatinine lvl`)
    ) %>%
    distinct(millennium.id) %>%
    semi_join(demog, by = "millennium.id") %>%
    semi_join(id, by = "millennium.id") %>%
    semi_join(order_location, by = "millennium.id") %>%
    semi_join(meds_prn, by = "millennium.id") %>%
    semi_join(temps, by = "millennium.id") %>%
    semi_join(uop, by = "millennium.id") 
    # sample_n(300)

data_demographics <- demog %>%
    semi_join(pts_labs, by = "millennium.id") %>%
    inner_join(id, by = "millennium.id") %>%
    inner_join(order_location, by = "millennium.id") %>%
    left_join(measures, by = "millennium.id") %>%
    select(fin, age, gender, location, height, weight)

data_labs <- labs_range %>%
    semi_join(pts_labs, by = "millennium.id") %>%
    inner_join(id, by = "millennium.id") %>%
    arrange(millennium.id, lab.datetime) %>%
    select(fin, lab.datetime, alt:egfr) 

data_meds_admin <- meds_prn %>%
    semi_join(pts_labs, by = "millennium.id") %>%
    inner_join(id, by = "millennium.id") %>%
    arrange(millennium.id, med.datetime, med) %>%
    select(
        fin, 
        med.datetime, 
        med, 
        med.dose,
        med.dose.units, 
        route.admin, 
        freq, 
        prn, 
        med.location
    )

data_temps <- temps %>%
    semi_join(pts_labs, by = "millennium.id") %>%
    inner_join(id, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime) %>%
    select(
        fin, 
        vital.datetime = event.datetime, 
        vital = event, 
        vital.result = event.result
    )

data_uop <- uop %>%
    semi_join(pts_labs, by = "millennium.id") %>%
    inner_join(id, by = "millennium.id") %>%
    arrange(millennium.id, event.datetime) %>%
    select(
        fin, 
        uop.datetime = event.datetime, 
        uop = event, 
        uop.result = event.result
    )

data_comments <- df_comments %>%
    inner_join(orders[c("millennium.id", "order.id", "order")], by = "order.id") %>%
    semi_join(pts_labs, by = "millennium.id") %>%
    inner_join(id, by = "millennium.id") %>%
    select(fin, order.id, order, order.comment)

data_list <- list(
    "demographics" = data_demographics,
    "labs" = data_labs,
    "meds" = data_meds_admin,
    "comments" = data_comments,
    "temps" = data_temps,
    "uop" = data_uop
)

file = "data/external/pedi_antipyretic_data_post.xlsx"
write.xlsx(data_list, file)
