`%>%` <- magrittr::`%>%`

source("R/ats_auth.R")
source("R/fetch_ats.R")

u <- "mary"
p <- "."

ats_login(u, p)

all_collars <- fetch_ats_devices()

length(all_collars)

some_collars <- sample(all_collars, 5)

length(some_collars)

all_trans <- fetch_ats_transmissions()
some_trans <- fetch_ats_transmissions(some_collars)

nrow(all_trans)
nrow(some_trans)

all_pos <- fetch_ats_positions()
some_pos <- fetch_ats_positions(some_collars)

nrow(all_pos)
nrow(some_pos)

new_trans <- fetch_ats_transmissions(new = TRUE)
new_pos <- fetch_ats_positions(new = TRUE)

events <- fetch_ats_events()

conf <- fetch_ats_config()

ats_logout()

# TODO enforce structure for events, config

# Run examples
# 2.1

ats_login("mary", ".")

ats_get(
 path = list(
    "download_all_transmission",
    "download_all_transmission.aspx?dw=all"
 ),
 task = "download transmission data"
) %>%
 ats_parse_trans()

#2.2
#' ats_login("mary", ".")

trans <- fetch_ats_transmissions()

ats_get(
  path = list(
    "download_all_data",
    paste0("Download_all_data.aspx?dw=all")
  ),
  task = "download position data"
) %>%
 httr::content("text", encoding = "UTF-8") %>%
    readr::read_csv(col_types = "ciiiiiidddiic_") %>%
    dplyr::rename(JulianDay = Julianday) %>%
    ats_join_trans(trans)

# 2.3
#' ats_login("mary", ".")
#'
#' trans <- fetch_ats_transmissions()

ats_get(
  path = list(
    "download_all_data",
    paste0("Download_all_data.aspx?dw=all")
  ),
  task = "download position data"
) %>%
  ats_parse_pos(trans)

# 2.4
#' ats_login("mary", ".")
trans <- fetch_ats_transmissions()

ats_get(
 path = list(
   "download_all_transmission",
   "download_all_transmission.aspx?dw=all"
 ),
 task = "download fix data"
) %>%
 ats_parse_trans()

# 2.5
ats_get(
  path = list(
    "download_all_events",
    "download_all_events.aspx?dw=all"
  ),
  task = "download event data"
) %>%
  ats_parse_txt()

# 2.6
ats_select_collars("044286")

ats_post(
  path = "Servidor.ashx",
  body = list(
    consulta = "download_trans_collars"
  ),
  task = "download transmission data"
) %>%
  ats_parse_xml()

# 2.7
ats_post(
  path = "Servidor.ashx",
  body = list(
    consulta = "download_trans_collars"
  ),
  task = "download transmission data"
) %>%
  ats_parse_xml()

# 2.8
#' ats_login("mary", ".")

all_collars <- fetch_ats_devices()
some_collars <- sample(all_collars, 5)
ats_select_collars(some_collars)

# 2.9
ats_select_collars("044286")

ats_post(
  path = "Servidor.ashx",
  body = list(
    consulta = "download_trans_collars"
  ),
  task = "download transmission data"
) %>%
  ats_parse_xml() %>%
  dplyr::mutate(
    DateCT = as.POSIXct(
      fecha,
      tz = "America/Menominee"
    ),
    GmtOffset = as.numeric(gmt) * -1
  ) %>%
  ats_trans_dates()

# 3.1
collar_details <- fetch_ats_config()

# 3.2
# get ids for all collars in this account
collar_list <- fetch_ats_devices()

# get ids for collars active collars
collar_list <- fetch_ats_devices("Active")

# get ids for collars inactive collars
collar_list <- fetch_ats_devices("Inactive")

# get ids for collars with low battery
collar_list <- fetch_ats_devices("Low_batt")

# get ids for collars in mortality
collar_list <- fetch_ats_devices("Mort")

# get ids for collars with birth events triggered
collar_list <- fetch_ats_devices("Birth")

# 3.3
alerts <- fetch_ats_events()

# 3.4
# get undownloaded fixes for all collars in this account
fixes <- fetch_ats_positions(new = TRUE)

# get all fixes for all collars in this account
fixes <- fetch_ats_positions()

# get all fixes for specific collars
collar_list <- sample(fetch_ats_devices(), 10)
fixes <- fetch_ats_positions(collar_list)

# get all fixes for collars in mortality
collar_list <- fetch_ats_devices("mortality")
fixes <- fetch_ats_positions(device_id = collar_list)

# get last 10 fixes for certain collars
fixes <- fetch_ats_positions(device_id = collar_list, n = 10)

# get fixes in 2019 for all collars
fixes <- fetch_ats_positions(
  start = as.POSIXct("2019-01-01"),
  end = as.POSIXct("2020-01-01")
)

# get fixes in 2019 for certain collars
fixes <- fetch_ats_positions(
  device_id = collar_list,
  start = as.POSIXct("2019-01-01"),
  end = as.POSIXct("2020-01-01")
)

# get undownloaded fixes for a single collar
fixes <- fetch_ats_positions("044286", new = TRUE)

# 3.5
# get undownloaded transmissions for all collars in this account
trans <- fetch_ats_transmissions(new = TRUE)

# get all transmissions for all collars in this account
trans <- fetch_ats_transmissions()

# get all transmissions for specific collars
collar_list <- sample(fetch_ats_devices(), 10)
trans <- fetch_ats_transmissions(device_id = collar_list)
