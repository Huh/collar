`%>%` <- magrittr::`%>%`

source("R/ats_auth.R")
source("R/fetch_ats.R")

u <- "Aval_Elk20"
p <- "Adobe20$"

ats_login(u, p)

all_collars <- fetch_ats_devices()

length(all_collars)

some_collars <- sample(all_collars, 5)

all_trans <- fetch_ats_transmissions()
some_trans <- fetch_ats_transmissions(some_collars)

nrow(all_trans)
nrow(some_trans)

all_pos <- fetch_ats_positions()
some_pos <- fetch_ats_positions(some_collars)

nrow(all_pos)
nrow(some_pos)

ats_logout()

u <- "mary"
p <- "."

ats_login(u, p)

new_trans <- fetch_ats_transmissions(new = TRUE)
new_pos <- fetch_ats_positions(new = TRUE)

# finding gmtoffset for each fix
get_offset <- function(collar_id, fix_date_time, transmissions) {

  transmissions %>%
    dplyr::filter(
      CollarSerialNumber == collar_id,
      DateUTC + lubridate::hours(GmtOffset) > fix_date_time
    ) %>%
    dplyr::arrange(DateUTC) %>%
    dplyr::slice(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(GmtOffset)

}

all_trans <- all_trans %>%
  dplyr::mutate(GmtOffset = GmtOffset * -1)

all_pos <- all_pos %>%
  dplyr::mutate(
    DateTimeOffset = lubridate::as_datetime(
      paste(Date, Hour, Minute),
      format = "%m/%d/%y %H %M"
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(    
    GmtOffset = get_offset(CollarSerialNumber, DateTimeOffset, all_trans)
  )

# takes forever and fails

# converting repair_lines to regex
resp <- ats_get(
  path = list(
    "download_all_transmission",
    "download_all_transmission.aspx?dw=all"
  ),
  task = "download transmission data"
)

rl <- httr::content(resp, "text", encoding = "UTF-8") %>%
  textConnection() %>%
  readLines() %>%
  magrittr::extract(nchar(.) > 0)

length(rl)

range(count_sep(rl))

nm <- strsplit(rl[1], ",")
x <- rl[count_sep(rl) == 14] %>%
  sample(1)

y <- rl[count_sep(rl) == 16] %>%
  sample(1)

# should be 15 columns, meaning 14 separators
# column 12 ('Event') is the issue

# regex to find which lines are bad - returns indices
rgx <- "(.*,){15,}"
bad_lines <- grep(rgx, rl)
length(bad_lines)
# 1719

# regex for first 11 columns
rgx <- "^(([^,]*,){11})(.*)$"
lc <- c(
  sub(rgx, "\\1", x),
  sub(rgx, "\\1", y)
)
lc

# regex for last 3 columns
rgx <- "^(.*)((,[^,]*){3})$"
rc <- c(
  sub(rgx, "\\2", x),
  sub(rgx, "\\2", y)
)
rc

# regex for first 11, whatever, last 3
rgx <- "^(([^,]*,){11})(.+)((,[^,]*){3})$"
allc <- c(
  sub(rgx, "\\1\"\\3\"\\4", x),
  sub(rgx, "\\1\"\\3\"\\4", y)
)
allc

sample(repair_lines(rl), 10)

z <- repair_lines(rl) %>%
      readr::read_csv(col_types = "ccidcciiidccddi")
head(z)

type <- "all"
resp <- ats_get(
  path = list(
    "download_all_transmission",
    paste0("download_all_transmission.aspx?dw=", type)
  ),
  task = "download transmission data"
)

rl <- httr::content(resp, "text", encoding = "UTF-8") %>%
  textConnection() %>%
  readLines() %>%
  {sub("^(([^,]*,){11})(.+)((,[^,]*){3})$", "\\1\"\\3\"\\4", .)} %>%
  readr::read_csv(col_types = "ccidcciiidccddi")

names(all_trans)
names(some_trans)

ats_select_collars(c("ABC"))
device_id <- c("ABCDEFG")

no_trans <- fetch_ats_transmissions(device_id)

some_trans <- fetch_ats_transmissions(device_id = fetch_ats_devices())
some_tr_2 <- some_trans %>%
  dplyr::select(CollarSerialNumber, DateCT, E0 = Event0, E1 = Event1, E2 = Event2)

comp_trans <- all_trans %>%
  dplyr::inner_join(some_tr_2) %>%
  dplyr::select(CollarSerialNumber, DateCT, Event0:Event2, E0:E2)

View(comp_trans)

unique(comp_trans$Event)
unique(all_trans$Mortality)

at_cl <- sapply(all_trans, "class")
st_cl <- sapply(some_trans, "class")
nt_cl <- sapply(no_trans, "class")

all.equal(at_cl, st_cl, nt_cl)

st_cl

nrow(some_trans)

all.equal(comp_trans$Event0, comp_trans$E0)
all.equal(comp_trans$Event1, comp_trans$E1)
all.equal(comp_trans$Event2, comp_trans$E2)

some_trans <- fetch_ats_transmissions(device_id = fetch_ats_devices()) %>%
  dplyr::arrange(CollarSerialNumber, DateUTC)

all_trans <- fetch_ats_transmissions() %>%
  dplyr::arrange(CollarSerialNumber, DateUTC)

purrr::map2(
  all_trans,
  some_trans,
  all.equal
)

# TODO document return data structures
# TODO check what time is used in events response
# TODO enforce structure for positions, events, config
# TODO deal with leading zero differences in devices
#   (try selecting with extra zeros? no zeros?)

# ok back to figuring out the offset

get_offset <- function(collar_id, fix_date_time, transmissions) {

  if (length(unique(transmissions$GmtOffset)) == 1) {
    transmissions$GmtOffset[1]
  } else {
    tr <- transmissions$CollarSerialNumber == collar_id
    tr <- transmissions[tr, c("GmtOffset", "DateTimeUTC")]
    if (length(unique(tr$GmtOffset)) == 1) {
      tr$GmtOffset[1]
    } else {
      tr$DateComp <- tr$DateTimeUTC + lubridate::hours(ts$GmtOffset)
      tr[tr$DateComp >= fix_date_time, "GmtOffset"]
      tr$GmtOffset[1]
    }
  }

}

at <- all_trans %>%
  dplyr::mutate(
    GmtOffset = GmtOffset * -1
  )

all_pos <- all_pos %>%
  dplyr::mutate(
    DateTimeOffset = lubridate::as_datetime(
      paste(Date, Hour, Minute),
      format = "%m/%d/%Y %H %M"
    ),
    GmtOffset = get_offset(CollarSerialNumber, DateTimeOffset, at)
  )

unique(all_pos$GmtOffset)

offset_to_string <- function(offset) {
  
  x <- as.character(offset)
  is_neg <- grepl("-", offset)

  x <- substr(gsub("-", "", x), 1, 4)

  x[nchar(x) == 1] <- paste0("0", x[nchar(x) == 1], "00")
  x[nchar(x) == 2] <- paste0(x[nchar(x) == 2], "00")
  x[nchar(x) == 3] <- paste0("0", x[nchar(x) == 3])
  x[is_neg] <- paste0("-", x[is_neg])

  x

}

all_pos <- all_pos %>%
  dplyr::mutate(
    DateTimeUTC = lubridate::force_tz(
      DateTimeOffset - lubridate::hours(GmtOffset),
      tz = "UTC"
    ),
    DateTimeLocal = lubridate::with_tz(DateTimeUTC, tz = Sys.timezone())
  )