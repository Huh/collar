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