## ATS Update

ATS changed their website in February 2021, which caused the code in collar to fail. As of 2021-04-12 the package has been updated to work with the redesigned site, but this change involved significant changes to the functions and their output. If you have scripts that were written using the `fetch_ats...` functions in previous versions of collar those scripts will need to be updated. If you're having trouble with ATS data make sure you're using the latest version of collar using `devtools::install_github("huh/collar")` or `remotes::install_github("huh/collar")`. For more details see the ATS section in `vignette("downloading-data", package = "collar")` or `?ats_login`, `?fetch_ats_positions`, etc. If you're still having trouble let us know by creating an [Issue](https://github.com/Huh/collar/issues).

## collar

We developed the collar package to help us automate downloads of data from telemetry collar manufacturer's websites/API's. The project is intended to be easy to use, provide a consistent user interface and encourage the automation of data downloads. Whether or not we achieved any of that is up to you. With this package we hope to encourage the use of data in the field, on your phone, visualization and storing it in databases (not Excel).

### The Problem

Telemetry collar manufacturers create devices that collect data about animal movements and they also dictate the format of the data. Unfortunately, there is not a single means of retrieving data, a consistent format or even similar types of information in the spreadsheets that proprietary software produce. This package provides functionality to read csv files, download data from the web, standardize telemetry data and provide basic summary information. 

The package is a work in progress, feedback is always appreciated.

## Overview

We focus on companies that our collaborators use often and that provide some means of automating access to data (e.g. API).

- Vectronics Aerospace
- Advanced Telemetry Solutions
- Lotek
- Loose csv's

Consistent naming prefixes were used when possible in an effort to enhance the user experience.

#### Helpers

- `filter_date_range()` - Returns locations within a specified date range
- `filter_last_loc()` - Retrieve the last location for each individual
- `make_gpx()` - Save locations to disc, for use on GPS device or phone app
- `make_map()` - Builds leaflet map using location data
- `save_map()` - Saves the map
- `cllr_add_id()` - Add individual identifier to each animal, some manufacturers do not include this information in downloads or store it in the file name
- `cllr_remove_header()` - Some manufacturers include metadata in the first n lines of a download, this function attempts to remove those headers so the data can be used
- `cllr_rename_id()` - The collar package has expectations about the column names in the data, this function exists to help the user change column names and conform to expectations


## Installation

This package is not available on CRAN.

*If required*
`install.packages("devtools")`

`devtools::install_github("Huh/collar")`

## Get Started

Check out the help pages and the vignettes to get a handle on how we use the package.

`vignette("downloading-data", package = "collar")`

`vignette("reading-csv", package = "collar")`

`vignette("lotek-api-functions", package = "collar")`

## Issues and Enhancements

If you have a problem or would like to see a new feature added please submit an [Issue](https://github.com/Huh/collar/issues)
