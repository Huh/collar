## collar

A package to download data from telemetry collar manufacturer's websites/API's. The project is intended to be easy to use, provide a consistent user interface and encourage the automation of data downloads. Much sugar is planned for the next releases. However, our focus will be on accessibility first and foremost. Secondarily we will encourage the use of data in the field, on your phone, visualization and storing it in databases (not Excel). 

### Purpose

This package is the first in a series to make it easier to work with collar data.  Telemetry collar companies create the devices that collect data about animal movements and they also dictate the format of the data.  Unfortunately, there is not a single means of retrieving data, a consistent format or even similar types of information in the spreadsheets that proprietary software produces.  This package provides functionality to read csv files, standardize telemetry data and provide basic summary information. Access to data is our primary objective.

## Companies

At the time of this writing we have focused on developing tools to work with the new Vectronics API (thank you Vectronics) and web scraping utilities for ATS. Other manufacturers will be developed as requested.

1) Vectronics Aerospace
2) Advanced Telemetry Solutions
3) Loose csv's

For a great example of accessing Lotek data check out https://github.com/rgzn/CollarScraper.

## Installation

*If required*

#install.packages("devtools")

devtools::install_github("Huh/collar")

## Issues and Enhancements

If you have a problem or would like to see a new feature added please submit an [Issue](https://github.com/Huh/collar/issues)
