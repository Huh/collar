## collar

A package to download data from telemetry collar manufacturer's websites/API's. The project is intended to be easy to use, provide a consistent user interface and encourage the automation of data downloads. Much sugar is planned for the next releases. However, our focus is on accessibility first and foremost. Secondarily we will encourage the use of data in the field, on your phone, visualization and storing it in databases (not Excel). 

Telemetry collar manufacturers create devices that collect data about animal movements and they also dictate the format of the data. Unfortunately, there is not a single means of retrieving data, a consistent format or even similar types of information in the spreadsheets that proprietary software produce. This package provides functionality to read csv files, download data from the web, standardize telemetry data and provide basic summary information.

The package is a work in progress, feedback is always appreciated.

## Companies

We focus on companies that our collaborators use often and that provide some means of automating access to data (e.g. API).
1) Vectronics Aerospace
2) Advanced Telemetry Solutions
3) Lotek (Under Development)
4) Loose csv's


## Installation

*If required*

#install.packages("devtools")

devtools::install_github("Huh/collar")

## Get Started

Check out the help pages and the vignette to get a handle on how we use the package.

## Issues and Enhancements

If you have a problem or would like to see a new feature added please submit an [Issue](https://github.com/Huh/collar/issues)
