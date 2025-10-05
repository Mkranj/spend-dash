library(dplyr)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
library(tidyr)
library(stringi)
library(htmltools)
library(shinyjs)
library(feasts)
library(fable)
library(tsibble)

source("functions/plotting.R")
source("functions/data_manipulation.R")
source("functions/development_tools.R")
source("functions/data_import_validators.R")

source("modules/m_dateSelect.R")
source("modules/m_expenses_over_time_plot.R")
source("modules/m_categories_barchart.R")
source("modules/m_upload_error_message.R")

source("env_variables.R")

# Initial data
expenses <- read.csv2(file = "data_files/sample_expenses.csv")
expenses$Date <- as_date(expenses$Date)