library(readxl)
library(data.table)
library(tools)

library(lubridate)
library(dplyr)

read_data_file <- function(filename) {
  extension <- tools::file_ext(filename)
  if (extension %in% c("xls", "xlsx")) {
    df <- readxl::read_excel(filename)
  } else if (extension == "csv") {
    # fread recognises if a csv is comma- or semicolon-separated
    df <- data.table::fread(filename)
  } else {
    stop(paste0("Unsupported filetype: ", filename))
  }
  
  as_tibble(df)
}

# Column names ----

trim_column_ws <- function(df) {
  df_colnames <- colnames(df)
  trimmed_colnames <- trimws(df_colnames)
  
  colnames(df) <- trimmed_colnames
  df
}

colnames_to_lowercase <- function(df) {
  df_colnames <- colnames(df)
  lowercase_colnames <- tolower(df_colnames)
  
  colnames(df) <- lowercase_colnames
  df
}

# Column detection ----

detect_data_columns <- function(df) {
  date_present <- "date" %in% colnames(df)
  amount_present <- "amount" %in% colnames(df)
  description_present <- "description" %in% colnames(df)
  category_present <- "category" %in% colnames(df)
  
  
  list(date = date_present,
       amount = amount_present,
       description = description_present,
       category = category_present)
}

validate_date_amount_present <- function(detected_columns) {
  if (detected_columns$date == F) {
    stop("Error: 'date' column not found")
  }
  if (detected_columns$amount == F) {
    stop("Error: 'amount' column not found")
  }
  
  invisible(detected_columns)
}

# Date checking ----

validate_date_column <- function(df) {
  date_col <- pull(df, date)
  if (is.Date(date_col)) return(df)
  
  # If the date column has been read as POSIXct, that already contains appropriate
  # information, but is not of the appropriate type.
  if (is.POSIXct(date_col)) {
    date_col <- as_date(date_col)

    modified_df <- df
    modified_df$date <- date_col
    return(modified_df)
  }
  
  date_formats <- c("%Y-%m-%d", "%Y.%m.%d", "%d-%m-%Y", "%d.%m.%Y", "%d.%m.%Y.")
  tryCatch(date_col <- as_date(date_col, format = date_formats),
           error = function(e) {
             error_message = paste0(
               "Error: couldn't create date column of valid Date type:\n", e$message
             )
             stop(error_message)
           })
  
  modified_df <- df
  modified_df$date <- date_col
  modified_df
}

# Amount checking ----

validate_amount_column <- function(df) {
  amount_col <- pull(df, amount)
  if (is.numeric(amount_col)) return(df)
  
  if (is.character(amount_col)) {
    # possible decimal commas instead of dots
    amount_col <- sub(",", ".", amount_col) |> as.numeric()
  }
  
  modified_df <- df
  modified_df$amount <- amount_col
  modified_df
}

# Description checking ----

empty_string_to_na <- function(df, detected_columns) {
  modified_df <- df
  
  if (detected_columns$description) {
    modified_df$description[modified_df$description == ""] <- NA_character_
  }
  
  if (detected_columns$category) {
    modified_df$category[modified_df$category == ""] <- NA_character_
  }
  
  modified_df
}

# Integrate into loading data ----

load_and_prepare_data <- function(filename){
  original_df <- read_data_file(filename)
  
  # Column work - other functions depend on correct names
  validated_df <- original_df |> trim_column_ws() |> colnames_to_lowercase()
  
  detected_columns <- detect_data_columns(validated_df)
  validate_date_amount_present(detected_columns)
  
  # If code proceeds, date and amount can be accessed
  
  validated_df <- validated_df |> validate_date_column() |> 
    validate_amount_column() |> empty_string_to_na(detected_columns)
  
  # All column should have the first letter uppercase, to conform to rest of app
  colnames(validated_df) <- stringr::str_to_title(colnames(validated_df))
  
  list(data = validated_df,
       detected_columns = detected_columns)
}
