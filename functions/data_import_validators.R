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
  df_colnames <- tolower(colnames(df))
  
  date_present <- "date" %in% df_colnames
  amount_present <- "amount" %in% df_colnames
  description_present <- "description" %in% df_colnames
  category_present <- "category" %in% df_colnames
  
  
  list(Date = date_present,
       Amount = amount_present,
       Description = description_present,
       Category = category_present)
}

validate_date_amount_present <- function(detected_columns) {
  if (detected_columns$Date == F) {
    stop("Error: 'Date' column not found")
  }
  if (detected_columns$Amount == F) {
    stop("Error: 'Amount' column not found")
  }
  
  invisible(detected_columns)
}

# Date checking ----

validate_date_column <- function(df) {
  date_col <- pull(df, date)
  
  # Reading with data.table gives IDate class object which will break joins
  # with regular dates, so they need to be converted.
  if ("IDate" %in% class(date_col)) {
    date_col <- date_col %>% as_date()
    modified_df <- df
    modified_df$date <- date_col
    return(modified_df)
  }
  
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
               "Error: couldn't create date column of valid Date type:", e$message
             )
             stop(error_message)
           },
           # Failing to parse anything returns a warning, not an error.
           
           # potential issue: data with only some NA's present will still error out
           warning = function(w){
             error_message = paste0(
               "Warning to error:", w$message
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
  
  if (detected_columns$Description) {
    modified_df$description[modified_df$description == ""] <- NA_character_
  }
  
  if (detected_columns$Category) {
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
