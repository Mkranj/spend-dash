# UI definitions

sidebar <- dashboardSidebar(
  actionButton("upload_new", "Read data from file", class = "upload-btn"),
  dateSelectUI("date_range"),
  uiOutput("categories_ui")
)

# Header with help ----

header <- dashboardHeader(title = "SpendDash")

# Adding an informative icon to the navbar that acts like a button for popups
header <- tagQuery(header)$
  find(".navbar")$
  append(div(
    class = "help-container",
    actionButton("help_btn", 
                label = "",
                icon = icon("question"),
                class = "help-btn")
                )
    )$
  find(".help_btn")$
  removeClass("btn-default")$
  removeClass("action-button")$
  allTags()

# Help popup when clicking on the icon
help_content <- tagList(
  h2(paste0("SpendDash ", APP_VERSION)),
  tags$hr(),
  tags$a("Support SpendDash!",
         href = "https://www.buymeacoffee.com/mkranj61",
         target="_blank"),
  div(class = "space-divider"),
  p("For more information about the application, visit the ",
    tags$a("README",
           href = "https://github.com/Mkranj/spend-dash", 
           target="_blank",
           .noWS = "after"),
    "."),
  p("You can download an Excel sheet for tracking expenses ",
    tags$a("here",
           href = "https://github.com/Mkranj/spend-dash/raw/master/example_spending.xlsx",
           .noWS = "after"),
    ".")
)

help_modal_ui <- modalDialog(
  help_content,
  easyClose = T,
  size = "m",
  footer = NULL
)

# Page body ----

body <- dashboardBody(
  includeCSS("www/styling.css"),
  # JS functionality enabled
  useShinyjs(),
  fluidRow(valueBoxOutput("vb_total_amount", width = 4),
           valueBoxOutput("vb_average_monthly_expense", width = 4),
           valueBoxOutput("vb_three_month_average", width = 4)),
  fluidRow(expenses_over_time_plotUI("expenses_plot") %>% box(width = 12)),
  fluidRow(categories_barchart_UI("categories_plot") %>% box(width = 12)) %>% 
    # This row needs an ID so it can be disabled if data has no categories
    htmltools::tagAppendAttributes(id = "categories_bar_box")
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)

# Data upload modal dialog ----

# Default fileInput has multiple elements. We need only the selection button.
fileInput_button <- fileInput("user_sent_data", "Use selected data", accept = c(".csv", ".xlsx"))

fileInput_button <- htmltools::tagQuery(fileInput_button)$
  find(".input-group-btn.input-group-prepend")$
  selectedTags()

# We need to replace the text in the span, so we'll extract the unchanging part,
# empty the original (text + extracted part) and the fill it back with the extracted part
fileInput_text <- "Choose file..."

input_part <- htmltools::tagQuery(fileInput_button)$find("input")$
  selectedTags()

fileInput_button <- htmltools::tagQuery(fileInput_button)$
  find("span")$
  empty()$ # getting rid of the text and input_part
  append(fileInput_text, input_part)$
  allTags()

### Importing from Revolut section
revolut_button <- fileInput("user_sent_revolut", "Use selected data", accept = c(".csv"))

revolut_button <- htmltools::tagQuery(revolut_button)$
  find(".input-group-btn.input-group-prepend")$
  addClass("revolut_button")$
  selectedTags()

# We need to replace the text in the span, so we'll extract the unchanging part,
# empty the original (text + extracted part) and the fill it back with the extracted part
revolut_button_content <- tags$img(src = "revolut_logo.png", height = "50px", width = "50px")

input_part <- htmltools::tagQuery(revolut_button)$find("input")$
  selectedTags()

revolut_button <- htmltools::tagQuery(revolut_button)$
  find("span")$
  empty()$ # getting rid of the text and input_part
  append(revolut_button_content, input_part)$
  allTags()



instr_revolut <- tagList(
  span(
    revolut_button,
    span("Import expenses from Revolut!"),
    p("In Revolut, go to Expenses > Download Expenses > CSV. Then import the downloaded file here.")
  )
)

instructions <- tagList(
  p("Upload a file from disk to visualize your data."),
  p("Supported filetypes: ", tags$b(".xlsx"), "and ", tags$b(".csv"), "."),
  span(class = "space-divider"),
  instr_revolut,
  tags$hr(),
  h2("Custom file"),
  p("The file ", tags$b("must"), " contain columns named \"Date\" and
    \"Amount\" to be properly loaded. If a column called \"Category\" also 
    exists, additional features will be enabled."),
  p("You can see an example of valid data in the picture below."),
  img(src = "expenses_ex.png")
)

uploading_modal_ui <- modalDialog(
  title = "Analyse your data",
  instructions,
  # Dynamic part of popup that displays errors only if any occur, otherwise empty
  upload_popup_error_UI("data_upload"),
  easyClose = F,
  size = "l",
  footer = tagList(
    fileInput_button,
    actionButton("cancel_upload", "Cancel")
  )
)
