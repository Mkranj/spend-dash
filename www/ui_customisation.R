summary_div_style <- "border-style: groove;
  border-color: #151759;
  border-radius: 5px;
  border-width: 2px;
  padding: 5px;
  font-size: 20px;
  color: #151759;
  font-family: \"Lato\", sans-serif;
  margin-top: 8%;"

table_ind_expenses_css <- JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#151759', 'color': '#fff'});",
  "$(this.api().table().node()).css({'border-bottom': '3px dotted #151759'});",
  "}"
)

header_row_css <- "
background-color: #c2c3f0;
margin-bottom: 5px;
margin-top: -10px;
"