make_pretty <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE, 
          cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y", 
          round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE, 
          data_table = TRUE, nrows = 5) 
{
  require(Hmisc)
  require(DT)
  require(ggplot2)
  require(scales)
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        the_column <- scales::comma(the_column)
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    the_table <- DT::datatable(the_table, options = list(pageLength = nrows), 
                               rownames = FALSE)
  }
  return(the_table)
}