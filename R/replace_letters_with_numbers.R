replace_letters_with_numbers <- function(df, column_name) {
  # Create a named vector for the replacement
  replacement_vector <- setNames(1:7, LETTERS[1:7])
  
  # Replace the values in the specified column
  df[[column_name]] <- replacement_vector[df[[column_name]]]
  
  return(df)
}
