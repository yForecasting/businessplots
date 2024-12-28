#' Replace letters with numbers
#'
#' Function to replace letters with numbers
#'
#' This function replace letters with numbers for one column of a dataframe.
#'
#'
#' @param df Dataframe to replace letters of one column to numbers.
#' @param column_name Specify the name of the column in which the replacement
#' needs to happen.
#'
#' @author Yves R. Sagaert
#'
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'   \dontrun{
#'     data <- data.frame(
#'      id_column = c("ID1","ID2","ID3","ID4"),
#'      score = c("A","B","C","D"))
#'     replace_letters_with_numbers(data,column_name=score)
#'   }
#'

replace_letters_with_numbers <- function(df, column_name) {
  # Create a named vector for the replacement
  replacement_vector <- setNames(1:26, LETTERS[1:26])

  # Replace the values in the specified column
  df[[column_name]] <- replacement_vector[df[[column_name]]]

  return(df)
}
