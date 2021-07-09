#' Function maker
#'
#' Creates a function automatically (without body).
#'
#' This function creates a function automatically (without body).
#'
#' @author Yves R.Sagaert, Ruben Vanhecke
#'
#' @examples
#'   \dontrun{
#'      funmaker()
#'   }
#'
#'
#'

funmaker <- function(){

  # set current wd
  original_wd <- getwd()

  # read name of function
  fun_name <- readline(prompt = "File function:")

  # print wd new function
  print(paste("Function will be made in: ", original_wd))
  #dir <- readline(prompt="Dir/folder/enter: ")

  # define target wd
  path_functions <- "R"
  target_wd <- paste0(original_wd, "/", path_functions)

  # set new wd
  setwd(target_wd)

  # create fun file name
  fun_file <- paste0(fun_name, ".R")
  reassure <- "n"

  # check if fun file already exists
  if (file.exists(fun_file)){
    # fun file already exists
    reassure <- readline(prompt="This function already exists, overwrite??? (y/n): ")
  } else{
    # fun file does not exist
    reassure <- "y"
  }

  # check reassure to create file
  if (reassure == "y") {
    # Make the file
    file.create(fun_file)

    # define title
    title = readline(prompt="Title: ")

    # set arg count
    iarg = 1

    # create list of arg without default values
    list_arg = list()

    #
    list_explain_arg = list()

    # call of function with default values
    str_arg = ""

    #
    arg1 = 1

    #
    while (arg1 != 0){

      # ARGUMENT 1: parameter of function
      arg1 <- readline(prompt=paste0("Argument", iarg," (e.g. fixed = c(TRUE, FALSE) // type 0 to stop): "))

      # check for more arg
      if (arg1 == 0){
        # no more arg incoming
        break
      }

      # check if "=" is present in arg
      if ("=" %in% arg1) {

        # split parts of argument in strings
        s <- strsplit(arg1,"=")

        # append string split to list
        list_arg <- c(list_arg, trimws(s[[1]][1]))
        arg1 <- paste0(trimws(s[[1]][1]), " = ", trimws(s[[1]][2]))

      } else {
        #
        arg1 <- trimws(arg1)
        list_arg <- c(list_arg, arg1)
      }

      # arguments listing for base use notation
      if (str_arg == ""){
        str_arg <- arg1
      } else{
        str_arg <- paste0(str_arg, ", ", arg1)
      }

      # ARGUMENT 2: description of parameter
      arg2 <- readline(prompt=paste0("Description of arg ", str(iarg), ": "))

      # add short description to list
      list_explain_arg <- c(list_explain_arg, trimws(arg2))

      # End loop - keep counter (for prompt arg2=)
      iarg <- iarg + 1
    }

    use = paste0(fun_name, "(", str_arg, ")")
    short_description = readline(prompt="Short description: ")
    long_description = readline(prompt="Long description: ")
    author = readline(prompt="Author: ")
    return_value = readline(prompt="Return: ")
    export_value = readline(prompt="Export: ")

    #write to file
    sink(fun_file) # open file

    comment <- "#' "
    empty_comment <- paste0(comment, "\n")

    # write title
    cat(paste0(comment, title, "\n", empty_comment))

    # write short description
    cat(paste0(comment, short_description, "\n", empty_comment))

    # write long description
    cat(paste0(comment, long_description, "\n", empty_comment))

    # write arg
    for (i in 1:length(list_arg)){
      cat(paste0(comment, "@param ", list_arg[[i]][1], " ", list_explain_arg[[i]][1], "\n"))
    }
    cat(empty_comment)

    # write author
    cat(paste0(comment, "@author ", author, "\n", empty_comment))

    # write return
    cat(paste0(comment, "@return ", return_value, "\n", empty_comment))

    # write export
    cat(paste0(comment, "@export ", export_value, "\n", empty_comment, "\n"))


    # write func
    cat(paste0(fun_name," <- function(", paste(unlist(list_arg), collapse=', '), ") {\n", "\n", "}\n"))

    sink() # close file
    # Alternative code:
    # fileConn<-file(fun_file)
    # writeLines(c("Hello","World"), fileConn)
    # close(fileConn)
  }

  # reset wd
  setwd(original_wd)
}
