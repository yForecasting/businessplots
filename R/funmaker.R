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

# create function automatically
funmaker <- function(){

  # set current wd
  original_wd <- getwd()

  # read fun file name
  fun_name = readline(prompt = "File name (e.g. funmaker): ")
  while (fun_name == ""){
    print("File name can not be empty. Please enter a name.")
    fun_name <- readline(prompt = "File name (e.g. funmaker): ")
    if (fun_name != ""){

      # valid name
      break
    }
  }

  # print wd new function
  print(paste("File will be made in: ", original_wd))
  #dir <- readline(prompt="Dir/folder/enter: ")

  # define target wd
  path_functions <- "R"
  target_wd <- paste0(original_wd, "/", path_functions)

  # set new wd
  setwd(target_wd)

  # create full fun file name
  fun_file <- paste0(fun_name, ".R")
  reassure <- "n"

  # check if fun file already exists
  if (file.exists(fun_file)){

    # fun file already exists
    reassure <- readline(prompt="This file already exists, overwrite??? (y/n): ")
    reassure <- tolower(reassure)
  } else{

    # fun file does not exist
    reassure <- "y"
  }

  # file can be written
  if (reassure == "y") {

    # create file
    file.create(fun_file)

    # define title
    title = readline(prompt="Title (e.g. funmaker): ")
    while (title == ""){
      print("Title can not be empty. Please enter a title.")
      title <- readline(prompt="Title (e.g. funmaker): ")
      if (title != ""){

        # valid title
        break
      }
    }

    # set argument count
    iarg = 1

    # create list of arguments
    list_arg = list()

    # create list of names of arguments
    list_arg_names = list()

    # create list of explanation or arguments
    list_explain_arg = list()

    # while more args, keep adding arguments
    more_args = 1

    # set argument counter
    iarg = 1

    # add arguments
    while (more_args != 0){

      # ARGUMENT 1: arg
      arg <- readline(prompt=paste0("Argument ", iarg," (e.g. title) // type 0 to stop: "))

      # check for empty argument
      while (arg == ""){
        print("Argument can not be empty. Please enter an argument.")
        arg <- readline(prompt=paste0("Argument ", iarg," (e.g. title) // type 0 to stop: "))
        if (arg != ""){

          # valid arg
          break
        }
      }

      # check for more arguments
      if (arg == 0){

        # no more arguments incoming
        break
      }

      list_arg <- c(list_arg, arg)

      # ARGUMENT 2: description
      description <- readline(prompt=paste0("Description of argument ", iarg, ": "))

      # check for empty description
      while (description == ""){
        print("Description can not be empty. Please enter a description.")
        description <- readline(prompt=paste0("Description of argument ", iarg, ": "))
        if (description != ""){

          # valid description
          break
        }
      }

      # add short description to list
      list_explain_arg <- c(list_explain_arg, trimws(description))

      # end loop - keep counter
      iarg <- iarg + 1
    }

    # define short description
    short_description = readline(prompt="Short description: ")
    while (short_description == ""){
      print("Short description can not be empty. Please enter a short description.")
      short_description <- readline(prompt="Short description: ")
      if (short_description != ""){

        # valid short description
        break
      }
    }

    # define long description
    long_description = readline(prompt="Long description: ")
    while (long_description == ""){
      print("Long description can not be empty. Please enter a long description.")
      long_description <- readline(prompt="Long description: ")
      if (long_description != ""){

        # valid long description
        break
      }
    }

    # define author
    author = readline(prompt="Author(s): ")
    while (author == ""){
      print("Author can not be empty. Please enter an author.")
      author <- readline(prompt="Author(s): ")
      if (author != ""){

        # valid author
        break
      }
    }

    # define return value
    return_value = readline(prompt="Return (leave blank for no return value): ")

    # disable/enable export
    enable_export = tolower(readline(prompt="Export? (y/n): "))

    # write to file
    sink(fun_file) # open file

    # define comments
    comment <- "#' "
    empty_comment <- paste0(comment, "\n")

    # write title
    cat(paste0(comment, title, "\n"))

    # write short description
    cat(paste0(empty_comment, comment, short_description, "\n"))

    # write long description
    cat(paste0(empty_comment, comment, long_description, "\n"))

    # write arguments
    if (length(list_arg) > 0){
      cat(empty_comment)
      for (i in 1:length(list_arg)){
        cat(paste0(comment, "@param ", noquote(trimws(strsplit(list_arg[[i]][1],"=")[[1]][1])), " ", list_explain_arg[[i]][1], "\n"))
      }
    }

    # write author
    cat(paste0(empty_comment, comment, "@author ", author, "\n", empty_comment))

    # write return
    if (return_value != ""){
      cat(paste0(comment, "@return ", return_value, "\n", empty_comment))
    }

    # write export
    if (enable_export == "y"){
      cat(paste0(comment, "@export\n", empty_comment))
    }

    # write functions
    cat(paste0("\n", fun_name," <- function(", paste(unlist(list_arg), collapse=', '), ") {\n", "\n", "}\n"))

    sink() # close file
    # Alternative code:
    # fileConn<-file(fun_file)
    # writeLines(c("Hello","World"), fileConn)
    # close(fileConn)
  }

  # reset wd
  setwd(original_wd)
}
