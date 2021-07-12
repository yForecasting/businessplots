#' Markdown maker
#'
#' Creates a markdown automatically.
#'
#' This function creates a markdown automatically.
#'
#' @author Ruben Vanhecke
#'
#' @examples
#'   \dontrun{
#'      markdownmaker()
#'   }
#'
#'
#'

# create markdown automatically
markdownmaker <- function(){

  # set current wd
  original_wd <- getwd()

  # read name of markdown
  markdown_name <- readline(prompt = "File markdown: ")

  # print wd markdown
  print(paste("Markdown will be made in: ", original_wd))
  #dir <- readline(prompt="Dir/folder/enter: ")

  # define target wd
  path_functions <- "R"
  target_wd <- paste0(original_wd, "/", path_functions)

  # set new wd
  setwd(target_wd)

  # create markdown file name
  markdown_file <- paste0(markdown_name, ".Rmd")
  reassure <- "n"

  # check if markdown file already exists
  if (file.exists(markdown_file)){

    # markdown file already exists
    reassure <- readline(prompt="This markdown already exists, overwrite??? (y/n): ")
    reassure <- tolower(reassure)
  } else{

    # markdown file does not exist
    reassure <- "y"
  }

  # file can be written
  if (reassure == "y"){

    # create file
    file.create(markdown_file)

    # define title
    title = readline(prompt="Title: ")

    # define author
    author = readline(prompt="Author(s): ")

    # define date
    date = readline(prompt="Date: ")

    # define output
    output = readline(prompt="Output: ")

    # set source count
    isource = 1

    # create list of sources
    list_sources = list()

    # create list of function headers
    list_func_headers = list()

    # create list of r names of functions
    list_r_func = list()

    # create list of functions
    list_func = list()

    # while more sources, keep adding new sources
    more_sources = 1

    # add sources
    while (more_sources != 0){

      # ARGUMENT 1: source
      source <- readline(prompt=paste0("Name of source file ", isource," (e.g. hist) // type 0 to stop: "))

      # check for empty source
      if (source != ""){

        # check for more sources
        if (source == 0){

          # no more sources incoming
          break
        }

        # add source to list of sources
        list_sources <- c(list_sources, source)

        # set func count
        ifunc = 1

        # while more sources, keep adding new sources
        more_func = 1

        # add functions
        while (more_func != 0){

          # ARGUMENT 2: func
          func <- readline(prompt=paste0("Name of function ", ifunc," (e.g. plot) // type 0 to stop: "))

          # check for empty function
          if (func != ""){

            # check for more functions
            if (func == 0){

              # no more functions incoming
              break
            }

            # ARGUMENT 3: r func
            r_func <- readline(prompt=paste0("R name of function ", ifunc," (e.g. plot): "))

            if (r_func != ""){

              # ARGUMENT 4: header above function
              func_header <- readline(prompt=paste0("Header above function ", ifunc," (e.g. Horizontal bar): "))

              # check for empty header above function
              if (func_header != ""){

                # set argument count
                iarg = 1

                # create list of arguments
                list_arg = list()

                # while more args, keep adding new arguments
                more_args = 1

                # add arguments
                while (more_args != 0){

                  # ARGUMENT 5: arg
                  arg <- readline(prompt=paste0("Enter argument ", iarg," (e.g. filepath) // type 0 to stop: "))

                  # check for empty argument
                  if (arg != ""){

                    # check for more arguments
                    if (arg == 0){

                      # no more arguments incoming
                      break
                    }

                    # add argument to list of arguments
                    list_arg <- c(list_arg, arg)

                    # end loop - keep counter
                    iarg <- iarg + 1
                  } else {
                    print("Argument can not be empty. Please enter an argument.")
                  }
                }

                # add function header to list of function headers
                list_func_headers <- c(list_func_headers, func_header)

                # add r name of function to list of r names of functions
                list_r_func <- c(list_r_func, r_func)

                # prepare argument(s) to add to function
                func_arg <- paste(unlist(list_arg), collapse=', ')

                # add argument(s) to function
                func <- paste0(func, "(", func_arg, ")")

                # add function (with argument(s)) to list of functions
                list_func <- c(list_func, func)

                # end loop - keep counter
                ifunc <- ifunc + 1
              } else {

                # empty header
                print("Header above function can not be empty. Please enter a header.")
              }
            } else {

              # empty R name of function
              print("R name of function can not be empty. Please enter a r name of the function.")
            }
          } else {

            # empty function
            print("Function can not be empty. Please enter a function.")
          }
        }

        # end loop - keep counter
        isource <- isource + 1
      } else {

        # empty source
        print("Source can not be empty. Please enter a source.")
      }
    }

    # write to file
    sink(markdown_file) # open file

    # set ticks
    ticks <- "```"

    # write info
    cat("---")
    cat(paste0('\ntitle: "', title, '"'))
    cat(paste0('\nauthor: "', author, '"'))
    cat(paste0('\ndate: "', date, '"'))
    cat(paste0('\noutput: ', output))
    cat("\n---\n\n")

    # write setup
    cat(paste0(ticks, "{r setup, include=FALSE}\n"))
    cat("knitr::opts_chunk$set(echo = TRUE)\n")
    for (i in 1:length(list_sources)){
      cat(paste0('source("', list_sources[[i]][1], '.R", local = knitr::knit_global())\n'))
    }
    cat(paste0(ticks, "\n\n"))

    # write functions with arguments
    for (i in 1:length(list_func)){
      cat(paste0("## ", func_header, "\n"))
      cat(paste0(ticks, " {r ", list_r_func[[i]][1], ", echo=FALSE}\n"))
      cat(paste0(list_func[[i]][1], "\n"))
      cat(paste0(ticks, "\n\n"))
    }

    # close file
    sink()
    # Alternative code:
    # fileConn<-file(fun_file)
    # writeLines(c("Hello","World"), fileConn)
    # close(fileConn)
  }

  # reset wd
  setwd(original_wd)
}
