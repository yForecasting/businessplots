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

markdownmaker <- function(){

  # set current wd
  original_wd <- getwd()

  # read name of markdown
  markdown_name <- readline(prompt = "File markdown: ")

  # print wd new markdown
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
  } else{
    # markdown file does not exist
    reassure <- "y"
  }

  if (reassure == "y"){

    # Make the file
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

    # create list of function comments
    list_func_comments = list()

    # create list of r functions
    list_r_func = list()

    # create list of functions
    list_func = list()

    # while more_sources, keep adding new sources
    more_sources = 1

    # add sources
    while (more_sources != 0){

      # ARGUMENT 1: source
      source <- readline(prompt=paste0("Name of source file ", isource," (e.g. hist) // type 0 to stop): "))

      # check for more sources
      if (source == 0){
        # no more sources incoming
        break
      }

      # add source to list of sources
      list_sources <- c(list_sources, source)

      # set func count
      ifunc = 1

      # while more_sources, keep adding new sources
      more_func = 1

      # add sources
      while (more_func != 0){

        # ARGUMENT 2: func
        func <- readline(prompt=paste0("Name of func ", ifunc," (e.g. plot) // type 0 to stop): "))

        # check for more functions
        if (func == 0){
          # no more functions incoming
          break
        }

        # ARGUMENT 3: r func
        r_func <- readline(prompt=paste0("R name of func ", ifunc," (e.g. plot) // type 0 to stop): "))

        # check for more functions
        if (r_func == 0){
          # no more functions incoming
          break
        }

        # ARGUMENT 4: comment above func
        func_comment <- readline(prompt=paste0("Comment above func ", ifunc," (e.g. Horizontal bar) // type 0 to stop): "))

        # check for more functions
        if (func_comment == 0){
          # no more functions incoming
          break
        }

        # set arg count
        iarg = 1

        # create list of args
        list_arg = list()

        # while more_args, keep adding new args
        more_args = 1

        # add args
        while (more_args != 0){

          # ARGUMENT 5: arg
          arg <- readline(prompt=paste0("Enter the arg ", iarg," (e.g. title='My title') // type 0 to stop): "))

          # check for more args
          if (arg == 0){
            # no more args incoming
            break
          }

          # add arg to list of args
          list_arg <- c(list_arg, arg)

          # End loop - keep counter
          iarg <- iarg + 1
        }

        # add function comment to list of function comments
        list_func_comments <- c(list_func_comments, func_comment)

        # add r name function to list of r name functions
        list_r_func <- c(list_r_func, r_func)

        # prepare arg(s) to add to function
        func_arg <- paste(unlist(list_arg), collapse=', ')

        # add arg(s) to function
        func <- paste0(func, "(", func_arg, ")")

        # add function (with arg(s)) to list of functions
        list_func <- c(list_func, func)

        # End loop - keep counter
        ifunc <- ifunc + 1
      }

      # End loop - keep counter (for prompt arg2=)
      isource <- isource + 1
    }

    #write to file
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

    # write functions with args
    for (i in 1:length(list_func)){
      cat(paste0("## ", func_comment, "\n"))
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
