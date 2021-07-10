#' Flexdashboard maker
#'
#' Creates a flexdashboard automatically.
#'
#' This function creates a flexdashboard automatically.
#'
#' @author Ruben Vanhecke
#'
#' @examples
#'   \dontrun{
#'      flexdashboardmaker()
#'   }
#'
#'
#'

flexdashboardmaker <- function(){

  # set current wd
  original_wd <- getwd()

  # read name of flexdashboard
  flexdashboard <- readline(prompt = "File flexdashboard: ")

  # print wd flexdashboard
  print(paste("Flexdashboard will be made in: ", original_wd))
  #dir <- readline(prompt="Dir/folder/enter: ")

  # define target wd
  path_functions <- "R"
  target_wd <- paste0(original_wd, "/", path_functions)

  # set new wd
  setwd(target_wd)

  # create flexdashboard file name
  flexdashboard_file <- paste0(flexdashboard, ".Rmd")
  reassure <- "n"

  # check if flexdashboard file already exists
  if (file.exists(flexdashboard_file)){
    # flexdashboard file already exists
    reassure <- readline(prompt="This flexdashboard already exists, overwrite??? (y/n): ")
  } else{
    # flexdashboard file does not exist
    reassure <- "y"
  }

  if (reassure == "y"){

    # Make the file
    file.create(flexdashboard_file)

    # define title
    title = readline(prompt="Title: ")

    # define author
    author = readline(prompt="Author(s): ")

    # define output
    orientation = readline(prompt="Orientation: ")

    # define output
    vertical_layout = readline(prompt="Vertical layout: ")

    # set source count
    isource = 1

    # create list of sources
    list_sources = list()

    # create list of function headers
    list_func_headers = list()

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

        # ARGUMENT 4: header above func
        func_header <- readline(prompt=paste0("Header above func ", ifunc," (e.g. Horizontal bar) // type 0 to stop): "))

        # check for more functions
        if (func_header == 0){
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

        # add header function to list of function headers
        list_func_headers <- c(list_func_headers, func_header)

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

    # amount of tabs
    amount_tabs = 3

    # max plots each tab
    max_plots_tab = 4

    # dataframe to save plots for each tab
    plots = data.frame(tab1=character(1), tab2=character(1), tab3=character(1))
    plots_df_names = names(plots)

    itab = 1

    while (amount_tabs != 0){
      name_tab <- readline(prompt=paste0("Name for tab ", itab, ": "))

      print("Available plots:")
      print(list_func)

      amount_plots_tab = 0
      chosen_plot_list = list()

      while (amount_plots_tab < 4){
        chosen_plot <- readline(prompt=paste0("Enter number of plot for tab ", name_tab," (", max_plots_tab - amount_plots_tab,
                                              " plots remaining for ", name_tab, ") // type 0 to stop: "))

        chosen_plot = strtoi(chosen_plot)

        if (chosen_plot == 0) {
          break
        }

        if (chosen_plot > 0 && chosen_plot <= length(list_func)){
          chosen_plot_list <- c(chosen_plot_list, list_func[[chosen_plot]])
          amount_plots_tab <- amount_plots_tab + 1
        } else {
          print("Invalid number.")
        }

        amount_plots_tab <- amount_plots_tab + 1
      }

      plots[, plots_df_names[itab]] <- unlist(chosen_plot_list)
      amount_tabs <- amount_tabs - 1
      itab <- itab + 1
    }

    print(plots)

    #write to file
    sink(flexdashboard_file) # open file

    # set ticks
    ticks <- "```"

    # write info
    cat("---")
    cat(paste0('\ntitle: "', title, '"'))
    cat(paste0('\nauthor: "', author, '"'))
    cat("\noutput:\n")
    cat("\tflexdashboard::flex_dashboard:")
    cat(paste0("\n\t\torientation: ", orientation))
    cat(paste0("\n\t\tvertical_layout: ", vertical_layout))
    cat("\n---\n\n")

    # write setup
    cat(paste0(ticks, "{r setup, include=FALSE}\n"))
    cat("library(flexdashboard)\n\n")
    for (i in 1:length(list_sources)){
      cat(paste0('source("', list_sources[[i]][1], '.R", local = knitr::knit_global())\n'))
    }
    cat(paste0(ticks, "\n\n"))

    # write functions with args
    for (i in 1:length(list_func)){
      cat(paste0("### ", func_header, "\n"))
      cat(paste0(ticks, "{r ", list_r_func[[i]][1], ", echo=FALSE}\n"))
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
