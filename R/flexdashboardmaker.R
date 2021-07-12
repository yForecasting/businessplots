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

# create flexdashboard automatically
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
    reassure <- tolower(reassure)
  } else{

    # flexdashboard file does not exist
    reassure <- "y"
  }

  # file can be written
  if (reassure == "y"){

    # create file
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

        # set function count
        ifunc = 1

        # while more functions, keep adding new functions
        more_func = 1

        # add functions
        while (more_func != 0){

          # ARGUMENT 2: func
          func <- readline(prompt=paste0("Name of func ", ifunc," (e.g. plot) // type 0 to stop: "))

          # check for empty function
          if (func != ""){

            # check for more functions
            if (func == 0){
              # no more functions incoming
              break
            }

            # set argument count
            iarg = 1

            # create list of arguments
            list_arg = list()

            # while more arguments, keep adding new arguments
            more_args = 1

            # add arguments
            while (more_args != 0){

              # ARGUMENT 3: arg
              arg <- readline(prompt=paste0("Enter argument ", iarg," (e.g. filepath) // type 0 to stop: "))

              # check for empty argument
              if (arg != "") {

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

                # empty argument
                print("Argument can not be empty. Please enter an argument")
              }
            }

            # prepare argument(s) to add to function
            func_arg <- paste(unlist(list_arg), collapse=', ')

            # add argument(s) to function
            func <- paste0(func, "(", func_arg, ")")

            # add function (with argument(s)) to list of functions
            list_func <- c(list_func, func)

            # end loop - keep counter
            ifunc <- ifunc + 1
          } else {

            # empty function
            print("Function can not be empty. Please enter a function")
          }
        }

        # end loop - keep counter
        isource <- isource + 1
      } else {

        # empty source
        print("Source can not be empty. Please enter a source.")
      }
    }

    # amount of tabs
    amount_tabs = 3

    # max plots each tab
    max_plots_tab = 4

    # create list of names of tabs
    list_tabs = list()

    # create list of selected plots
    list_plots = list()

    # create list of function headers
    list_func_headers = list()

    # create list of r names of functions
    list_r_func = list()

    # current tab
    itab = 1

    # prepare tabs for creation
    while (amount_tabs != 0){

      # read name tab
      name_tab <- readline(prompt=paste0("Name of tab ", itab, " // type 0 to stop: "))

      # check for empty name of tab
      if (name_tab != ""){

        # check for more tabs
        if (name_tab == 0) {
          # no more tabs incoming
          break
        }

        # add name of tab to list
        list_tabs <- c(list_tabs, name_tab)

        # print available plots
        print("Available plots:")
        print(list_func)

        # amount of chosen plots for tab
        amount_plots_tab = 0

        # list of chosen plots for tab
        list_chosen_plots = list()

        # list of entered function headers
        list_entered_func_headers = list()

        # list of entered r functions
        list_entered_r_func = list()

        # set number of function
        ifunc <- 1

        # choose plots for tab
        while (amount_plots_tab < 4){

          # index of chosen plot for tab
          chosen_plot <- readline(prompt=paste0("Enter index of plot for tab ", name_tab," (", max_plots_tab - amount_plots_tab,
                                                " plots remaining for ", name_tab, ") // type 0 to stop: "))

          # check for empty index
          if (chosen_plot != "") {

            # check for more plots for tab
            if (chosen_plot == 0) {
              # no more plots incoming
              break
            }

            # convert index to integer
            chosen_plot = strtoi(chosen_plot)

            # check valid index
            if (chosen_plot > 0 && chosen_plot <= length(list_func)){

              # valid index
              # ARGUMENT 4: func header
              func_header <- readline(prompt=paste0("Header above function ", ifunc," (e.g. Horizontal bar): "))

              # check for empty header
              if (func_header != ""){

                # ARGUMENT 5: r func
                r_func <- readline(prompt=paste0("R name of function ", ifunc," (e.g. plot): "))

                # check for empty r name
                if (r_func != ""){

                  # add chosen plots to list
                  list_chosen_plots <- c(list_chosen_plots, list_func[[chosen_plot]])

                  # add entered function headers to list
                  list_entered_func_headers <- c(list_entered_func_headers, func_header)

                  # add entered r functions to list
                  list_entered_r_func <- c(list_entered_r_func, r_func)

                  # decrease available amount of plots for tab
                  amount_plots_tab <- amount_plots_tab + 1

                  # increase number of functions
                  ifunc <- ifunc + 1
                } else {

                  # empty r name of function
                  print("R name of function can not be empty. Please enter a name.")
                }
              } else {

                # empty header of function
                print("Header of function can not be empty. Please enter a header.")
              }
            } else {

              # invalid index
              print("Invalid index. Please enter a valid index")
            }
          } else {

            # empty value
            print("Index can not be empty. Please enter a valid index")
          }
        }

        # add chosen plots to list of plots
        list_plots[[length(list_plots)+1]] <- list_chosen_plots

        # add function headers to list of function headers
        list_func_headers[[length(list_func_headers)+1]] <- list_entered_func_headers

        # add r function to list of r functions
        list_r_func[[length(list_r_func)+1]] <- list_entered_r_func

        # decrease tabs to prepare for creation
        amount_tabs <- amount_tabs - 1

        # increase done tabs
        itab <- itab + 1
      } else {

        # check for empty name of tab
        print("Name of the tab can not be empty. Please enter a name.")
      }
    }

    # write to file
    sink(flexdashboard_file) # open file

    # set ticks
    ticks <- "```"

    # write info
    cat("---")
    cat(paste0('\ntitle: "', title, '"'))
    cat(paste0('\nauthor: "', author, '"\n'))
    cat("output:\n")
    cat("  flexdashboard::flex_dashboard:\n") # \t can not be used
    cat(paste0("    orientation: ", orientation, "\n")) # \t can not be used
    cat(paste0("    vertical_layout: ", vertical_layout)) # \t can not be used
    cat("\n---\n\n")

    # write setup
    cat(paste0(ticks, "{r setup, include=FALSE}\n"))
    cat("library(flexdashboard)\n\n")
    for (i in 1:length(list_sources)){
      cat(paste0('source("', list_sources[[i]][1], '.R", local = knitr::knit_global())\n'))
    }
    cat(paste0(ticks, "\n\n"))

    # write functions with arguments
    for (i in 1:length(list_plots)){
      cat(paste0(list_tabs[i], "\n"))
      cat("===\n\n")
      for (j in 1:length(list_plots[[i]])){
        if (j %% 2 == 1){
          cat("Column {data-width=500}\n")
          cat("-----------------------------------------------------------------------\n\n")
        }
        cat(paste0("### ", list_func_headers[[i]][j], "\n"))
        cat(paste0(ticks, "{r ", list_r_func[[i]][j], ", echo=FALSE}\n"))
        cat(paste0(list_plots[[i]][j], "\n"))
        cat(paste0(ticks, "\n\n"))
      }
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
