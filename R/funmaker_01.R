fun_name <- readline(prompt="Function name: ")
print(paste("Function will be made in:",getwd()))
#dir <- readline(prompt="Dir/folder/enter: ")
path_functions <- "R"
wd0 <- getwd()
wd1 <- paste0(wd0,"/",path_functions)
setwd(wd1) # change working dir

fun_file <- paste0(fun_name, ".R")
reassure <- "n"
if (file.exists(fun_file)){
  reassure = readline(prompt="This function already exists, overwrite??? (y/n): ")
} else{
  reassure <- "y"
}

if (reassure == "y"){
  # Make the file
  file.create(fun_file)

    
  title = readline(prompt="Title: ")
  iarg = 1
  list_arg = list() # list of arg without deault values
  list_explain_arg = list()
  str_arg = "" # call of function with default values
  arg1 = 1
  while (arg1 !=0){
    # ARGUMENT 1: parameter of function
    arg1 <- readline(prompt=paste0("Argument", iarg," (e.g. fixed = c(TRUE, FALSE) // type 0 to stop): "))
    if (arg1 == 0){
      break
    }
    if ("=" %in% arg1) {
      # split parts of argument in strings
      s <- strsplit(arg1,"=")
      # append string split to list
      list_arg <- c(list_arg, trimws(s[[1]][1]))
      arg1 <- paste0(trimws(s[[1]][1]), " = ", trimws(s[[1]][2]))
    } else {
      arg1 <- trimws(arg1)
      list_arg <- c(list_arg, arg1)
    }  
  
    # Arguments listing for base use notation
    if (str_arg == ""){
      str_arg <- arg1 
    } else{
      str_arg <- paste0(str_arg, ", ", arg1)
    }
    # ARGUMENT 2: description of parameter
    arg2 <- readline(prompt=paste0("Explain arg ", str(iarg), ": "))
    # Add description to list
    list_explain_arg <- c(list_explain_arg, trimws(arg2))
    # End loop - keep counter (for prompt arg2=)
    iarg <- iarg + 1
  }
  
  
  use = paste0(fun_name, "(", str_arg, ")")
  description = readline(prompt="Description: ")

  #write to file
  sink(fun_file) # open file
  cat(paste0(fun_name," <- function() {\n"))
  cat("\n")
  cat("}\n")
  cat(paste0(use,"\n"))
  cat("\\arguments{\n")
  for (i in 1:length(list_arg)){
    cat("\\item{", list_arg[[i]][1], "}{", list_explain_arg[[i]][1], "}", "\n")
  }
  sink() # close file
  # Alternative code:
  # fileConn<-file(fun_file)
  # writeLines(c("Hello","World"), fileConn)
  # close(fileConn)
  
}
setwd(wd0) # reset working dir
