#' Set global debug value
#'
#' sets debugging to on for logging purposes
#' @param x TRUE or FALSE, whether or not to print debug messages
#' @keywords debug log print
#' @export
#' @examples
#' set_debug_print(TRUE)
set_debug_print <- function(x) {
  if(x == TRUE)
  {
    global_debug <<- TRUE
  }
  else
  {
    global_debug <<- FALSE
  }
}

#' Set log output file
#'
#' sets debugging to on for logging purposes
#' @param file the local file path to print to in the working directory
#' @keywords debug log print
#' @export
#' @examples
#' set_log_file(file)
set_log_file <- function(file) {
  print(paste("saving log to", file, sep=" "))
  log_file <<- file
}

#' Clear log output file
#'
#' clears the logging file
#' @keywords debug log print
#' @export
#' @examples
#' clearfile()
clear_log_file <- function() {
  print("clearing log file")
  write("", file = log_file, append = FALSE, sep = "")
}

print_classification <- function(content, ...){
  printf("<table>", ...)
  printf("<tr>", ...)
  for (i in 1:length(colnames(content$centers))){
    printf(paste("<th>", colnames(content$centers)[i], "</th>"), ...)
  }
  printf("</tr>", ...)
  for (i in 1:length(content$centers[,1])){
    printf("<tr>", ...)
    for (j in 1:length(content$centers[1,])){
      printf(paste("<td>", content$centers[i,j], "</td>"), ...)
    }
    printf("</tr>", ...)
  }
  printf("</table>", ...)
}

#' Print logging information
#'
#' Prints content to the console, debug, and file outputs
#' @param content the content to print
#' @param file whether to print to a file
#' @param debug whether this is debug output
#' @param console whether to print to console
#' @keywords debug log print
#' @export
#' @examples
#' printf("hello", TRUE, FALSE, TRUE)
printf <- function(content, file=TRUE, debug=FALSE, console=TRUE, ...) {
  #if debug is false OR we want to print debug strings
  if(!debug || (debug && global_debug) ) {
    if(file && !debug){
      if(is.list(content)){
        #we can't print lists right now, so we redirect output
        sink(file=logfile, append=TRUE)
          print(content, ...)
        sink()
      }
      else {
        write(content, file = logfile, append=TRUE, sep = " ", ...)
      }
    }
    if(console || debug) {  
      print(content)
    }
  }
}

#' Set up log to allow us to use printf()
#'
#' Prints content to the console, debug, and file outputs
#' @param file_name the name of the file to print to
#' @param debug whether to print debut output
#' @keywords debug log print
#' @export
#' @examples
#' set_up_log("log.txt", TRUE)
set_up_log <- function(file_name, debug) {
  set_debug_print(debug)
  set_log_file(file_name)
  clear_file()
}