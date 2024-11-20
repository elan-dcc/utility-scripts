#' List the sizes of a dir and its subdirs
#' Date: 19-12-2024
#' @author Lisette de Schipper
#' @param directory the path of the directory
#' @param recurse depth of the recursion (the higher this number, the
#' more nested the subdirs will be that are reported in the output)
#' @returns a dataframe with all the sizes
#' @example
#' writexl::write_xlsx(get_subdirs_size("H:"), "mapgroottes.xlsx")
library(fs)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

get_subdirs_size <- function(directory, recurse = 1) {
  dirs <- dir_ls(directory, type = "directory", recurse = recurse)
  
  sizes <- sapply(dirs, function(dir) { sum(dir_info(dir, recurse = TRUE)$size)})
  
  results <- data.frame(path = dirs,
                        dir = basename(dirs),
                        size = sizes,
                        size_readable = as.character(fs_bytes(sizes)))
  
  return(results[order(-results$size), ])
}