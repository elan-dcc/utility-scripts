#' Get the count and total size of files per extension of subdirs
#' Date: 19-12-2024
#' @author Lisette de Schipper
#' @param directory the path of the directory
#' @param recurse depth of the recursion
#' @returns two dataframes, one with the counts and one with the sizes
#' @example
#' storage <- get_ext_info(r"(H:\Datavoorbereiding)")
#' writexl::write_xlsx(storage$sizes, "extensions_grootte.xlsx")
#' writexl::write_xlsx(storage$counts, "extensions_aantal.xlsx") 

library(fs)
library(data.table)

get_ext_info <- function(directory, recurse = 0) {
  dirs <- dir_ls(directory, type = "directory", recurse = recurse)
  files <- lapply(dirs, function(subdir) {
    list.files(subdir, pattern = ".", all.files = FALSE,
               recursive = TRUE, full.names = TRUE)})
  names(files) <- dirs
  
  sizes <- lapply(seq_along(files), function(file) {
    info <- file_info(files[[file]])
    info$ext <- tools::file_ext(info$path)
    info$dir <- names(files)[[file]]
    return(info[, c("dir", "ext", "size")])
  })
  
  sizes <- rbindlist(sizes)
  sizes <- sizes[, .(size = sum(size)), by = list(dir, ext)]
  sizes[, size := as.character(fs_bytes(size))]
  sizes <- dcast(sizes, dir ~ ext, value.var = "size")
  setnames(sizes, "V1", "")
  
  counts <- lapply(files, function(my_files) {
    as.list(table(tools::file_ext(my_files)))
  })
  
  counts <- rbindlist(lapply(counts, data.frame), fill = TRUE)
  counts <- cbind(dir = names(files), counts)
  return(list(sizes = sizes, counts = counts))
}