#' Retrieve picture of specific MPs
#' 
#' A function for retrieving Norwegian MP pictures by id.
#' 
#' @usage get_mp_pic(id = NA, size = "middels", destfile = NA, show_plot = FALSE)
#' 
#' @param id Character string indicating the id of the MP to retrieve.
#' @param size Character string size of the picture. Accepts values "lite" (small), "middels" (medium -- default), and "stort" (big).
#' @param destfile Character string specifying where to save the picture
#' @param show_plot Logical. FALSE (default) if no plot should be produced and TRUE if plot should be produced.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return Picture of the requested MP in the preferred size.
#' 
#' @family get_mp_data
#' 
#' @examples 
#' # Request one MP by id
#' get_mp_pic(id = "AAMH", destfile = "~/Pictures/AAMH.jpeg", show_plot = TRUE, size = "stort")
#' 
#' # With good manners for multiple calls
#' lapply(c("AAMH", "CIH", "TKF"), function(x){
#'   get_mp_pic(id = x, destfile = paste0("~/Pictures/", x), show_plot = TRUE, size = "stort", good_manners = 2)
#' })
#' 
#' @export
#' 



get_mp_pic <- function(id, size = "middels", destfile = NA, show_plot = FALSE, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/personbilde?personid=", id, "&storrelse=", size)
  
  if(is.na(destfile) == TRUE & show_plot == TRUE){
    p <- imager::load.image(url)
    plot(p)
  }
  
  if(is.na(destfile) == TRUE & show_plot == FALSE){
    message("No destination file specified and plot specified as FALSE. Nothing to return")
  }
  
  if(is.na(destfile) == FALSE & show_plot == TRUE){
    download.file(url, destfile = destfile)
    p <- imager::load.image(destfile)
    plot(p)
  }
  
  if(is.na(destfile) == FALSE & show_plot == FALSE){
    download.file(url, destfile = destfile)
    message(paste("Image saved as:", destfile))
  } 
  
  Sys.sleep(good_manners)
  
  
  
  
}


