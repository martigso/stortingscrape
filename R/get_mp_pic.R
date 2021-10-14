#' Retrieve picture of specific MPs
#' 
#' @description A function for retrieving Norwegian MP pictures by id.
#' 
#' @usage get_mp_pic(mpid = NA, size = "middels", 
#'            destfile = NA, show_plot = FALSE, 
#'            good_manners = 0)
#' 
#' @param mpid Character string indicating the id of the MP to retrieve.
#' @param size Character string size of the picture. Accepts values "lite" (small), "middels" (medium -- default), and "stort" (big).
#' @param destfile Character string specifying where to save the picture
#' @param show_plot Logical. FALSE (default) if no plot should be produced and TRUE if plot should be produced. Requires the "imager" package.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return Picture of the requested MP in the preferred size.
#' 
#' @seealso [get_mp] [get_parlperiod_mps] [get_mp_bio]
#' 
#' @examples 
#' \dontrun{
#' # Request one MP by id
#' get_mp_pic(mpid = "AAMH", destfile = "~/Pictures/AAMH.jpeg", show_plot = TRUE, size = "stort")
#' 
#' # With good manners for multiple calls
#' lapply(c("AAMH", "CIH", "TKF"), function(x){
#'   get_mp_pic(mpid = x, destfile = paste0("~/Pictures/", x), 
#'   show_plot = TRUE, size = "stort", good_manners = 2)
#'   })
#' }
#' 
#' @import httr rvest
#' @importFrom utils download.file 
#' 
#' @export
#' 



get_mp_pic <- function(mpid = NA, size = "middels", 
                       destfile = NA, show_plot = FALSE, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/personbilde?personid=", mpid, "&storrelse=", size)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "image/jpeg") stop(paste0("Response of ", url, " is not image/jpeg."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  if(is.na(destfile) == TRUE & show_plot == TRUE){
    p <- imager::load.image(url)
    plot(p, labels = FALSE)
  }
  
  if(is.na(destfile) == TRUE & show_plot == FALSE){
    message("No destination file specified and plot specified as FALSE. Nothing to return")
  }
  
  if(is.na(destfile) == FALSE & show_plot == TRUE){
    download.file(url, destfile = destfile)
    p <- imager::load.image(destfile)
    plot(p,)
  }
  
  if(is.na(destfile) == FALSE & show_plot == FALSE){
    download.file(url, destfile = destfile)
    message(paste("Image saved as:", destfile))
  } 
  
  Sys.sleep(good_manners)
  
  
  
  
}


