#' @export
clean_names <- function(x){
	toupper(str_remove_all(x, "[:punct:]"))
}