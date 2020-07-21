strsplit_i <- function(string, chr, i, default = NULL){
        require(stringr)
        
        str_list <- strsplit(string, chr)[[1]]
        list_lng <- length(str_list)
        
        if (list_lng > 1 & list_lng >= i) str_list[i] else
                if (is.null(default)) str_list[1] else
                        default
}