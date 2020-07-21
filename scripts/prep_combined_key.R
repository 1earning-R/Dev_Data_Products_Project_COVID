## Create combined county-state key for referencing COVID data set
prep_com_key <- function(countystate){
        county <- countystate[1]
        state <- countystate[2]
        if (county != "") {
                
                county <- strsplit(county, " and")[[1]]
                county <- strsplit(county, " Census Area")[[1]]
                county <- strsplit(county, " Borough")[[1]]
                county <- strsplit(county, " County")[[1]]
                county <- strsplit(county, " Parish")[[1]]
                county <- strsplit(county, " Municipality")[[1]]
                county <- strsplit(county, " City")[[1]]
        }
        com_key <- paste(county, state, sep = ", ")
}