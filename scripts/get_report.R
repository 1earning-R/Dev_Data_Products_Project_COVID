get_report <- function(file_name, warnings = FALSE){

        report <- read.csv(file_name)
        rep_cols <- names(report)
        rep_lng <- dim(report)[1]
        
        ## RegEx search for column names
        lat <- grepl("[Ll][Aa][Tt]", rep_cols)
        if (sum(lat) != 1) {
                if (warnings) warning("Could not match Latitude")
                return(NULL)
        }
        lng <- grepl("[Ll][Oo][Nn][Gg]", rep_cols)
        if(sum(lng) != 1) {
                if (warnings) warning("Could not match Longitude")
                return(NULL)
        }
        state <- grepl("[Pp]rovince", rep_cols)
        if(sum(state) != 1) {
                if (warnings) warning("Could not match State")
                return(NULL)
        }
        country <- grepl("[Cc]ountry", rep_cols)
        if(sum(country) != 1){
                if (warnings) warning("Could not match Country")
                return(NULL)
        }
        
        ## Select for US data
        report <- report[report[,rep_cols[country]] == "US",]        
        
        # Redefine columns so all reports will have same names
        report$LatLong <- paste(report[,rep_cols[lat]],
                                report[,rep_cols[lng]],
                                sep = ":")
        report$State <- report[,rep_cols[state]]
        
        ## Create Date column
        file_lng <- nchar(file_name)
        Date <- substr(file_name, file_lng - 13, file_lng -4)
        report$Date <- as.Date(Date, format = "%m-%d-%Y")
        
        
        
        ## Create County column
        ## If admin2 column does not exist split the State column into
        ## County and State information, using a comma as the
        ## separating variable
        if (!("Admin2" %in% rep_cols)) {
                report$County <- sapply(report$State,
                                        strsplit_i,
                                        chr = ", ",
                                        i = 1,
                                        default = "")
                report$State <- sapply(report$State,
                                       strsplit_i,
                                       chr = ",",
                                       i = 2)
                for (i in 1:dim(State_Abb)[1]) {
                        full_state <- State_Abb[i,1]
                        abb <- State_Abb[i,2]
                        index <- grep(abb, report$State)
                        report$State[index] <- full_state
                }
                
        } else report$County <- report$Admin2
        
        
        report[,c("Date","County","State","LatLong",
                  "Confirmed","Deaths","Recovered")]
}