extractDateFilename <- function(filename){	  	
    string_length<-str_length(filename)
		string_split<-suppressWarnings(as.numeric(str_split_fixed(filename,"", string_length)))
		sub_string<-string_split[which(is.na(string_split)==FALSE)]

    yyyy <- str_c(as.character(sub_string[1]),as.character(sub_string[2]),as.character(sub_string[3]),as.character(sub_string[4]))
    MM <- str_c(as.character(sub_string[5]),as.character(sub_string[6]))
    DD <- str_c(as.character(sub_string[7]),as.character(sub_string[8]))
  
		yyyyMMDD<-paste(yyyy,"-",MM,"-",DD,sep="")
  
		HH<- str_c(as.character(sub_string[9]),as.character(sub_string[10]))
		if (is.na(as.character(sub_string[11])))  mm <- '00' else {
		mm<- str_c(as.character(sub_string[11]),as.character(sub_string[12]))	
		}
    date<-as.POSIXct(strptime(paste(yyyyMMDD,' ',HH,':',mm,sep=''),"%Y-%m-%d %H:%M"),'GMT')
	return(date)	
}