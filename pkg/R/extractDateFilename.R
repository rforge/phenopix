extractDateFilename <- function(filename){	  	
 	pos.underscore <- gregexpr('_', filename)[[1]][1]	
 	filename.cleaned <- substr(filename, pos.underscore+1, nchar(filename))
    string_length <- nchar(filename.cleaned)
	string_split <-suppressWarnings(as.numeric(str_split_fixed(filename.cleaned,"", string_length+1)))
	sub_string<- string_split[which(is.na(string_split)==FALSE)]
    yyyy <- str_c(as.character(sub_string[1]),as.character(sub_string[2]),as.character(sub_string[3]),as.character(sub_string[4]))
    MM <- str_c(as.character(sub_string[5]),as.character(sub_string[6]))
    DD <- str_c(as.character(sub_string[7]),as.character(sub_string[8]))  
	yyyyMMDD<-paste(yyyy,"-",MM,"-",DD,sep="")  
	if (is.na(as.character(sub_string[9])))  HH <- '00' else {
	HH<- str_c(as.character(sub_string[9]),as.character(sub_string[10]))
		}
	if (is.na(as.character(sub_string[11])))  mm <- '00' else {
		mm<- str_c(as.character(sub_string[11]),as.character(sub_string[12]))	
		}
    date<-as.POSIXct(strptime(paste(yyyyMMDD,' ',HH,':',mm,sep=''),"%Y-%m-%d %H:%M"),'GMT')
	return(date)	
}