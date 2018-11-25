genTestTimeSeq5mins <- function(iEnd) {

	time = c('01/01/18 22:00:00')
	time = strptime(time, format='%d/%m/%y %H:%M:%S')

	i=2
	while (i<=iEnd) {

	        lastTime = time[i-1]
	
	        # plus 5 mins
	        thisTime = lastTime + 60*5
	
	        time = c(time, thisTime)
	        i=i+1
	}

	return(time)
}


