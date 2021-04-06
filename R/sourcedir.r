sourcedir <- function(path, trace = TRUE, ...) {
    
    for(nm in list.files(path, pattern = "\\.[RrSsQq]$", all.files = TRUE)) {
        
        if(trace) cat(nm,":")                     
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
        
        }
    
    }


