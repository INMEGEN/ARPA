library(fs)

getVolumesHome <- function (exclude) 
{
    if (missing(exclude)) 
        exclude <- NULL
    function() {
        osSystem <- Sys.info()["sysname"]
        if (osSystem == "Darwin") {
            volumes <- dir_ls("/Volumes")
            names(volumes) <- basename(volumes)
        }
        else if (osSystem == "Linux") {
            volumes <- c(Computer = "/")
            if (dir.exists("/media")){
                media <- dir_ls("/media")
            }else{
                media <- dir_ls("/home")
            }
            
            names(media) <- basename(media)
            volumes <- c(volumes, media)
        }
        else if (osSystem == "Windows") {
            wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
            if (!file.exists(wmic)) {
                message("\nThe wmic program does not seem to be in the default location")
                message("Please report this problem and include output from the command")
                message("'where wmic' to https://github.com/thomasp85/shinyFiles/issues")
                volumes <- Sys.getenv("HOMEDRIVE")
                volNames <- ""
            }
            else {
                volumes <- system(paste(wmic, "logicaldisk get Caption"), 
                  intern = T)
                volumes <- sub(" *\\r$", "", volumes)
                keep <- !tolower(volumes) %in% c("caption", "")
                volumes <- volumes[keep]
                volNames <- system(paste(wmic, "logicaldisk get VolumeName"), 
                  intern = T)
                volNames <- sub(" *\\r$", "", volNames)
                volNames <- volNames[keep]
                volNames <- paste0(volNames, ifelse(volNames == 
                  "", "", " "))
            }
            volNames <- paste0(volNames, "(", volumes, ")")
            names(volumes) <- volNames
            volumes <- gsub(":$", ":/", volumes)
        }
        else {
            stop("unsupported OS")
        }
        if (!is.null(exclude)) {
            volumes <- volumes[!names(volumes) %in% exclude]
        }
        volumes
    }
}