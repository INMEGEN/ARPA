

###### DETERMINE FIREFOX AS THE DEFAULT BROWSER IN LINNUX
osSystem <- Sys.info()["sysname"]

if (osSystem == "Linux"){
	options(browser = "/usr/bin/firefox")
}

options(shiny::runApp('.', launch.browser = TRUE))
