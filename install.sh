
#######################################################
##### THIS SCRIPT WILL INSTALL ALL R DEPENDENCIES
#######################################################


##### VERIFY THE EXISTENCE OF COMMAND RSCRIPT
command -v Rscript >/dev/null 2>&1 || { echo >&2 "I require Rscript but it's not installed.  Aborting."; exit 1; }


##### INSTALLING DEPENDENCIES
sudo Rscript -e 'ifelse("shiny" %in% rownames(installed.packages()), paste("shiny v.", packageVersion("shiny"), "installed"), install.packages("shiny", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("shinythemes" %in% rownames(installed.packages()), paste("shinythemes v.", packageVersion("shinythemes"), "installed"), install.packages("shinythemes", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("shinyFiles" %in% rownames(installed.packages()), paste("shinyFiles v.", packageVersion("shinyFiles"), "installed"), install.packages("shinyFiles", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("DT" %in% rownames(installed.packages()), paste("DT v.", packageVersion("DT"), "installed"), install.packages("DT", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("tidyverse" %in% rownames(installed.packages()), paste("tidyverse v.", packageVersion("tidyverse"), "installed"), install.packages("tidyverse", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("rmarkdown" %in% rownames(installed.packages()), paste("rmarkdown v.", packageVersion("rmarkdown"), "installed"), install.packages("rmarkdown", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("cowplot" %in% rownames(installed.packages()), paste("cowplot v.", packageVersion("cowplot"), "installed"), install.packages("cowplot", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("ggpubr" %in% rownames(installed.packages()), paste("ggpubr v.", packageVersion("ggpubr"), "installed"), install.packages("ggpubr", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("kableExtra" %in% rownames(installed.packages()), paste("kableExtra v.", packageVersion("kableExtra"), "installed"), install.packages("kableExtra", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("growthrates" %in% rownames(installed.packages()), paste("growthrates v.", packageVersion("growthrates"), "installed"), install.packages("growthrates", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("fs" %in% rownames(installed.packages()), paste("fs v.", packageVersion("fs"), "installed"), install.packages("fs", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("janitor" %in% rownames(installed.packages()), paste("janitor v.", packageVersion("janitor"), "installed"), install.packages("janitor", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("vroom" %in% rownames(installed.packages()), paste("vroom v.", packageVersion("vroom"), "installed"), install.packages("vroom", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'
sudo Rscript -e 'ifelse("ezknitr" %in% rownames(installed.packages()), paste("ezknitr v.", packageVersion("ezknitr"), "installed"), install.packages("ezknitr", dependencies = TRUE, repos = "https://cloud.r-project.org/"))'


