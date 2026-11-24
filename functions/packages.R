# Pakete für die Skripte in lecture_example/ installieren
# Installiert nur Pakete, die noch fehlen. Vorhandene Pakete bleiben unverändert,
# das Skript kann also gefahrlos bei jedem Lauf mit source() eingebunden werden.

pakete <- c("tidyverse", "lubridate", "reshape2", "knitr", "kableExtra",
            "car", "MASS", "psych", "nortest", "moments", "ggpubr",
            "corrplot", "lmtest", "sandwich", "DataExplorer", "caret")

fehlend <- setdiff(pakete, rownames(installed.packages()))
if (length(fehlend) > 0) {
  install.packages(fehlend, repos = "https://cloud.r-project.org")
}
