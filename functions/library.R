# Pakete für die Skripte in lecture_example/ laden
# Vorher einmal functions/packages.R ausführen, damit alle Pakete installiert sind.

library(reshape2)
library(tidyverse)   # enthält ggplot2, dplyr, readr und lubridate
library(knitr)
library(kableExtra)
library(car)         # residualPlots(), vif(), leveneTest()
# library(MASS)      # bewusst nicht geladen: MASS::select() verdeckt dplyr::select()
library(psych)
library(nortest)     # ad.test(), lillie.test()
library(moments)
library(ggpubr)
library(corrplot)
library(lmtest)      # bptest()
library(sandwich)
library(DataExplorer)
