# ASCE-Journal-Disparity-Index-Data-Repository
READ BEFORE WORKING WITH THE DATA AND CODE

A repository for the data used in the development of the Drinking Water and Wastewater Disparity Indices

Data was compiled and imputed in Microsoft Excel

The code was written and must be run in R Studio

The following packages are necessary to run the analysis detailed in the article, as well as making any plots:

COINr - Used to develop the index equations. Information can be found here: https://bluefoxr.github.io/COINr/index.html

readxl - Used to import Excel spreadsheets into R Studio. Information can be found here: https://readxl.tidyverse.org/

ggplot2 - Used to create figures. Information can be found here: https://ggplot2.tidyverse.org/

nortest - Used to conduct the Anderson-Darling normality test. Information can be found here: https://cran.r-project.org/web/packages/nortest/index.html

The following packages are optional:

tidyverse - Contains many packages, including readxl and ggplot2, that could better optimize the code. Informaton can be found here: https://www.tidyverse.org/packages/

gridExtra - Allows figures to be placed on a grid, allowing for side-by-side comparison. This package was used in the included code, but is not necessary. Information can be found here: https://cran.r-project.org/web/packages/gridExtra/index.html

The development process for the Disparity Indices was developed by the European Council's Joint Research Centre. More information can be found here https://knowledge4policy.ec.europa.eu/composite-indicators/2021-jrc-week-composite-indicators-scoreboards_en and 
