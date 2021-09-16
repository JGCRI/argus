<!-- badges: start -->
[![build](https://github.com/JGCRI/argus/actions/workflows/build.yml/badge.svg)](https://github.com/JGCRI/argus/actions/workflows/build.yml)
[![docs](https://github.com/JGCRI/argus/actions/workflows/docs.yaml/badge.svg?branch=main)](https://github.com/JGCRI/argus/actions/workflows/docs.yaml)
[![test_coverage](https://github.com/JGCRI/argus/actions/workflows/test_coverage.yml/badge.svg?branch=main)](https://github.com/JGCRI/argus/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/JGCRI/argus/branch/dev/graph/badge.svg?token=NDE0ZK7OHN)](https://codecov.io/gh/JGCRI/argus)
<!-- badges: end -->

<br>
  
<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

`argus` is an R Shiny App to interactively visualize data across scenarios, parameters, and regions. It can accomodate data inputs of the user's choice,
including the options to read a URL, upload a .csv file, or use GCAM data. Features of the application include selecting which scenarios will run, 
choosing the relevant parameters for individual cases, and selecting the geographic regions of interest. The designated parameters can be viewed as 
line graphs plotted over time, weighted bar graphs showing makeup breakdown within each bar, maps showing values by country, or as output tables. 
All results can be downloaded onto a local machine, or the user can obtain a bookmark to save their work.

For more detailed information on using argus, click the green **User Guide** button below. 
To launch argus in your browser, click the blue **Launch App** button.

<br>

<p align="center">
<a href="https://jgcri.github.io/argus/articles/vignette_argus.html" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_user_guide.PNG?raw=true" alt="https://jgcri.github.io/argus/articles/vignette_argus.html" height="60"/></a>
<img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_divider.PNG?raw=true" height="40"/>
<a href="https://jgcri.shinyapps.io/argus/" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_launch_app.PNG?raw=true" alt="https://jgcri.shinyapps.io/argus/" height="60"/></a>
</p>


<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

Khan, Z., Tang, S., Warmka, A., Zhao, M., Wild, T., Vernon, C., 2021. Argus - An interactive platform to effectively communicate and share spatio-temporal scientific data. (In progress) Journal of Open Source Software, DOI: XXXX



<!-- ------------------------>
<!-- ------------------------>
# <a name="Argus Offline"></a>Argus Offline
<!-- ------------------------>
<!-- ------------------------>

If users want to use argus locally on their machines they can install the software as follows:

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)  
    
2. Open R studio:

```r
install.packages("devtools")
devtools::install_github("JGCRI/rgcam")
devtools::install_github("JGCRI/rmap")
devtools::install_github("JGCRI/argus")

library(argus)
argus::run()
```



  
