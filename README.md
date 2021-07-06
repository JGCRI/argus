<!-- badges: start -->
[![build](https://github.com/JGCRI/argus/actions/workflows/build.yml/badge.svg)](https://github.com/JGCRI/argus/actions/workflows/build.yml)
[![pkgdown](https://github.com/JGCRI/argus/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/JGCRI/argus/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/gh/JGCRI/argus/branch/dev/graph/badge.svg?token=NDE0ZK7OHN)](https://codecov.io/gh/JGCRI/argus)
<!-- badges: end -->

<br>
  
<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

`argus` is an R Shiny App to interactively visualize data across scenarios, parameters and regions.

<div class="container">
<a href="https://jgcri.shinyapps.io/argus/" target="_blank" class="button button1">Launch App</a> 
<a href="https://jgcri.github.io/argus/articles/vignette_argus.html" target="_blank" class="button button2">User Guide</a>
</div>

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
install.packages(“devtools”)
devtools::install_github(“JGCRI/rgcam”)
devtools::install_github(“JGCRI/rmap”)
devtools::install_github(“JGCRI/argus”)
```

<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

Citation to be added

<hr>


<style>

.container {
  display: flex;
  justify-content: left;
}

.button {
  border: none;
  padding: 30px 30px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 32px;
  margin: 30px 30px;
  cursor: pointer;
  transition-duration: 0.4s;
  border-radius: 8px;
  box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);
}

.button1{
 background-color: #008CBB; /* Green */
 color: white;
}

.button1:hover{
 background-color: #008CBB; /* Green */
 color: black;
}

.button2{
 background-color: #4CAF50; /* Green */
 color: white;
}

.button2:hover{
 background-color: #4CAF59; /* Green */
 color: black;
}

</style>

  
