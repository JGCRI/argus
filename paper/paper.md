---
title: 'Argus: An interactive app to enable scientific discovery through multi-sector and multi-scale visual analytics'
tags:
  - 
authors:
  - name: Zarrar Khan
    orcid: 0000-0002-8147-8553
    affiliation: 1
  - name: Chris R. Vernon
    orcid: 0000-0002-3406-6214
    affiliation: 1  
  - name: Sean X. Tang
    orcid: 0000-0003-3248-5607
    affiliation: 1
  - name: Anna M. Warmka
    orcid: 0000-0001-6790-4512
    affiliation: 1
affiliations:
 - name: Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA
   index: 1
date: 22 July 2021
bibliography: paper.bib
---
# Summary
Summary of importance of science communication and gap between science and policy makers, etc.

`Argus` is an R Shiny App to interactively visualize data across scenarios, parameters, and regions. In can accommodate data inputs of the user's choice or load a GCAM directory.
Features of the application include selecting which scenarios will run and choosing relevant parameters and regions of interest. The designated parameters can be viewed as line plots over time, categorical bar graphs, maps broken apart by region, or as a table.
All results can be downloaded onto a local machine, or the user can obtain a bookmark to save their work.

`Argus` can be accessed via this [Home Page](https://jgcri.github.io/argus/index.html). We provide an R vignette guide walking users through `Argus` and its features, which is accessible here: [User Guide](https://jgcri.github.io/argus/articles/vignette_argus.html)

# Statement of need
Summary of importance of science communication, communication gap between science and policy makers, decision makers, and the public.
Brief review of other visualization bashboard/science communcation tools.

# Functionality
An Argus user can choose to input their own spatial and temporal data via a URL or .csv file, or they can choose to use a GCAM directory, specifying the parameters and regions of interest.
Additionally, the user can at any time change which scenarios are selected, set the reference scenario, and select or deselect different parameters and regions. Changing these selections will instantaneously change the data visualizations throughout the application.

Argus includes several functions for data visualization, separated as different tabs within the application.
Note that all functions are explained in full detail in the [User Guide](https://jgcri.github.io/argus/articles/vignette_argus.html), which includes documentation for each individual tab.

+ Focus: The focus tab gives an overview of the inputted data, and the user can select a year, parameter, and scenario they would like to visualize. Based on the selections, a world map, comparative line plot, and categorical bar chart will populate.
+ Lines: The lines tab creates line plots of each parameter by scenario over time. Within this tab there is also a compare regions option, which will create several line graphs for each parameter, broken up by region.
+ Charts: The charts tab shows bar charts for each parameter by scenario, breaking the parameters up categorically. Within this tab, there are also difference charts that compare each scenario to the specified reference scenario.
+ Maps: The maps tab shows maps for each parameter by scenario, splitting up the map by the desired regions. In this tab, there is also an option to look at the regional differences between each scenario compared to the reference scenario.
+ Table: The table tab displays all inputted data, within which the user can search for specific values using the search bar. Additionally, the table can be filtered in each column.

Argus also includes a function to create a bookmark to save any changes made and allow others to view the application exactly how the user left it.
Detailed instructions on creating, sharing, and loading bookmarks can be found in the [Bookmarks](https://jgcri.github.io/argus/articles/vignette_argus.html#bookmarks-1) section of the User Guide.
Within Argus, the user can also download all figures created based on their data, or they can choose to download figures from specific tabs.

# Acknowledgements
The research described in this paper was conducted under the Laboratory Directed Research and Development Program at Pacific Northwest National Laboratory, a multiprogram national laboratory operated by Battelle for the U.S. Department of Energy. 
The views and opinions expressed in this paper are those of the authors alone.

# References
