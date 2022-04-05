---
title: 'Argus: An interactive application to enable scientific discovery through multi-sector and multi-scale visual analytics'
tags:
  - 
authors:
  - name: Zarrar Khan
    orcid: 0000-0002-8147-8553
    affiliation: 1
  - name: Sean X. Tang
    orcid: 0000-0003-3248-5607
    affiliation: 1
  - name: Anna M. Warmka
    orcid: 0000-0001-6790-4512
    affiliation: 1
  - name: Chris R. Vernon
    orcid: 0000-0002-3406-6214
    affiliation: 1  
  - name: Mengqi Zhao
    orcid: 0000-0001-5385-2758
    affiliation: 2 
  - name: Thomas Wild
    orcid: 0000-0002-6045-7729
    affiliation: "1, 2"
affiliations:
 - name: Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA
   index: 1
 - name: Earth System Science Interdisciplinary Center (ESSIC), University of Maryland, College Park, MD, USA
   index: 2
date: 27 July 2021
bibliography: paper.bib
---
# Summary
`Argus` is an R Shiny App to interactively visualize data across scenarios, parameters, and regions. Argus is designed to ingest simple `.csv` data tables which can be loaded directly into the application or from a `url` if the data is hosted online. The platform comes preloaded with various shapefiles for countries, states, river basins and in the case of the United States, U.S. counties. The platform processes the input data into interactive maps, charts and tables to allow users to easily explore their data across regions, parameters, time periods and scenarios. The primary purpose of Argus is for users to be able to highlight key messages from their datasets and to this end the application allows users to subset their data by choosing relevant scenarios, regions and time periods, annotate each page with storylines and then save the state of the application via bookmarks to be shared as urls or `.rds` files which can be loaded back up in the application at a later time and place. The application is designed to be used as a tool by scientists to easily curate and share large datasets with their audiences, in an impactful and interactive way. While Argus can be used for any data that is spatial and temporal in nature the  application has been developed specifically for large multi-sector global modeling outputs such as those from the open source Global Change Analysis Model (GCAM) model [@Calvin2019]. 

![`Argus` landing page](figure1.PNG)

`Argus` can be accessed via this [Home Page](https://jgcri.github.io/argus/index.html). A detailed [User Guide](https://jgcri.github.io/argus/articles/vignette_argus.html) is also available which walks users through all the features of `Argus`.

# Statement of need

Global multi-sector models have advanced significantly over the years and continue to push the boundaries of spatial, sectoral and temproal resolution and detail. This push towards more detail results in increasing complextiy as well as a larger number of both inputs and outputs. Output databases from these models (e.g. GCAM [@Calvin2019], MESSAGE [@huppmann2019messageix]) can have up to several GB of data capturing combinations of thousands of sectors and sub-sectors; subregions ranging from countries to cities to river basins; and time periods ranging from decades to hours. Climate models outputs such as those from the Weather Research & Forecasting (WRF) model [@powers2017weather] or the Coupled Model Intercomparison Project Phase (CMIP) series [@eyring2016overview] can be even larger and range up to several Tera Bytes of data at globally gridded resolutions.

A key challenge that continues to become more critical with this increasing complexity of models is the ability to synthesize the outputs and extract relevant trends and messages. Visual and interactive communications are a particularly effective method of delivering key messages from complex topics and large datasets, with audeinces having been shown to have a higher retention of knowledge and comprehension of ideas when using such tools [@otten2015infographics, @mcintyre1998www, @janvrin2014idv]. 

In response to this, interactive data visualization dashboards are starting to be used across several fields of study to analyze large, complex data sets. Examples include the World Resources Institute's (WRI's) [WRI Aqueduct]()[@wri2021aqueduct], the International Institute for Applied Systems Analysis's (IIASA's) [Global Hotspots Explorer](https://hotspots-explorer.org/) [@iiasa2021globalhotspots], the Pacific Northwest National Lab's (PNNL's) [Hector UI](https://jgcri.shinyapps.io/HectorUI/) [@evanoff2020hectorui], the Intergovernmental Panel on Climate Change's (IPCC's) [WGI Interactive atlas](https://interactive-atlas.ipcc.ch/) for the IPCC Sixth Assessment Report [] and the Model for the Assessment of Greenhouse Gas Induced Climate Change ([MAGICC](https://v2.magicc.org/))[@meinshausen2011emulating]. These interactive applciations have an advantage over static applications because they allow users to choose and subset information that is most relevant and useful for their own purposes [@janvrin2014idv].

`Argus` addresses these key issues by allowing users to interactively visualize and subset relevant portions of their datasets as well as annotate the outputs in order to deliver a final product that highlights key messages and storylines from complex and large datasets. Users are also able to save the state of Argus at any point in time so they can return to it later on further modification as well as for easy sharing with other users and audiences, who can then interact with the data themselves. Additionally, while most of other applications come with limited preloaded datasets (with good reason as they are meant to be viewers for particular data), Argus is designed to be used with user datasets and is thus not restricted to any particular model or type of analysis.

# Functionality
An Argus user can choose to input their own spatial and temporal data via a URL or .csv file. In addition `Argus` has also been customized to be used directly with the Global CHange Analysis Model output databases [@Calvin2019]. Users can at any time change which scenarios are selected, set the reference scenario, and select or deselect different parameters and regions. Changing these selections will instantaneously change the data visualizations throughout the application.

Argus includes several functions for data visualization, separated as different tabs within the application.
Note that all functions are explained in full detail in the [User Guide](https://jgcri.github.io/argus/articles/vignette_argus.html), which includes documentation for each individual tab.

+ Focus: The focus tab gives an overview of the inputted data, and the user can select a year, parameter, and scenario they would like to visualize. Based on the selections, a world map, comparative line plot, and categorical bar chart will populate.
+ Lines: The lines tab creates line plots of each parameter by scenario over time. Within this tab there is also a compare regions option, which will create several line graphs for each parameter, broken up by region.
+ Charts: The charts tab shows bar charts for each parameter by scenario over time, breaking the parameters up categorically. Within this tab, there are also difference charts that compare each scenario to the specified reference scenario.
+ Maps: The maps tab shows maps for each parameter by scenario, splitting up the map by the desired regions. In this tab, there is also an option to look at the regional differences between each scenario compared to the reference scenario.
+ Table: The table tab displays all inputted data, within which the user can search for specific values using the search bar. Additionally, the table can be filtered in each column.
+ Bookmarks: Argus also includes a function to create a bookmark to save any changes made and allow others to view the application exactly how the user left it. Detailed instructions on creating, sharing, and loading bookmarks can be found in the [Bookmarks](https://jgcri.github.io/argus/articles/vignette_argus.html#bookmarks-1) section of the User Guide.
+ Preloaded Data: `Argus` comes with a number of preloaded datasets which will be updated regularly. These include curated data sets from official GCAM model releases as well other datasets of interests. 
+ Data Story: Each tab in `Argus` has a collapsable `Data Story` button on top which allows users to annotate the visuals seen on the particular tab in order to highlight particular aspects of what is being seen.

Within Argus, the user can also download all figures created based on their data, or they can choose to download figures from specific tabs.

Finally, the application is continually being developed to support the needs of its users, and some of the features described may change. Additionally, new features are likely to be included in future releases. Updated documentation of all features and their functionality will always be available in the [User Guide](https://jgcri.github.io/argus/articles/vignette_argus.html).

# Acknowledgements
This research was supported by the US Department of Energy, Office of Science, as part of research in MultiSector Dynamics, Earth and Environmental System Modeling Program. The Pacific Northwest National Laboratory is operated for DOE by Battelle Memorial Institute under contract DE-AC05-76RL01830. The views and opinions expressed in this paper are those of the authors alone.

# References
