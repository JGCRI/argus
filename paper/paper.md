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
date: 27 July 2021
bibliography: paper.bib
---
# Summary
Existing scientific literature and case studies display the gap found in science communication, where public interest in scientific topics is much higher than their actual knowledge, suggesting flaws in science communication tools.
In order to increase the public understanding of science, use of interactive data visualization dashboards is crucial to allow users to explore and modify data. A tool creating simple, easy to understand visuals is also necessary in order to quickly communicate the most important results
and relationships to others.

`Argus` is an R Shiny App to interactively visualize data across scenarios, parameters, and regions. It can accommodate data inputs of the user's choice or load a GCAM directory.
Features of the application include selecting which scenarios will run and choosing relevant parameters and regions of interest. The designated parameters can be viewed as line plots over time, categorical bar graphs, maps broken apart by region, or as a table.
All results can be downloaded onto a local machine, or the user can obtain a bookmark to save their work.

![`Argus` landing page](figure1.png)

`Argus` can be accessed via this [Home Page](https://jgcri.github.io/argus/index.html). We provide an R vignette guide walking users through `Argus` and its features, which is accessible here: [User Guide](https://jgcri.github.io/argus/articles/vignette_argus.html)

# Statement of need
There has often been cosnidered a "gap" or "distance" between science and the public, which can be problematic since democracies depend on educated citizens to make informed decisions for their own lives and the lives of others. 
Public understanding of science is even more important today when citizens are regularly exposed to contradicting information on complex topics through various media sources [@sinatra2016pus].
Data from a five-country survey suggests that scientists agree it is essential to create communication between scientists and the public, but they are also not convinced of the public's ability to understand scientific findings and participate in decision-making about research policy [@peters2013gap].
In order to create a better relationship between science and the media, the gap between internal science communication and public science communication needs to be narrowed, meaning a special effort must be made in communicating scientific research finding to those creating policies and making decisions. 
One method of effectively communicating scientific findings is through data visualization tools.
Visual communication helps highlight important data and interactions, and it helps clearly communicate the most significant aspects in a short amount of time [@otten2015infographics]. This can be especially helpful for media interactions and relaying information to the public.
Visual and interactive learning tools are also very effective in formal education. In an experiment comparing university students' comprehension of material either with or without the aid of online interactive learning tools, the final grade distribution found 90 percent of students using online learning tools to receive a passing grade,
while only 70 percent of students received a passing grade without the aid of interactive tools [@mcintyre1998www].

Interactive data visualization dashboards are becoming widely used across several fields of study to analyze large, complex data sets. They have an advantage over static applications because they allow the user to choose their information and formatting to be the most useful for communication and decision making [@janvrin2014idv].
The purpose of the development of `Argus` was to create a widely accessible and easy to use interactive platform to help create data visualizations that are simple and easily understood by the public. It not only includes features allowing the user to customize their visualizations based on needs,
but it also allows users to share the platform with others so they themselves can interact with the data.

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

Finally, the application is continually being developed to support the needs of its users, and some of the features described may change. Additionally, new features are likely to be included in future releases.
Updated documentation of all features and their functionality will always be available in the [User Guide](https://jgcri.github.io/argus/articles/vignette_argus.html).

# Acknowledgements
The research described in this paper was conducted under the Laboratory Directed Research and Development Program at Pacific Northwest National Laboratory, a multiprogram national laboratory operated by Battelle for the U.S. Department of Energy. 
The views and opinions expressed in this paper are those of the authors alone.

# References
