# Outlier Detection in Sensor Data using Functional Depth Measures

This is the final project of the OSE data science course (summer semester 2021) by Jakob R. Jürgens.
![ci](https://github.com/OpenSourceEconomics/ose-data-science-course-projeect-JakobJuergens/workflows/ci.yml/badge.svg)

## Project overview

This project was made possible by a cooperation with Daimler AG. The main focus is on outlier classification in functional data.
The algorithm implemented in Project_main.ipynb was taken from: 

Febrero, M., Galeano, P. and González-Manteiga, W. (2008), Outlier detection in functional data by depth measures, with application to identify abnormal NOx levels. 
Environmetrics, 19: 331-345. https://doi.org/10.1002/env.878

The method is explained an implemented in the Jupyter Notebook Project_Main.ipynb, which can be accessed using the following badges. All visualizations are done using a shiny app, that is separate from the Notebook.

<a href="https://nbviewer.jupyter.org/github/OpenSourceEconomics/ose-data-science-course-projeect-JakobJuergens/blob/master/Project_Main.ipynb"
   target="_parent">
   <img align="center"
  src="https://raw.githubusercontent.com/jupyter/design/master/logos/Badges/nbviewer_badge.png"
      width="109" height="20">
</a>

[![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/OpenSourceEconomics/ose-data-science-course-projeect-JakobJuergens/master?filepath=Project_Main.ipynb)

This shiny app can be started using this badge:

RShiny: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/OpenSourceEconomics/ose-data-science-course-projeect-JakobJuergens/master?urlpath=shiny/visual/)

## Acknowledgements
I want to thank a few people that either helped in making this project possible or aided me in some other way.
* Vincent Dekker and Bernhard Häußler, my contacts at Daimler AG who made this project possible
* Prof. Philipp Eisenhauer and Manuel Huth, who made this cooperation possible from the Universities' side
* Marc Kerstan, who allowed me to use his code for preparing sensor data collected in production processes at Daimler
* Prof. Dominik Liebl, who introduced me to the concept of using functional depth to identify outliers in functional data

## Structure of notebook

A typical project notebook has the following structure:

* presentation of baseline article with proper citation and brief summary

* using causal graphs to illustrate the authors' identification strategy

* replication of selected key results

* critical assessment of quality

* independent contribution, e.g. additional external evidence, robustness checks, visualization

There might be good reason to deviate from this structure. If so, please simply document your reasoning and go ahead. Please use the opportunity to review other student projects for some inspirations as well.
