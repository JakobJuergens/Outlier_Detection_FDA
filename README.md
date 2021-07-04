# Outlier Detection in Functional Data using functional depth measures

This is the final project of the OSE data science course (summer semester 2021) by Jakob R. Jürgens.

## Project overview

This project was made possible by a cooperation with Daimler AG. The main focus is on outlier classification in functional data.
The algorithm implemented in Project_main.ipynb was taken from: 

Febrero, M., Galeano, P. and González-Manteiga, W. (2008), Outlier detection in functional data by depth measures, with application to identify abnormal NOx levels. 
Environmetrics, 19: 331-345. https://doi.org/10.1002/env.878


<a href="https://nbviewer.jupyter.org/github/OpenSourceEconomics/ose-data-science-course-projeect-JakobJuergens/blob/master/Project_Main.ipynb"
   target="_parent">
   <img align="center"
  src="https://raw.githubusercontent.com/jupyter/design/master/logos/Badges/nbviewer_badge.png"
      width="109" height="20">
</a>

[![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/OpenSourceEconomics/ose-data-science-course-projeect-JakobJuergens/master?filepath=Project_Main.ipynb)

If you want to start the visualization click here:

RShiny: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/OpenSourceEconomics/ose-data-science-course-projeect-JakobJuergens/master?urlpath=shiny/visual/)

## Acknowledgements
Special thanks to Marc Kerstan, who allowed me to use his code for preparing sensor data collected in production processes at Daimler.

## Reproducibility

To ensure full reproducibility of your project, please try to set up a [GitHub Actions CI](https://docs.github.com/en/actions) as your continuous integration service. An introductory tutorial for [conda](https://conda.io) and [GitHub Actions](https://docs.github.com/en/actions/learn-github-actions/introduction-to-github-actions) is provided [here](https://github.com/OpenSourceEconomics/ose-template-course-project/blob/master/tutorial_conda_actions.ipynb). While not at all mandatory, setting up a proper continuous integration workflow is an extra credit that can improve the final grade.

![Continuous Integration](https://github.com/OpenSourceEconomics/ose-template-course-project/workflows/Continuous%20Integration/badge.svg)

In some cases you might not be able to run parts of your code on  [GitHub Actions CI](https://docs.github.com/en/actions) as, for example, the computation of results takes multiple hours. In those cases you can add the result in a file to your repository and load it in the notebook. See below for an example code.

```python
# If we are running on GitHub Actions CI we will simply load a file with existing results.
if os.environ.get("CI") == "true":
  rslt = pkl.load(open('stored_results.pkl', 'br'))
else:
  rslt = compute_results()

# Now we are ready for further processing.
...
```

However, if you decide to do so, please be sure to provide an explanation in your notebook explaining why exactly this is required in your case.

## Structure of notebook

A typical project notebook has the following structure:

* presentation of baseline article with proper citation and brief summary

* using causal graphs to illustrate the authors' identification strategy

* replication of selected key results

* critical assessment of quality

* independent contribution, e.g. additional external evidence, robustness checks, visualization

There might be good reason to deviate from this structure. If so, please simply document your reasoning and go ahead. Please use the opportunity to review other student projects for some inspirations as well.
