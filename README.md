# EMA-mindfulness

Exploring the effects of mindfulness and positive fantasizing on depression and rumination (from a network perspective).

Thesis at the University of Groningen (MSc. Computational Cognitive Science, 2020 - 2022)

### Prerequisits

```
Python, version 3.6+

```

### File structure

All relevant R-scripts and subfolders are located in the Data folder.

##### ESM data
ESM data and files associated with it (e.g. the questionnaire used) are located in the ESM folder. The actual data and the preprocessed_data (created by the preprocessing_ESM R-script) are in the mindcog_[date] subfolder.

##### SART data
The different SART files (games, numbers, questions) are located in the SART folder. The sart_w_thoughtProbes.csv file is also stored there (created by the preprocessing_SART R-script).

##### R-scripts
The preprocessing_ESM / _SART scripts should be run first. After that, the descriptive_ESM / _SART scripts can be run. The ESM-SART_merge script merges the ESM and SART data (with the SART results being aggregated per session and day).

Regressions can be run via the gams R-script. Note: This file has not been cleaned up yet.

The hierarchicalClustering script was used to determine multicolinearity. The results were used to decide on a subset of ESM items to use in the analyses.

Networks are created in the network_analysis_final script. Note: This file has not been cleaned up yet.

##### created functions

NetworkPermutationTest is a large function that either tests a network's stability or compares to networks. It is based on permutation testing.

common_plot_theme was created to ensure consistency in figure appearance.

##### Figures
All figures are placed in the Figures subfolder.

### Authors

Clemens M Kaiser - [Linkedin](https://www.linkedin.com/in/clemens-kaiser-702713164/)

### Acknowledgements

- Marieke van Vugt and Marlijn Besten, the supervisors for this project, for mentoring my project, giving me guidance

