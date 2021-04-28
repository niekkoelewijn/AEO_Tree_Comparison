# AEO_Tree_Comparison

## Script for comparison of UAV and MLS datasets

Research project by Niek Koelewijn, MGI student at Wageningen University, for the course Advanced Earth Observation

In this script, pointclouds of trees, aqcuired using UAV and MLS systems, are loaded into R. A subset of the trees is created at 1.3 meter height, with a buffer of 5 cm, to calculate the DBH using the circular::lsfit.circle function. The minimal Z value is subtracted from the maximal Z value to determine the tree heights. At the end of the script, statistical analysis of the results can be found, as well as the visualizations.
