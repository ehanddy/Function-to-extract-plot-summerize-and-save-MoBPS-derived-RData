# Function-to-extract-plot-summerize-and-save-MoBPS-derived-RData

# Applied Effective R in Animal breeding - Summer semester 2021 Project
# Integrated plant and animal breeding (iPAB) - University of Göttingen

## Title: [Function to extract RData from MOBPS, produce plots and HTML summary based on the extracted data, and provide options to save the raw data.]{.ul}

## Introduction

The function do_plot() load RData from runs of Modular Breeding Program Simulator (MOBPS), extract the necessary information of breeding value, generations, time, traits, cohorts, repeats, population data and inbreeding estimation. It will present plots, with possibility to save data and deliver html summary.

It will accept as an input an rdata from MoBPS. It allows its users to plot True Breeding Value box plot, Average Breeding Value line plots, development of inbreeding across time and generations, options to save the data and options to get html summary. The program will guide its user at every step. The user should normally type the given option numbers for the program to output the desired result.

The code chunks are divided in to the following 7 parts:

1.  Data extraction and initialization

2.  Data separation based on respective traits to a data frame in a list

3.  Calculation of Average Breeding Value and saving to the respective trait data frame in a list

4.  Dialogue based user interface to control desired output and guide the user

5.  Data visualization using ggplot2 employing the interactive functionality of plotly package, to plot:

    1.  Box plots of True breeding values, with options to plot for all cohorts or a selected cohorts
    2.  Line plots of Average breeding values, with options to plot for all cohorts or a selected cohorts
    3.  Line plots showing the development of inbreeding across time and across generations

6.  Options to save data to computer either to automatically created folder under working directory, or to user defined path

7.  Option to get an HTML summary or over view of the simulated breeding program, including tables and plots

The function is able to handle errors that arise either from the user such as when typing the wrong number and wrong file path or errors that arise from supplying wrong file type. In addition, if something failed the function at least try to give the minimum output that survived the cash, for instance in case of supplying MOBPS RData, which has unexpected structure that could cause a potential crash. I observed such behavior of RData when testing the file, gtesfay_cattleDairy.RData. The other five RData I used as input to test the function didn't cause observable unexpected behavior.
