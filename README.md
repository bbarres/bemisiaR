[![DOI](https://zenodo.org/badge/133033505.svg)](https://zenodo.org/badge/latestdoi/133033505)

# Supporting data and code for: Insecticide resistance and fitness cost in *Bemisia tabaci* (Hemiptera: Aleyrodidae) invasive and resident species in La Réunion Island

Some of the R code used for Alizée's analysis on bemisia resistance tests

![alt_text](https://xuopmw.db.files.1drv.com/y4mmBsydCI6BUdvygvSlPIAmIrPLoghf_x33qWhWKiPlDOKpS-GyyH7aQWgXA0Dou0CgV5fvmV6OTWGJZOvmzMrX_wJCj9ceO__eJCVUoaQi3VyBS45TNtDpb_KI1uVgcardjHfRSi0DRWlZqjnZGXbCMg2BwSc9KgPTX1zW9MTiVrGWXj2fv40nsjKjrGuotKJQK1m7OvYXgYbDdjP_TggQQ?width=1584&height=588&cropmode=none)


## Context


## Datasets
The datasets used in the study can be found in the "dataset" folder. 

The first dataset contains the results of the dose-response experiments. Each line depict the results for one population of one species at one concentration of one pesticide. 
+ insecticides-mortality.txt
  + *population_ID*: population ID
  + *species*: the Bemisia tabaci species name (either IO or MEAM1)
  + *environment_type*: the type of environment the population was sampled from
  + *pesticide*: the name of the pesticide tested (either plenum or supreme)
  + *modality*: either 'control' when there is no pesticide or 'dose'
  + *dose*: concentration of the pesticide (mg/L)
  + *repetition_ID*: an ID for the different repetition for the same population
  + *alive*: number of individuals alive at the end of the test
  + *dead*: number of dead individuals at the end of the test
  + *total*: the total number of individuals tested

The second dataset contains the results of the fitness experiments


## R scripts
+ **load_bemisia_data.R**: the script to load the different datasets and the necessary package in the environment
+ **bioassays_drcanalysis.R**: this script is used to estimate the 50% lethal dose for the different experiments and to compare the different LC50 of the different populations
+ **fitness_analysis.R**: 


## Authors



## Citation
You can cite the related study as follow : 
+ Taquet A., Delatte H., Barrès B., Simiand C., Grondin M. and Jourdan H. Insecticide resistance and fitness cost in *Bemisia tabaci* (Hemiptera: Aleyrodidae) invasive and resident species in La Réunion Island. *submitted*. [[link]](https://onlinelibrary.wiley.com/doi/abs/10.1002/ps.5461)

If you want to use (some of) the code found on this page or if you want to cite this repository : 
+ Alizée Taquet, Hélène Jourdan and Benoit Barrès. bbarres/bemisiaR: [Supporting data and code for: Insecticide resistance and fitness cost in *Bemisia tabaci* (Hemiptera: Aleyrodidae) invasive and resident species in La Réunion Island. Zenodo; 2019.](https://zenodo.org/badge/latestdoi/133033505)

