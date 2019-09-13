[![DOI](https://zenodo.org/badge/133033505.svg)](https://zenodo.org/badge/latestdoi/133033505)

# Supporting data and code for: Insecticide resistance and fitness cost in *Bemisia tabaci* (Hemiptera: Aleyrodidae) invasive and resident species in La Réunion Island

*The data and R code used for the related study. This study is a part of the phD thesis of Alizée Taquet.* 

![alt_text](https://xuopmw.db.files.1drv.com/y4mmBsydCI6BUdvygvSlPIAmIrPLoghf_x33qWhWKiPlDOKpS-GyyH7aQWgXA0Dou0CgV5fvmV6OTWGJZOvmzMrX_wJCj9ceO__eJCVUoaQi3VyBS45TNtDpb_KI1uVgcardjHfRSi0DRWlZqjnZGXbCMg2BwSc9KgPTX1zW9MTiVrGWXj2fv40nsjKjrGuotKJQK1m7OvYXgYbDdjP_TggQQ?width=1584&height=588&cropmode=none)


## Context
Whitefly species are among the most difficult pest to control. Some withefly species have an exceptional evolutionary potential that has led to repeated evolution of resistance against most registered insecticides. In La Réunion, an island of the South West Indian Ocean, three whitefly species coexist, two of which are predominant, the indigenous Indian Ocean (IO) and the invasive Middle East Asia Minor 1 (MEAM1) species. Here we assess the resistance level against two insecticides (plenum and supreme, active substance pymetrozine and acetamipride, respectively) of these two species with very different evolutionnary trajectories. We also investigated the effect of resistance to acetamiprid on life history traits related to fitness in the MEAM1 species. This repository describe the datasets used in the study as well as the R scripts necessary for the statistical analyses. 

## Datasets
The datasets used in the study can be found in the "dataset" folder. 

The first dataset contains the results of the dose-response experiments. Each line depict the results for one population of one species at one concentration of one pesticide. 
+ insecticides-mortality.txt
  + *population_ID*: population ID
  + *species*: the Bemisia tabaci species name (either IO or MEAM1)
  + *environment_type*: the type of environment in which the population was sampled (Non-cultivated, Open field, Field surroundings, Greenhouse or Laboratory)
  + *pesticide*: the name of the pesticide tested (either plenum or supreme)
  + *modality*: either 'control' when there is no pesticide or 'dose'
  + *dose*: concentration of the pesticide (mg/L)
  + *repetition_ID*: an ID for the different repetition for the same population
  + *alive*: number of individuals alive at the end of the test
  + *dead*: number of dead individuals at the end of the test
  + *total*: the total number of individuals tested

The second dataset contains most of the results of the fitness experiments. These tests were made on 7 field populations and one reference laboratory population of the MEAM1 *Bemisia tabaci* species. Each line correspond to a repetition consisting of one adult female fecondated by a male, put in a clip-cage. 
+ traits-egg-larvae.txt
  + *pop_ID*: population ID
  + *life_expect*: number of days the female survived in the clip-cage. Mortality was only recorded every 3 days 
  + *egg_tot*: total number of eggs laid over the entire life
  + *repetition*: the ID of the repetition for a specific population
  + *plant*: ID of the coton plant used for the repetition
  + *eggs*: the number of eggs laid over the first three days period of the experiment
  + *adults*: number of second generation adults produced during the first three days period of the experiment
  + *larvae*: number of larvae produced during the first three days period of the experiment
  + *hatch_rate*: the proportion of eggs produced during the first three days period that has successfully hatched
  + *larval_surv_rate*: the proportion of eggs produced during the first three days period that has lead to an adult
  + *DLpop*: the estimate of the median lethal dose of the related population

The third dataset contains the specific results of the female size trait. For each population (7 field populations and one reference laboratory population of the MEAM1 *Bemisia tabaci* species), 20 females were measured from the tip of the head to the end of the abdomen. 
+ female-size.txt
  + *pop_ID*: population ID
  + *size*: size of the individual from the tip of the head to the end of the abdomen in millimeter
  + *DLpop*: the estimate of the median lethal dose of the related population

## R scripts
+ **load_bemisia_data.R**: the script to load the different datasets and the necessary package in the environment
+ **bioassays_drcanalysis.R**: this script is used to estimate the 50% lethal dose for the different experiments and to compare the different LC50 of the different populations
+ **fitness_analysis.R**: this script was used to analyze the data on the impact of insecticide resistance on the fitness of *Bemisia tabaci* MEAM1 populations. 


## Authors
Alizée Taquet wrote the first draft of **bioassays_drcanalysis.R** script with the help of Benoit Barrès and Hélène Jourdan-Pineau. Hélène Jourdan-Pineau wrote the **fitness_analysis.R** script. Benoit Barrès edited, homogenized and commented the final version of the code.


## Citation
You will hopefully soon be able to cite the related study as follow: 
+ Taquet A., Delatte H., Barrès B., Simiand C., Grondin M. and Jourdan-Pineau H. [Insecticide resistance and fitness cost in *Bemisia tabaci* (Hemiptera: Aleyrodidae) invasive and resident species in La Réunion Island. *submitted*.](to be completed)

If you want to use (some of) the code found on this page or if you want to cite this repository: 
+ Alizée Taquet, Hélène Jourdan-Pineau and Benoit Barrès. bbarres/bemisiaR: [Supporting data and code for: Insecticide resistance and fitness cost in *Bemisia tabaci* (Hemiptera: Aleyrodidae) invasive and resident species in La Réunion Island. Zenodo; 2019.](https://zenodo.org/badge/latestdoi/133033505)
