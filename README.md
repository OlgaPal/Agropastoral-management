# Agropastoral-management #
This is a resource code from a script written by Olga Palacios, Juan Antonio Barceló and Rosario Delgado to conduct the experimental analyses of the research
article 'Exploring the role of ecology and social organisation in agropastoral societies: A Bayesian network approach' (currently under review).

The objective of this study is to explore and quantify the interaction of people and environment in small-scale farming societies. 

## Description
This code is written in R programming language and applied in the 'Dataset_S2", found in the Supporting Information of the research article. 
It does the following processes: 
- Data pre-processing and cleaning
- Recoding and discretization
- Construction of three structurally different Bayesian networks: of binary relevance (Model A - Naïve Bayes, Augmented Naïve Bayes-) and one of all outputs 
at once (Model B) for all the five scenarios explored in the article (with different outputs / inputs)
- Model validation through k-fold cross-validation of the models' accuracy
- Model comparison using statistical methods
- Design of the directed acyclic graphs (DAGs) of the three Bayesian network models for each scenario
- Quantification of the models arc strength 
- Construction of the final models with the complete dataset and prediction of outputs in hypothetical scenarios (defined in form of inputs values) with the level of confidence

## Getting started

### Usage 

#### Script Input: 
- Dataset
- Design of five scenarios exploring the relationship between different inputs/outputs
- Definition of the hypothetical scenarios used to predict the outputs 

#### Script Output: 
- p-values obtained in the k-fold cross-validation conducted as part of the model validation
- arc strengths, plots and predictions of the all models considering the five scenarios explored

### Packages
The following packages are used: arules, base, BiocGenerics, bnlearn, caret, datasets, DescTools, ggplot2, gplots, gRain, graph, graphics, gRbase, 
grDevices, grid, lattice, Matrix, methods, naniar, parallel, plyr, readxl, Rgraphviz, utils, visdat. 


## Support
Contact olga.palacios@uab.cat 

## License
Distributed under the MIT License. See LICENSE.txt for more information.
