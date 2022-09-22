
######################################################################################
######################################################################################
### Version 2.0                                                                    ###  
### Script for the paper: "Exploring the role of ecology and social organisation   ###
###                        in agropastoral societies: A Bayesian network approach" ###
### authored by Olga Palacios, Juan Antonio Barceló and Rosario Delgado.           ###
######################################################################################
######################################################################################

######################################################################################
### REMOVE PREVIOUS OBJECTS AND LOAD R LIBRARIES                                   ###
######################################################################################

rm(list=ls())
 
library(readxl)   # function read_excel to load data
library(arules)
library(plyr)     # function revalue for pre-processing

## For Exploratory Data Analysis (EDA):
library(visdat)  # function vis_miss (NAs visualization)
library(naniar)  # function gg_miss_var (counting NAs per variable)

library(gplots)  # for plots (balloonplots)
library(DescTools)  # for the Cramer V

## For the k-fold cross-validation:

library(caret)  # for function createFolds. Required packages: lattice and ggplot2
# library(ggplot2)
# library(lattice)

## For construction and inference with Bayesian networks:
library(bnlearn)  # Bayesian networks learning
library(gRain)    # Exact inference with Bayesian networks

# if (! requireNamespace ("BiocManager", quietly = TRUE))
#   install.packages ("BiocManager")
# BiocManager :: install ("Rgraphviz")
# BiocManager :: install ("RBGL")
library(Rgraphviz)  # To plot the DAGs of the Bayesian networks



### LOAD DATA

# setwd("")   # set working directory
data=as.data.frame(read_excel("S2_Dataset.xlsx",col_names = TRUE))
str(data)

######################################################################################
#### PRE-PROCESSING                                                                ###
######################################################################################

## Delete variables we do not used

data$Society <- NULL # delate names of case studies
data$Region <- NULL

## All variables are "character". We convert some to "numeric", 
## to later discretize them

numeric<-c("Distance_coast","Elevation","Slope",
           "Annual_Temperature","Variation_Temperature",
           "Monthly_Precipitation","Variation_Precipitation",
           "Monthly_Productivity","Variance_Productivity")

for (i in numeric) data[[i]] <- as.numeric(data[[i]])

##  Introduce new variables: annual coefficient of variation for temperature, 
##  precipitation and productivity (to be used instead of variation)

data$CV_Annual_Temperature<-sqrt(data$Variation_Temperature)/abs(data$Annual_Temperature)
data$CV_Annual_Precipitation<-sqrt(data$Variation_Precipitation)/abs(data$Monthly_Precipitation*12)
data$CV_Annual_Productivity<-sqrt(data$Variance_Productivity)/abs(data$Monthly_Productivity*12)

## Dicretization and categorization of numeric variables
## The work data frame will be "d" 

d <- data[,-c(2:10, 31:33)]

d$Distance_coast<-arules::discretize(data$Distance_coast,method="fixed",
                                     breaks=c(0, 10, 100, 2000),
                                     labels = c("Short_distance", "Mid_distance", "Long_distance"))

d$Elevation<-arules::discretize(data$Elevation,method="fixed",
                                breaks=c(0, 300, 1000, 5000),
                                labels = c("Low", "Medium", "High"))

d$Slope<-arules::discretize(data$Slope,method="fixed",
                            breaks=c(0, 0.75, 2.5, 2000),
                            labels = c("Low", "Medium", "High"))

d$Annual_Temperature<-arules::discretize(data$Annual_Temperature,method="fixed",
                                         breaks=c(-13, 5, 20, 30),
                                         labels = c("Low", "Medium", "High"))

d$CV_Annual_Temperature<-arules::discretize(data$CV_Annual_Temperature,method="fixed",
                                            breaks=c(0.0100, 0.05, 0.15,  2.5),
                                            labels = c("Low", "Medium", "High"))

d$Monthly_Precipitation<-arules::discretize(data$Monthly_Precipitation,method="fixed",
                                            breaks=c(0, 95000, 130000, 1850000),
                                            labels = c("Low", "Medium", "High"))

d$CV_Annual_Precipitation<-arules::discretize(data$CV_Annual_Precipitation,method="fixed",
                                              breaks=c(-0.01, 0.06, 0.08, 1),
                                              labels = c("Low", "Medium", "High"))

d$Monthly_Productivity<-arules::discretize(data$Monthly_Productivity,method="fixed",
                                           breaks=c(-0.200, 1, 3, 4.5),
                                           labels = c("Low", "Medium", "High"))

d$CV_Annual_Productivity<-arules::discretize(data$CV_Annual_Productivity,method="fixed",
                                             breaks=c(0.01, 0.03, 0.05, 1),
                                             labels = c("Low", "Medium", "High"))

str(d)

## Revalue categories of the rest of variables:

d$Hunting<-revalue(d$Hunting, c("0-5"= "None",
                                "6-15"= "<25",
                                "16-25"= "<25",
                                "26-35"= ">=25",
                                "36-45"= ">=25")) 

d$Gathering<-revalue(d$Gathering, c("0-5"= "None",
                                    "6-15"= "<25",
                                    "16-25"= "<25",
                                    "26-35"= ">=25")) 

d$Fishing<-revalue(d$Fishing, c("0-5"= "None",
                                "6-15"= "<25",
                                "16-25"= "<25",
                                "26-35"= ">=25",
                                "36-45"= ">=25",
                                "46-55"= ">=25")) 

d$Animal_husbandry<-revalue(d$Animal_husbandry, c("0-5"= "None",
                                                  "6-15"= "<25",
                                                  "16-25"= "<25",
                                                  "26-35"= ">=25",
                                                  "36-45"= ">=25",
                                                  "46-55"= ">=25",
                                                  "56-65"= ">=25",
                                                  "66-75"= ">=25", 
                                                  "76-85"= ">=25",
                                                  "86-100"= ">=25")) 

d$Agriculture<-revalue(d$Agriculture, c("0-5"= "None",
                                        "6-15"= "<55",
                                        "16-25"= "<55",
                                        "26-35"= "<55",
                                        "36-45"= "<55",
                                        "46-55"= "<55",
                                        "56-65"= ">=55",
                                        "66-75"= ">=55",
                                        "76-85"= ">=55",
                                        "86-100"= ">=55")) 

d$Community_size<-revalue(d$Community_size, c("<50"= "<200",
                                              "50-99"= "<200",
                                              "100-199"= "<200",
                                              "200-399"= ">=200",
                                              ">400"= ">=200"))

d$Landscape<-revalue(d$Landscape, c("Tropical & Subtropical Dry Broadleaf Forests"= "Forest",
                                    "Tropical & Subtropical Moist Broadleaf Forests"= "Forest",
                                    "Tropical & Subtropical Moist Broadleaf Forest"= "Forest",
                                    "Tropical & Subtropical Coniferous Forests"= "Forest",
                                    "Temperate Broadleaf & Mixed Forests"= "Forest",
                                    "Temperate Conifer Forests"= "Forest",
                                    "Boreal Forests/Taiga"= "Forest",
                                    "Mediterranean Forests, Woodlands & Scrub"= "Forest",
                                    "Tropical & Subtropical Grasslands, Savannas & Shrublands"= "Grassland",
                                    "Temperate Grasslands, Savannas & Shrublands"= "Grassland",
                                    "Flooded Grasslands & Savannas"= "Grassland",
                                    "Shrublands"= "Grassland",
                                    "Montane Grasslands & Shrublands"= "Grassland",
                                    "Ice"= "Aquatic",
                                    "Inland Water"= "Aquatic",
                                    "Deserts & Xeric Shrublands"= "Desert")) 


d$Settlement_types<- factor (d$Settlement_types, 
                             levels= c("Camp", "Homesteads", "Hamlet", "Village"))

d$Community_size<- factor (d$Community_size, levels= c("<200", ">=200"))

d$Community_organisation<- factor (d$Community_organisation, 
                                   levels= c("No exogamous clans", "Clans"))

d$Household_organisation<- factor (d$Household_organisation, 
                                   levels= c("Nuclear", "Small extended", "Large extended"))


summary(d)  

d <- lapply(d, factor) # all the variables of type "factor"
str(d) 

names.d<-names(d)

d<-as.data.frame(d) # convert to dataframe
apply(d, 2, table)  # have a look to the frequency tables 

#save("")  #if we want to save the dataset in .RData format


######################################################################################
#### EXPLORATORY DATA ANALYSIS                                                     ###
######################################################################################

## Visualizing NAs per variable

vis_miss(d, sort_miss = TRUE) # plot of NAs
gg_miss_var(d)  # NAs per variable

## Relationship between pairs of variables:
## For each pair of variables, we explore their relationship 
## with the joint frequency table, the balloonplot representation, and the Cramer V.
## For example:

elevation_husbandry <- table(d$Elevation,d$Animal_husbandry)
balloonplot(t(elevation_husbandry), main ="Elevation vs Animal husbandry", 
            xlab ="Intensity of Animal husbandry", ylab="Elevation",
            label = FALSE, show.margins = FALSE)
elevation_husbandry_matrix <- as.matrix (elevation_husbandry)
CramerV(elevation_husbandry_matrix,conf.level=0.95)


## NAs are coded as "Unknown" 
## since bnlearn can not deal with NAs

nas<-vector() # any NA per variable
for (i in 1:dim(d)[2])
{nas[i]<-sum(is.na(d[[i]]))}
nas   # how many NAs per variable
names.d[nas>0]  # names of variables with NAs

for (i in 1:dim(d)[2])
{ if (nas[i]!=0)
{levels(d[[i]])<-c(levels(d[[i]]),"Unknown")
d[[i]][which(is.na(d[[i]])==TRUE)]<-"Unknown"
}
}

str(d)
anyNA(d)


######################################################################################
###  INTRODUCING SCENARIOS                                                         ###
######################################################################################

# For summarizing the scenarios that we explore, we name them:
# scenario i: Ecological factors constrain the subsistence strategy 
#           (Input: environmental characteristics / Output: subsistence strategies)
# scenario ii: Ecological factors constrain the way the community is socially organised
#           (Input: environmental characteristics / Output: social organisation)
# scenario iii: Ecological factors constrain social/economic decisions made by the community 
#           (Input: environmental characteristics / Output: social decisions)
# scenario iv: Social organisation constrains the dominant form of subsistence strategy adopted 
#           (Input: social organisation / Output: subsistence strategies)
# scenario v: Type of social organisation and the subsistence strategy  constrain social/economic decisions made by the community
#           (Input: social organisation and subsistence strategies / Output: social decisions)

## Input sets of variables
input_scenario_i_ii_iii<-c("Landscape","Distance_coast","Elevation", "Slope", 
                           "Annual_Temperature","CV_Annual_Temperature",
                           "Monthly_Precipitation","CV_Annual_Precipitation",
                           "Monthly_Productivity","CV_Annual_Productivity")  # Ecology
numbers_input_scenario_i_ii_iii<-which(names.d %in% input_scenario_i_ii_iii)


input_scenario_iv<-c("Community_size","Settlement_types",
                     "Community_organisation","Household_organisation") # Social organisation
numbers_input_scenario_iv<-which(names.d %in% input_scenario_iv)


input_scenario_v<-c(input_scenario_iv,c("Hunting","Gathering","Animal_husbandry","Fishing","Agriculture"))
# Social organisation + Subsistence strategies
numbers_input_scenario_v<-which(names.d %in% input_scenario_v)

list.inputs.scenarios<-list()
list.inputs.scenarios[[1]]<-input_scenario_i_ii_iii
list.inputs.scenarios[[2]]<-input_scenario_i_ii_iii
list.inputs.scenarios[[3]]<-input_scenario_i_ii_iii
list.inputs.scenarios[[4]]<-input_scenario_iv
list.inputs.scenarios[[5]]<-input_scenario_v


## Output sets of variable
output_scenario_i_iv<-c("Hunting","Gathering","Animal_husbandry","Fishing","Agriculture")
# Subsistence strategies
numbers_output_scenario_i_iv<-which(names.d %in% output_scenario_i_iv)

output_scenario_ii<-input_scenario_iv  # Social organisation
numbers_output_scenario_ii<-which(names.d %in% output_scenario_ii)

output_scenario_iii_v<-c("None","Resource_diversification","Crop_specialisation",
                         "Foraging_intensification","Storage","Transhumance",
                         "Temporal_migration","Permanent_migration",
                         "Exchange_outsettlement","Exchange_insettlement","Reciprocity")
# Social decisions
numbers_output_scenario_iii_v<-which(names.d %in% output_scenario_iii_v)


list.outputs.scenarios<-list()
list.outputs.scenarios[[1]]<-output_scenario_i_iv
list.outputs.scenarios[[2]]<-output_scenario_ii
list.outputs.scenarios[[3]]<-output_scenario_iii_v
list.outputs.scenarios[[4]]<-output_scenario_i_iv
list.outputs.scenarios[[5]]<-output_scenario_iii_v


## Subset of the dataset corresponding to the variables at each scenario

data_scenario_i<-d[,c(numbers_input_scenario_i_ii_iii,numbers_output_scenario_i_iv)]
data_scenario_ii<-d[,c(numbers_input_scenario_i_ii_iii,numbers_output_scenario_ii)]
data_scenario_iii<-d[,c(numbers_input_scenario_i_ii_iii,numbers_output_scenario_iii_v)]
data_scenario_iv<-d[,c(numbers_input_scenario_iv,numbers_output_scenario_i_iv)]
data_scenario_v<-d[,c(numbers_input_scenario_v,numbers_output_scenario_iii_v)]

list.data.scenarios<-list()
list.data.scenarios[[1]]<-data_scenario_i
list.data.scenarios[[2]]<-data_scenario_ii
list.data.scenarios[[3]]<-data_scenario_iii
list.data.scenarios[[4]]<-data_scenario_iv
list.data.scenarios[[5]]<-data_scenario_v

######################################################################################
###  K-FOLD CROSS VALIDATION                                                       ###
######################################################################################

### Training and validation data frames for the k-fold cross-validation. 
### The same for all the models

k=5  # number of folds used for validating the model with k-fold cross-validation
# the folds are the same for all the models (partition of the instances)
set.seed(123)  # random seed
Folds <- createFolds(d$Agriculture, k=k, list = TRUE, returnTrain = FALSE)
Folds

training<-NULL  # i=scenario (1:5), f=division in folds (1:k)
validation<-NULL
for (i in 1:5)  # for any scenario
{
  training[[i]]<-list()
  validation[[i]]<-list()
  for (f in 1:k)  # for any division in folds
  {
    validation[[i]][[f]]<-list.data.scenarios[[i]][Folds[[f]],]
    training[[i]][[f]]<-list.data.scenarios[[i]][-Folds[[f]],]
  }
}


######################################################################################
### A-NB AND A-ANB MODELS                                                          ###
######################################################################################

########################################
### CONSTRUCTION OF A-NB (NAIVE BAYES) # 
########################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.

###  White-list for the A-NB and A-ANB models (from output to any of the inputs)
wl.A<-NULL
for (i in 1:5)
{wl.A[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]]))
{
  wl.A[[i]][[j]]=data.frame(from=list.outputs.scenarios[[i]][j], to=list.inputs.scenarios[[i]])
}}


###  Black-list for the A-NB model (between the inputs). NO blacklist for the A-ANB model
bl.A.NB<-NULL
for (i in 1:5)
{bl.A.NB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]]))
{bl.A.NB[[i]][[j]]<-data.frame(expand.grid(from = list.inputs.scenarios[[i]], 
                                                                  to = list.inputs.scenarios[[i]]))
}
}


### Function that gives the A.NB model learned with bnlearn, in gRain format
Model.A.NB<-function(train,white,black)
{
  x <- bnlearn::hc(train, score = 'bic', whitelist = white, blacklist = black)
  xx <- bnlearn::bn.fit(x, train, method = 'mle',replace.unidentifiable=TRUE)
  xxx <- suppressWarnings(bnlearn::as.grain(xx))
  return(xxx)
}

### Function that generates the plot of the A.NB model 
Model.A.NB.plot<-function(train,white,black)
{
  x <- bnlearn::hc(train, score = 'bic', whitelist = white, blacklist = black)
  y <- bnlearn::graphviz.plot(x,layout="dot",shape="ellipse")  # uses Rgraphviz package
  return(y)
}


### A.NB is the list of A.NB models.
### A.NB[[i]][[j]][[f]] is the A.NB model for the scenario i=1,...,5, for the 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### and for the division in folds f=1,...,k=5
A.NB<-list()
for (i in 1:5)   # i=1,...,5 scenario
  {A.NB[[i]]<-list()
   for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                    # is the output, each time.
   {A.NB[[i]][[j]]<-list()
    for (f in 1:k) # for any division in folds
    {A.NB[[i]][[j]][[f]]<-Model.A.NB(training[[i]][[f]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                                     wl.A[[i]][[j]],bl.A.NB[[i]][[j]])}
   }
  }


### If we want to save the plots of the A.NB models to reproduce them latter, for any
### scenario i, output variable j (of the scenario i), and division in folds f, we use:
png(filename=paste("A_NB_scenario","_",i,"_output_",j,"_fold_",f,".png"),width=800,height=600)
Model.A.NB.plot(training[[i]][[f]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                wl.A[[i]][[j]],bl.A.NB[[i]][[j]])

dev.off()
file.show(paste("A_NB_scenario","_",i,"_output_",j,"_fold_",f,".png"))
###


###################################################
## CONSTRUCTION OF A-ANB (AUGMENTED NAIVE BAYES)  #
###################################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time


### Function that gives the A.ANB model learned with bnlearn in gRain format
Model.A.ANB<-function(train,white)
{
  x <- bnlearn::hc(train, score = 'bic', whitelist = white)
  xx <- bnlearn::bn.fit(x, train, method = 'mle',replace.unidentifiable=TRUE)
  xxx <- suppressWarnings(bnlearn::as.grain(xx))
  return(xxx)
}

### Function that generates the plot of the A.NB model 
Model.A.ANB.plot<-function(train,white)
{
  x <- bnlearn::hc(train, score = 'bic', whitelist = white)
  y <- bnlearn::graphviz.plot(x,layout="dot",shape="ellipse") # uses Rgraphviz package
  return(y)
}


### A.ANB is the list of A.ANB models.
### A.ANB[[i]][[j]][[f]] is the A.ANB model for the scenario i=1,...,5, for the 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### and for the division in folds f=1,...,k=5
A.ANB<-list()
for (i in 1:5)   # i=1,...,5 scenario
{A.ANB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{A.ANB[[i]][[j]]<-list()
for (f in 1:k) # for any division in folds
{A.ANB[[i]][[j]][[f]]<-Model.A.ANB(training[[i]][[f]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                                 wl.A[[i]][[j]])}
}
}


### If we want to save the plots of the A.ANB models to reproduce them latter, for any
### scenario i, output variable j (of the scenario i), and division in folds f, we use:
png(filename=paste("A_ANB_scenario","_",i,"_output_",j,"_fold_",f,".png"),width=800,height=600)
Model.A.ANB.plot(training[[i]][[f]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                wl.A[[i]][[j]])

dev.off()
file.show(paste("A_ANB_scenario","_",i,"_output_",j,"_fold_",f,".png"))
###


#######################################################################################
### FUNCTION FOR THE PREDICTION OF THE OUTPUT FROM THE INPUTS, FOR THE INSTANCES    ###
### OF THE VALIDATION SET, GIVEN A BAYESIAN NETWORK MODEL (in gRain format)         ###
### (also useful for prediction of one single case from the evidence on the inputs) ###
### Note that prediction can not be "Unknown".                                      ###
#######################################################################################

Predictions<-function(output,input,model,validation)
{
  pred<-vector()
  for (m in 1:dim(validation)[1])
  {if (is.null(predict(model, response = output, validation[m, ], 
                       predictors = input, type = 'class')$pred[[1]])==TRUE) 
  {pred[m]<-NA} else {
    if (predict(model, response = output, validation[m, ], 
                predictors = input, type = 'class')$pred[[1]]=="Unknown") 
    {pred[m]<-levels(d[[output]])[which.max(predict(model, response = output, validation[m, ], 
              predictors = input, type = 'dist')$pred[[1]][,-length(levels(d[[output]]))])]} else {
    pred[m]<-predict(model, response = output, validation[m, ], 
                     predictors = input, type = 'class')$pred[[1]]}
  }
  }
  return(pred)
}


#######################################################
### CONFUSION MATRICES FOR MODEL A-NB (NAIVE BAYES) ###
### FOR k-FOLD CROSS-VALIDATION, with k = 5         ###
#######################################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.
###

### List with the predictions for the instances of the validation sets, with models A.NB
### Pred.A.NB[[i]][[j]][[f]] are the predictions with the A.NB model for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5
Pred.A.NB<-list()
for (i in 1:5)   # i=1,...,5 scenario
{Pred.A.NB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{Pred.A.NB[[i]][[j]]<-list()
for (f in 1:k) # for any division in folds
{Pred.A.NB[[i]][[j]][[f]]<-Predictions(list.outputs.scenarios[[i]][j],
                                       list.inputs.scenarios[[i]],
                                       A.NB[[i]][[j]][[f]],
                                       validation[[i]][[f]])

}
}
}

#############
### This function completes matrix A with zeroes if needed in rows or columns, to make
### a square matrix with the corresponding (complete) names of rows and columns: labels
############

square.matrix <- function(A,labels){  # A=matrix, labels = desired names of rows(=names of columns)
  r<-length(labels)
  B<-matrix(rep(0,r*r),ncol=r)
  
  for (x in 1:r)
  {for (y in 1:r)
  { if (sum(rownames(A)==labels[x])*sum(colnames(A)==labels[y])==1)
  {B[x,y]<-A[which(rownames(A)==labels[x]),which(colnames(A)==labels[y])]}
  }
  }
  
  rownames(B)<-labels
  colnames(B)<-labels
  
  return(B)
} 


### List with the confusion matrices (completed with zeroes if needed, and without 
### rows or columns = "Unknown") with models A.NB
### Confus.A.NB[[i]][[j]][[f]] are the confusion matrices with the A.NB model for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5

Confus.A.NB<-list() 
for (i in 1:5)   # i=1,...,5 scenario
{Confus.A.NB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{Confus.A.NB[[i]][[j]]<-list()
for (f in 1:k) # for any division in folds
{Confus.A.NB[[i]][[j]][[f]]<-as.matrix(table(Pred.A.NB[[i]][[j]][[f]],
                          validation[[i]][[f]][[list.outputs.scenarios[[i]][[j]]]]))
labels<-levels(d[[list.outputs.scenarios[[i]][[j]]]])
labels<-labels[which(labels!="Unknown")] 
Confus.A.NB[[i]][[j]][[f]]<-square.matrix(Confus.A.NB[[i]][[j]][[f]],labels)
}
}
}


##################################################################
### CONFUSION MATRICES FOR MODEL A-ANB (AUGMENTED NAIVE BAYES) ###
### FOR k-FOLD CROSS-VALIDATION, with k = 5                    ###
##################################################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.
###


### List with the predictions for the instances of the validation sets, with models A.ANB
### Pred.A.ANB[[i]][[j]][[f]] are the predictions with the A.NB model for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5
Pred.A.ANB<-list()
for (i in 1:5)   # i=1,...,5 scenario
{Pred.A.ANB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{Pred.A.ANB[[i]][[j]]<-list()
for (f in 1:k) # for any division in folds
{Pred.A.ANB[[i]][[j]][[f]]<-Predictions(list.outputs.scenarios[[i]][j],
                                       list.inputs.scenarios[[i]],
                                       A.ANB[[i]][[j]][[f]],
                                       validation[[i]][[f]])

}
}
}


### List with the confusion matrices (completed with zeroes if needed, and without 
### rows or columns = "Unknown") with models A.ANB
### Confus.A.ANB[[i]][[j]][[f]] are the confusion matrices with the A.ANB model for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5

Confus.A.ANB<-list() 
for (i in 1:5)   # i=1,...,5 scenario
{Confus.A.ANB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{Confus.A.ANB[[i]][[j]]<-list()
for (f in 1:k) # for any division in folds
{Confus.A.ANB[[i]][[j]][[f]]<-as.matrix(table(Pred.A.ANB[[i]][[j]][[f]],
                                             validation[[i]][[f]][[list.outputs.scenarios[[i]][[j]]]]))
labels<-levels(d[[list.outputs.scenarios[[i]][[j]]]])
labels<-labels[which(labels!="Unknown")] 
Confus.A.ANB[[i]][[j]][[f]]<-square.matrix(Confus.A.ANB[[i]][[j]][[f]],labels)
}
}
}


###############################################
### ACCURACY FOR MODEL A-NB (NAIVE BAYES)   ###
### FROM THE CONFUSION MATRICES             ###
###############################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.
###               f=1,...,k=5 is the division in folds

### List with the accuracy values with models A.NB
### Accuracy.A.NB[[i]][[j]][f] are the accuracy values with the A.NB model,for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5
###
### Mean.accuracy.A.NB[[i]][j] are the mean of the k accuracy values, for any i and j
### CI.accuracy.A.NB[[i]][[j]] are the two limits of the confidence level (95%) for the
### accuracy mean
### Error.accuracy.A.NB[[i]][j] are the errors of estimate accuracy by its mean value (confidence 95%)
### for any scenario i, and output variable j

Accuracy.A.NB<-list() 
Mean.accuracy.A.NB<-list()
CI.accuracy.A.NB<-list()
Error.accuracy.A.NB<-list()

for (i in 1:5)   # i=1,...,5 scenario
{Accuracy.A.NB[[i]]<-list()
 Mean.accuracy.A.NB[[i]]<-vector()
 CI.accuracy.A.NB[[i]]<-list()
 Error.accuracy.A.NB[[i]]<-vector()
 
 for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{Accuracy.A.NB[[i]][[j]]<-vector()
 CI.accuracy.A.NB[[i]][[j]]<-vector()
 
 for (f in 1:k) # for any division in folds
{Accuracy.A.NB[[i]][[j]][f]<-sum(diag(Confus.A.NB[[i]][[j]][[f]]))/sum(Confus.A.NB[[i]][[j]][[f]])
}

Mean.accuracy.A.NB[[i]][j]<-mean(Accuracy.A.NB[[i]][[j]])

z<-t.test(Accuracy.A.NB[[i]][[j]],alternative="greater",mu=0,conf.level=0.95) # confidence interval
CI.accuracy.A.NB[[i]][[j]]<-c(max(0,z$conf.int[1]),min(1,z$conf.int[2]))

Error.accuracy.A.NB[[i]][j]<- max(Mean.accuracy.A.NB[[i]][j]-CI.accuracy.A.NB[[i]][[j]][1],
                                  CI.accuracy.A.NB[[i]][[j]][2]-Mean.accuracy.A.NB[[i]][j])

}
}

#########################################################
### ACCURACY FOR MODEL A-ANB (AUGMENTE NAIVE BAYES)   ###
### FROM THE CONFUSION MATRICES                       ###
#########################################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.
###               f=1,...,k=5 is the division in folds



### List with the accuracy values with models A.ANB
### Accuracy.A.ANB[[i]][[j]][f] are the accuracy values with the A.ANB model,for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5
###
### Mean.accuracy.A.ANB[[i]][j] are the mean of the k accuracy values, for any i and j
### CI.accuracy.A.ANB[[i]][[j]] are the two limits of the confidence level (95%) for the
### accuracy mean
### Error.accuracy.A.ANB[[i]][j] are the errors of estimate accuracy by its mean value (confidence 95%)
### for any scenario i, and output variable j

Accuracy.A.ANB<-list() 
Mean.accuracy.A.ANB<-list()
CI.accuracy.A.ANB<-list()
Error.accuracy.A.ANB<-list()

for (i in 1:5)   # i=1,...,5 scenario
{Accuracy.A.ANB[[i]]<-list()
Mean.accuracy.A.ANB[[i]]<-vector()
CI.accuracy.A.ANB[[i]]<-list()
Error.accuracy.A.ANB[[i]]<-vector()

for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{Accuracy.A.ANB[[i]][[j]]<-vector()
CI.accuracy.A.ANB[[i]][[j]]<-vector()

for (f in 1:k) # for any division in folds
{Accuracy.A.ANB[[i]][[j]][f]<-sum(diag(Confus.A.ANB[[i]][[j]][[f]]))/sum(Confus.A.ANB[[i]][[j]][[f]])
}

Mean.accuracy.A.ANB[[i]][j]<-mean(Accuracy.A.ANB[[i]][[j]])

z<-t.test(Accuracy.A.ANB[[i]][[j]],alternative="greater",mu=0,conf.level=0.95) # confidence interval
CI.accuracy.A.ANB[[i]][[j]]<-c(max(0,z$conf.int[1]),min(1,z$conf.int[2]))

Error.accuracy.A.ANB[[i]][j]<- max(Mean.accuracy.A.ANB[[i]][j]-CI.accuracy.A.ANB[[i]][[j]][1],
                                  CI.accuracy.A.ANB[[i]][[j]][2]-Mean.accuracy.A.ANB[[i]][j])

}
}


###############################################
### COMPARISON OF THE ACCURACY VALUES       ###
### MODEL A-NB VS MODEL A-ANB               ###
###############################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.


### p-values of the Shapiro test of normality to check normality 
### of the difference of accuracy values between
### the A-NB and the A-ANB models, for any scenario i, and output j. 
### p-value = 1 means that both vector of accuracy values are identical. 
### normality is rejected if p-value < 0.05

p.value.shapiro.NB.ANB<-list()
for (i in 1:5)   # i=1,...,5 scenario
{p.value.shapiro.NB.ANB[[i]]<-rep(1,length(list.outputs.scenarios[[i]]))
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{if (all.equal(Accuracy.A.NB[[i]][[j]]-Accuracy.A.ANB[[i]][[j]],rep(0,k))!=TRUE)
  {p.value.shapiro.NB.ANB[[i]][j]<-shapiro.test(Accuracy.A.NB[[i]][[j]]-Accuracy.A.ANB[[i]][[j]])$p.value}
}
}


### p-values of the paired t-test or Wilcoxon test to compare
### the accuracy values of the A-NB and the A-ANB models, for any scenario i, and output j. 
### p-value = 1 means that both vector of accuracy values are identical. 
### Mean accuracy of A-NB is significantly greater than that of A-ANB if p.value.NB.greater.ANB<0.05
### Mean accuracy of A-NB is significantly less than that of A-ANB if p.value.NB.less.ANB<0.05
### Otherwise: no significant differences

p.value.NB.greater.ANB<-list()
p.value.NB.less.ANB<-list()
for (i in 1:5)   # i=1,...,5 scenario
{p.value.NB.greater.ANB[[i]]<-vector()
p.value.NB.less.ANB[[i]]<-vector()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the output, each time.
{
  # assing test.type<- 1 if Wilcoxon, <-2 if t.test, <-3 if both vector of accuracy values are identical
if (p.value.shapiro.NB.ANB[[i]][j]<0.05)
{test.type<-1} else {
  if (all.equal(Accuracy.A.NB[[i]][[j]]-Accuracy.A.ANB[[i]][[j]],rep(0,k))!=TRUE)
  {test.type<-2} else {test.type<-3}
}

if (test.type==1)
{p.value.NB.greater.ANB[[i]][j]<-wilcox.test(Accuracy.A.NB[[i]][[j]],Accuracy.A.ANB[[i]][[j]],
                                             paired=TRUE,alternative="greater")$p.value 
p.value.NB.less.ANB[[i]][j]<-wilcox.test(Accuracy.A.NB[[i]][[j]],Accuracy.A.ANB[[i]][[j]],
                                         paired=TRUE,alternative="less")$p.value 
}

if (test.type==2)
{p.value.NB.greater.ANB[[i]][j]<-t.test(Accuracy.A.NB[[i]][[j]],Accuracy.A.ANB[[i]][[j]],
                                        paired=TRUE,alternative="greater")$p.value 
p.value.NB.less.ANB[[i]][j]<-t.test(Accuracy.A.NB[[i]][[j]],Accuracy.A.ANB[[i]][[j]],
                                    paired=TRUE,alternative="less")$p.value 
}
  
if (test.type==3)
{p.value.NB.greater.ANB[[i]][j]<-1
p.value.NB.less.ANB[[i]][j]<-1
} 
}
}

################################################################
####   RESULTS COMPARATIVE ACCURACY A-NB AND A-ANB MODELS:   ###
p.value.NB.greater.ANB                                       ###
p.value.NB.less.ANB                                          ###
### For Table S6                                             ###
################################################################

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


######################################################################################
### B-MODEL                                                                        ###
######################################################################################

########################################
### CONSTRUCTION OF B-MODEL            # 
########################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is any of the outputs


###  Black-list for the B model (from inputs to outputs). NO whitelist for the B model
bl.B<-list()
for (i in 1:5)
{bl.B[[i]]<-data.frame(expand.grid(from = list.inputs.scenarios[[i]], 
                                           to = list.outputs.scenarios[[i]]))
}

### Function that gives the B model learned with bnlearn, in gRain format
Model.B<-function(train,black)
{
  x <- bnlearn::hc(train, score = 'bic', blacklist = black)
  xx <- bnlearn::bn.fit(x, train, method = 'mle',replace.unidentifiable=TRUE)
  xxx <- suppressWarnings(bnlearn::as.grain(xx))
  return(xxx)
}

### Function that generates the plot of the A.NB model 
Model.B.plot<-function(train,black)
{
  x <- bnlearn::hc(train, score = 'bic', blacklist = black)
  y <- bnlearn::graphviz.plot(x,layout="dot",shape="ellipse")
  return(y)
}


### B.Model is the list of B models.
### B.Model[[i]][[f]] is the B model for the scenario i=1,...,5, for 
### the division in folds f=1,...,k=5
B.Model<-list()
for (i in 1:5)   # i=1,...,5 scenario
{B.Model[[i]]<-list()
for (f in 1:k) # for any division in folds
{B.Model[[i]][[f]]<-Model.B(training[[i]][[f]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]])],
                                 bl.B[[i]])}
}



### If we want to save the plots of the B models to reproduce them latter, for any
### scenario i and division in folds f, we use:
png(filename=paste("B_Model_scenario","_",i,"_fold_",f,".png"),width=800,height=600)
Model.B.plot(training[[i]][[f]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]])],
                bl.B[[i]])

dev.off()
file.show(paste("B_Model_scenario","_",i,"_fold_",f,".png"))
###


#######################################################
### CONFUSION MATRICES FOR MODEL B                  ###
### FOR k-FOLD CROSS-VALIDATION, with k = 5         ###
#######################################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.
###               f=1,...,k=5 is the division in folds

### List with the predictions for the instances of the validation sets, with models B
### Pred.B.Model[[i]][[j]][[f]] are the predictions with the B model for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5
Pred.B.Model<-list()
for (i in 1:5)   # i=1,...,5 scenario
{Pred.B.Model[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{Pred.B.Model[[i]][[j]]<-list()
for (f in 1:k) # for any division in folds
{Pred.B.Model[[i]][[j]][[f]]<-Predictions(list.outputs.scenarios[[i]][j],
                                       list.inputs.scenarios[[i]],
                                       B.Model[[i]][[f]],
                                       validation[[i]][[f]])

}
}
}


### List with the confusion matrices (completed with zeroes if needed, and without 
### rows or columns = "Unknown") with models B
### Confus.B.Model[[i]][[j]][[f]] are the confusion matrices with the B model for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5

Confus.B.Model<-list() 
for (i in 1:5)   # i=1,...,5 scenario
{Confus.B.Model[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{Confus.B.Model[[i]][[j]]<-list()
for (f in 1:k) # for any division in folds
{Confus.B.Model[[i]][[j]][[f]]<-as.matrix(table(Pred.B.Model[[i]][[j]][[f]],
                                             validation[[i]][[f]][[list.outputs.scenarios[[i]][[j]]]]))
labels<-levels(d[[list.outputs.scenarios[[i]][[j]]]])
labels<-labels[which(labels!="Unknown")] 
Confus.B.Model[[i]][[j]][[f]]<-square.matrix(Confus.B.Model[[i]][[j]][[f]],labels)
}
}
}



###############################################
### ACCURACY FOR MODEL B                    ###
### FROM THE CONFUSION MATRICES             ###
###############################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.
###               f=1,...,k=5 is the division in folds


### List with the accuracy values with models B
### Accuracy.B.Model[[i]][[j]][f] are the accuracy values with the B model,for 
### scenario i=1,...,5, 
### output j=1,...,length(list.outputs.scenarios[[i]]), 
### division in folds f=1,...,k=5
###
### Mean.accuracy.B.Model[[i]][j] are the mean of the k accuracy values, for any i and j
### CI.accuracy.B.Model[[i]][[j]] are the two limits of the confidence level (95%) for the
### accuracy mean
### Error.accuracy.B.Model[[i]][j] are the errors of estimate accuracy by its mean value (confidence 95%)
### for any scenario i, and output variable j

Accuracy.B.Model<-list() 
Mean.accuracy.B.Model<-list()
CI.accuracy.B.Model<-list()
Error.accuracy.B.Model<-list()

for (i in 1:5)   # i=1,...,5 scenario
{Accuracy.B.Model[[i]]<-list()
Mean.accuracy.B.Model[[i]]<-vector()
CI.accuracy.B.Model[[i]]<-list()
Error.accuracy.B.Model[[i]]<-vector()

for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{Accuracy.B.Model[[i]][[j]]<-vector()
CI.accuracy.B.Model[[i]][[j]]<-vector()

for (f in 1:k) # for any division in folds
{Accuracy.B.Model[[i]][[j]][f]<-sum(diag(Confus.B.Model[[i]][[j]][[f]]))/sum(Confus.B.Model[[i]][[j]][[f]])
}

Mean.accuracy.B.Model[[i]][j]<-mean(Accuracy.B.Model[[i]][[j]])

z<-t.test(Accuracy.B.Model[[i]][[j]],alternative="greater",mu=0,conf.level=0.95) # confidence interval
CI.accuracy.B.Model[[i]][[j]]<-c(max(0,z$conf.int[1]),min(1,z$conf.int[2]))

Error.accuracy.B.Model[[i]][j]<- max(Mean.accuracy.B.Model[[i]][j]-CI.accuracy.B.Model[[i]][[j]][1],
                                  CI.accuracy.B.Model[[i]][[j]][2]-Mean.accuracy.B.Model[[i]][j])

}
}



###############################################
### COMPARISON OF THE ACCURACY VALUES       ###
### MODEL A-NB VS MODEL B                   ###
###############################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.

### p-values of the Shapiro test of normality to check normality 
### of the difference of accuracy values between
### the A-NB and the B models, for any scenario i, and output j. 
### p-value = 1 means that both vector of accuracy values are identical. 
### normality is rejected if p-value < 0.05

p.value.shapiro.NB.B.Model<-list()
for (i in 1:5)   # i=1,...,5 scenario
{p.value.shapiro.NB.B.Model[[i]]<-rep(1,length(list.outputs.scenarios[[i]]))
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{if (all.equal(Accuracy.A.NB[[i]][[j]]-Accuracy.B.Model[[i]][[j]],rep(0,k))!=TRUE)
{p.value.shapiro.NB.B.Model[[i]][j]<-shapiro.test(Accuracy.A.NB[[i]][[j]]-Accuracy.B.Model[[i]][[j]])$p.value}
}
}


### p-values of the paired t-test or Wilcoxon test to compare
### the accuracy values of the A-NB and the B models, for any scenario i, and output j. 
### p-value = 1 means that both vector of accuracy values are identical. 
### Mean accuracy of A-NB is significantly greater than that of B model if p.value.NB.greater.B.Model<0.05
### Mean accuracy of A-NB is significantly less than that of B model if p.value.NB.less.B.Model<0.05
### Otherwise: no significant differences

p.value.NB.greater.B.Model<-list()
p.value.NB.less.B.Model<-list()
for (i in 1:5)   # i=1,...,5 scenario
{p.value.NB.greater.B.Model[[i]]<-vector()
p.value.NB.less.B.Model[[i]]<-vector()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{
  # assign test.type= 1 if Wilcoxon, =2 if t.test, =3 if both vector of accuracy values are identical
  if (p.value.shapiro.NB.B.Model[[i]][j]<0.05)
  {test.type<-1} else {
    if (all.equal(Accuracy.A.NB[[i]][[j]]-Accuracy.B.Model[[i]][[j]],rep(0,k))!=TRUE)
    {test.type<-2} else {test.type<-3}
  }
  
  if (test.type==1)
  {p.value.NB.greater.B.Model[[i]][j]<-wilcox.test(Accuracy.A.NB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                                   paired=TRUE,alternative="greater")$p.value 
  p.value.NB.less.B.Model[[i]][j]<-wilcox.test(Accuracy.A.NB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                               paired=TRUE,alternative="less")$p.value 
  }
  
  if (test.type==2)
  {p.value.NB.greater.B.Model[[i]][j]<-t.test(Accuracy.A.NB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                              paired=TRUE,alternative="greater")$p.value 
  p.value.NB.less.B.Model[[i]][j]<-t.test(Accuracy.A.NB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                          paired=TRUE,alternative="less")$p.value 
  }
  
  if (test.type==3)
  {p.value.NB.greater.B.Model[[i]][j]<-1
  p.value.NB.less.B.Model[[i]][j]<-1
  } 
}
}


################################################################
####   RESULTS COMPARATIVE ACCURACY A-NB AND B MODELS:       ###
p.value.NB.greater.B.Model                                   ###
p.value.NB.less.B.Model                                      ###
### For Table S6                                             ###
################################################################


###############################################
### COMPARISON OF THE ACCURACY VALUES       ###
### MODEL A-ANB VS MODEL B                   ###
###############################################

### Index models: i=1,2,3,4,5 is the scenario
###               j=1,...,length(list.outputs.scenarios[[i]]) is the output, each time.


### p-values of the Shapiro test of normality to check normality 
### of the difference of accuracy values between
### the A-ANB and the B models, for any scenario i, and output j. 
### p-value = 1 means that both vector of accuracy values are identical. 
### normality is rejected if p-value < 0.05

p.value.shapiro.ANB.B.Model<-list()
for (i in 1:5)   # i=1,...,5 scenario
{p.value.shapiro.ANB.B.Model[[i]]<-rep(1,length(list.outputs.scenarios[[i]]))
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{if (all.equal(Accuracy.A.ANB[[i]][[j]]-Accuracy.B.Model[[i]][[j]],rep(0,k))!=TRUE)
{p.value.shapiro.ANB.B.Model[[i]][j]<-shapiro.test(Accuracy.A.ANB[[i]][[j]]-Accuracy.B.Model[[i]][[j]])$p.value}
}
}


### p-values of the paired t-test or Wilcoxon test to compare
### the accuracy values of the A-ANB and the B models, for any scenario i, and output j. 
### p-value = 1 means that both vector of accuracy values are identical. 
### Mean accuracy of A-ANB is significantly greater than that of B model if p.value.ANB.greater.B.Model<0.05
### Mean accuracy of A-ANB is significantly less than that of B model if p.value.ANB.less.B.Model<0.05
### Otherwise: no significant differences

p.value.ANB.greater.B.Model<-list()
p.value.ANB.less.B.Model<-list()
for (i in 1:5)   # i=1,...,5 scenario
{p.value.ANB.greater.B.Model[[i]]<-vector()
p.value.ANB.less.B.Model[[i]]<-vector()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{
  # assign test.type= 1 if Wilcoxon, =2 if t.test, =3 if both vector of accuracy values are identical
  if (p.value.shapiro.ANB.B.Model[[i]][j]<0.05)
  {test.type<-1} else {
    if (all.equal(Accuracy.A.ANB[[i]][[j]]-Accuracy.B.Model[[i]][[j]],rep(0,k))!=TRUE)
    {test.type<-2} else {test.type<-3}
  }
  
  if (test.type==1)
  {p.value.ANB.greater.B.Model[[i]][j]<-wilcox.test(Accuracy.A.ANB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                                    paired=TRUE,alternative="greater")$p.value 
  p.value.ANB.less.B.Model[[i]][j]<-wilcox.test(Accuracy.A.ANB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                                paired=TRUE,alternative="less")$p.value 
  }
  
  if (test.type==2)
  {p.value.ANB.greater.B.Model[[i]][j]<-t.test(Accuracy.A.ANB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                               paired=TRUE,alternative="greater")$p.value 
  p.value.ANB.less.B.Model[[i]][j]<-t.test(Accuracy.A.ANB[[i]][[j]],Accuracy.B.Model[[i]][[j]],
                                           paired=TRUE,alternative="less")$p.value 
  }
  
  if (test.type==3)
  {p.value.ANB.greater.B.Model[[i]][j]<-1
  p.value.ANB.less.B.Model[[i]][j]<-1
  } 
}
}


################################################################
####   RESULTS COMPARATIVE ACCURACY A-ANB AND B MODELS:      ###
p.value.ANB.greater.B.Model                                  ###
p.value.ANB.less.B.Model                                     ###
### For Table S6                                             ###
################################################################



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


######################################################################################
### ARC STRENGTH                                                                   ###
######################################################################################

### Function to obtain the arc strength for the A-NB model,
### learned from a train dataset

Arc.str.A.NB<-function(train,white,black)
{
  x <- bnlearn::hc(train, score = 'bic', whitelist = white,blacklist=black)
  h<-bnlearn::arc.strength(x,train,criterion="x2")
  return(h)
}

### Function to obtain the arc strength for the A-ANB model,
### learned from a train dataset

Arc.str.A.ANB<-function(train,white)
{
  x <- bnlearn::hc(train, score = 'bic', whitelist = white)
  h<-bnlearn::arc.strength(x,train,criterion="x2")
  return(h)
}

### Function to obtain the arc strength for the B-Model,
### learned from a train dataset

Arc.str.B.Model<-function(train,black)
{
  x <- bnlearn::hc(train, score = 'bic', blacklist = black)
  h<-bnlearn::arc.strength(x,train,criterion="x2")
  return(h)
}



### List with the strength of the probabilistic relationships between the output variable
### and any of the input variables, with the A-NB model, 
### learned from the whole dataset

Arc.strength.A.NB<-list() 
for (i in 1:5)   # i=1,...,5 scenario
{Arc.strength.A.NB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{Arc.strength.A.NB[[i]][[j]]<-Arc.str.A.NB(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                                           wl.A[[i]][[j]],bl.A.NB[[i]][[j]])}
}

### List with the strength of the probabilistic relationships between the output variable
### and any of the input variables, with the A-ANB model,
### learned from the whole dataset

Arc.strength.A.ANB<-list() 
for (i in 1:5)   # i=1,...,5 scenario
{Arc.strength.A.ANB[[i]]<-list()
for (j in 1:length(list.outputs.scenarios[[i]])) # j=1,...,length(list.outputs.scenarios[[i]]) 
                                                 # is the predicted output.
{Arc.strength.A.ANB[[i]][[j]]<-Arc.str.A.ANB(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                                             wl.A[[i]][[j]])}
}


### List with the strength of the probabilistic relationships between the output variable
### and any of the input variables, with the B-Model,
### learned from the whole dataset

Arc.strength.B.Model<-list() 
for (i in 1:5)   # i=1,...,5 scenario
{Arc.strength.B.Model[[i]]<-Arc.str.B.Model(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]])],
                                             bl.B[[i]])}




################################################
####   RESULTS ARC STRENGTH                  ###
Arc.strength.A.NB   # with the A-NB model    ###
Arc.strength.A.ANB   # with the A-ANB model  ###
Arc.strength.B.Model  # with the B-Model     ###
### For Table S7                             ### 
################################################



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


######################################################################################
### APPLICATION: N EXAMPLE                                                         ###
######################################################################################

############################################################################
### EXAMPLE: to predict the intensity of Agriculture                      ###
### in an hypothetical scenario with the different models                 ###
### Scenario i=1                                                          ###
### Output variable "Agriculture" (corresponds to j=5 in this scenario)   ###
#############################################################################

## EXAMPLE DATA TO BE INTRODUCED "BY HAND"
i=1 # scenario 1
j=which(list.outputs.scenarios[[i]]=="Agriculture") # j=5 

# Choose (uncomment) only the best model:
best<-"A.NB"
# best<-"A.ANB"
# best<-"B.Model"


evidence<-as.data.frame(cbind(t(rep("",length(list.inputs.scenarios[[i]])))))
colnames(evidence)<-list.inputs.scenarios[[i]]
evidence
evidence[1,1]<-"Forest"    # Landscape 
evidence[1,2]<-"Short_dist" # Distance_coast
evidence[1,3]<-"High"    # Elevation
evidence[1,4]<-"High"  # Slope
evidence[1,5]<-"Medium"    # Annual_Temperature
evidence[1,6]<-"Low"  # CV_Annual_Temperature
evidence[1,7]<-"High"    # Monthly_Precipitation
evidence[1,8]<-"Low"  # CV_Annual_Precipitation
evidence[1,9]<-"Medium"    # Monthly_Productivity
evidence[1,10]<-"Low"  # CV_Annual_Productivity
evidence

##### END OF EXAMPLE DATA TO BE INTRODUCED "BY HAND"

### Function that gives not only the prediction, but the confidence level (CL) of the prediction 
### for an output variable, given a model, from the input variables
### as predictors, and for a validation set (which could be an evidence). 

CL.prediction<-function(output,input,model,validation)
{
  pred<-vector()
  CL<-vector()
  for (m in 1:dim(validation)[1])
  {if (is.null(predict(model, response = output, validation[m, ], 
                       predictors = input, type = 'class')$pred[[1]])==TRUE) 
  {pred[m]<-NA
  CL[m]<-0} else {
    if (predict(model, response = output, validation[m, ], 
                predictors = input, type = 'class')$pred[[1]]=="Unknown") 
    {pred[m]<-levels(d[[output]])[which.max(predict(model, response = output, validation[m, ], 
                predictors = input, type = 'dist')$pred[[1]][,-length(levels(d[[output]]))])]
    CL[m]<-round(max(predict(model, response = output, validation[m, ], 
                             predictors = input, type = 'dist')$pred[[1]][,-length(levels(d[[output]]))]),5)
    } else {
    pred[m]<-predict(model, response = output, validation[m, ], 
            predictors = input, type = 'class')$pred[[1]]
    CL[m]<-round(max(predict(model, response = output, validation[m, ], 
                       predictors = input, type = 'dist')$pred[[1]]),5)
    
  }
  }
  }
  return(cbind(pred,CL))
}


### We now obtain the prediction and CL for "Agriculture", in the scenario 1, with
### the best model:

if (best=="A.NB")
{model<-Model.A.NB(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                                  wl.A[[i]][[j]],bl.A.NB[[i]][[j]])} 

if (best=="A.ANB")
{model<-Model.A.ANB(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
                                                                       wl.A[[i]][[j]])                             }

if (best=="B.Model")
{model<-Model.B(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]])],
                                  bl.B[[i]])}




Prediction.CL<-as.data.frame(CL.prediction(list.outputs.scenarios[[i]][j],
            list.inputs.scenarios[[i]],
            model,
            evidence))

Prediction.CL

### 

Result_example<-as.data.frame(rbind(" ", t(evidence)," ", " ", Prediction.CL[,1], Prediction.CL[,2]," ", best))
colnames(Result_example)<-c(" ")

bbb<-which(Result_example==" ")

rownames(Result_example)[bbb]<-c(paste("EVIDENCE, SCENARIO",i), " ", "  ","   ")

rownames(Result_example)[dim(Result_example)[1]-3]<- 
      paste("PREDICTION FOR",list.outputs.scenarios[[i]][j])
rownames(Result_example)[dim(Result_example)[1]-2]<-"CL OF THE PREDICTION"
rownames(Result_example)[dim(Result_example)[1]]<-"With the model"


################################################
####   RESULTS: PREDICTION IN THE EXAMPLE    ###
Result_example                               ###
################################################



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


######################################################################################
### PLOTS OF THE DIFFERENT MODELS (LEARNED FROM THE WHOLE DATASET)                 ###
######################################################################################

### For any scenario i=1,..., 5, we plot the corresponding B-Model: 

for (i in 1:5)
{
png(filename=paste("FINAL_B_Model_scenario","_",i,".png"),width=800,height=600)
Model.B.plot(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]])],
             bl.B[[i]])

dev.off()
file.show(paste("FINAL_B_Model_scenario","_",i,".png"))
}

### For any scenario i=1,..., 5, and any output j=1,...,length(list.outputs.scenarios[[i]]),
###  we plot the corresponding A-ANB model: 

for (i in 1:5)
{
  for (j in 1:length(list.outputs.scenarios[[i]]))
  {
    png(filename=paste("FINAL_A_ANB_scenario","_",i,"_output_",list.outputs.scenarios[[i]][j],".png"),width=800,height=600)
  Model.A.ANB.plot(list.data.scenarios[[i]][,c(list.inputs.scenarios[[i]],list.outputs.scenarios[[i]][j])],
               wl.A[[i]][[j]])
  
  dev.off()
  file.show(paste("FINAL_A_ANB_scenario","_",i,"_output_",list.outputs.scenarios[[i]][j],".png"))
  }
}



###########  THE END! #######################################

