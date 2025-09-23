### classification and regression trees (CART)

## to download this file, use: https://charlesctaylor.github.io/MATH1700/learn.r

library(rpart) # "rpart" = Recursive PARTitioning
library(MASS) # datasets (and more) used in Modern Applied Statistics using Splus (Ripley)


## example of regression tree ##
################################

r1=rpart(medv~.,data=Boston)  # rpart() is main function (in library(rpart)) for CART 
r1 # see tree on screen
## variabes used mean (in each district):
# rm - avergae number of rooms per dwelling
# lstat - lower status of population (percent)
# crim - per capita crime rate
# dis - distance to unemployment centres
# medv - median value of property - target variable
plot(r1); text(r1) # plot tree
par(xpd = NA) # maybe necessary to avoid clipping text in the plot
### try to understand all this

### can compare to multiple linear regression
lm1=lm(medv~.,data=Boston)
summary(lm1)
# RSS as criterion for performance
sum((predict(r1)-Boston$medv)^2); sum((predict(lm1)-Boston$medv)^2)
matplot(cbind(predict(r1),predict(lm1)),cbind(residuals(r1),residuals(lm1)))

## example of classification tree ##
####################################

r2=rpart(type~.,data=Pima.tr)  ## rpart() can be used for classification (and survival) as well
r2
plot(r2); text(r2)
table(predict(r2,type="class"),Pima.tr$type) # confusion matrix
mean(predict(r2,type="class")==Pima.tr$type) # proportion correctly classified
### try to understand all this



























Aids2                   Australian AIDS Survival Data
Animals                 Brain and Body Weights for 28 Species
Boston                  Housing Values in Suburbs of Boston
Cars93                  Data from 93 Cars on Sale in the USA in 1993
Cushings                Diagnostic Tests on Patients with Cushing's Syndrome
DDT                     DDT in Kale
GAGurine                Level of GAG in Urine of Children
Insurance               Numbers of Car Insurance claims
Melanoma                Survival from Malignant Melanoma
OME                     Tests of Auditory Perception in Children with OME
Pima.tr                 Diabetes in Pima Indian Women
Rabbit                  Blood Pressure in Rabbits
Rubber                  Accelerated Testing of Tyre Rubber
SP500                   Returns of the Standard and Poors 500
Sitka                   Growth Curves for Sitka Spruce Trees in 1988
Sitka89                 Growth Curves for Sitka Spruce Trees in 1989
Skye                    AFM Compositions of Aphyric Skye Lavas
Traffic                 Effect of Swedish Speed Limits on Accidents
UScereal                Nutritional and Marketing Information on US Cereals
UScrime                 The Effect of Punishment Regimes on Crime Rates
VA                      Veteran's Administration Lung Cancer Trial
abbey                   Determinations of Nickel Content
accdeaths               Accidental Deaths in the US 1973-1978
anorexia                Anorexia Data on Weight Change
bacteria                Presence of Bacteria after Drug Treatments
beav1                   Body Temperature Series of Beaver 1
beav2                   Body Temperature Series of Beaver 2
biopsy                  Biopsy Data on Breast Cancer Patients
birthwt                 Risk Factors Associated with Low Infant Birth Weight
cabbages                Data from a cabbage field trial
caith                   Colours of Eyes and Hair of People in Caithness
cats                    Anatomical Data from Domestic Cats
cement                  Heat Evolved by Setting Cements
chem                    Copper in Wholemeal Flour
coop                    Co-operative Trial in Analytical Chemistry
cpus                    Performance of Computer CPUs
crabs                   Morphological Measurements on Leptograpsus Crabs
deaths                  Monthly Deaths from Lung Diseases in the UK
drivers                 Deaths of Car Drivers in Great Britain 1969-84
eagles                  Foraging Ecology of Bald Eagles
epil                    Seizure Counts for Epileptics
farms                   Ecological Factors in Farm Management
fgl                     Measurements of Forensic Glass Fragments
forbes                  Forbes' Data on Boiling Points in the Alps
galaxies                Velocities for 82 Galaxies
gehan                   Remission Times of Leukaemia Patients
genotype                Rat Genotype Data
geyser                  Old Faithful Geyser Data
gilgais                 Line Transect of Soil in Gilgai Territory
hills                   Record Times in Scottish Hill Races
housing                 Frequency Table from a Copenhagen Housing Conditions 
                          Survey
immer                   Yields from a Barley Field Trial
leuk                    Survival Times and White Blood Counts for
                          Leukaemia Patients
mammals                 Brain and Body Weights for 62 Species of Land Mammals
mcycle                  Data from a Simulated Motorcycle Accident
menarche                Age of Menarche in Warsaw
michelson               Michelson's Speed of Light Data
minn38                  Minnesota High School Graduates of 1938
motors                  Accelerated Life Testing of Motorettes
muscle                  Effect of Calcium Chloride on Muscle Contraction
                          in Rat Hearts
newcomb                 Newcomb's Measurements of the Passage Time of Light
nlschools               Eighth-Grade Pupils in the Netherlands
npk                     Classical N, P, K Factorial Experiment
npr1                    US Naval Petroleum Reserve No. 1 data
oats                    Data from an Oats Field Trial
painters                The Painter's Data of de Piles
petrol                  N. L. Prater's Petrol Refinery Data
phones                  Belgium Phone Calls 1950-1973
quine                   Absenteeism from School in Rural New South Wales
road                    Road Accident Deaths in US States
rotifer                 Numbers of Rotifers by Fluid Density
ships                   Ships Damage Data
shoes                   Shoe wear data of Box, Hunter and Hunter
shrimp                  Percentage of Shrimp in Shrimp Cocktail
shuttle                 Space Shuttle Autolander Problem
snails                  Snail Mortality Data
steam                   The Saturated Steam Pressure Data
stormer                 The Stormer Viscometer Data
survey                  Student Survey Data
synth.tr                Synthetic Classification Problem
topo                    Spatial Topographic Data
waders                  Counts of Waders at 15 Sites in South Africa
whiteside               House Insulation: Whiteside's Data
wtloss                  Weight Loss Data from an Obese Patient

