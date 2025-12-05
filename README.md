# The Freshwater Hotspots Documentation
This is an adapted version of the Hotspots pipeline suited for freshwater processing. While it returns a 
similar output, the nature of a freshwater network is sufficiently different that we need to create many
new scripts and functions in order to assign relevant environmental covariates and connectivity fields
to a freshwater network. 

The only data that is pulled directly from our original Hotspots pipeline is the species data, which is then 
filtered and processed in the intergation stage of this pipeline.

## Pipeline

### Import

The first folder here is labeled "import". It imports two of the three vital pieces of data you need to run
the pipeline. a) The environmental variables that you need to assign to rivers later on in the pipeline, and
b) the river network itself. SPecies data here is just imported directly from the Hotspots pipeline - 
you do not need to run new species data here.

#### Environmental import (riverEvironmentalImport.R)

This script calculates our environmental covariates on a catchment or river level. It depends on the user already
having downloaded the catchment area and river network data from Elvenett (download location in script). This
should be run first.

#### Water area creation (catchmentToWaterAreas.R)

This script defines our water areas that we will run models on for the pipeline. It can be run independently of the
environmental import script, results from the two won't meet until later on.

### Processing

#### Water area metric graph creation (catchmentGraphs.R)

This script works on a water area basis, and should be run in Sigma2. For each water area, it takes all relevant river segments, 
creates a metric graph object, and then assigns a mesh and creates initial integration points. These are then used in the
final model run.

#### River catchment matching (riverCatchmentMatching.R)

Rivers need to be assigned to the correct drainage fields in order to match the correct environmental covariates for
that drainage field to a river segment. However, a small eprcentage don't have a field pre-assigned. This script
assigns relevant drainage fields and their attached environment covariates to river segments, and completes
our environmental data file.

#### Water distance calculation (waterDistanceCalculation.R)

One of the key aspects of our species data processing script is removing observations which take place too far from the
waterbodies. For this, we need an approximation of how far each pixel in Norway is from a river or lake boundary, so we know which 
observations to remove. Note that while some observations are removed later on when the actual models are run,
we need to remove as many as possible using the water distance so that we know which species can be viably modelled in a 
given catchment.

#### Species data processing (speciesDataProcessing.R)

While our raw imported data depends on the original Hotspots pipeline (instructions for parameters below), the species data 
processing needs to undergo some specific changes here. It is sorted into a per water area list, with data per water area
and species that can be modelled per water area produced here. It also removes species that are too far from a lake
or river, and reshuffles the data for the new modelling approach.

### Modelling

#### Model preparation (freshwaterModelPreparation.R)

Unlike the regular Hotspots pipeline, model preparation for this version can be run locally. All this does is figure out
which species can be run in which catchments, and produces a segmentList, which tells R which species/water area combination 
to model. After this has been run, all data can eb rsynced to Sigma2.

#### Model run (freshwaterModelRun.R and freshwaterModelRunCounts.R)

This needs to be run on Sigma2. This produces a full model run, and does NOT save the model object, instead simply delivering
the likelihoods, covariate effects and posterior intensities from the model. Two arguments are required - the species group and 
the segment number. The segment number, much like the the first Hotspot pipeline, is delivered as part of an array.

Note that there are two versions of this script. One which is applied for count data  (so just the insectd ata at the moment)
and another for data with no counts involved.

### Output

#### Freshwater Output Processing (freshwaterOutputProcessing.R)

This script produces a full map of likelihoods for different species, and for richnesses across catchments. It also
produces a combined fixed effect analysis. It takes a while to run, so it's designed to be run from the command line.

#### Freshwater Output Processing (outputVisualisation.R)

This produces visualisations based on the output from the previous script. At the moment that includes richness, hotspot and 
uncertainty figures for 2 catchments.

## Data

The data storage for the freshwater repo is currently somewhat messier than the original Hotspots repo. Nevertheless, an
explanation of the structure can be found below.

### External

This folder holds all data that needs to be imported from external sources. It includes river networks and catchment
mapos from Elvenett, lake borders from Geonorge, as well as all our imported covariate data.

#### Covariates

All data that is improted when we run the code in environmentalImport. The folders are automatically created here, and the
rasters are all automatically saved here.

#### River Network

All data directly imported from Elvenett and REGINE is saved here. Intermediate data that is created on the way to creating 
our river network metric graph object is also here - the waterGraphs folders contain these.

#### Water distance

The water distance raster which give the average distance to a river/lake is found here, as is the geonorge data 
required toc reate it.

### Final data

Processed data which is ready to be fed into the model scripts is stored here. The head folder contains the input 
required to define a) which observations to feed into the model and b) which catchment/species combinations
to model.

### Species

All species relevant data can be found here. Processed datasets for each taxa (as produced by the normal Hotspots 
pipeline) can be found in rawData, the lists of species for each taxa can be found in taxaLists.

#### Model outputs

Here's where we save all the outputs from our models. They are first divided up into taxa names, then by catchments. 
Visualisations, richness estimates, and other outputs are all saved in the head folder. 

## Functions

A list of functions used regularly in the pipeline. Many have been imported straight from the regular Hotspots
pipeline, a few have minor adjustments made.
