# salt-modeling-data

This repository contains reproducible code for downloading, processing, and modeling data related to river salinization dynamics. Using [The Turing Ways's definitions](https://the-turing-way.netlify.app/reproducible-research/overview/overview-definitions), this code and analysis are intended to be *fully reproducible* and could be *somewhat replicable* with different states and/or dates. 

## Associated publications and resources

The code supports the analysis for Lindsay Platt's ([@lindsayplatt](https://github.com/lindsa%5D(https://github.com/lindsayplatt))) Master's Thesis, *Basin characteristics modulate signatures of river salinization*.

## Running the code 

This repository is setup as an automated pipeline using the [`targets` R package](https://books.ropensci.org/targets/) in order to orchestrate a complex, modular workflow where dependency tracking determines which components need to be built. As written, this pipeline will need about 2.5 hours to build and will need to have an internet connection.

The pipeline is broken into 6 different phases:

* `1_Download` contains all the code that pulls from NWIS, ScienceBase, NHD+, etc. It will require an internet connection. It also sets the spatial and temporal limits for the analysis.
* `2_Prepare` this phase is doing all of the heavy lifting to process data into a useable state for both our time series data (prefixed with `p2_ts`) and our static attributes data (prefixed with `p2_attr`).
* `3_Filter` applies all of our criteria to remove sites that are not up to our minimum standards and/or are sites with unexpected features (tidal, in a really high agricultural area, don't have an associated NHD+ catchment area, etc).
* `4_EpisodicSalinization` applies an algorithm that has been used to identify storms by finding steep peaks in a hydrograph to the specific conductance time series in order to identify winter storms where road salts are washed into streams and cause sharp peaks (similar to storm hydrographs). In the end, this phase identifies sites with specific conductance data that exhibit this episodic behavior.
* `5_BaseflowSalinization` is doing two different things: applying a baseflow separation algorithm to extract only the days in the specific conductance time series that occurred on a baseflow day, and then using a seasonal Mann-Kendall to evaluate whether the baseflow specific conductance is experiencing any sort of trend. In the end, this returns a table with each site and what the salinization trend was for the baseflow days (either `positive`, `none`, or `negative`).
* `6_DefineCharacteristics` uses the information gathered in `4_EpisodicSalinization` and `5_BaseflowSalinization` to categorize the sites based on whether they exhibit episodic salinization and/or they have positive trends in baseflow specific conductance. Then, it applies random forest models to these categorizations with the collection of static attributes prepared and filtered in `2_Prepare` and `3_Filter` to define the attributes and values that are important for determining a site's category.
* `7_Disseminate` takes all of the model input output to generate figures and explain the results. The figures generated in this phase were all used in the manuscript. Two datasets are also saved in this step and represent the final salinization signature classifications for each site and values for all 16 static attributes. These two datasets were used by the random forest models to create final results explaining which characteristics were important for each of the salinization signatures.

### Pipeline setup

Run the following command to make sure you have all the necessary packages before trying to build the pipeline.

``` r
install.packages(c(
    'targets', 
    'tarchetypes',
    'accelerometry',
    'arrow',
    'cowplot',
    'dataRetrieval',
    'EnvStats',
    'exactextractr',
    'FlowScreen',
    'GGally', 
    'httr',
    'MESS',
    'nhdplusTools',
    'pdp',
    'qs',
    'randomForest',
    'raster',
    'sbtools',
    'scico',
    'sf',
    'tidytext',
    'tidyverse',
    'units',
    'usmap',
    'yaml',
    'zip'
))
```

The following package versions were used during the original pipeline build. You shouldn't need to install these versions specifically, but if there are errors cropping up, you could try installing these specific versions.

|Package       |Version  | |:-------------|:--------| |targets       |1.5.1    | |tarchetypes   |0.7.12   | |accelerometry |3.1.2    | |arrow         |14.0.2.1 | |cowplot       |1.1.3    | |dataRetrieval |2.7.15   | |EnvStats      |2.8.1    | |exactextractr |0.10.0   | |FlowScreen    |1.2.6    | |GGally        |2.2.1    | |httr          |1.4.7    | |MESS          |0.5.12   | |nhdplusTools  |1.0.0    | |pdp           |0.8.1    | |qs            |0.25.7   | |randomForest  |4.7.1.1  | |raster        |3.6.26   | |sbtools       |1.3.1    | |scico         |1.5.0    | |sf            |1.0.15   | |tidytext      |0.4.1    | |tidyverse     |2.0.0    | |units         |0.8.5    | |usmap         |0.7.0    | |yaml          |2.3.8    | |zip           |2.3.1    |

### Pipeline build

To build this pipeline (after running the setup section), you should 

1. Open the `run_pipeline.R` script.
1. Click on the `Background Jobs` tab in RStudio (next to `Console` and `Terminal`).
1. Choose `Start Background Job` and make sure the `run_pipeline.R` script is selected.
1. Accept the defaults and click `Start` to kick off the pipeline build.

This will build the pipeline in the background, so that your RStudio session can still be used as the job is running.

### Pipeline outputs

Many of the pipeline's artifacts are "object targets" but there are some files created. As of 5/7/2024, the best way to see how the pipeline and analysis ran is to open the figures and data stored in  `7_Disseminate/out/`. This will only have built if all the other pipeline steps were successfully run. It contains all of the figures that appeared in the manuscript.
