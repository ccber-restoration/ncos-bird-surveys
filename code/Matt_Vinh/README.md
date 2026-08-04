# North Campus Open Space Bird Survey Data Exploration and Analysis
### Cheadle Center | [Matt Vinh](www.linkedin.com/in/matthew-vinh)

---

This subdirectory contains all the scripts and non-data outputs relevant to my exploration and analysis of Cheadle Center bird survey data for North Campus Open Space (NCOS) with R.

---

## Main Scripts

A script with an *italicized* title cannot be run as it exists currently.

**1.** *bird_surveys_compilation_and_cleaning.R*

This script can be found within the "dataset_scripts" folder. It compiles all of the bird surveys recorded in [Dryad](https://datadryad.org/dataset/doi:10.5061/dryad.bvq83bkhz) for NCOS, reads in .csv files and row binds them, cleans variable levels and species names and adds taxonomic data. This process yields all observation-level data recorded by Cheadle Center bird surveys between September 2017 and August 2025. The resulting data frame is saved as a .csv and an .rds for retention of data types.

The script cannot be run currently because of an update to the link to Dryad data.

**2.** Wright-Ueda_replication.qmd

Find this script within the "wright-ueda_paper" folder. It includes my replication of the work of Wright-Ueda et al. in their paper [Mixed population trends inside a California protected area: Evidence from long-term community science monitoring](https://onlinelibrary.wiley.com/doi/full/10.1111/ibi.13280) using bird survey data collected with the Cheadle Center at NCOS *and bird survey data sourced from Cornell eBird*.

#### Requirements

1. Download the [INLA package](https://www.r-inla.org/download/index.html)
2. Obtain survey-level presence-absence data

#### Running the script with Cheadle Center Data

*instructions for cloning repository?*

1. Install and/or load the libraries listed in the `setup` chunk.
2. Run lines 38 and 39 in the `setup` chunk to load the most recently cleaned Cheadle Center survey-level data.
3. Run the `Fitting INLA models` chunk. This will fit one INLA model per Cheadle Center species.
4. Run the `Calculating Average Annual Trend and Visualizing Results` chunk. This will calculate Average Annual Trends and plot the results.
- For more details about the INLA model specifications and the importance of the Average Annual Trend, see https://onlinelibrary.wiley.com/doi/full/10.1111/ibi.13280.

**3.** developing_report.qmd

A work in progress that includes a multitude of visualizations. This script facilitates my ongoing work and is unorganized. While I do not recommend trying to run anything, all necessary data files should be in the `setup` chunk at the top. Different visualizations require different formats of data - although some notes exist on what is required to run certain chunks, you will likely have to interpret code yourself to determine the data needed for successful execution.

---

## Data

Find data for use with the scripts above at https://github.com/ccber-restoration/ncos-bird-surveys/tree/main/data/Matt_Vinh.

---

## Other Subdirectory Contents

The remainder of the contents of this subdirectory consist of folders for organization of my work. These are generally less important.

1. archived

Old scripts documenting my exploration and analysis of Cheadle Center data. Grouped by month and year.
   
2. dataset_scripts

Scripts used to generate data found in the "Matt_Vinh" data subdirectory.

Instructions:
todo instructions

3. figures

Assorted figures generated during Cheadle Center data analysis.

4. gotelli_paper

Scratch scripts for the replication of the work of Gotelli et al. in their paper [Detecting Temporal Trends in Species  Assemblages with Bootstrapping Procedures and Hierarchical Models](https://pmc.ncbi.nlm.nih.gov/articles/PMC2981998/).

6. URCA_poster_figures

Contains figures used for my Spring 2026 Undergraduate Research and Creative Activities poster presentation. See https://escholarship.org/uc/item/9zz0r7s4 for more information about my poster.

7. wright-ueda_paper

Contains all of my work completed for the replication of Wright-Ueda modelling. See **Main Scripts** above for the most relevant contents.
