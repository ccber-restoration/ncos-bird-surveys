# North Campus Open Space Bird Survey Data and Related Data

### Cheadle Center | [Matt Vinh](www.linkedin.com/in/matthew-vinh)

---

This branch contains all of the data files I have created from raw Cheadle Center bird survey data. Timespan from 2017-09 to 2025-08. All files are available in .csv and .rds format.

---

## Contents

1. `archived`

Documents previous iterations of data.

2. `SBA_hourly_precipitation`

Contains hourly precipitation data recorded by the SBA NOAA station. *todo: better details of data source?*

3. `compiled_and_cleaned`

A compilation of all bird survey recordings. Each row represents the documentation of one sighting during a Cheadle Center bird survey. Includes all default variables from the recording software.

4. `species-level`

Documents every species recorded at North Campus Open Space by Cheadle Center bird surveys. Each row includes a unique bird species, Cheadle Center bird survey related species-specific information and taxonomic information.

5. `survey_level`

Documents bird presence-absence data for each of the 96 Cheadle Center bird surveys within the mentioned timeframe. Used with `Wright-Ueda_replication.qmd`.
