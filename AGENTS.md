# AGENTS.md

This file provides guidance to Codex (Codex.ai/code) when working with code in this repository.

## Project Overview

Research project for the NetMob 2025 Data Challenge analyzing the relationship between space-time accessibility and leisure activity participation in the Île-de-France (Greater Paris) region. Uses mobility data from 3,300+ participants over a full week.

## Languages and Key Dependencies

- **Python 3.11**: Data processing, mobility quantification, accessibility preparation
- **R 4.5.1**: Spatial accessibility computation (r5r 2.3.0), SEM modeling (lavaan), visualization

Key Python libraries: pandas, numpy, geopandas, shapely, scikit-learn, pickle
Key R packages: r5r, lavaan, sf, ggplot2, dplyr

## Repository Structure

Scripts are numbered by execution order (1-12). Later scripts depend on earlier results.

- `src/`: Main analysis pipeline
  - `1-*.ipynb` through `12-*.R`: Sequential processing steps
  - `visualization/`: Figure generation (R and Python)
- `lib/helpers.py`: Shared utilities (ROOT_dir constant, EBMResultsOrganizer class, geo helpers)
- `dbs/`: Local data storage (not tracked in git)
  - `accessibility/`, `sp_accessibility/`: Accessibility computation outputs
  - `data/`, `data_p/`: Processed data files
  - `geo/`: Geographic data
- `results/`: Intermediate outputs (parquet, gpkg, csv files)
- `figures/`: Generated figures for manuscript
- `legacy/`: Deprecated scripts and intermediate files

## Running Scripts

Scripts must be run in numerical order. R scripts handle spatial accessibility computation and SEM modeling. Python notebooks handle data processing and exploratory analysis.

The root directory is hardcoded in `lib/helpers.py` as `ROOT_dir = "D:\netmob25"`.

## Data Notes

- Input data from NetMob 2025 is GDPR-restricted and not publicly available
- Venue data from Overture Maps, road network from OpenStreetMap, GTFS from transport.data.gouv.fr
- r5r used for multimodal accessibility computation (car and public transit)
