# Space-Time Accessibility and Leisure Activity Participation

Analysis of the relationship between space-time accessibility (STA) and leisure activity participation in the Île-de-France (Greater Paris) region. Uses mobility data from the NetMob 2025 Data Challenge capturing a full week of daily activity for over 3,300 participants.

## Dependencies

- **Python 3.11**: pandas, numpy, geopandas, shapely, scikit-learn
- **R 4.5.1**: r5r (2.3.0), lavaan, sf, ggplot2, dplyr, magick

## Data Sources

| Data | Source |
|------|--------|
| Mobility traces | [NetMob 2025 Data Challenge](https://netmob.org/www25/datachallenge) (GDPR-restricted) |
| Venue locations | [Overture Maps](https://overturemaps.org/) |
| Road network | [OpenStreetMap](https://download.geofabrik.de/europe.html) |
| Transit schedules | [transport.data.gouv.fr](https://transport.data.gouv.fr/) |

## Repository Structure

```
src/                          # Analysis pipeline (numbered by execution order)
├── 1-raw-data-processing.ipynb
├── 2a-mobility-quantification.ipynb
├── 2b-commuter-time-budget.ipynb
├── 2c-poi-activity-association.ipynb
├── 2d-leisure-metrics-weekday.ipynb
├── 3-accessibility-data-prep.ipynb
├── 4-sp-accessibility-wk.R         # Weekday accessibility (r5r)
├── 5-sp-accessibility-kh-prep.ipynb
├── 6-sp-accessibility-kh.R         # Post-work trip-chain accessibility
├── 7-sp-accessibility-results.ipynb
├── 8-sp-accessibility-desc.ipynb   # Descriptive statistics
├── 9a-sp-accessibility-proc.ipynb  # Main STA processing
├── 9b-sensitivity-visualization.ipynb  # Sensitivity analysis plots
├── 10-spa-set-vs-visits.ipynb      # Selectivity analysis
├── 11-dag-tests.ipynb              # Measurement model tests
├── 12a-sem-model-v4.R              # SEM with latent leisure participation
├── 12b-sem-model-v4-sensitivity.R  # SEM sensitivity (60/120 min budgets)
├── 13-sem-plot.R                   # SEM diagram visualization
└── visualization/
    ├── f1-paris-region.R
    ├── f4-sp-access-map.R
    └── figure-maker.R              # Composite figure assembly

lib/helpers.py                # Shared utilities
results/                      # Intermediate outputs (parquet, gpkg, csv)
├── sem/                      # Main SEM results
├── sem_sensitivity/          # Sensitivity analysis results
└── sensitivity/              # STA sensitivity by hour/budget
figures/                      # Generated manuscript figures
dbs/                          # Local data storage (not in git)
```

## Key Methods

- **Space-Time Accessibility (STA)**: Count of leisure POIs reachable via a home-work-activity-home trip chain, computed using r5r for multimodal routing
- **STA transformation**: Inverse hyperbolic sine (IHS) to handle zero values
- **Outcome measure**: Latent leisure participation factor (Hill diversity index q=1 and mean leisure duration)
- **Estimation**: Structural equation model (SEM) with DWLS estimator and sampling weights

## References

1. Pereira RHM et al. (2021). "r5r: Rapid Realistic Routing on Multimodal Transport Networks with R5 in R." *Findings*. https://doi.org/10.32866/001c.21262

2. Chasse A et al. (2025). "The NetMob25 Dataset: A High-resolution Multi-layered View of Individual Mobility in Greater Paris Region." *arXiv*. https://arxiv.org/abs/2506.05903
