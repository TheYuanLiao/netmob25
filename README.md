# From Reachability to Realization: Urban Mobility Gaps in Access and Efficiency
Using a comprehensive, multi-layered account of individual mobility patterns across the Île-de-France region, capturing a full week of daily activity for over 3,300 participants, we investigated how different population groups convert accessibility at their neighborhood into realized mobility and how efficiently they navigate urban transport systems, uncovering disparities in both access and travel performance.

## Dependencies
Python (version 3.11) code and R (version 4.0.2) code were used to analyse and visualize the data. 
r5r (2.2.0) was used to compute accessibility to POIs by car and public transit $^1$.

## Data
The data supporting the findings of this study were provided by [NetMob 2025 Data Challenge](https://netmob.org/www25/datachallenge)$^2$ and 
are subject to restrictions due to licensing 
and privacy considerations under the European General Data Protection Regulation. 
Consequently, these data are not publicly available.

Venue locations and categories were obtained from [Overture](https://overturemaps.org/).
Road network data were collected from [OpenStreetMap](https://download.geofabrik.de/europe.html).
GTFS data were downloaded from [transport.data.gouv.fr](https://transport.data.gouv.fr/).

## Scripts
The repo contains the scripts (`src/`), libraries (`lib/`) for conducting the data processing, analysis, and visualisation.
The original input data are stored under `dbs/` locally and intermediate results are stored in a local database.
The produced figures are stored under `figures/`.

Under `src/`, the scripts are stored by their functionality, with the first number indicating the
order of running the script.
This is because some later analysis may depend on earlier steps.

- `src/visualization/` produce figures inserted in the manuscript.

## References
1. Pereira RHM, Saraiva M, Herszenhut D, Braga CKV, Conway MW (2021). “r5r: Rapid Realistic Routing on Multimodal Transport Networks with R5 in R.” Findings. doi:10.32866/001c. 21262, https://doi.org/10.32866/001c.21262.

2. Chasse A, Kouam AJ, Viana AC, Stanica R, Lobato WV, Ramos G, Deperle G, Bouroudi A, Bussod S, Molano F (2025). “The NetMob25 Dataset: A High-resolution Multi-layered View of Individual Mobility in Greater Paris Region.” arXiv preprint. arXiv:2506.05903, https://arxiv.org/abs/2506.05903.