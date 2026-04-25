# ============================================================================
# Prepare H3 Spatial Data for Figure 4
# ============================================================================
# Creates h3_s7_sta_v2.gpkg with:
#   - sta_nonzero_share: Share of inhabitants with STA > 0 (%)
#   - hill_q1_median: Weighted median leisure location diversity
#   - duration_median: Weighted median leisure duration (minutes)
# ============================================================================

import os
os.environ['USE_PYGEOS'] = '0'

import pandas as pd
import geopandas as gpd
import numpy as np
import h3.api.numpy_int as h3
from shapely.geometry import Polygon

os.chdir('D:/netmob25')

# ============================================================================
# Helper Functions
# ============================================================================

def weighted_median(data, weights):
    """Compute the weighted median of data with given weights."""
    data, weights = np.array(data), np.array(weights)
    valid = ~np.isnan(data) & ~np.isnan(weights) & (weights > 0)
    if valid.sum() == 0:
        return np.nan
    data, weights = data[valid], weights[valid]
    sorter = np.argsort(data)
    data, weights = data[sorter], weights[sorter]
    cumsum = np.cumsum(weights) / np.sum(weights)
    return data[np.searchsorted(cumsum, 0.5)]

# ============================================================================
# 1. Load Individual-Level Data
# ============================================================================

print("Loading data...")

# Load main features file (has STA, hill_q1, etc.)
df_ind = pd.read_csv('dbs/data_p/commuter_model_features_r.csv')
print(f"  Individuals: {len(df_ind)}")

# Load weekday leisure metrics (has mean_leisure_duration)
df_leisure = pd.read_csv('dbs/data_p/commuter_leisure_metrics_weekday.csv')
print(f"  Leisure metrics: {len(df_leisure)}")

# Merge mean_leisure_duration if not already present
if 'mean_leisure_duration' not in df_ind.columns:
    df_ind = df_ind.merge(
        df_leisure[['ID', 'mean_leisure_duration']],
        on='ID',
        how='left'
    )
    print(f"  After merging duration: {df_ind['mean_leisure_duration'].notna().sum()} with duration")

# Load trips for home coordinates
df_trips = pd.read_csv('dbs/data_p/commuter_trips.csv')
df_home = df_trips[df_trips['purpose_d'] == 'HOME'].drop_duplicates(subset=['ID'])
df_home = df_home[['ID', 'end_lon', 'end_lat']].rename(columns={'end_lon': 'lon', 'end_lat': 'lat'})
print(f"  Home locations: {len(df_home)}")

# Merge home coordinates
df_ind = df_ind.merge(df_home, on='ID', how='left')

# Check STA column
if 'ak' in df_ind.columns:
    df_ind['sta'] = df_ind['ak']
elif 'ak_90' in df_ind.columns:
    df_ind['sta'] = df_ind['ak_90']
else:
    raise ValueError("No STA column found (ak or ak_90)")

print(f"  STA range: {df_ind['sta'].min():.0f} to {df_ind['sta'].max():.0f}")
print(f"  Duration range: {df_ind['mean_leisure_duration'].min():.1f} to {df_ind['mean_leisure_duration'].max():.1f} (non-NA)")

# ============================================================================
# 2. Assign H3 Cells
# ============================================================================

print("Assigning H3 cells (resolution 7)...")

# Filter valid coordinates
df_valid = df_ind[df_ind['lon'].notna() & df_ind['lat'].notna()].copy()

# Assign H3 cells
df_valid['h3_id'] = df_valid.apply(
    lambda row: h3.latlng_to_cell(row['lat'], row['lon'], res=7),
    axis=1
)

print(f"  Valid individuals with H3: {len(df_valid)}")
print(f"  Unique H3 cells: {df_valid['h3_id'].nunique()}")

# ============================================================================
# 3. Aggregate to H3 Level
# ============================================================================

print("Aggregating to H3 level...")

def area_agg(data):
    """Aggregate individual data to H3 cell level."""
    n_individuals = len(data)
    total_weight = data['weight_ind'].sum()

    # Share with STA > 0
    sta_nonzero_share = (
        data[data['sta'] > 0]['weight_ind'].sum() / total_weight * 100
        if total_weight > 0 else np.nan
    )

    # Weighted median diversity (only for those with diversity > 0)
    valid_div = data[(data['hill_q1'] > 0) & data['hill_q1'].notna()]
    if len(valid_div) > 0:
        hill_q1_median = weighted_median(valid_div['hill_q1'].values, valid_div['weight_ind'].values)
    else:
        hill_q1_median = np.nan

    # Weighted median duration (only for those with duration > 0)
    valid_dur = data[(data['mean_leisure_duration'] > 0) & data['mean_leisure_duration'].notna()]
    if len(valid_dur) > 0:
        duration_median = weighted_median(valid_dur['mean_leisure_duration'].values, valid_dur['weight_ind'].values)
    else:
        duration_median = np.nan

    return pd.Series({
        'n_individuals': n_individuals,
        'total_weight': total_weight,
        'sta_nonzero_share': sta_nonzero_share,
        'hill_q1_median': hill_q1_median,
        'duration_median': duration_median
    })

df_h3_agg = df_valid.groupby('h3_id').apply(area_agg, include_groups=False).reset_index()
print(f"  Aggregated to {len(df_h3_agg)} H3 cells")

# ============================================================================
# 4. Create H3 Geometries
# ============================================================================

print("Creating H3 geometries...")

h3_list = df_h3_agg['h3_id'].tolist()
polygons = [Polygon(h3.cells_to_geo([x])['coordinates'][0]) for x in h3_list]

gdf_h3 = gpd.GeoDataFrame(df_h3_agg, geometry=polygons, crs="EPSG:4326")

# ============================================================================
# 5. Save Output
# ============================================================================

output_path = 'results/h3_s7_sta_v2.gpkg'
gdf_h3.to_file(output_path, driver='GPKG')
print(f"\nSaved: {output_path}")

# Summary statistics
print("\n=== Summary Statistics ===")
print(f"H3 cells: {len(gdf_h3)}")
print(f"STA>0 share: {gdf_h3['sta_nonzero_share'].mean():.1f}% (mean)")
print(f"Diversity median: {gdf_h3['hill_q1_median'].median():.1f} (median of medians)")
print(f"Duration median: {gdf_h3['duration_median'].median():.1f} min (median of medians)")
print(f"Cells with duration data: {gdf_h3['duration_median'].notna().sum()}")
