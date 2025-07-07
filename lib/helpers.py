from shapely.geometry import mapping
import io, os
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import pickle
import pandas as pd
import numpy as np
from pprint import pprint


ROOT_dir = "D:\netmob25"


def gdf2poly(geodata=None, targetfile=None, buffer=0.03):
    """
    :param filename: .shp, a polygon
    :param targetfile: .poly, a polygon
    :param buffer: default 0.03, for the ease of cropping
    :return: None
    """
    g = [i for i in geodata.to_crs(4326).buffer(buffer).geometry]
    all_coords = mapping(g[0])["coordinates"][0]
    F = open(targetfile, "w")
    F.write("polygon\n")
    F.write("1\n")
    for point in all_coords:
        F.write("\t" + str(point[0]) + "\t" + str(point[1]) + "\n")
    F.write("END\n")
    F.write("END\n")
    F.close()


def osm_country2region(osm_file=None, terget_file=None, poly_file=None, osmosis_path=None):
    command_line = f'''{osmosis_path} --read-pbf file="{osm_file}" --bounding-polygon file="{poly_file}" --write-pbf file="{terget_file}"'''
    print(command_line)
    os.system(command_line)


class EBMResultsOrganizer:
    def __init__(self, file_loc=None):
        self.model = pickle.load(open(file_loc, "rb"))
        self.all_fscore = {}
        self.X_train = None
        self.df = None
        self.var_cat = ['time_threshold', 'amenity', 'mode',
                        'Gender', 'Education', 'Household_type',
                        'Car_no', 'Bike_no', 'Two_wheeler_no', 'Escooter_no', 'pt_sub', 'main_mode']
        # Define mapping rules for naming the features
        self.feature_dict = {'d2h_nh': 'Distance to Home Neighborhood',
                            'Age': 'Age',
                            'access_h': 'Access (home)',
                            'time_threshold': 'Time Threshold',
                            'amenity': 'Amenity',
                            'mode': 'Mode',
                            'Gender': 'Gender',
                            'Education': 'Education',
                            'Household_type': 'Household type',
                            'Car_no': 'Car number',
                            'Bike_no': 'Bike number',
                            'Two_wheeler_no': 'Two-wheeler number',
                            'Escooter_no': 'E-scooter number',
                            'pt_sub': 'Public Transport Subscription',
                            'main_mode': 'Main mode of transport'
                            }
        self.color_dict = {'d2h_nh': 'steelblue',
                            'Age': 'coral',
                            'access_h': 'steelblue',
                            'time_threshold': 'darkgreen',
                            'amenity': 'darkgreen',
                            'mode': 'darkgreen',
                            'Gender': 'coral',
                            'Education': 'coral',
                            'Household_type': 'coral',
                            'Car_no': 'steelblue',
                            'Bike_no': 'steelblue',
                            'Two_wheeler_no': 'steelblue',
                            'Escooter_no': 'steelblue',
                            'pt_sub': 'steelblue',
                            'main_mode': 'steelblue'
                            }

    def load_raw_data(self, select=None):
        var_cat = ['time_threshold', 'amenity', 'mode', 'Gender', 'Education', 'Household_type', 
                   'Car_no', 'Bike_no', 'Two_wheeler_no', 'Escooter_no', 'pt_sub', 'main_mode']
        var_con = ['d2h_nh', 'Age', 'access_h']
        # Load data for modelling
        print('Load data.')
        df = pd.read_parquet("results/activity_access_ind_model.parquet")
        df['log_disparity'] = np.log(df['gap'])

        # Step 4: Combine into final feature list
        features = var_con + var_cat
        target_column = 'log_disparity'
        df = df[features + [target_column]]
        X = df[features]
        y = df[target_column]

        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)
        self.X_train = X_train
        self.df = df

    def performance(self):
        perf_train = self.model["perf_train"]
        perf_test = self.model["perf_test"]
        return dict(rmse_train=perf_train['rmse'],
                    r2_train=perf_train['r2'],
                    rmse_test=perf_test['rmse'],
                    r2_test=perf_test['r2'])

    def feature_importance(self):
        all_features = {}
        for var, score in zip(self.model["overall"]["names"], self.model["overall"]["scores"]):
            if var not in all_features:
                all_features[var] = score
        #pprint(all_features)
        # Feature score data prep.
        df_f = pd.DataFrame([(k, v) for k, v in all_features.items()],
                            columns=('Feature/Interaction', 'Score')).sort_values(by='Score', ascending=False)
        # Safely relabel features, even if Feature 1 contains ' & '
        df_f['Name'] = df_f['Feature/Interaction'].apply(
            lambda x: self.feature_dict[x]
            if ' & ' not in x
            else ' & '.join([
                self.feature_dict.get(part.strip(), part.strip())
                for part in x.rsplit(' & ', 1)
            ])
        )

        # Assign color only if the full feature name is matched; otherwise default to black
        df_f['Color'] = df_f['Feature/Interaction'].apply(
            lambda x: self.color_dict.get(x, 'black') if ' & ' not in x else 'black'
        )
        df_f = df_f.loc[:, ['Name', 'Color', 'Score']].sort_values(by='Score', ascending=False)
        return df_f

    def feature_scores(self):
        ## f_score
        self.all_fscore["feature"] = {}
        self.all_fscore["interaction"] = {}
        for var, score in self.model["f_score"].items():
            if var not in self.all_fscore:
                if len(score) == 7:
                    self.all_fscore["feature"][var] = [score["names"],
                                                       score["scores"],
                                                       score["lower_bounds"],
                                                       score["upper_bounds"]]
                else:
                    self.all_fscore["interaction"][var] = [score["left_names"],
                                                           score["right_names"],
                                                           score["scores"]]

    def single_feature_effect(self):
        fscore_f = []
        for k, v in self.all_fscore['feature'].items():
            #print(len(v[0]), len(v[1]), len(v[2]), len(v[3]))
            tp = pd.DataFrame()
            tp['var'] = [k] * len(v[1])
            # Drop StandardScaler, directly assign raw values
            if k in self.var_cat:
                # For categorical: just pass the raw values
                tp['x'] = v[0]
            else:
                # For continuous: drop last value and assign directly
                tp['x'] = v[0][:-1]
            tp['y'] = v[1]
            tp['y_lower'] = v[2]
            tp['y_upper'] = v[3]
            fscore_f.append(tp)
        df_fscore_f = pd.concat(fscore_f)
        return df_fscore_f

    def interection_effect(self, path2save=None):
        labels_cat = self.var_cat
        labels_cat_names = {x: self.feature_dict[x] for x in self.var_cat}
        interaction_data = []

        for key, value in self.all_fscore["interaction"].items():
            feature1, feature2 = key.split(" & ")
            x_edges = np.array(value[0])  # either edges or categorical labels
            y_edges = np.array(value[1])
            effect_matrix = np.array(value[2])

            # Determine dimensionality
            is_x_categorical = not np.issubdtype(x_edges.dtype, np.number)
            is_y_categorical = not np.issubdtype(y_edges.dtype, np.number)

            # Compute x-axis values
            if is_x_categorical:
                x_vals = x_edges
            else:
                x_vals = (x_edges[:-1] + x_edges[1:]) / 2

            # Compute y-axis values
            if is_y_categorical:
                y_vals = y_edges
            else:
                y_vals = (y_edges[:-1] + y_edges[1:]) / 2

            for i, y in enumerate(y_vals):
                for j, x in enumerate(x_vals):
                    # Adjust indexing based on layout
                    effect = effect_matrix[i, j] if effect_matrix.shape == (len(y_vals), len(x_vals)) else effect_matrix[j, i]
                    interaction_data.append({
                        "feature1": feature1,
                        "feature2": feature2,
                        "x": x,
                        "y": y,
                        "effect": effect
                    })

        df_interactions = pd.DataFrame(interaction_data)
        df_interactions.to_csv(path2save + 'interactions.csv', index=False)