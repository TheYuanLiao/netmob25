import os, sys
from pathlib import Path
from interpret.glassbox import ExplainableBoostingRegressor
from interpret.perf import RegressionPerf
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import pandas as pd
import shap
import numpy as np
import pickle

ROOT_dir = Path(__file__).parent.parent
sys.path.append(ROOT_dir)
sys.path.insert(0, os.path.join(ROOT_dir, '/lib'))


class GAMModel:
    def __init__(self):
        self.data = None
        # ['time_threshold', 'amenity', 'mode', 'Gender', 'Education', 'Household_type', 'Car_no', 'Bike_no', 'Two_wheeler_no', 'Escooter_no', 'pt_sub'] +\
        # ['d2h_nh', 'Age']
        self.target_column = 'log_disparity'
        self.X_train = None
        self.features = None

    def load_data(self, filter=True):
        # Load data for modelling
        print('Load data.')
        df = pd.read_parquet("results/activity_access_ind_model.parquet")
        var_con = ['d2h_nh', 'Age', 'access_h']
        if filter:
            df = df[(df['time_threshold'] == "15 min") & (df['mode'] == 'Public transit') & (df['amenity'] == "Essential needs")]
            var_cat = ['Gender', 'Education', 'Household_type', 
                       'Car_no', 'Bike_no', 'Two_wheeler_no', 'Escooter_no', 'pt_sub']  # 
        else:
            var_cat = ['time_threshold', 'amenity', 'mode', 
                'Gender', 'Education', 'Household_type', 
                'Car_no', 'Bike_no', 'Two_wheeler_no', 'Escooter_no', 'pt_sub']  # 
        df['log_disparity'] = np.log(df['gap'])
        #print('One Hot Encoder')
        # One-hot encode
        #df_encoded = pd.get_dummies(df, columns=var_cat, drop_first=True)
        #categorical_encoded_features = [
        #    col for col in df_encoded.columns
        #    if any(col.startswith(var + '_') for var in var_cat)
        #]

        # Step 4: Combine into final feature list
        for v in var_cat:
            df[v] = df[v].astype(str).astype('category')
        self.features = var_con + var_cat
        self.data = df[self.features + [self.target_column]]
        print(self.data.iloc[0])

    def ebm_model(self, df):
        print('Preprocess features...')
        X = df[self.features]
        y = df[self.target_column]

        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)

        print('Normalization...')
        # scaler = StandardScaler().fit(X_train)
        # X_train = scaler.transform(X_train)
        # X_test = scaler.transform(X_test)
        self.X_train = X_train

        print('Training...')
        seed = 1
        ebm = ExplainableBoostingRegressor(
            interactions=2,                 # reduce number of pairwise interactions
            max_bins=128,                   # reduce discretization resolution
            max_leaves=3,                   # smaller trees → simpler models
            min_samples_leaf=20,           # require more data per leaf
            learning_rate=0.01,            # slower learning to smooth effects
            outer_bags=2,                  # increase ensembling for variance reduction
            validation_size=0.2,           # use more data to validate generalizability
            early_stopping_rounds=100,      # stop adding trees if no improvement
            random_state=seed,
            feature_names=self.features
        )
        ebm.fit(X_train, y_train)  # Works on dataframes and numpy arrays
        # Performance
        ebm_perf_test = RegressionPerf(ebm.predict).explain_perf(X_test, y_test, name='EBM')
        ebm_perf_train = RegressionPerf(ebm.predict).explain_perf(X_train, y_train, name='EBM')

        ebm_global = ebm.explain_global(name='EBM')

        # Log data
        overall = ebm_global.data()
        f_score = {}
        for i, f in zip(range(0, len(ebm_global.feature_names)), ebm_global.feature_names):
            f_score[f] = ebm_global.data(i)
        return ebm, ebm_perf_train.data(), ebm_perf_test.data(), overall, f_score

    def explain_ebm(self, ebm_model=None):
        # explain the GAM model with SHAP
        np.random.seed(1)
        background = shap.maskers.Independent(self.X_train, max_samples=1000)
        explainer_ebm = shap.Explainer(ebm_model.predict, background)
        shap_values_ebm = explainer_ebm(self.X_train[np.random.randint(self.X_train.shape[0], size=10000), :])
        shap_values_ebm.feature_names = self.features
        return shap_values_ebm

    def ebm_model_train(self, df=None, file_name='model', explain=False):
        ebm, perf_train, perf_test, overall, f_score = self.ebm_model(df=df)
        ebm_learned = {"perf_train": perf_train, "perf_test": perf_test,
                       "overall": overall, "f_score": f_score}
        pickle.dump(ebm_learned, open(os.path.join(ROOT_dir, 'results', 'ebm', f"{file_name}.p"), "wb"))
        print("Results pickled.")
        if explain:
            shap_values_ebm = self.explain_ebm(ebm_model=ebm)
            pickle.dump(shap_values_ebm,
                        open(os.path.join(ROOT_dir, 'results', 'ebm', f"{file_name}_explained.p"), "wb"))
            print("Shap values pickled.")
            shap.plots.beeswarm(shap_values_ebm, max_display=10)


if __name__ == '__main__':
    gam = GAMModel()
    gam.load_data(filter=True)
    # Modelling all data
    print("Model all the individuals.")
    gam.ebm_model_train(df=gam.data,
                        file_name='model_all',
                        explain=False)
