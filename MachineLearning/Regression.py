import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import Lasso
from sklearn.model_selection import train_test_split,  GridSearchCV, StratifiedKFold, cross_validate
import os
from sklearn.metrics import make_scorer, mean_squared_error, r2_score
from sklearn.svm import SVR
import numpy as np
from xgboost.sklearn import XGBRegressor
from sklearn.preprocessing import StandardScaler

def performance(y_test,y_preds):
    score = r2_score(y_test, y_preds)
    return round(score,2)

def results_to_df(**models):
    for model in models: 
        models[model] = models[model].assign(Model = model)  
    df = pd.concat([model for model in models.values()])
    first_column = df.pop('Model')
    df.insert(0, 'Model', first_column)
    return df

def results_to_tex(df):
    print(df.to_latex())
    



def random_search(model, X_train, y_train, param_grid, refit_score='mean_squared_error_score'):
    """
    fits a GridSearchCV classifier using refit_score for optimization
    prints classifier performance metrics
    """
    grid_search = GridSearchCV(model, param_grid, scoring='neg_mean_squared_error', refit=refit_score,
                               cv=5, return_train_score=True, n_jobs = -1, verbose =1)
    grid_search.fit(X_train, y_train)
    return grid_search.best_estimator_

def compute_models(X, y, model_list):
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)
    X_train, X_test, y_train, y_test = train_test_split(X_scaled,y,random_state=123,test_size=0.25)

    log = Lasso()
    log_params = {"alpha":[.01,.1,1,10,100]}
    best_log = random_search(log, X_train, y_train,log_params, refit_score='mean_squared_error_score')
    log_scores = cross_validate(best_log, X_scaled, y, cv = 5, scoring = 'neg_root_mean_squared_error')
    y_preds = best_log.predict(X_test)
    log_perf = performance(y_test,y_preds)
    print()
    print()
    print(log_perf)
    print()
    print()
    print(log_scores)

    rfc = RandomForestRegressor()
    rfc_params = {'min_samples_split':[5,20,35], 'min_samples_leaf':[10,20,30], 'max_depth' : [None,5,15,25]}
    best_rfc = random_search(rfc, X_train,  y_train, rfc_params, refit_score='mean_squared_error_score')
    rfc_scores = cross_validate(best_rfc, X_scaled, y, cv = 5, scoring = 'neg_root_mean_squared_error')
    y_preds = best_rfc.predict(X_test)
    rfc_perf = performance(y_test,y_preds)
    print()
    print()
    print(rfc_perf)
    print()
    print()
    print(rfc_scores)

    svc = SVR(max_iter = 1000000)
    svc_params = {'C':[0.01,.1,10,100], 'kernel':('linear', 'rbf', 'poly')}
    best_svc = random_search(svc, X_train,  y_train, svc_params, refit_score='mean_squared_error_score')
    svc_scores =cross_validate(best_svc, X_scaled, y, cv = 5, scoring = 'neg_root_mean_squared_error')
    y_preds = best_svc.predict(X_test)
    svc_perf = performance(y_test,y_preds)

    print()
    print()
    print(svc_perf)
    print()
    print()
    print(svc_scores)

    xgb = XGBRegressor(n_estimators = 200,subsample = 0.5, min_child_weight = 1,  gamma = 0, colsample_bytree = 0.8, scale_pos_weight = 1,learning_rate =0.01)
    xgb_params = {'max_depth' : [None,5,15,25] }  
    best_xgb = random_search(xgb, X_train,  y_train,  xgb_params, refit_score='mean_squared_error_score')
    xgb_scores =cross_validate(best_xgb, X_scaled, y, cv = 5, scoring = 'neg_root_mean_squared_error')
    y_preds = best_xgb.predict(X_test)
    xgb_perf = performance(y_test,y_preds)

    print()
    print()
    print(xgb_perf)
    print()
    print()
    print(xgb_scores)

    return [log_perf, rfc_perf, svc_perf, xgb_perf], pd.DataFrame(np.array([log_scores['test_score'].tolist(), rfc_scores['test_score'].tolist(), svc_scores['test_score'].tolist(), xgb_scores['test_score'].tolist()]).T.tolist(),columns = model_list)

EGA_PERF = dict()
PCA_PERF = dict()
NON_PERF = dict()
ICA_PERF = dict()
UVA_PERF = dict()


EGA_dir = r"C:\Users\seanm\Dropbox\Research\EGA_vs_PCA\Data\Prepped\EGA\Regression"
PCA_dir = r"C:\Users\seanm\Dropbox\Research\EGA_vs_PCA\Data\Prepped\PCA\Regression"
ICA_dir = r"C:\Users\seanm\Dropbox\Research\EGA_vs_PCA\Data\Prepped\ICA\Regression"
NON_dir = r"C:\Users\seanm\Dropbox\Research\EGA_vs_PCA\Data\Prepped\Non-reduced\Regression"
UVA_dir = r"C:\Users\seanm\Dropbox\Research\EGA_vs_PCA\Data\Prepped\UVA\Regression"

model_list = ["Logit", "RFC", "SVM", "XGB"]
EGA_SCORES_DF = pd.DataFrame([[None, None,None, None]], columns = model_list)
PCA_SCORES_DF = pd.DataFrame([[None, None,None, None]], columns = model_list)
ICA_SCORES_DF = pd.DataFrame([[None, None,None, None]], columns = model_list)
NON_SCORES_DF = pd.DataFrame([[None, None,None, None]], columns = model_list)
UVA_SCORES_DF = pd.DataFrame([[None, None,None, None]], columns = model_list)

UVA_data = os.listdir(UVA_dir)
os.chdir(UVA_dir)
for data in UVA_data:
    df = pd.read_csv(data)
    X = df.drop('target', axis = 1)
    y = df['target']
    UVA_PERF[data], scores = compute_models(X,y, model_list)
    UVA_SCORES_DF = pd.concat([UVA_SCORES_DF, scores])

ICA_data = os.listdir(ICA_dir)
os.chdir(ICA_dir)
for data in ICA_data:
    df = pd.read_csv(data)
    X = df.drop('target', axis = 1)
    y = df['target']
    ICA_PERF[data], scores = compute_models(X,y, model_list)
    ICA_SCORES_DF = pd.concat([ICA_SCORES_DF, scores])

EGA_data = os.listdir(EGA_dir)
os.chdir(EGA_dir)
for data in EGA_data:
    df = pd.read_csv(data).dropna()
    X = df.drop('target', axis = 1)
    y = df['target']
    EGA_PERF[data], scores = compute_models(X,y, model_list)
    EGA_SCORES_DF = pd.concat([EGA_SCORES_DF, scores])


PCA_data = os.listdir(PCA_dir)
os.chdir(PCA_dir)
for data in PCA_data:
    df = pd.read_csv(data).dropna()
    X = df.drop('target', axis = 1)
    y = df['target']
    PCA_PERF[data], scores = compute_models(X,y, model_list)
    PCA_SCORES_DF = pd.concat([PCA_SCORES_DF, scores])

NON_data = os.listdir(NON_dir)
os.chdir(NON_dir)
for data in NON_data:
    df = pd.read_csv(data).dropna()
    X = df.drop('target', axis = 1)
    y = df['target']
    NON_PERF[data], scores = compute_models(X,y, model_list)
    NON_SCORES_DF = pd.concat([NON_SCORES_DF, scores])


EGA_results_df = pd.DataFrame(EGA_PERF, index = model_list).T
UVA_results_df = pd.DataFrame(UVA_PERF, index = model_list).T
PCA_results_df = pd.DataFrame(PCA_PERF, index = model_list).T
ICA_results_df = pd.DataFrame(ICA_PERF, index = model_list).T
non_results_df = pd.DataFrame(NON_PERF, index = model_list).T


compiled_results = results_to_df(EGA = EGA_results_df, UVA = UVA_results_df, PCA = PCA_results_df , ICA = ICA_results_df, Non = non_results_df)
compiled_scores = results_to_df(EGA = EGA_SCORES_DF, UVA = UVA_SCORES_DF, PCA = PCA_SCORES_DF, ICA = ICA_SCORES_DF, Non = NON_SCORES_DF)

os.chdir(r"C:\Users\seanm\Dropbox\Research\EGA_vs_PCA\Output")

compiled_scores.to_csv(r"Regression_CV_Scores.csv")
print(compiled_results.to_latex)