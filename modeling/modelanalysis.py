# modelanalysis.py
# Christopher V. Cosgriff, Harvard Chan School and NYU School of Medicine
# This file includes a number of helper functions for generating plots and 
# metrics for the severity score calibration project.

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import roc_curve, roc_auc_score
from sklearn.calibration import calibration_curve

# colors from Tableau for prettier plots
tableau20 = [
(0.12156862745098039, 0.4666666666666667, 0.7058823529411765),
(0.6823529411764706, 0.7803921568627451, 0.9098039215686274),
(1.0, 0.4980392156862745, 0.054901960784313725),
(1.0, 0.7333333333333333, 0.47058823529411764),
(0.17254901960784313, 0.6274509803921569, 0.17254901960784313),
(0.596078431372549, 0.8745098039215686, 0.5411764705882353),
(0.8392156862745098, 0.15294117647058825, 0.1568627450980392),
(1.0, 0.596078431372549, 0.5882352941176471),
(0.5803921568627451, 0.403921568627451, 0.7411764705882353),
(0.7725490196078432, 0.6901960784313725, 0.8352941176470589),
(0.5490196078431373, 0.33725490196078434, 0.29411764705882354),
(0.7686274509803922, 0.611764705882353, 0.5803921568627451),
(0.8901960784313725, 0.4666666666666667, 0.7607843137254902),
(0.9686274509803922, 0.7137254901960784, 0.8235294117647058),
(0.4980392156862745, 0.4980392156862745, 0.4980392156862745),
(0.7803921568627451, 0.7803921568627451, 0.7803921568627451),
(0.7372549019607844, 0.7411764705882353, 0.13333333333333333),
(0.8588235294117647, 0.8588235294117647, 0.5529411764705883),
(0.09019607843137255, 0.7450980392156863, 0.8117647058823529),
(0.6196078431372549, 0.8549019607843137, 0.8980392156862745)]

# Confidence interval bootstrap functions
def auc_ci(f_hat, y_true, n_bootstraps=2000, ci_level=0.95):
    li = (1. - ci_level)/2
    ui = 1 - li

    rng = np.random.RandomState(seed=42)
    bootstrapped_auc = []

    for i in range(n_bootstraps):
        indices = rng.randint(0, len(f_hat), len(f_hat))
        auc = roc_auc_score(y_true[indices], f_hat[indices])
        bootstrapped_auc.append(auc)

    sorted_scores = np.array(bootstrapped_auc)
    sorted_scores.sort()
    confidence_lower = sorted_scores[int(li * len(sorted_scores))]
    confidence_upper = sorted_scores[int(ui * len(sorted_scores))]

    return confidence_lower, confidence_upper

def op_ratio_ci(f_hat, y_true, n_bootstraps=2000, ci_level=0.95):
    li = (1. - ci_level)/2
    ui = 1. - li

    rng = np.random.RandomState(seed=42)
    bootstrapped_opr = []
    
    for i in range(n_bootstraps):
        indices = rng.randint(0, len(f_hat), len(f_hat))
        opr = y_true[indices].mean() / f_hat[indices].mean()
        bootstrapped_opr.append(opr)

    sorted_scores = np.array(bootstrapped_opr)
    sorted_scores.sort()
    confidence_lower = sorted_scores[int(li * len(sorted_scores))]
    confidence_upper = sorted_scores[int(ui * len(sorted_scores))]

    return confidence_lower, confidence_upper

# Metric plot functions
def gen_auc_plot(models, names, colors, title, X, y, save_name=None):
    plt.figure(figsize=(10, 10))
    for i, model in enumerate(models):
        f_hat = model.predict_proba(X)
        roc = roc_curve(y, f_hat[:, 1])
        auc = roc_auc_score(y, f_hat[:, 1])
        plt.plot(roc[0], roc[1], color=colors[i], 
         label='{0}\n(AUC = {1:.3f} [{2:.3f}, {3:.3f}])'.format(names[i], auc, *auc_ci(f_hat[:, 1], y)))
    
    plt.plot([0, 1], [0, 1], 'k:', color=tableau20[1])
    plt.grid()
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title(title)
    plt.legend(loc="lower right")
    if save_name != None:
        plt.savefig('./figures/' + save_name + '.svg', bbox_inches='tight')
    plt.show()
    
def gen_calib_plot(models, names, colors, title, X, y, save_name=None):
    plt.figure(figsize=(10, 10))
    for i, model in enumerate(models):
        f_hat = model.predict_proba(X)
        fraction_of_positives, mean_predicted_value = calibration_curve(y, f_hat[:, 1], n_bins=10)
        plt.plot(mean_predicted_value, fraction_of_positives, color=colors[i], 
         label=names[i])
    
    plt.plot([0, 1], [0, 1], 'k:', color=tableau20[1])
    plt.ylabel("Fraction of positives")
    plt.legend(loc="lower right")
    plt.title(title)
    plt.xlabel("Mean predicted value")
    plt.grid()
    if save_name != None:
        plt.savefig('./figures/' + save_name + '.svg', bbox_inches='tight')
    plt.show()

# Observed to predicted mortality ratio funtion
def op_ratio(model, X, y):
    f_hat = model.predict_proba(X)
    observed = y.mean()
    predicted = f_hat[:, 1].mean()
    return (observed / predicted, *op_ratio_ci(f_hat[:, 1], y))

# A feature importance plot function for regression models
def gen_logodds_plot(model, features, n_features=10, title='Log Odds Plot', save_name=None):
    plt.figure(figsize=(10, 10))
    coef = {k:v for k, v in zip(features, model.named_steps['ridge'].coef_.ravel())}
    coef = pd.DataFrame.from_dict(coef, orient='index', columns=['log_odds'])
    coef = coef.reindex(coef.log_odds.abs().sort_values(ascending=False).index).iloc[0:n_features, :]
    coef = coef.reindex(index=coef.index[::-1])
    pos_neg = coef.log_odds > 0
    color_map = pos_neg.map({True: tableau20[8], False: tableau20[10]})
    coef.plot(kind='barh', grid=True, sort_columns=False,
                   title=title,
                   color=[color_map.values], ax=plt.axes(), width=0.20,
                   legend=False)
    plt.xlabel('log(OR)')
    plt.ylabel('Features')
    if save_name != None:
        plt.savefig('./figures/' + save_name + '.svg', bbox_inches='tight')
    plt.show()