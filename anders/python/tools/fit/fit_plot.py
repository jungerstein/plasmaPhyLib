'''
fit_plot.py
NOT TO BE INVOKED INDIVIDUALLY
Plots data line and fitted line. 

Coded by jungerstein. 

For Your original research, it would be nice to contact the coder
  before the usage of this piece of codes in Your project. 
'''

import matplotlib.pyplot as plt
from numpy import linspace, array


def plot_original(series, dx, style):
    x_series = linspace(0, dx*(len(series)), num=len(series),
                        endpoint=False)
    plt.plot(x_series, series, style)


def plot_fitted(n_points, dx, fit_func, param):
    x_series = linspace(0, dx*n_points, num=n_points, endpoint=False)
    y_series = array([fit_func(x, *param) for x in x_series])
    plt.plot(x_series, y_series, 'r', hold=True)
