'''
fit_exp.py
NOT TO BE INVOKED INDIVIDUALLY
Backend module providing exponential fitting. 

Coded by jungerstein. 

For Your original research, it would be nice to contact the coder 
  before the usage of this piece of codes in Your project.
'''

from numpy import exp
from scipy.optimize import curve_fit

def func_exponential(x, A0, gamma_decaying, C):
    return C + A0 * exp(-gamma_decaying * x)

def fit_exponential(x, y):
    '''
    The actual fit. 
    '''
    params_fit, cov_fit = curve_fit(func_exponential, x, y)
    return params_fit

