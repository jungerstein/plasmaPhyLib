'''
fit_exp.py
NOT TO BE INVOKED INDIVIDUALLY
Backend module providing exponential fitting. 

Coded by jungerstein. 

For Your original research, it would be nice to contact the coder 
  before the usage of this piece of codes in Your project.
'''

from numpy import exp, average, log
from scipy.optimize import curve_fit

def func_exponential(x, C, A0, gamma_decaying): 
    return C + A0 * exp(-gamma_decaying * x)

def fit_exponential(x, y): 
    '''
    The actual fit. 
    '''
    guess_C = average(y)
    guess_A0 = (max(y) - min(y)) / 2
    y1 = y[0]
    y2 = y[1]
    x1 = x[0]
    x2 = x[1]
    guess_gamma = -(log(y2-guess_C) - log(y1-guess_C)) / (x2 - x1)
    init_guess = [guess_C, guess_A0, guess_gamma]
    params_fit, cov_fit = curve_fit(func_exponential, x, y, init_guess)
    return params_fit

