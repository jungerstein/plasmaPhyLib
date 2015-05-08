#!/anaconda/bin/python -O

'''
attenuation.py
Estimates parameters in exponential attenuations. 

Coded by jungerstein. 

For Your original research, it would be nice to contact the coder
  before the usage of this piece of codes in Your project.
'''

from fit_exp import fit_exponential
from numpy import array, zeros
from fit_plot import plot_original
# Temp fix.
import matplotlib.pyplot as plt


def running_avg(serial, half_width):
    smoothed = zeros(len(serial))
    for i in range(len(serial)):
        min_subscript = max(0, i - half_width)
        max_subscript = min(len(serial) - 1, i + half_width)
        smoothed[i] = sum(serial[min_subscript:max_subscript+1]) / \
                    (max_subscript-min_subscript+1)
    return smoothed


def find_peaks(serial, half_width):
    '''
    Find peak points in time serial <i>serial</i>. 
    INPUT
    serial: A time serial. 
    OUTPUT
    2 (Two) lists:
    the_i_peaks: the subscripts of peaks. 
    the_y_peaks: the values at peaks. 
    '''
    smooth = running_avg(serial, half_width)
    the_i_peaks = array([
        i + 2
        for i in range(len(serial) - 4)
        if smooth[i] <= smooth[i+2] >= smooth[i+4]
        ])
    the_y_peaks = array([
        serial[i + 2]
        for i in range(len(serial) - 4)
        if smooth[i] <= smooth[i+2] >= smooth[i+4]
        ])
    return the_i_peaks, the_y_peaks


def evaluate_attenuation(serial, dt, scale_for_peak):
    '''
    The interface to do the fit. 
    serial = C + A0 exp (- gamma t) cos (omega t)
    INPUT
    serial: time series, <b> must be in numpy array! </b> 
    dx: real number, the advance of adjacent time points in serial.
    scale_for_peak: integer, the half-width, in points.
                    it should be effectively less than wave length
                                 but well above 2
    OUTPUT
    3 (Three) parameters
      A0, gamma, C
      <em> Yes, we have ignored omega. </em>
    '''
    i_peaks, y_peaks = find_peaks(serial, scale_for_peak)
    x_peaks = dt * i_peaks

    plt.plot(x_peaks, y_peaks, 'b+')
    # Regression, roughly
    the_param_attenuation = fit_exponential(x_peaks, y_peaks)
    return the_param_attenuation
