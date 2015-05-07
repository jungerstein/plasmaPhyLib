#!/anaconda/bin/python -O
'''
attenuation.py
Estimates parameters in exponential attenuations. 

Coded by jungerstein. 

For Your original research, it would be nice to contact the coder
  before the usage of this piece of codes in Your project. 
'''

from fit_exp import fit_exponential

def find_peaks(serial):
    '''
    Find peak points in time serial <i>serial</i>. 
    INPUT
    serial: A time serial. 
    OUTPUT
    2 (Two) lists:
    the_i_peaks: the subscripts of peaks. 
    the_y_peaks: the values at peaks. 
    '''
    the_i_peaks = np.array([
        i + 1 
        for i in xrange(len(serial) - 2)
        if serial[i] <= serial[i+1] >= serial[i+2]
        ])
    the_y_peaks = np.array([
        serial[i + 1]
        for i in xrange(len(serial) - 2)
        if serial[i] <= serial[i+1] >= serial[i+2]
        ])
    return the_i_peaks, the_y_peaks

def evaluate_attenuation(serial, dt): 
    '''
    The interface to do the fit. 
    serial = C + A0 exp (- gamma t) cos (omega t)
    INPUT
    serial: time series, <b> must be in numpy array! </b> 
    dx: real number, the advance of adjacent time points in serial. 
    OUTPUT
    3 (Three) parameters
      C, A0, gamma
      <em> Yes, we have ignored omega. </em>
    '''
    i_peaks, y_peaks = find_peaks(serial)
    x_peaks = dt * i_peaks
    # Regression, roughly
    the_param_attenuation = fit_exponential(x_peaks, y_peaks)
    return the_param_attenuation
