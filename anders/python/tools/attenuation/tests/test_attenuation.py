from fit_plot import plot_original, plot_fitted
from fit_exp import func_exponential
from attenuation import evaluate_attenuation
from numpy.random import rand
from numpy import array, exp, cos
import matplotlib.pyplot as plt

test_C = 1.0
test_A0 = 2.2
test_gamma = 0.05
test_dx = 0.05
test_omega = 1
test_n = 600
test_noise = 0.10
signals = array([test_C - test_noise / 2
                 + test_A0*exp(-test_gamma*test_dx*i)*cos(test_omega*test_dx*i)
                 + test_noise * rand()
                 for i in range(test_n)])
fitted_parameters = evaluate_attenuation(signals, test_dx, scale_for_peak=20)
print(fitted_parameters)
plot_original(signals, test_dx, 'k')
plot_fitted(test_n, test_dx, func_exponential, fitted_parameters)
plt.savefig('sample.png')

