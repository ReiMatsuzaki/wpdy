import os
join = os.path.join
exists = os.path.exists
expanduser = os.path.expanduser
import sys
import matplotlib as mpl
mpl.use('PDF')
import matplotlib.pylab as plt
import numpy as np
tr = np.transpose
import pandas as pd

from naewdy2.math import csv2mat

xs = csv2mat("out/xs.csv")
for it in [0,10]:
    coef = csv2mat("out/{0}/coef.csv".format(it))
    plt.plot(xs, coef[:,0].real, label=str(it))

plt.savefig("psi.pdf")



