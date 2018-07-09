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
for it in [0, 10, 20, 30, 40]:
    coef = csv2mat("out/{0}/coef.idx.csv".format(it))
    plt.plot(xs, coef[:,0].real, label=str(it))

plt.legend()
plt.savefig("psi.pdf")



