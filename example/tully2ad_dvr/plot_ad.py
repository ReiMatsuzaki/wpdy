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

from naewdy2.math import csv2mat, mat2csv

xs = pd.read_csv("out/xs.csv")["val"]
hel = csv2mat("v.idx.csv")
xij = csv2mat("xij.idx.csv")

print len(xs), len(xij[:,0,1])
plt.plot(xs[:-1], xij[:,0,1])
plt.savefig("fig/xij.pdf")

