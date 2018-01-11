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

ts = pd.read_csv("out/ts.csv")["val"]
ad_diag = pd.read_csv("out/prob.csv")["val1"]
ad_kr50 = pd.read_csv("krylov50/out/prob.csv")["val1"]
di_diag = pd.read_csv("../tully2_dvr/out/prob.csv")["val1"]

plt.plot(ts, ad_diag, label="ad_diag")
plt.plot(ts, ad_kr50, label="ad_kr50")
plt.plot(ts, di_diag, label="di_diag")
plt.ylim(0.9, 1.0)
plt.legend()
plt.savefig("fig/prob.pdf")

