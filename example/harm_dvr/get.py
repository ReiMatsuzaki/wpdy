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

dt = 0.1
nt = 10
ntskip = 1

for it in range(nt):
    dir_it  = join("out", str(it))
    df = pd.read_csv(join(dir_it, "coef.csv"))
    print it
    coef = df["re"] + 1j * df["im"]
    print np.sum(abs(coef)**2)
    


