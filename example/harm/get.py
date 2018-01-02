import json
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

j = json.load(open("wpdy.in.json"))
nt = j["timestep"]["nt"]

prob0 = []
prob1 = []
xs = pd.read_csv("out/xs.csv")["val"]
dx = xs[1]-xs[0]
r1 = []
r2 = []
for it in range(nt+1):
    dir_it = join("out", str(it))
    coef = csv2mat(join(dir_it, "coef.idx.csv"))
    prob0.append(dx*np.sum(abs(coef[:,0])**2))
    prob1.append(dx*np.sum(abs(coef[:,1])**2))

    r1.append( dx*np.sum(coef[:,0].conjugate() * xs * coef[:,0] + coef[:,1].conjugate() * xs    * coef[:,1] ).real)
    r2.append( dx*np.sum(coef[:,0].conjugate() * xs * coef[:,0] + coef[:,1].conjugate() * xs**2 * coef[:,1] ).real)
    
    pd.DataFrame({"re1":coef[:,0].real, "im1":coef[:,0].imag, "abs1":abs(coef[:,0]),
                  "re2":coef[:,1].real, "im2":coef[:,1].imag, "abs2":abs(coef[:,1])}).to_csv(join(dir_it, "psi.csv"), index=None)

pd.DataFrame({"val1":prob0, "val2":prob1}).to_csv("out/prob.csv", index=None)
pd.DataFrame({"val1":r1, "val2":r2}).to_csv("out/r.csv", index=None)

