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
import json
from wpdy.dvr import ExpDVR

j = json.load(open("out/params.json"))
dvr = ExpDVR(j["dvr_n"], j["dvr_x0"], j["dvr_xN"])
mass = j["mass"]
nstate = j["nstate"]
xs = np.array(pd.read_csv("out/xs.csv")["val"])
nx = len(xs)
ws = pd.read_csv("out/ws.csv")["val"]
ts = pd.read_csv("out/ts.csv")["val"]

its = range(len(ts))
p1s = []
p2s = []
for it in its:
    df = pd.read_csv("out/{0}/coef.csv".format(it))
    tmp  = np.array(df["re"] + 1j * df["im"])
    coef = tmp.reshape((len(xs), nstate))

    p1s.append(np.sum(abs(coef[:,0])**2))
    p2s.append(np.sum(abs(coef[:,1])**2))
    
plt.plot(ts, p1s)
plt.plot(ts, p2s)
plt.savefig("prob.pdf")


