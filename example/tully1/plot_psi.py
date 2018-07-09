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

its = [0,20,40,60,80]
for it in its:
    df = pd.read_csv("out/{0}/coef.csv".format(it))
    tmp  = np.array(df["re"] + 1j * df["im"])
    coef = tmp.reshape((len(xs), nstate))
    
    psi1 = coef[:,0] / np.sqrt(ws)
    psi2 = coef[:,1] / np.sqrt(ws)

    # pl, = plt.plot(xs, abs(psi1)**2, label="state 1")
    pl, = plt.plot(xs, psi1.real, label="state 1")
    plt.plot(      xs, psi2.real, color=pl.get_color(), linestyle="--", label="state 2")
plt.xlim(-11,11)
plt.savefig("psi.pdf")


