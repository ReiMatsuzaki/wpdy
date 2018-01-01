import os
join = os.path.join
exists = os.path.exists
expanduser = os.path.expanduser
import sys
import json
import subprocess 
import matplotlib as mpl
mpl.use('PDF')
import matplotlib.pylab as plt
import numpy as np
tr = np.transpose
import pandas as pd

# ==== const ====
wpdy = os.path.expanduser("~/src/wpdy/build/wpdy")

# ==== calc info ====
xt = 1.0
pt = 0.0
at = 1.0

nx = 100
dx = 0.1
x0 = -5.0

dt = 1.0
nt = 1
ntskip = 1

nstate = 2
m = 1.0

# ==== psi0 ====
xs = x0 + np.arange(nx)*dx
gs = np.exp(-at*(xs-xt)**2 + 1j*pt*(xs-xt))
with open("psi0.csv", "w") as f:
    f.write("i,j,re,im\n")
    for ix in range(nx):
        f.write("{0},0,{1},{2}\n".format(ix, gs[ix].real, gs[ix].imag))

# ==== wpdy ====
j = {"wpdy": {"x0":x0, "nx":nx, "dx":dx, "m":m, "nstate":nstate },
     "timestep": {"dt": 0.1, "nt":nt, "ntskip": 1}}
with open("wpdy.in.json", "w") as f:
    s = json.dumps(j)
    f.write(s)
subprocess.check_call([wpdy])
