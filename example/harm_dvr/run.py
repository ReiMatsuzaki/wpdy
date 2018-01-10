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

from wpdy.dvr import *

# ==== const ====
wpdy = os.path.expanduser("~/src/wpdy/build/wpdy_dvr")

# ==== calc info ====
nstate = 1
m = 1.2
w = 1.1
a = m*w/2

xt = 0.9
pt = 0.0
at = a

n = 128
x0 = -5.0
xN = 5.0
dvr = ExpDVR(n, x0, xN)
xs = dvr.xs
nx = len(xs)

dt = 1.0
nt = 10
ntskip = 1

# ==== psi0 ====
gs = np.exp(-at*(xs-xt)**2 + 1j*pt*(xs-xt))
with open("psi0.idx.csv", "w") as f:
    f.write("i,j,re,im\n")
    for ix in range(nx):
        f.write("{0},1,{1},{2}\n".format(ix+1, gs[ix].real, gs[ix].imag))

# ==== potential ====
vs1 = m*w*w/2*xs**2
with open("v.idx.csv", "w") as f:
    f.write("i,j,k,re,im\n")
    for ix in range(nx):
        f.write("{0},1,1,{1},0.0\n".format(ix+1, vs1[ix]))
with open("xij.idx.csv", "w") as f:
    f.write("i,j,k,val\n")

# ==== wpdy ====
cmd = map(str,
          [wpdy,
           "-dvr_n",  dvr.n,
           "-dvr_x0", dvr.x0,
           "-dvr_xN", dvr.xN,
           "-fn_psi0", "psi0.idx.csv",
           "-fn_hel", "v.idx.csv",
           "-fn_xij", "xij.idx.csv",
           "-mass", m,
           "-nstate", nstate,
           "-dt", dt,
           "-nt", nt,
           "-ntskip", ntskip,
           "-inte", "diag"])
subprocess.check_call(cmd)


