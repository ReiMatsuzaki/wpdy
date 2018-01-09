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
wpdy = os.path.expanduser("~/src/wpdy/build/wpdy_dvr")

# ==== calc info ====
xt = 1.0
pt = 1.0
at = 1.0

n = 128
nx = 2*n+1
xs = np.linspace(-10.0, 10.0, nx)
x0 = xs[0]
dx = xs[1]-xs[0]

dt = 0.1
nt = 10
ntskip = 1

nstate = 2
m = 1.0

# ==== psi0 ====
gs = np.exp(-at*(xs-xt)**2 + 1j*pt*(xs-xt))
with open("psi0.idx.csv", "w") as f:
    f.write("i,j,re,im\n")
    for ix in range(nx):
        f.write("{0},1,{1},{2}\n".format(ix+1, gs[ix].real, gs[ix].imag))

# ==== potential ====
vs1 = xs**2
#vs2 = xs**2 + 1.0        
with open("v.idx.csv", "w") as f:
    f.write("i,j,k,val\n")
    for ix in range(nx):
        f.write("1,1,{0},{1}\n".format(ix+1, vs1[ix]))
with open("xij.idx.csv", "w") as f:
    f.write("i,j,k,val\n")

# ==== wpdy ====
cmd = map(str,
          [wpdy,
           "-dvr_n", n,
           "-dvr_x0", x0,
           "-dvr_x1", xs[-1],
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


