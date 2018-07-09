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
wpdy = os.path.expanduser("~/src/wpdy/build/debug/wpdy.x")

# ==== calc info ====
xt = 1.0
pt = 1.0
at = 1.0

xs = np.linspace(-10.0, 10.0, 256*2)
nx = len(xs)
x0 = xs[0]
dx = xs[1]-xs[0]

dt = 0.1
nt = 100
ntskip = 1

nstate = 1
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

# ==== wpdy ====
j = {"wpdy": {"x0":x0, "nx":nx, "dx":dx, "m":m, "nstate":nstate, "imaginary_prop":True},
     "timestep": {"dt": dt, "nt":nt, "ntskip": 1}}
with open("wpdy.in.json", "w") as f:
    f.write(json.dumps(j, indent=1))    
subprocess.check_call([wpdy])
