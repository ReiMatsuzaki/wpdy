import os
join = os.path.join
exists = os.path.exists
expanduser = os.path.expanduser
import sys
import json
import subprocess 
import numpy as np
tr = np.transpose
exp = np.exp
import pandas as pd

from wpdy.dvr import *

# ==== const ====
wpdy = os.path.expanduser("~/src/wpdy/build/debug/wpdy_dvr.x")

# ==== calc info ====
nstate = 2
m = 2000.0

n = 256
x0 = -13.0
xN = 13.0
dvr = ExpDVR(n, x0, xN)
xs = dvr.xs
nx = len(xs)

dt = 20.0
nt = 100
ntskip = 1

# ==== psi0 ====
xt = -7.0
pt = 15.0
at = 1.0
gs = np.exp(-at*(xs-xt)**2 + 1j*pt*(xs-xt))
with open("psi0.idx.csv", "w") as f:
    f.write("i,j,re,im\n")
    for ix in range(nx):
        f.write("{0},1,{1},{2}\n".format(ix+1, gs[ix].real, gs[ix].imag))

# ==== potential ====
A = 0.01
B = 1.6
C = 0.005
D = 1.0
v11 = np.where(xs>0, A*(1-exp(-B*xs)), -A*(1-exp(+B*xs)))
v22 = -v11
v12 = C*exp(-D*xs*xs)
v21 = v12
with open("v.idx.csv", "w") as f:
    f.write("i,j,k,re,im\n")
    for ix in range(nx):
        f.write("{0},1,1,{1},0.0\n".format(ix+1, v11[ix]))
        f.write("{0},1,2,{1},0.0\n".format(ix+1, v12[ix]))
        f.write("{0},2,1,{1},0.0\n".format(ix+1, v21[ix]))
        f.write("{0},2,2,{1},0.0\n".format(ix+1, v22[ix]))
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


