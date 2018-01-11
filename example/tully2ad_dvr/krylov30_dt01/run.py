import os
join = os.path.join
exists = os.path.exists
expanduser = os.path.expanduser
import sys
import json
import subprocess 
import numpy as np
tr = np.transpose
import pandas as pd

from wpdy.dvr import *

# ==== const ====
wpdy = os.path.expanduser("~/src/wpdy/build/wpdy_dvr")

# ==== calc info ====
nstate = 2
m = 2000.0

n = 256
x0 = -13.0
xN = 13.0
dvr = ExpDVR(n, x0, xN)
xs = dvr.xs
nx = len(xs)
dx = xs[1]-xs[0]

ntskip = 10
dt = 10.0/ntskip
nt = 150*ntskip

# ==== psi0 ====
xt = -7.0
pt = 20.0
at = 1.0
gs = np.exp(-at*(xs-xt)**2 + 1j*pt*(xs-xt))
with open("psi0.idx.csv", "w") as f:
    f.write("i,j,re,im\n")
    for ix in range(nx):
        f.write("{0},1,{1},{2}\n".format(ix+1, gs[ix].real, gs[ix].imag))

# ==== potential ====
A = 0.1
B = 0.28
C = 0.015
D = 0.06
E = 0.05
v11 = xs*0.0
v22 = -A*np.exp(-B*xs**2) + E
v12 = C*np.exp(-D*xs**2)
v21 = v12
e1 = (v11+v22)/2 - np.sqrt((v11+v22)**2 -4*(v11*v22-v12*v21))/2
e2 = (v11+v22)/2 + np.sqrt((v11+v22)**2 -4*(v11*v22-v12*v21))/2
us = np.array([[v12, v12], [e1-v11, e2-v11]])
n0 = 1/np.sqrt(us[0,0,:]**2 + us[1,0,:]**2)
n1 = 1/np.sqrt(us[0,1,:]**2 + us[1,1,:]**2)
uss = np.array([[n0*us[0,0,:], n1*us[0,1,:]],
                [n0*us[1,0,:], n1*us[1,1,:]]])

with open("v.idx.csv", "w") as f:
    f.write("i,j,k,re,im\n")
    for ix in range(nx):
        f.write("{0},1,1,{1},0.0\n".format(ix+1, e1[ix]))
        f.write("{0},2,2,{1},0.0\n".format(ix+1, e2[ix]))
        
with open("xij.idx.csv", "w") as f:
    f.write("i,j,k,re,im\n")
    for ix in range(1,nx-1):
        x12  = (uss[0,0,ix]*(uss[0,1,ix+1] - uss[0,1,ix-1])/(2*dx) +
                uss[1,0,ix]*(uss[1,1,ix+1] - uss[1,1,ix-1])/(2*dx) )
        f.write("{0},1,2,{1},0.0\n".format(ix+1, x12))
        f.write("{0},2,1,{1},0.0\n".format(ix+1, -x12))
        
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
           "-inte", "krylov",
           "-krylov_num", 30])
subprocess.check_call(cmd)


