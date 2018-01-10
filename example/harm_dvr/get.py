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

xs = pd.read_csv("out/xs.csv")["val"]
ws = pd.read_csv("out/ws.csv")["val"]
ts = pd.read_csv("out/ts.csv")["val"]
f_norm = open("out/norm.csv", "w"); f_norm.write("val\n")
f_r = open("out/r.csv", "w"); f_r.write("val\n")
print len(ts)

for it in range(len(ts)):
    
    t = ts[it]
    print
    print "{0}: {1}/{2}".format(__file__, it, len(ts)-1)
    print "t = {0}".format(t)    
    dir_it  = join("out", str(it))
    
    df = pd.read_csv(join(dir_it, "coef.csv"))    
    coef = df["re"] + 1j * df["im"]

    psi = coef / np.sqrt(ws)
    with open(join(dir_it, "psi.csv"), "w") as f:
        f.write("re,im,abs\n")
        for y in psi:
            f.write("{0},{1},{2}\n".format(y.real, y.imag, abs(y)))
    
    norm = np.sqrt(np.sum(abs(coef)**2))
    f_norm.write(str(norm)+"\n")

    r = np.sum(abs(coef)**2 * xs)
    f_r.write(str(r)+"\n")

f_norm.close()    
f_r.close()    


