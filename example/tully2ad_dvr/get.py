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

jsize = json.load(open("out/size.json"))
nstate = jsize["nstate"]
xs = pd.read_csv("out/xs.csv")["val"]
nx = len(xs)
ws = pd.read_csv("out/ws.csv")["val"]
ts = pd.read_csv("out/ts.csv")["val"]

f_norm = open("out/norm.csv", "w"); f_norm.write("val\n")
f_r = open("out/r.csv", "w"); f_r.write("val\n")
f_prob = open("out/prob.csv", "w"); f_prob.write("val1,val2\n")

for it in range(len(ts)):
    
    t = ts[it]
    print
    print "{0}: {1}/{2}".format(__file__, it, len(ts)-1)
    print "t = {0}".format(t)    
    dir_it  = join("out", str(it))
    
    df = pd.read_csv(join(dir_it, "coef.csv"))
    tmp  = np.array(df["re"] + 1j * df["im"])
    coef = tmp.reshape((len(xs), nstate))

    psi1 = coef[:,0] / np.sqrt(ws)
    with open(join(dir_it, "psi1.csv"), "w") as f:
        f.write("re,im,abs\n")
        for ix in range(len(xs)):
            y = psi1[ix]
            f.write("{0},{1},{2}\n".format(y.real, y.imag, abs(y)))
    
    norm = np.sqrt(np.sum(abs(coef)**2))
    f_norm.write(str(norm)+"\n")

    x = np.vstack([xs]*nstate).T
    r = np.sum(abs(coef)**2 * x)
    f_r.write(str(r)+"\n")

    prob1 = np.sum(abs(coef[:,0])**2)
    prob2 = np.sum(abs(coef[:,1])**2)
    f_prob.write("{0},{1}\n".format(prob1, prob2))

f_norm.close()    
f_r.close()    
f_prob.close()

