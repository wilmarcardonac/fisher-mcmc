import numpy as np

Data = np.loadtxt('mcmc_output.dat')

Cov = np.cov(Data[1e5:,2:],rowvar=0)

np.savetxt('covariance_matrix.dat',Cov,fmt='%.10E')

index = np.where(Data[:,1]==np.max(Data[:,1]))[0][0]

bestfit = np.zeros(7)

for i in range(0,7) :
    bestfit[i] = Data[index,i+2]

np.savetxt('bestfit.dat',bestfit,fmt='%.10E')

exit()
