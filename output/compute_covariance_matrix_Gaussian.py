import numpy as np

data = np.loadtxt('mcmc_output.txt')

Cov = np.cov(data[:,2:],rowvar=0)

np.savetxt('covariance_matrix.txt',Cov,fmt='%.10E')

exit()
