import numpy as np

data = np.loadtxt('mcmc_output.txt')

for index in range(len(data[:,5])):
    data[index,5] = np.log(1e1**1e1*data[index,5])
    data[index,9] = np.log10(data[index,9])

Cov = np.cov(data[:,2:],rowvar=0)

np.savetxt('covariance_matrix.txt',Cov,fmt='%.10E',header=' ',comments='# ')

exit()
