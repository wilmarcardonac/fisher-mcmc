from getdist import loadMCSamples,plots,covmat
import numpy as np
import os,fnmatch

#filenames = fnmatch.filter(os.listdir("../output/chains/"),"mcmc_*.txt")

#for index in range(len(filenames)):
    
#    os.rename("../output/chains/"+str(filenames[index]),"../output/chains/mcmc_final_output_"+str(index+1)+".txt")

number_of_parameters = 10

samples = loadMCSamples('../output/chains/NC-run4/mcmc_final_output',settings={'ignore_rows':0.})

p = samples.getParams()

samples.addDerived(np.log(1.e1**10*p.A_s),name='ln1010As',label='\ln 10^{10}A_s')

samples.addDerived(np.log10(p.cs2_fld),name='logcs2fld',label='\log c_s^2')

bestfit = samples.getLikeStats()

means = samples.setMeans()

filebestfit = open("../output/chains/bestfit.txt",'w')

filemeans = open("../output/chains/means.txt",'w')

#filebestfit.write("-log(Like) = "+str(bestfit.logLike_sample)+"\n")

for index in range(number_of_parameters) : 

    filebestfit.write(str(bestfit.names[index].bestfit_sample)+"\n")

    filemeans.write(str(means[index])+"\n")

filebestfit.close()

filemeans.close()

covariance_matrix = samples.getCov(pars=[0,1,2,10,4,5,6,11,8,9])#nparam=number_of_parameters)

covariance_matrix_2 = covmat.CovMat(matrix=covariance_matrix)

covariance_matrix_2.saveToFile('../output/chains/covariance_matrix.txt')

print 'COVARIANCE MATRIX CREATED'

exit()

