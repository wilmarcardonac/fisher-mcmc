import matplotlib as mpl
mpl.use('Agg')
import numpy as np
import matplotlib.pyplot as py
#import pylab as py

# Loading data files into arrays 

# Fiducial model 
Clfid = np.loadtxt('../data/Cl_fiducial_lensing_cl.dat',unpack=True)
# Error file
#Clbest = np.loadtxt('../data/Cl_bestfit_no_lensing_cl.dat',unpack=True)
Clnmnu = np.loadtxt('../data/Cl_fiducial_lensing_cl-without-massive-nu.dat',unpack=True)

# Fiducial model without lensing
Clfidnl = np.loadtxt('../data/Cl_fiducial_no_lensing_cl.dat',unpack=True) 

# 1-1

py.loglog(Clfid[0],abs((Clfid[1]-Clfidnl[1])/Clfid[1])*100.,label='1-1',color='blue')

#py.loglog(Clfidnl[0],abs(Clfidnl[1]),label='fiducial without lensing')

py.loglog(Clfid[0],abs((Clfid[1]-Clnmnu[1])/Clfid[1])*100.,label='1-1',color='green')

#py.loglog(Clfid[0],abs((Clfid[1]-Clfidnl[1])/Clfid[1])*100.,label='1-1',color='blue')

#py.loglog(Clfidnl[0],abs(Clfidnl[1]),label='fiducial without lensing')

py.loglog(Clfid[0],abs((Clfid[5]-Clfidnl[5])/Clfid[5])*100.,label='1-5',color='blue',linestyle='dotted')

py.loglog(Clfid[0],abs((Clfid[5]-Clnmnu[5])/Clfid[5])*100.,label='1-5',color='green',linestyle='dotted')

py.loglog(Clfid[0],abs((Clfid[15]-Clfidnl[15])/Clfid[15])*100.,label='5-5',color='blue',linestyle='dashed')

py.loglog(Clfid[0],abs((Clfid[15]-Clnmnu[15])/Clfid[15])*100.,label='5-5',color='green',linestyle='dashed')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell/C_\ell^{\mathrm{fid}}|*100$')

py.xlim(1,450)

py.legend(loc=0)

#py.title('Correlation bins 1-1')

py.savefig('correlation_comparison.pdf')

py.close()

exit()

# 1-2

py.loglog(Clfid[0],abs(Clfid[2]-Clfidnl[2]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[2]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[2]-Clbest[2]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 1-2')

py.savefig('correlation_1_2.pdf')

py.close()

# 1-3

py.loglog(Clfid[0],abs(Clfid[3]-Clfidnl[3]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[3]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[3]-Clbest[3]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 1-3')

py.savefig('correlation_1_3.pdf')

py.close()

# 1-4

py.loglog(Clfid[0],abs(Clfid[4]-Clfidnl[4]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[4]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[4]-Clbest[4]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 1-4')

py.savefig('correlation_1_4.pdf')

py.close()

# 1-5

py.loglog(Clfid[0],abs(Clfid[5]-Clfidnl[5]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[5]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[5]-Clbest[5]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 1-5')

py.savefig('correlation_1_5.pdf')

py.close()

# 2-2

py.loglog(Clfid[0],abs(Clfid[6]-Clfidnl[6]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[6]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[6]-Clbest[6]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 2-2')

py.savefig('correlation_2_2.pdf')

py.close()

# 2-3

py.loglog(Clfid[0],abs(Clfid[7]-Clfidnl[7]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[7]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[7]-Clbest[7]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')

py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 2-3')

py.savefig('correlation_2_3.pdf')

py.close()

# 2-4

py.loglog(Clfid[0],abs(Clfid[8]-Clfidnl[8]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[8]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[8]-Clbest[8]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 2-4')

py.savefig('correlation_2_4.pdf')

py.close()

# 2-5

py.loglog(Clfid[0],abs(Clfid[9]-Clfidnl[9]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[9]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[9]-Clbest[9]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 2-5')

py.savefig('correlation_2_5.pdf')

py.close()

# 3-3

py.loglog(Clfid[0],abs(Clfid[10]-Clfidnl[10]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[10]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[10]-Clbest[10]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 3-3')

py.savefig('correlation_3_3.pdf')

py.close()

# 3-4

py.loglog(Clfid[0],abs(Clfid[11]-Clfidnl[11]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[11]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[11]-Clbest[11]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 3-4')

py.savefig('correlation_3_4.pdf')

py.close()

# 3-5

py.loglog(Clfid[0],abs(Clfid[12]-Clfidnl[12]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[12]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[12]-Clbest[12]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 3-5')

py.savefig('correlation_3_5.pdf')

py.close()

# 4-4

py.loglog(Clfid[0],abs(Clfid[13]-Clfidnl[13]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[13]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[13]-Clbest[13]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 4-4')

py.savefig('correlation_4_4.pdf')

py.close()

# 4-5

py.loglog(Clfid[0],abs(Clfid[14]-Clfidnl[14]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[14]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[14]-Clbest[14]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 4-5')

py.savefig('correlation_4_5.pdf')

py.close()

# 5-5

py.loglog(Clfid[0],abs(Clfid[15]-Clfidnl[15]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[15]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clfid[15]-Clbest[15]),label='fiducial with lensing - bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 5-5')

py.savefig('correlation_5_5.pdf')

py.close()

exit()
