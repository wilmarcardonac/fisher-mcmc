import matplotlib as mpl
mpl.use('Agg')
import numpy as np
import matplotlib.pyplot as py
#import pylab as py

# Loading data files into arrays 

# Fiducial model 
Clfid = np.loadtxt('../data/Cl_fiducial_lensing_cl.dat',unpack=True)
# Error file
Clbest = np.loadtxt('../data/Cl_bestfit_no_lensing_cl.dat',unpack=True)

# Fiducial model without lensing
Clfidnl = np.loadtxt('../data/Cl_fiducial_no_lensing_cl.dat',unpack=True) 

# 1-1

py.loglog(Clfid[0],abs(Clfid[1]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[1]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[1]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')

py.legend(loc=0)

py.title('Correlation bins 1-1')

py.savefig('correlation_1_1.pdf')

py.close()

# 1-2

py.loglog(Clfid[0],abs(Clfid[2]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[2]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[2]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 1-2')

py.savefig('correlation_1_2.pdf')

py.close()

# 1-3

py.loglog(Clfid[0],abs(Clfid[3]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[3]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[3]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 1-3')

py.savefig('correlation_1_3.pdf')

py.close()

# 1-4

py.loglog(Clfid[0],abs(Clfid[4]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[4]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[4]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 1-4')

py.savefig('correlation_1_4.pdf')

py.close()

# 1-5

py.loglog(Clfid[0],abs(Clfid[5]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[5]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[5]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 1-5')

py.savefig('correlation_1_5.pdf')

py.close()

# 2-2

py.loglog(Clfid[0],abs(Clfid[6]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[6]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[6]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 2-2')

py.savefig('correlation_2_2.pdf')

py.close()

# 2-3

py.loglog(Clfid[0],abs(Clfid[7]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[7]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[7]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 2-3')

py.savefig('correlation_2_3.pdf')

py.close()

# 2-4

py.loglog(Clfid[0],abs(Clfid[8]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[8]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[8]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 2-4')

py.savefig('correlation_2_4.pdf')

py.close()

# 2-5

py.loglog(Clfid[0],abs(Clfid[9]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[9]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[9]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 2-5')

py.savefig('correlation_2_5.pdf')

py.close()

# 3-3

py.loglog(Clfid[0],abs(Clfid[10]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[10]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[10]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 3-3')

py.savefig('correlation_3_3.pdf')

py.close()

# 3-4

py.loglog(Clfid[0],abs(Clfid[11]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[11]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[11]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 3-4')

py.savefig('correlation_3_4.pdf')

py.close()

# 3-5

py.loglog(Clfid[0],abs(Clfid[12]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[12]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[12]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 3-5')

py.savefig('correlation_3_5.pdf')

py.close()

# 4-4

py.loglog(Clfid[0],abs(Clfid[13]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[13]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[13]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 4-4')

py.savefig('correlation_4_4.pdf')

py.close()

# 4-5

py.loglog(Clfid[0],abs(Clfid[14]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[14]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[14]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 4-5')

py.savefig('correlation_4_5.pdf')

py.close()

# 5-5

py.loglog(Clfid[0],abs(Clfid[15]),label='fiducial with lensing')

py.loglog(Clfidnl[0],abs(Clfidnl[15]),label='fiducial without lensing')

py.loglog(Clbest[0],abs(Clbest[15]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$C_\ell$')
    
py.legend(loc=0)

py.title('Correlation bins 5-5')

py.savefig('correlation_5_5.pdf')

py.close()

exit()
