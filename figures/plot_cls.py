import matplotlib as mpl
mpl.use('Agg')
import numpy as np
import matplotlib.pyplot as py
#import pylab as py
import math

# Loading data files into arrays 

# Fiducial model 
Clfid = np.loadtxt('../data/Cl_fiducial_lensing_cl.dat',unpack=True)

# Error fiducial model 
Elfid = np.loadtxt('../data/El_cl.dat',unpack=True)

# Best fit file
Clbest = np.loadtxt('../data/Cl_bestfit_cl.dat',unpack=True)

# Fiducial model without lensing
Clfidnl = np.loadtxt('../data/Cl_fiducial_no_lensing_cl.dat',unpack=True) 

Nl = np.ones(len(Elfid[0]))

# 1-1

#py.loglog(Clfid[0],abs(Clfid[1]-Clfidnl[1]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[1]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[1]-Clbest[1]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')

#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 1-1')

#py.savefig('correlation_1_1.pdf')

#py.close()

# 1-2

#py.loglog(Clfid[0],abs(Clfid[2]-Clfidnl[2]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[2]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[2]-Clbest[2]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 1-2')

#py.savefig('correlation_1_2.pdf')

#py.close()

# 1-3

#py.plot(Clfid[0],Clfid[3]-Clfidnl[3],label='fiducial with lensing - fiducial without lensing',color='blue')

###### PLOT \DELTA CL/EL
py.plot(Clfid[0],(Clfid[3]-Clfidnl[3])/Elfid[3],label='fiducial with lensing - fiducial without lensing',color='blue')

#py.loglog(Clfid[0],abs(Clfid[3]),label='fiducial with lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[3]),label='fiducial without lensing')

#py.plot(Elfid[0],Elfid[3],label='Error with lensing',color='black')

#py.plot(Elfid[0],-Elfid[3],color='black',ls='--')

#py.plot(Clbest[0],Clfid[3]-Clbest[3],label='fiducial with lensing - bestfit without lensing',color='green')

py.plot(Clbest[0],(Clfid[3]-Clbest[3])/Elfid[3],label='fiducial with lensing - bestfit without lensing',color='green')

py.hlines(0.,1,450,linestyles='dotted')

#py.loglog(Clbest[0],abs(Clbest[3]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$\Delta C_\ell/E_\ell$')

#py.ylabel(r'$|C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 1-3')

py.savefig('correlation_1_3.pdf')

py.close()
#### END PLOT \DELTA CL/EL

#### PLOT \DELTA CL
py.plot(Clfid[0],(Clfid[3]-Clfidnl[3])*2.*math.pi/Clfid[0]/(Clfid[0]+1.),label='fiducial with lensing - fiducial without lensing',color='blue')

#py.loglog(Clfid[0],abs(Clfid[3]),label='fiducial with lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[3]),label='fiducial without lensing')

py.plot(Elfid[0],Elfid[3]*2.*math.pi/Elfid[0]/(Elfid[0]+1.),label='Error with lensing',color='black')

py.plot(Elfid[0],-Elfid[3]*2.*math.pi/Elfid[0]/(Elfid[0]+1.),color='black',ls='--')

#py.plot(Clbest[0],Clfid[3]-Clbest[3],label='fiducial with lensing - bestfit without lensing',color='green')

py.plot(Clbest[0],(Clfid[3]-Clbest[3])*2.*math.pi/Clfid[0]/(Clfid[0]+1.),label='fiducial with lensing - bestfit without lensing',color='green')

py.hlines(0.,1,450,linestyles='dotted')

#py.loglog(Clbest[0],abs(Clbest[3]),label='bestfit without lensing')

py.xlabel(r'$\ell$')

py.ylabel(r'$\Delta C_\ell$')

#py.ylabel(r'$|C_\ell|$')
    
py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 1-3')

py.savefig('correlation_Cl_1_3.pdf')

py.close()
#### END PLOT \DELTA CL

#### LOGLOG PLOT \DELTA CL/EL
py.loglog(Clfid[0],abs((Clfid[3]-Clfidnl[3])/Elfid[3]),label='fiducial with lensing - fiducial without lensing',color='blue')

py.loglog(Clbest[0],abs((Clfid[3]-Clbest[3])/Elfid[3]),label='fiducial with lensing - bestfit without lensing',color='green')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell/E_\ell|$')

py.xlim(1,450)

py.legend(loc=0)

py.title('Correlation bins 1-3')

py.savefig('correlation_abs_1_3.pdf')

py.close()
#### END LOGLOG PLOT \DELTA CL/EL

#py.plot(Clfid[0],Clfid[3],label='fiducial with lensing')

#py.plot(Clfidnl[0],Clfidnl[3],label='fiducial without lensing')

#py.plot(Elfid[0],Elfid[3],label='Error with lensing',color='black')

#py.plot(Clbest[0],Clbest[3],label='bestfit without lensing')

#py.hlines(0.,1,450,linestyles='dotted')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$C_\ell$')

#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 1-3')

#py.savefig('cl_1_3.pdf')

#py.close()

# 1-4

#py.loglog(Clfid[0],abs(Clfid[4]-Clfidnl[4]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[4]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[4]-Clbest[4]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 1-4')

#py.savefig('correlation_1_4.pdf')

#py.close()

# 1-5

#py.loglog(Clfid[0],abs(Clfid[5]-Clfidnl[5]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[5]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[5]-Clbest[5]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 1-5')

#py.savefig('correlation_1_5.pdf')

#py.close()

# 2-2

#py.loglog(Clfid[0],abs(Clfid[6]-Clfidnl[6]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[6]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[6]-Clbest[6]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 2-2')

#py.savefig('correlation_2_2.pdf')

#py.close()

# 2-3

#py.loglog(Clfid[0],abs(Clfid[7]-Clfidnl[7]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[7]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[7]-Clbest[7]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')

#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 2-3')

#py.savefig('correlation_2_3.pdf')

#py.close()

# 2-4

#py.loglog(Clfid[0],abs(Clfid[8]-Clfidnl[8]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[8]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[8]-Clbest[8]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 2-4')

#py.savefig('correlation_2_4.pdf')

#py.close()

# 2-5

#py.loglog(Clfid[0],abs(Clfid[9]-Clfidnl[9]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[9]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[9]-Clbest[9]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 2-5')

#py.savefig('correlation_2_5.pdf')

#py.close()

# 3-3

#### PLOT \DELTA CL/EL
py.plot(Clfid[0],(Clfid[8]-Clfidnl[8])/Elfid[8],label='fiducial with lensing - fiducial without lensing',color='blue')
#    else:
#        py.plot(Clfid[0,:],abs(Clfid[8,:]-Clfidnl[8,:]),label='fiducial with lensing - fiducial without lensing',color='b',ls='--')

#py.loglog(Clfid[0],abs(Clfid[8]),label='fiducial with lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[8]),label='fiducial without lensing')

#py.plot(Elfid[0],Elfid[8],label='Error with lensing',color='black')

#py.plot(Elfid[0],-Elfid[8],label='Minus Error with lensing',color='black',ls='--')

#py.plot(Clbest[0],Clfid[8]-Clbest[8],label='fiducial with lensing - bestfit without lensing',color='green')

py.plot(Clbest[0],(Clfid[8]-Clbest[8])/Elfid[8],label='fiducial with lensing - bestfit without lensing',color='green')

py.hlines(0.,1.,450.,linestyles='dotted')

py.xlabel(r'$\ell$')

py.ylabel(r'$\Delta C_\ell/E_\ell$')
    
py.xlim(1,450)

#py.ylim(-3.e-4,6.e-4)

py.legend(loc=0)

py.title('Correlation bins 3-3')

py.savefig('correlation_3_3.pdf')

py.close()
#### END PLOT \DELTA CL/EL

#### LOGLOG PLOT \DELTA CL/EL
py.loglog(Clfid[0],abs((Clfid[8]-Clfidnl[8])/Elfid[8]),label='fiducial with lensing - fiducial without lensing',color='blue')

py.loglog(Clbest[0],abs((Clfid[8]-Clbest[8])/Elfid[8]),label='fiducial with lensing - bestfit without lensing',color='green')

#py.hlines(0.,1.,450.,linestyles='dotted')

py.xlabel(r'$\ell$')

py.ylabel(r'$|\Delta C_\ell/E_\ell|$')
    
py.xlim(1,450)

#py.ylim(-3.e-4,6.e-4)

py.legend(loc=0)

py.title('Correlation bins 3-3')

py.savefig('correlation_abs_3_3.pdf')

py.close()
#### END LOGLOG \DELTA CL/EL

#### PLOT \DELTA CL

py.plot(Clfid[0],(Clfid[8]-Clfidnl[8])*2.*math.pi/Clfid[0]/(Clfid[0]+1.),label='fiducial with lensing - fiducial without lensing',color='blue')
#    else:
#        py.plot(Clfid[0,:],abs(Clfid[8,:]-Clfidnl[8,:]),label='fiducial with lensing - fiducial without lensing',color='b',ls='--')

#py.loglog(Clfid[0],abs(Clfid[8]),label='fiducial with lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[8]),label='fiducial without lensing')

py.plot(Elfid[0],Nl[:]*1.e-6,label='shot noise',color='red')

py.plot(Elfid[0],Elfid[8]*2.*math.pi/Elfid[0]/(Elfid[0]+1.),label='Error with lensing',color='black',ls='--')

#py.plot(Clbest[0],Clfid[8]-Clbest[8],label='fiducial with lensing - bestfit without lensing',color='green')

py.plot(Clbest[0],(Clfid[8]-Clbest[8])*2.*math.pi/Clbest[0]/(Clbest[0]+1.),label='fiducial with lensing - bestfit without lensing',color='green')

py.hlines(0.,1.,450.,linestyles='dotted')

py.xlabel(r'$\ell$')

py.ylabel(r'$\Delta C_\ell$')
    
py.xlim(1,450)

py.ylim(-2.e-7,2.e-6)

py.legend(loc=0)

py.title('$\Delta C_\ell$ Correlation bins 3-3')

py.savefig('correlation_Cl_3_3.pdf')

py.close()
#### END PLOT \DELTA CL

#py.plot(Clfid[0],Clfid[8],label='fiducial with lensing')

#py.plot(Clfidnl[0],Clfidnl[8],label='fiducial without lensing')

#py.plot(Elfid[0],Elfid[8],label='Error with lensing',color='black')

#py.plot(Clbest[0],Clbest[8],label='bestfit without lensing')

#py.hlines(0.,1,450,linestyles='dotted')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$C_\ell$')

#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 3-3')

#py.savefig('cl_3_3.pdf')

#py.close()

# 3-4

#py.loglog(Clfid[0],abs(Clfid[11]-Clfidnl[11]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[11]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[11]-Clbest[11]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 3-4')

#py.savefig('correlation_3_4.pdf')

#py.close()

# 3-5

#py.loglog(Clfid[0],abs(Clfid[12]-Clfidnl[12]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[12]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[12]-Clbest[12]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 3-5')

#py.savefig('correlation_3_5.pdf')

#py.close()

# 4-4

#py.loglog(Clfid[0],abs(Clfid[13]-Clfidnl[13]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[13]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[13]-Clbest[13]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 4-4')

#py.savefig('correlation_4_4.pdf')

#py.close()

# 4-5

#py.loglog(Clfid[0],abs(Clfid[14]-Clfidnl[14]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[14]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[14]-Clbest[14]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 4-5')

#py.savefig('correlation_4_5.pdf')

#py.close()

# 5-5

#py.loglog(Clfid[0],abs(Clfid[15]-Clfidnl[15]),label='fiducial with lensing - fiducial without lensing')

#py.loglog(Clfidnl[0],abs(Clfidnl[15]),label='fiducial without lensing')

#py.loglog(Clbest[0],abs(Clfid[15]-Clbest[15]),label='fiducial with lensing - bestfit without lensing')

#py.xlabel(r'$\ell$')

#py.ylabel(r'$|\Delta C_\ell|$')
    
#py.xlim(1,450)

#py.legend(loc=0)

#py.title('Correlation bins 5-5')

#py.savefig('correlation_5_5.pdf')

#py.close()

exit()
