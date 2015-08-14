import numpy as np
import pylab as py

# Loading data files into arrays 

# A_s files
Asl = np.loadtxt('dCl_A_s_lensing.dat',unpack=True)
As = np.loadtxt('dCl_A_s_.dat',unpack=True)
AsF = np.loadtxt('../../number counts - convergence/data/Fisher_comparision/francesco/dCl_A_s_lensing.dat',unpack=True)

# H0 files
H0l = np.loadtxt('dCl_H0_lensing.dat',unpack=True)
H0 = np.loadtxt('dCl_H0_.dat',unpack=True)
#H0F = np.loadtxt('../../number counts - convergence/data/Fisher_comparision/francesco/dCl_H0_.dat~',unpack=True)

# m_ncdm files
ml = np.loadtxt('dCl_m_ncdm_lensing.dat',unpack=True)
m = np.loadtxt('dCl_m_ncdm_.dat',unpack=True)
#mF = np.loadtxt('../../number counts - convergence/data/Fisher_comparision/francesco/dCl_m_ncdm_.dat~',unpack=True)

# n_s files
nsl = np.loadtxt('dCl_n_s_lensing.dat',unpack=True)
ns = np.loadtxt('dCl_n_s_.dat',unpack=True)
# omega_b files 
obl = np.loadtxt('dCl_omega_b_lensing.dat',unpack=True)
ob = np.loadtxt('dCl_omega_b_.dat',unpack=True)
# omega_cdm files
ocl = np.loadtxt('dCl_omega_cdm_lensing.dat',unpack=True)
oc = np.loadtxt('dCl_omega_cdm_.dat',unpack=True)
ocF = np.loadtxt('../../number counts - convergence/data/Fisher_comparision/francesco/dCl_omega_cdm_.dat',unpack=True)

# As files 

for index in range(1,56):
    py.loglog(Asl[0],abs(Asl[index]),label='Wilmar')
    py.loglog(AsF[0],abs(AsF[index]),label='Francesco')
    py.legend()
    print 'correlation ', index
    py.show()
exit()

#for index in range(1,56):
#    py.loglog(As[0],abs(As[index]),label='Wilmar')
 #   py.loglog(AsF[0],abs(AsF[index]),label='Francesco')
  #  py.legend()
#    print 'correlation ', index
#    py.show()
#exit()

#py.show()

# H0 files

#for index in range(1,56):
#    py.loglog(H0l[0],abs(H0l[index]),label='Wilmar')
#    py.loglog(H0l[0],abs(H0l[index]),label='Francesco')
#    py.legend()
#    print 'correlation ', index
#    py.show()
#exit()

#for index in range(1,56):
#    py.loglog(H0[0],abs(H0[index]),label='Wilmar')
#    py.loglog(H0F[0],abs(H0F[index]),label='Francesco')
#    py.legend()
#    print 'correlation ', index
#    py.show()
#exit()
#py.show()

# m_ncdm files 

#for index in range(1,56):
#    py.loglog(ml[0],abs(ml[index]))
#py.show()

#for index in range(1,56):
#    py.loglog(m[0],abs(m[index]),label='Wilmar')
#    py.loglog(mF[0],abs(mF[index]),label='Francesco')
#    py.legend()
#    print index
#    py.show()
#exit()

# n_s files 

#for index in range(1,56):
#    py.loglog(nsl[0],abs(nsl[index]))
#py.show()

for index in range(1,56):
    py.loglog(ns[0],abs(ns[index]))
py.show()

# omega_b files 

for index in range(1,56):
    py.loglog(obl[0],abs(obl[index]))
py.show()

for index in range(1,56):
    py.loglog(ob[0],abs(ob[index]))
py.show()

# omega_cdm 

for index in range(1,56):
    py.loglog(ocl[0],abs(ocl[index]))
py.show()

for index in range(1,56):
    py.loglog(oc[0],abs(oc[index]))
py.show()

exit()
