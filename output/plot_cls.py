import numpy as np
import pylab as py

# Loading data files into arrays 

# Fiducial model 
Clfid = np.loadtxt('fiducial_euclid_galaxy_cl.dat',unpack=True)
# Error file
El = np.loadtxt('El_fiducial_euclid_galaxy_cl.dat',unpack=True)
# Fiducial model without lensing
Clfidnl = np.loadtxt('fiducial_euclid_galaxy_no_lensing_cl.dat',unpack=True) 
# A_s files
Asl1 = np.loadtxt('Cl_euclid_galaxy_A_s_1.9376000000E-09_lensing_cl.dat',unpack=True)
Asl2 = np.loadtxt('Cl_euclid_galaxy_A_s_2.0622000000E-09_lensing_cl.dat',unpack=True)
Asl3 = np.loadtxt('Cl_euclid_galaxy_A_s_2.3114000000E-09_lensing_cl.dat',unpack=True)
Asl4 = np.loadtxt('Cl_euclid_galaxy_A_s_2.4360000000E-09_lensing_cl.dat',unpack=True)
As1 = np.loadtxt('Cl_euclid_galaxy_A_s_1.9376000000E-09_cl.dat',unpack=True)
As2 = np.loadtxt('Cl_euclid_galaxy_A_s_2.0622000000E-09_cl.dat',unpack=True)
As3 = np.loadtxt('Cl_euclid_galaxy_A_s_2.3114000000E-09_cl.dat',unpack=True)
As4 = np.loadtxt('Cl_euclid_galaxy_A_s_2.4360000000E-09_cl.dat',unpack=True)
# H0 files
H0l1 = np.loadtxt('Cl_euclid_galaxy_H0_6.4900000000E+01_lensing_cl.dat',unpack=True)
H0l2 = np.loadtxt('Cl_euclid_galaxy_H0_6.6400000000E+01_lensing_cl.dat',unpack=True)
H0l3 = np.loadtxt('Cl_euclid_galaxy_H0_6.9400000000E+01_lensing_cl.dat',unpack=True)
H0l4 = np.loadtxt('Cl_euclid_galaxy_H0_7.0900000000E+01_lensing_cl.dat',unpack=True)
H01 = np.loadtxt('Cl_euclid_galaxy_H0_6.4900000000E+01_cl.dat',unpack=True)
H02 = np.loadtxt('Cl_euclid_galaxy_H0_6.6400000000E+01_cl.dat',unpack=True)
H03 = np.loadtxt('Cl_euclid_galaxy_H0_6.9400000000E+01_cl.dat',unpack=True)
H04 = np.loadtxt('Cl_euclid_galaxy_H0_7.0900000000E+01_cl.dat',unpack=True)
# m_ncdm files
ml1 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_5.0000000000E-02_lensing_cl.dat',unpack=True)
ml2 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_5.5000000000E-02_lensing_cl.dat',unpack=True)
ml3 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_6.5000000000E-02_lensing_cl.dat',unpack=True)
ml4 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_7.0000000000E-02_lensing_cl.dat',unpack=True)
m1 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_5.0000000000E-02_cl.dat',unpack=True)
m2 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_5.5000000000E-02_cl.dat',unpack=True)
m3 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_6.5000000000E-02_cl.dat',unpack=True)
m4 = np.loadtxt('Cl_euclid_galaxy_m_ncdm_7.0000000000E-02_cl.dat',unpack=True)
# n_s files
nsl1 = np.loadtxt('Cl_euclid_galaxy_n_s_9.4470000000E-01_lensing_cl.dat',unpack=True)
nsl2 = np.loadtxt('Cl_euclid_galaxy_n_s_9.5410000000E-01_lensing_cl.dat',unpack=True)
nsl3 = np.loadtxt('Cl_euclid_galaxy_n_s_9.7290000000E-01_lensing_cl.dat',unpack=True)
nsl4 = np.loadtxt('Cl_euclid_galaxy_n_s_9.8230000000E-01_lensing_cl.dat',unpack=True)
ns1 = np.loadtxt('Cl_euclid_galaxy_n_s_9.4470000000E-01_cl.dat',unpack=True)
ns2 = np.loadtxt('Cl_euclid_galaxy_n_s_9.5410000000E-01_cl.dat',unpack=True)
ns3 = np.loadtxt('Cl_euclid_galaxy_n_s_9.7290000000E-01_cl.dat',unpack=True)
ns4 = np.loadtxt('Cl_euclid_galaxy_n_s_9.8230000000E-01_cl.dat',unpack=True)
# omega_b files 
obl1 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.1510000000E-02_lensing_cl.dat',unpack=True)
obl2 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.1840000000E-02_lensing_cl.dat',unpack=True)
obl3 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.2500000000E-02_lensing_cl.dat',unpack=True)
obl4 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.2830000000E-02_lensing_cl.dat',unpack=True)
ob1 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.1510000000E-02_cl.dat',unpack=True)
ob2 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.1840000000E-02_cl.dat',unpack=True)
ob3 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.2500000000E-02_cl.dat',unpack=True)
ob4 = np.loadtxt('Cl_euclid_galaxy_omega_b_2.2830000000E-02_cl.dat',unpack=True)
# omega_cdm files
ocl1 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.1240000000E-01_lensing_cl.dat',unpack=True)
ocl2 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.1550000000E-01_lensing_cl.dat',unpack=True)
ocl3 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.2170000000E-01_lensing_cl.dat',unpack=True)
ocl4 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.2480000000E-01_lensing_cl.dat',unpack=True)
oc1 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.1240000000E-01_cl.dat',unpack=True)
oc2 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.1550000000E-01_cl.dat',unpack=True)
oc3 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.2170000000E-01_cl.dat',unpack=True)
oc4 = np.loadtxt('Cl_euclid_galaxy_omega_cdm_1.2480000000E-01_cl.dat',unpack=True)

# Plotting fiducial model 

for index in range(1,56):
    py.loglog(Clfid[0],abs(Clfid[index]))
py.show()

# Error file

for index in range(1,56):
    py.loglog(El[0],abs(El[index]))
py.show()

# fiducial without lensing

for index in range(1,56):
    py.loglog(Clfidnl[0],abs(Clfidnl[index]))
py.show()

# As files 

for index in range(1,56):
    py.loglog(Asl1[0],abs(Asl1[index]))
py.show()

for index in range(1,56):
    py.loglog(Asl2[0],abs(Asl2[index]))
py.show()

for index in range(1,56):
    py.loglog(Asl3[0],abs(Asl3[index]))
py.show()

for index in range(1,56):
    py.loglog(Asl4[0],abs(Asl4[index]))
py.show()

for index in range(1,56):
    py.loglog(As1[0],abs(As1[index]))
py.show()

for index in range(1,56):
    py.loglog(As2[0],abs(As2[index]))
py.show()

for index in range(1,56):
    py.loglog(As3[0],abs(As3[index]))
py.show()

for index in range(1,56):
    py.loglog(As4[0],abs(As4[index]))
py.show()

# H0 files
for index in range(1,56):
    py.loglog(H0l1[0],abs(H0l1[index]))
py.show()

for index in range(1,56):
    py.loglog(H0l2[0],abs(H0l2[index]))
py.show()

for index in range(1,56):
    py.loglog(H0l3[0],abs(H0l3[index]))
py.show()

for index in range(1,56):
    py.loglog(H0l4[0],abs(H0l4[index]))
py.show()

for index in range(1,56):
    py.loglog(H01[0],abs(H01[index]))
py.show()

for index in range(1,56):
    py.loglog(H02[0],abs(H02[index]))
py.show()

for index in range(1,56):
    py.loglog(H03[0],abs(H03[index]))
py.show()

for index in range(1,56):
    py.loglog(H04[0],abs(H04[index]))
py.show()

# m_ncdm files 
for index in range(1,56):
    py.loglog(ml1[0],abs(ml1[index]))
py.show()

for index in range(1,56):
    py.loglog(ml2[0],abs(ml2[index]))
py.show()

for index in range(1,56):
    py.loglog(ml3[0],abs(ml3[index]))
py.show()

for index in range(1,56):
    py.loglog(ml4[0],abs(ml4[index]))
py.show()

for index in range(1,56):
    py.loglog(m1[0],abs(m1[index]))
py.show()

for index in range(1,56):
    py.loglog(m2[0],abs(m2[index]))
py.show()

for index in range(1,56):
    py.loglog(m3[0],abs(m3[index]))
py.show()

for index in range(1,56):
    py.loglog(m4[0],abs(m4[index]))
py.show()

# n_s files 
for index in range(1,56):
    py.loglog(nsl1[0],abs(nsl1[index]))
py.show()

for index in range(1,56):
    py.loglog(nsl2[0],abs(nsl2[index]))
py.show()

for index in range(1,56):
    py.loglog(nsl3[0],abs(nsl3[index]))
py.show()

for index in range(1,56):
    py.loglog(nsl4[0],abs(nsl4[index]))
py.show()

for index in range(1,56):
    py.loglog(ns1[0],abs(ns1[index]))
py.show()

for index in range(1,56):
    py.loglog(ns2[0],abs(ns2[index]))
py.show()

for index in range(1,56):
    py.loglog(ns3[0],abs(ns3[index]))
py.show()

for index in range(1,56):
    py.loglog(ns4[0],abs(ns4[index]))
py.show()

# omega_b files 
for index in range(1,56):
    py.loglog(obl1[0],abs(obl1[index]))
py.show()

for index in range(1,56):
    py.loglog(obl2[0],abs(obl2[index]))
py.show()

for index in range(1,56):
    py.loglog(obl3[0],abs(obl3[index]))
py.show()

for index in range(1,56):
    py.loglog(obl4[0],abs(obl4[index]))
py.show()

for index in range(1,56):
    py.loglog(ob1[0],abs(ob1[index]))
py.show()

for index in range(1,56):
    py.loglog(ob2[0],abs(ob2[index]))
py.show()

for index in range(1,56):
    py.loglog(ob3[0],abs(ob3[index]))
py.show()

for index in range(1,56):
    py.loglog(ob4[0],abs(ob4[index]))
py.show()

# omega_cdm 
for index in range(1,56):
    py.loglog(ocl1[0],abs(ocl1[index]))
py.show()

for index in range(1,56):
    py.loglog(ocl2[0],abs(ocl2[index]))
py.show()

for index in range(1,56):
    py.loglog(ocl3[0],abs(ocl3[index]))
py.show()

for index in range(1,56):
    py.loglog(ocl4[0],abs(ocl4[index]))
py.show()

for index in range(1,56):
    py.loglog(oc1[0],abs(oc1[index]))
py.show()

for index in range(1,56):
    py.loglog(oc2[0],abs(oc2[index]))
py.show()

for index in range(1,56):
    py.loglog(oc3[0],abs(oc3[index]))
py.show()

for index in range(1,56):
    py.loglog(oc4[0],abs(oc4[index]))
py.show()

exit()
