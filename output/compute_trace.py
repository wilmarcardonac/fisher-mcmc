import numpy as np

C2 = np.loadtxt('covariance_matrix_l_0002.dat')

C200 = np.loadtxt('covariance_matrix_l_0200.dat')

C2000 = np.loadtxt('covariance_matrix_l_2000.dat')

lT = np.zeros((3,2))

lT[0,0] = 2

lT[1,0] = 200

lT[2,0] = 2000

lT[0,1] = np.trace(C2)

lT[1,1] = np.trace(C200)

lT[2,1] = np.trace(C2000) 

np.savetxt('trace_covariance_matrices.dat',lT,fmt='%5d %.10E',header='l \t \t trace')

C2 = np.loadtxt('inverse_covariance_matrix_l_0002.dat')

C200 = np.loadtxt('inverse_covariance_matrix_l_0200.dat')

C2000 = np.loadtxt('inverse_covariance_matrix_l_2000.dat')

lT[0,1] = np.trace(C2)

lT[1,1] = np.trace(C200)

lT[2,1] = np.trace(C2000) 

np.savetxt('trace_inverse_covariance_matrices.dat',lT,fmt='%5d %.10E',header='l \t \t trace')

F1 = np.loadtxt('fisher_matrix.dat')

F2 = np.loadtxt('fisher_matrix_lensing.dat')

F3 = np.loadtxt('fisher_matrix_lensing_only_autocorrelations.dat')

F4 = np.loadtxt('fisher_matrix_only_autocorrelations.dat')
 
FT = np.zeros((4,3))

FT[0,0] = 0 
FT[0,1] = 1
FT[0,2] = np.trace(F1)

FT[1,0] = 1
FT[1,1] = 1
FT[1,2] = np.trace(F2)

FT[2,0] = 1
FT[2,1] = 0
FT[2,2] = np.trace(F3)

FT[3,0] = 0
FT[3,1] = 0
FT[3,2] = np.trace(F4)

np.savetxt('trace_fisher_matrices.dat',FT,fmt='%5d %10d %20.10E',header='lensing flag \t correlations flag \t trace \n 0 = no lensing. 1 = lensing. 1 = cross correlations included. 0 = cross correlations not included')

exit()
