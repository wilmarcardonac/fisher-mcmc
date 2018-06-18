from getdist import loadMCSamples,plots,covmat
import numpy as np
import os,fnmatch

filenames = fnmatch.filter(os.listdir("../output/chains/"),"mcmc_*.txt")

for index in range(len(filenames)):

    os.rename("../output/chains/"+str(filenames[index]),"../output/chains/mcmc_final_output_"+str(index+1)+".txt")

number_of_parameters = 11

samples = loadMCSamples('../output/chains/mcmc_final_output',settings={'ignore_rows':0.})

#print 'CONVERGENCE FOR SAMPLES WITH LENSING ', samples.getGelmanRubin()

g = plots.getSinglePlotter()

g.settings.rcSizes(axes_fontsize = 4,lab_fontsize = 7)

g.settings.x_label_rotation = -45

#g.triangle_plot(samples,filled=True)

#g.export('../output/chains/triangle_figure.pdf')

#print 'TRIANGLE PLOT CREATED'

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

print 'MEANS AND BESTFIT FILES CREATED'

stats = samples.getMargeStats()

stats.saveAsText('../output/chains/1Dstatistics.txt')

print '1D STATISTICS FILE CREATED'

f = plots.getSubplotPlotter()

f.settings.rcSizes(axes_fontsize = 2,lab_fontsize = 7)

f.plots_1d(samples,['omega_b','omega_cdm','n_s','ln1010As','H0','m_ncdm','nc_bias_b0','logcs2fld','w0_fld','e_pi','tau'])#,markers=[2.225e-2,1.198e-1,9.645e-1,2.20652e-9,6.727e1,6.0e-2,1.],nx=3)

f.export('../output/chains/1D_plots.pdf')

print '1D PLOTS CREATED'

g.triangle_plot(samples,params=['omega_b','omega_cdm','n_s','ln1010As','H0','m_ncdm','nc_bias_b0','logcs2fld','w0_fld','e_pi'],filled=True,contour_colors=['red'],legend_labels=['with lensing'],legend_loc='upper right')

for ax in g.subplots[:,0]:
    ax.axvline(2.218e-2,color='black',ls='--')

for ax in g.subplots[1:,1]:
    ax.axvline(1.205e-1,color='black',ls='--')

for ax in g.subplots[2:,2]:
    ax.axvline(9.619e-1,color='black',ls='--')
    
for ax in g.subplots[3:,3]:
    ax.axvline(3.056,color='black',ls='--')

for ax in g.subplots[4:,4]:
    ax.axvline(66.93,color='black',ls='--')

for ax in g.subplots[5:,5]:
    ax.axvline(6.e-2,color='black',ls='--')

for ax in g.subplots[6:,6]:
    ax.axvline(1.,color='black',ls='--')

for ax in g.subplots[7:,7]:
    ax.axvline(0.,color='black',ls='--')

for ax in g.subplots[8:,8]:
    ax.axvline(-0.8,color='black',ls='--')

for ax in g.subplots[9:,9]:
    ax.axvline(0.,color='black',ls='--')

for ax in g.subplots[9,0:9]:
    ax.axhline(0.,color='black',ls='dotted')

for ax in g.subplots[1,0:1]:
    ax.axhline(1.205e-1,color='black',ls='dotted')

for ax in g.subplots[2,0:2]:
    ax.axhline(9.619e-1,color='black',ls='dotted')

for ax in g.subplots[3,0:3]:
    ax.axhline(3.056,color='black',ls='dotted')

for ax in g.subplots[4,0:4]:
    ax.axhline(66.93,color='black',ls='dotted')

for ax in g.subplots[5,0:5]:
    ax.axhline(6.e-2,color='black',ls='dotted')

for ax in g.subplots[6,0:6]:
    ax.axhline(1.,color='black',ls='dotted')

for ax in g.subplots[7,0:7]:
    ax.axhline(0.,color='black',ls='dotted')

for ax in g.subplots[8,0:8]:
    ax.axhline(-0.8,color='black',ls='dotted')

for ax in g.subplots[9,0:9]:
    ax.axhline(0.,color='black',ls='dotted')

g.export('../output/chains/triangle_figure.pdf')

covariance_matrix = samples.getCov(pars=[0,1,2,11,4,5,6,12,8,9,10])#nparam=number_of_parameters)

covariance_matrix_2 = covmat.CovMat(matrix=covariance_matrix)

covariance_matrix_2.saveToFile('../output/chains/covariance_matrix.txt')

print 'COVARIANCE MATRIX CREATED'

exit()

