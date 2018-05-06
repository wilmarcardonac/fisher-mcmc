from getdist import loadMCSamples,plots,covmat
import numpy as np
import os,fnmatch

number_of_parameters = 10

samples = loadMCSamples('../output/chains/NC-run4/mcmc_final_output',settings={'ignore_rows':0.})

samples_neglecting_lensing = loadMCSamples('../../../neglecting-lensing/fisher-mcmc/output/chains/NC-run4/mcmc_final_output',settings={'ignore_rows':0.})

samples_neglecting_lensing_only_auto = loadMCSamples('../../../neglecting-lensing-only-auto/fisher-mcmc/output/chains/NC-run4/mcmc_final_output',settings={'ignore_rows':0.})

g = plots.getSinglePlotter()

g.settings.rcSizes(axes_fontsize = 4,lab_fontsize = 7)

g.settings.x_label_rotation = -45

#g.triangle_plot(samples,filled=True)

#g.export('triangle_figure.pdf')

#print 'TRIANGLE PLOT CREATED'

p = samples.getParams()

samples.addDerived(np.log(1.e1**10*p.A_s),name='ln1010As',label='\ln 10^{10}A_s')

bestfit = samples.getLikeStats()

means = samples.setMeans()

stats = samples.getMargeStats()

p_neglecting_lensing = samples_neglecting_lensing.getParams()

samples_neglecting_lensing.addDerived(np.log(1.e1**10*p_neglecting_lensing.A_s),name='ln1010As',label='\ln 10^{10}A_s')

bestfit_neglecting_lensing = samples_neglecting_lensing.getLikeStats()

means_neglecting_lensing = samples_neglecting_lensing.setMeans()

stats_neglecting_lensing = samples_neglecting_lensing.getMargeStats()

p_neglecting_lensing_only_auto = samples_neglecting_lensing_only_auto.getParams()

samples_neglecting_lensing_only_auto.addDerived(np.log(1.e1**10*p_neglecting_lensing_only_auto.A_s),name='ln1010As',label='\ln 10^{10}A_s')

bestfit_neglecting_lensing_only_auto = samples_neglecting_lensing_only_auto.getLikeStats()

means_neglecting_lensing_only_auto = samples_neglecting_lensing_only_auto.setMeans()

stats_neglecting_lensing_only_auto = samples_neglecting_lensing_only_auto.getMargeStats()

#f = plots.getSubplotPlotter()

#f.settings.rcSizes(axes_fontsize = 4,lab_fontsize = 7)

#f.plots_1d([samples,samples_neglecting_lensing,samples_neglecting_lensing_only_auto],['omega_b','omega_cdm','n_s','ln1010As','H0','m_ncdm','nc_bias_b0','cs2_fld','w0_fld','e_pi'],colors=['red','blue','gray'],legend_labels=['with lensing','without lensing','only auto-correlations'])#,markers=[2.225e-2,1.198e-1,9.645e-1,2.20652e-9,6.727e1,6.0e-2,1.],nx=3)

#f.export('1D_plots.pdf')

#print '1D PLOTS CREATED'

g.triangle_plot([samples,samples_neglecting_lensing,samples_neglecting_lensing_only_auto],params=['omega_b','omega_cdm','n_s','ln1010As','H0','m_ncdm','nc_bias_b0','cs2_fld','w0_fld','e_pi'],filled=True,contour_colors=['red','blue','gray'],legend_labels=['with lensing','without lensing','only auto-correlations'],legend_loc='upper right')

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
    ax.axvline(1.,color='black',ls='--')

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
    ax.axhline(1.,color='black',ls='dotted')

for ax in g.subplots[8,0:8]:
    ax.axhline(-0.8,color='black',ls='dotted')

for ax in g.subplots[9,0:9]:
    ax.axhline(0.,color='black',ls='dotted')

g.export('triangle_figure.pdf')

exit()

