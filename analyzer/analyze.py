from getdist import loadMCSamples,plots

number_of_parameters = 7

samples = loadMCSamples('../output/chains/mcmc_final_output',settings={'ignore_rows':0.2})

g = plots.getSinglePlotter()

g.settings.rcSizes(axes_fontsize = 2,lab_fontsize = 7)

g.triangle_plot(samples,filled=True)

g.export('../output/chains/triangle_figure.pdf')

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

stats = samples.getMargeStats()

stats.saveAsText('../output/chains/1Dstatistics.txt')

print 'FIGURE HAS BEEN CREATED'

f = plots.getSubplotPlotter()

f.settings.rcSizes(axes_fontsize = 2,lab_fontsize = 7)

f.plots_1d(samples,['omega_b','omega_cdm','n_s','A_s','H0','m_ncdm','MG_beta2'],markers=[2.225e-2,1.198e-1,9.645e-1,2.20652e-9,6.727e1,6.0e-2,1.],nx=3)

f.export('../output/chains/1D_plots.pdf')

exit()

