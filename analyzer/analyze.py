from getdist import loadMCSamples,plots

samples = loadMCSamples('../output/mcmc_final_output',settings={'ignore_rows':2})

g = plots.getSinglePlotter()

g.settings.rcSizes(axes_fontsize = 4,lab_fontsize = 7)

g.triangle_plot(samples,filled=True)

g.export('example.pdf')

print 'Happy '

exit()
