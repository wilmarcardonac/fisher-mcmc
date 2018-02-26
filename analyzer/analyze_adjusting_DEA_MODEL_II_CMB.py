from getdist import loadMCSamples,plots

number_of_parameters = 12

samples = loadMCSamples('../output/mcmc_output',settings={'ignore_rows':2})

g = plots.getSinglePlotter()

g.settings.rcSizes(axes_fontsize = 2,lab_fontsize = 7)

g.triangle_plot(samples,filled=True)

g.export('triangle_figure.pdf')

bestfit = samples.getLikeStats()

means = samples.setMeans()

filebestfit = open("../output/bestfit.txt",'w')

filemeans = open("../output/means.txt",'w')

#filebestfit.write("-log(Like) = "+str(bestfit.logLike_sample)+"\n")

for index in range(number_of_parameters) : 

    filebestfit.write(str(bestfit.names[index].bestfit_sample)+"\n")

    filemeans.write(str(means[index])+"\n")

filebestfit.close()

filemeans.close()

stats = samples.getMargeStats()

stats.saveAsText('1Dstatistics.txt')

print 'Figure has been created '

exit()
