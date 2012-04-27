#!/usr/bin/env python

"""
Creates a scatter plot of I1 maximum response locations and confidence
ellipses.

Confidence ellipses are created according to the method described in:
  Coe, D. (2009) Fisher matrices and confidence ellipses: A quick-start guide
    and software.  arXiv:0906.4123v1 [astro-ph.IM] 23 Jun 2009.
Further background information on confidence ellipses is provided in:
  Press, W.H., Teukolsky, S.A., Vetterling, W.T. and Flannery, B.P. (2007)
    Numerical Recipes: The Art of Scientific Computing, 3rd ed.  Chapter
    15.6: Confidence Limits on Estimated Model Parameters, pp. 807-817.
"""

import csv
import matplotlib
matplotlib.use('SVG')
matplotlib.rcParams['svg.fonttype'] = 'none'
from matplotlib import pyplot
from matplotlib import patches
import numpy
from math import atan, atan2, degrees, sqrt


def i1_scatter_plot():

    # Read in the file
    file_name = "../method-paper-output/max-I1-coords.csv"
    coords = {}
    reader = csv.reader(open(file_name))
    reader.next() # drop header line
    for row in reader:
        site, v_txt, u_txt, w_txt = map(lambda s: s.strip(), row)
        u = float(u_txt)
        v = float(v_txt)
        w = float(w_txt)
        if not (site in coords):
            coords[site] = []
        coords[site] += [[u, v, w]]

    # Plot the figures
    fig, axes = pyplot.subplots(1, 2, sharey=True, figsize=(12,6))
    aspect = 7.0 / 8.0
    axes[0].set_autoscale_on(False)
    axes[1].set_autoscale_on(False)
    axes[0].set_aspect(aspect, 'box-forced')
    axes[1].set_aspect(aspect, 'box-forced')
    pyplot.ylim(1.0, 0.0)
    pyplot.xlim(0.0, 1.0)
    #fig.tight_layout()

    def xs(xyw): return map(lambda z: z[0], xyw)
    def ys(xyw): return map(lambda z: z[1], xyw)
    def ws(xyw): return map(lambda z: z[2], xyw)
    axes[0].scatter(xs(coords['T6']), ys(coords['T6']), marker='.', c='k', label='T6')
    axes[0].scatter(xs(coords['T11']), ys(coords['T11']), marker='x', c='k', label='T11')
    axes[0].scatter(xs(coords['T16']), ys(coords['T16']), marker='+', c='k', label='T16')
    axes[1].scatter(xs(coords['G1']), ys(coords['G1']), marker='.', c='k', label='G1')
    axes[1].scatter(xs(coords['G2']), ys(coords['G2']), marker='x', c='k', label='G2')
    axes[1].scatter(xs(coords['G3']), ys(coords['G3']), marker='+', c='k', label='G3')

    t6ce = confidence_ellipse(xs(coords['T6']), ys(coords['T6']), ws(coords['T6']))
    plot_confidence_ellipse(axes[0], t6ce, 'solid')
    t11ce = confidence_ellipse(xs(coords['T11']), ys(coords['T11']), ws(coords['T11']))
    plot_confidence_ellipse(axes[0], t11ce, 'dashed')
    t16ce = confidence_ellipse(xs(coords['T16']), ys(coords['T16']), ws(coords['T16']))
    plot_confidence_ellipse(axes[0], t16ce, 'dotted')

    g1ce = confidence_ellipse(xs(coords['G1']), ys(coords['G1']), ws(coords['G1']))
    plot_confidence_ellipse(axes[1], g1ce, 'solid')
    g2ce = confidence_ellipse(xs(coords['G2']), ys(coords['G2']), ws(coords['G2']))
    plot_confidence_ellipse(axes[1], g2ce, 'dashed')
    g3ce = confidence_ellipse(xs(coords['G3']), ys(coords['G3']), ws(coords['G3']))
    plot_confidence_ellipse(axes[1], g3ce, 'dotted')

    axes[0].legend(scatterpoints=1, loc=4)
    axes[1].legend(scatterpoints=1, loc=1)

    axes[0].set_xlabel("$\\xi^1$")
    axes[1].set_xlabel("$\\xi^1$")
    axes[0].set_ylabel("$\\xi^2$")

    pyplot.savefig('I1-scatter-confidence-ellipsoids-raw.svg')


def confidence_ellipse(xs, ys, ws):

    """95% weighted confidence ellipse"""

    assert(len(xs) == len(ys) and len(xs) == len(ws))
    N = float(len(xs))

    xa = numpy.asarray(xs, 'double')
    ya = numpy.asarray(ys, 'double')
    wa = numpy.asarray(ws, 'double')

    wsum = sum(wa)
    x_bar = numpy.dot(wa, xa) / wsum
    y_bar = numpy.dot(wa, ya) / wsum
    xp = xa - x_bar
    yp = ya - y_bar

    sigma_x2 = numpy.dot(wa, pow(xp, 2.0)) / wsum
    sigma_y2 = numpy.dot(wa, pow(yp, 2.0)) / wsum
    sigma_xy = numpy.dot(wa * xp, yp) / wsum

    a2 = (sigma_x2 + sigma_y2) / 2 + \
        sqrt(pow(sigma_x2 - sigma_y2, 2) / 4.0 + pow(sigma_xy, 2))
    b2 = (sigma_x2 + sigma_y2) / 2 - \
        sqrt(pow(sigma_x2 - sigma_y2, 2) / 4.0 + pow(sigma_xy, 2))
    a = sqrt(a2)
    b = sqrt(b2)

    theta = atan2(2.0 * sigma_xy, sigma_x2 - sigma_y2) / 2.0

    # 95%
    a = a * 2.48
    b = b * 2.48

    return (x_bar, y_bar, a, b, theta)


def plot_confidence_ellipse(axis, ellipseparam, style='solid'):
    (x, y, a, b, theta) = ellipseparam
    e = patches.Ellipse((x, y), 2*a, 2*b, angle=degrees(theta), 
        facecolor='none', ls=style)
    axis.add_artist(e)


if __name__ == '__main__':
    i1_scatter_plot()