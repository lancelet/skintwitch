#!/usr/bin/env python
"""
Plotting for the results paper (Cynthia's).
Produces color map plots containing:
  - Color maps of I1 values (scalar field)
  - Quiver plots of Biot strain principal values (E) (2D tensor field)
Re-uses code from cmtensorplot.py in the method-paper-scripts directory.
"""

import sys
import os
import matplotlib
from matplotlib.pyplot import subplots, xlim, ylim, show, savefig, colorbar

# add the method-paper-scripts directory to our search path
method_paper_scripts_dir = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "../method-paper-scripts"))
if method_paper_scripts_dir not in sys.path:
    sys.path.insert(0, method_paper_scripts_dir)

import cmtensorplot
from cmtensorplot import load_avg_grid_file, plot_I1, plot_pokes, plot_e

def plot_result_paper_avg_grids():
    t6_data = load_avg_grid_file(open('../output/plots/avg-grid-T6.csv'))
    t11_data = load_avg_grid_file(open('../output/plots/avg-grid-T11.csv'))
    t16_data = load_avg_grid_file(open('../output/plots/avg-grid-T16.csv'))

    # limits for the color bar
    vmin = 3.0
    vmax = 3.02
    # poke circle radius
    radius = 1.5
    # scale of arrows for quiver plots (tensor arrows)
    arrow_scale = 0.7

    def do_plots(data, axes):
        i1Grid, eGrid, stimLoc = data
        axes.set_autoscale_on(False)
        axes.set_aspect(7.0 / 8.0, 'datalim')
        grow = 0.1
        xlim(-grow, 1+grow)
        ylim(1+grow, -grow)
        plot_I1(i1Grid, axes, vmin=vmin, vmax=vmax, cmap='Oranges')
        plot_pokes(stimLoc, axes, radius=radius)
        plot_e(eGrid, axes, scale=arrow_scale)
        axes.set_xticks([0,1])
        axes.set_yticks([0,1])

    fig, axes = subplots(1, 3, sharey=True, figsize=(10.0, 3.4))
    fig.subplots_adjust(left=0.07, bottom=0.23, right=0.86, top=0.85)

    do_plots(t6_data, axes[0])
    do_plots(t11_data, axes[1])
    do_plots(t16_data, axes[2])
    axes[0].set_title('T6')
    axes[1].set_title('T11')
    axes[2].set_title('T16')
    axes[0].set_xlabel('$\\xi^1$')
    axes[1].set_xlabel('$\\xi^1$')
    axes[2].set_xlabel('$\\xi^1$')
    axes[0].set_ylabel('$\\xi^2$')
    cax = fig.add_axes([0.90, 0.23, 0.02, 0.62])  # left, bottom, width, height
    cbar = colorbar(axes[0].get_images()[0], cax=cax)
    cbar.set_ticks([3.00, 3.01, 3.02])

    savefig('result-i1-and-strains.png', dpi=300)
    show()


if __name__ == '__main__':
    plot_result_paper_avg_grids()