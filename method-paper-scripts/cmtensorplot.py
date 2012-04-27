#!/usr/bin/env python
"""
Produces color map plots containing:
  - Color maps of I1 values (scalar field)
  - Quiver plots of Biot strain principal values (E) (2D tensor field)
"""

import csv
from numpy import array, linspace, meshgrid
import matplotlib
matplotlib.use('SVG')
matplotlib.rcParams['svg.fonttype'] = 'none'
matplotlib.rcParams['text.usetex'] = False
matplotlib.rcParams['text.latex.unicode'] = False
from mpl_toolkits.axes_grid1 import AxesGrid
from matplotlib.pyplot import colorbar, figure, gca, show, subplots, \
    xlim, ylim, subplot2grid, savefig
import scipy
from scipy.ndimage.interpolation import zoom

def load_avg_grid_file(file):
    """
    Loads an average grid file, exported by the Scala tools.

    @param should be a file (eg: file = open(file_name))

    The average grid file is essentially a CSV file with the following separate
    data sets concatenated:
    
    I1 grid (7 rows, 8 columns)
    I(0,0), I(0,1), ..., I(0,7)
    I(1,0), I(1,1), ..., I(1,7)
       |       |            |
    I(6,0), I(6,1), ..., I(6,7)

    Biot strain (E) (7 rows, 8 columns)
    (v1x;v1y;v2x;v2y;lambda1;lambda2)(0,0), ...
            |

    Poke locations (u,v)
    px0, py0
    px1, py1
       |

    This method returns 3 data sets:
    (
        i1Grid (2d numpy array)
        eGrid (3d numpy array - (x,y), then v1x,v1y,v2x,v2y in 3rd dimension)
        stimLoc (2d array of (x,y) poke / stimulus coordinates)
    )
    """

    reader = csv.reader(file)
    
    # read the grid of I1 values
    header = reader.next()
    assert(','.join(header).strip() == 'I1 grid (7 rows, 8 columns)')
    i1Grid_list = []
    for row_n in range(7):
        line = reader.next()
        i1Grid_list.append(map(lambda s: float(s.strip()), line))
    i1Grid = array(i1Grid_list, 'double')
    
    # read in the grid of E values
    header = reader.next()
    expected_header = 'Biot strain (E) (7 rows, 8 columns) ' + \
        '(v1x, v1y, v2x, v2y, lambda1, lambda2) ' + \
        'where v1 and v1 are principal strains directions ' + \
        'and lambda are the magnitudes.'
    assert(','.join(header).strip() == expected_header)
    eGrid_list = []
    for row_n in range(7):
        line = reader.next()
        def coords(v):
            vstrip = v.strip(' ()')
            vtxtnums = vstrip.split(';')
            vnums = map(float, vtxtnums)
            return vnums
        eGrid_list.append(map(coords, line))
    eGrid = array(eGrid_list, 'double')

    # read in the stimulus locations
    header = reader.next()
    expected_header = 'Poke locations (u,v).  u is column-linked, ' + \
        'v is row-linked.'
    assert(','.join(header).strip() == expected_header)
    stimLoc_list = []
    for row in reader:
        uv = map(lambda s: float(s.strip()), row)
        stimLoc_list.append(uv)
    stimLoc = array(stimLoc_list, 'double')

    return (i1Grid, eGrid, stimLoc)


def plot_I1(i1Grid, axes, vmin=3.0, vmax=3.05):
    """
    Plots I1 values as a color map image on the given axis.
    """
    axes.imshow(i1Grid, interpolation='bicubic', extent=(0, 1, 1, 0),
        cmap='PuBuGn', aspect=(7.0/8.0), vmin=vmin, vmax=vmax)
    interp_scale = 8
    Z = zoom(i1Grid, interp_scale)
    xi = linspace(0.0, 1.0, 8 * interp_scale)
    yi = linspace(0.0, 1.0, 7 * interp_scale)
    X, Y = meshgrid(xi, yi)
    levels = linspace(vmin, vmax, 25)
    axes.contour(X, Y, Z, levels, colors='k', alpha=0.2)

    
def plot_pokes(stimLoc, axes):
    """
    Plots poke locations on a given axis.
    """
    xs = stimLoc[:,0]
    ys = stimLoc[:,1]
    axes.scatter(xs, ys, marker='o', c=[0, 0, 0, 0.5], s=50, edgecolors='none')

def plot_e(eGrid, axes, scale=1.3, arrow_width=0.005, 
    color_compression=[0,0,0], color_tension=[0,0,0]):
    """
    Plots Biot strain tensor field.
    """
    def plot1vec(u, v, x, y, l):
        xl = x * l
        yl = y * l
        pivot = 'tail' if (l >= 0) else 'tip'
        color = color_compression if (l <= 0) else color_tension
        axes.quiver([u], [v], [xl], [yl], pivot=pivot, scale=scale,
            width=arrow_width, color=color)
        axes.quiver([u], [v], [-xl], [-yl], pivot=pivot, scale=scale,
            width=arrow_width, color=color)
    n_rows, n_cols, n_depth = eGrid.shape
    row_nfac = float(n_rows - 1)
    col_nfac = float(n_cols - 1)
    for row in range(n_rows):
        v = row / row_nfac
        for col in range(n_cols):
            v1x, v1y, v2x, v2y, lambda1, lambda2 = eGrid[row, col, :]
            u = col / col_nfac
            plot1vec(u, v, v1x, v1y, lambda1)
            plot1vec(u, v, v2x, v2y, lambda2)

def plot_method_paper_avg_grids():
    t6_data = load_avg_grid_file(open('../method-paper-output/avg-grid-T6.csv'))
    t11_data = load_avg_grid_file(open('../method-paper-output/avg-grid-T11.csv'))
    t16_data = load_avg_grid_file(open('../method-paper-output/avg-grid-T16.csv'))
    g1_data = load_avg_grid_file(open('../method-paper-output/avg-grid-G1.csv'))
    g2_data = load_avg_grid_file(open('../method-paper-output/avg-grid-G2.csv'))
    g3_data = load_avg_grid_file(open('../method-paper-output/avg-grid-G3.csv'))

    def do_plots(data, axes):
        i1Grid, eGrid, stimLoc = data
        axes.set_autoscale_on(False)
        axes.set_aspect(7.0 / 8.0, 'datalim')
        grow = 0.1
        xlim(-grow, 1+grow)
        ylim(1+grow, -grow)
        plot_I1(i1Grid, axes)
        plot_pokes(stimLoc, axes)
        plot_e(eGrid, axes)
        axes.set_xticks([0,1])
        axes.set_yticks([0,1])


    #fig = figure()
    #grid = AxesGrid(fig, 111, nrows_ncols=(2, 3), axes_pad=0, share_all=True)
    fig, axes = subplots(2, 3, sharex=True, sharey=True, figsize=(8,4.92))
    fig.subplots_adjust(left=0.06, right=0.88, wspace=0.02, top=0.93,
        hspace=0.14)

    do_plots(t6_data, axes[0,0])
    do_plots(t11_data, axes[0,1])
    do_plots(t16_data, axes[0,2])
    do_plots(g1_data, axes[1,0])
    do_plots(g2_data, axes[1,1])
    do_plots(g3_data, axes[1,2])
    axes[0,0].set_title('T6')
    axes[0,1].set_title('T11')
    axes[0,2].set_title('T16')
    axes[1,0].set_title('G1')
    axes[1,1].set_title('G2')
    axes[1,2].set_title('G3')
    axes[0,0].set_ylabel('$\\xi^2$')
    axes[1,0].set_ylabel('$\\xi^2$')
    axes[1,0].set_xlabel('$\\xi^1$')
    axes[1,1].set_xlabel('$\\xi^1$')
    axes[1,2].set_xlabel('$\\xi^1$')
    cax = fig.add_axes([0.90, 0.1, 0.02, 0.83])  # left, bottom, width, height
    cbar = colorbar(axes[0,0].get_images()[0], cax=cax)
    cbar.set_ticks([3.00, 3.01, 3.02, 3.03, 3.04, 3.05])

    #show()
    savefig('i1-and-strains.svg')



if __name__ == '__main__':
    plot_method_paper_avg_grids()
