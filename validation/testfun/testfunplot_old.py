"""
Plot the test function for validation of the method used to compute the
skin strain tensor.

The test function we're using is derived via a potential-gradient method.
Basically, we start with a scalar potential function.  We then take the
gradient of this scalar potential to find the displacement.  Finally, the
deformation gradient tensor is found by computing the Jacobian matrix of the
displacement field and adding the Identity matrix.

The purpose of this file is simply to create a 2D plot of the test function,
to confirm visually that the potential function, displacement field and
deformation gradient tensor are all correct.

We use matplotlib for plotting.
"""

import numpy
from numpy import array, asarray, cos, dot, sin
import numpy.linalg
from numpy.linalg import eig
import matplotlib.mlab as mlab
import matplotlib.pyplot as pyplot

k = 1.0
omega = 1.0
phi_x = 0.0
phi_y = 0.0

# compute the displacement potential function
delta = 0.025
x = numpy.arange(-3.0, 3.0, delta)
y = numpy.arange(-3.0, 3.0, delta)
X, Y = numpy.meshgrid(x, y)
P = k * sin(omega * X + phi_x) * sin(omega * Y + phi_y)
# plot the displacement potential as a contour plot
pyplot.figure()
contours = pyplot.contour(X, Y, P, 10, colors = 'k')
pyplot.clabel(contours, fontsize=9, inline=1)

# compute the vector displacement (gradient of the potential)
gx = omega * k * cos(omega * X + phi_x) * sin(omega * Y + phi_y)
gy = omega * k * sin(omega * X + phi_x) * cos(omega * Y + phi_y)
# plot the vector displacement as a quiver plot
stride = 6
pyplot.quiver(
    X[::stride, ::stride], 
    Y[::stride, ::stride], 
    gx[::stride, ::stride], 
    gy[::stride, ::stride],
    width = 0.0015)
    
# compute the deformation gradient tensor
h = omega * omega * k
dgxx = -h * sin(omega * X + phi_x) * sin(omega * Y + phi_y) + 1
dgxy =  h * cos(omega * X + phi_x) * cos(omega * Y + phi_y)
# F = [ dgxx dgxy; dgxy dgxx ]
# flatten out the components of the grid, so that we can process them
#  sequentially more easily
dgxxxy = zip(dgxx.flatten(), dgxy.flatten())
# function to produce tensor principal vectors
def xxxy_to_prinvecs(xxxy):
    (xx, xy) = xxxy
    F = array([[xx, xy], [xy, xx]])
    #C = dot(F.transpose(), F) # right Cauchy-Green tensor
    (el, ev) = eig(F)
    #print(eig(F))
    #print(F)
    #print(ev)
    (dx1, dy1) = (el[0] * ev[0,0], el[0] * ev[1,0])
    (dx2, dy2) = (el[1] * ev[0,1], el[1] * ev[1,1])
    #print(dx1, dy1)
    #print(dx2, dy2)
    #print("---")
    return [dx1, dy1, dx2, dy2]
prinvecs = asarray(map(xxxy_to_prinvecs, dgxxxy)).transpose()
(dx1, dy1, dx2, dy2) = (prinvecs[0,:], prinvecs[1,:], prinvecs[2,:], 
    prinvecs[3,:])
dx1 = dx1.reshape(X.shape)
dy1 = dy1.reshape(X.shape)
dx2 = dx2.reshape(X.shape)
dy2 = dy2.reshape(X.shape)

stride = 2 * stride
pyplot.quiver(
    X[::stride, ::stride],
    Y[::stride, ::stride],
    dx1[::stride, ::stride],
    dy1[::stride, ::stride],
    color = 'b',
    width = 0.001)
pyplot.quiver(
    X[::stride, ::stride],
    Y[::stride, ::stride],
    dx2[::stride, ::stride],
    dy2[::stride, ::stride],
    color = 'g',
    width = 0.001)


pyplot.show()