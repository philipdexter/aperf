import sys
if len(sys.argv) < 2:
    print('usage: graph.py resultfile [3d|2d]')

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import seaborn

ref_point = None

data = []
with open(sys.argv[1]) as f:
    for line in f:
        parts = line.split(' ')
        if parts[0] == '1.-1.': ref_point = parts
        data.append(parts)
data=data[1:]

ref_time = float(ref_point[2])
def calc_speedup(x):
    my_time = float(x[2])
    return ref_time/my_time
def calc_accuracy(x):
    my_accuracy = float(x[3])
    return my_accuracy

fig = plt.figure()

def subfigure(subfig, z):
    ax = fig.add_subplot(subfig, projection='3d')
    ax.set_xlim(0,1)
    ax.set_ylim(0,1)
    ax.set_xlabel("loop 1 perforation")
    ax.set_ylabel("loop 2 perforation")
    ax.set_zlabel(z, rotation=90)
    ax.view_init(10, 13)
    ax.zaxis.set_rotate_label(False)
    return ax

def plot3d():
    xs = list(map(lambda x: float(x[0].split('-')[0]), data))
    ys = list(map(lambda x: float(x[0].split('-')[1]), data))

    zs = list(map(calc_speedup, data))
    ax = subfigure(121, 'speedup (x)')
    ax.scatter(xs, ys, zs, c='r', marker='o')

    zs = list(map(calc_accuracy, data))
    ax = subfigure(122, 'accuracy loss (%)')
    ax.set_zlim(0,0.25)
    ax.scatter(xs, ys, zs, c='b', marker='o')

    fig.tight_layout()
    plt.show()

def plot2d():
    def calc_speedup(x):
        return ref_time/float(x[2])

    ax = fig.add_subplot(111)
    ax.set_xlabel("speedup (x)")
    ax.set_ylabel("accuracy loss (%)")
    ax.set_ylim(0,0.25)
    ax.set_xlim(0,6)

    xs = list(map(calc_speedup, data))
    ys = list(map(calc_accuracy, data))
    ax.scatter(xs, ys, c='b', marker='o')

    fig.tight_layout()
    plt.savefig('{}.png'.format(sys.argv[1]))
    plt.show()

if len(sys.argv) < 3 or sys.argv[2] == '3d': plot3d()
else: plot2d()
