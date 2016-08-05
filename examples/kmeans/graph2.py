import sys
if len(sys.argv) < 4:
    print('usage: graph.py resultfile1 resultfile2 newfile3')
    exit(1)

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import seaborn

ref_point = None

data1 = []
with open(sys.argv[1]) as f:
    for line in f:
        parts = line.split(' ')
        if parts[0] == '1.-1.': ref_point = parts
        data1.append(parts)
data1=data1[1:]

ref_time = float(ref_point[2])
def calc_speedup(x):
    my_time = float(x[2])
    return ref_time/my_time
def calc_accuracy(x):
    my_accuracy = float(x[3])
    return my_accuracy

fig = plt.figure()

def calc_speedup(x):
    return ref_time/float(x[2])

ax = fig.add_subplot(111)
ax.set_xlabel("speedup (x)")
ax.set_ylabel("accuracy loss (%)")
ax.set_ylim(0,0.25)
ax.set_xlim(0,6)

xs1 = list(map(calc_speedup, data1))
ys1 = list(map(calc_accuracy, data1))
ax.scatter(xs1, ys1, c='b', marker='o')

data2 = []
with open(sys.argv[2]) as f:
    for line in f:
        parts = line.split(' ')
        if parts[0] == '1.-1.': ref_point = parts
        data2.append(parts)
data2=data2[1:]

ref_time = float(ref_point[2])
def calc_speedup(x):
    my_time = float(x[2])
    return ref_time/my_time
def calc_accuracy(x):
    my_accuracy = float(x[3])
    return my_accuracy

xs2 = list(map(calc_speedup, data2))
ys2 = list(map(calc_accuracy, data2))
ax.scatter(xs2, ys2, c='r', marker='o')

for i in range((len(xs1))):
    c = 'r' if ys1[i] > ys2[i] else 'g'
    ax.plot([xs1[i],xs2[i]],[ys1[i],ys2[i]], c=c)

data3 = []
with open(sys.argv[3]) as f:
    for line in f:
        parts = line.split(' ')
        if parts[0] == '1.-1.': ref_point = parts
        data3.append(parts)
data3=data3[1:]

ref_time = float(ref_point[2])
def calc_speedup(x):
    my_time = float(x[2])
    return ref_time/my_time
def calc_accuracy(x):
    my_accuracy = float(x[3])
    return my_accuracy

xs2 = list(map(calc_speedup, data3))
ys2 = list(map(calc_accuracy, data3))
ax.scatter(xs2, ys2, c='g', marker='o')


fig.tight_layout()
#plt.savefig('{}.png'.format('combined'))
plt.show()
