import sys
if len(sys.argv) < 2:
    print('usage: graph.py resultfile')

import matplotlib.pyplot as plt
import seaborn

data = []
with open(sys.argv[1]) as f:
    for line in f:
        parts = line.split(' ')
        data.append(parts)
data=data[1:]

fig = plt.figure()

ax = fig.add_subplot(111)
ax.set_xlabel("perforation (x)")
ax.set_ylabel("accuracy loss (%)")
ax.set_xlim(0,1)
ax.set_ylim(0,1)
#ax.set_ylim(0,6000000)

xs = list(map(lambda x: float(x[0]), data))
#ys = list(map(lambda x: x[3], data))
#ax.scatter(xs, ys, c='b', marker='o')

ys = list(map(lambda x: float(x[3]), data))
ax.scatter(xs, ys, c='b', marker='o')

fig.tight_layout()
#plt.savefig('{}.png'.format(sys.argv[1]))
plt.show()
