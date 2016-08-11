import sys
import random

n = int(sys.argv[1])


for i in range(n):
    x, y = random.uniform(0,5000), random.uniform(0,5000)
    print('{},{}'.format(x,y))
