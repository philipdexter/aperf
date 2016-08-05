import sys
import random

n = int(sys.argv[1])


for i in range(n):
    x, y = random.uniform(0,50), random.uniform(0,50)
    print('{},{}'.format(x,y))
