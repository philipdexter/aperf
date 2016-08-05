import sys
import random

if len(sys.argv) < 3:
    print('usage: {} length max'.format(sys.argv[0]))
    exit(1)

n = int(sys.argv[1])
from_to = (0,int(sys.argv[2]))

for i in range(n):
    print(str(random.randint(*from_to)))
