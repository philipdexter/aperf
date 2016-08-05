import sys

if len(sys.argv) < 2:
    print('usage: {} inputs'.format(sys.argv[0]))
    exit(1)

for input in sys.argv[1:]:
    with open(input) as f:
        nums = map(int, f.readlines())
        print(sum(nums))
