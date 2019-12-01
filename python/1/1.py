import math
lines = None
with open('input', 'r') as fp:
    lines = [int(line.rstrip('\n')) for line in fp]



def getFuel(mass):
    num =  math.floor(mass/3) -2
    if num > 0:
        return num + getFuel(num)
    return 0
print(getFuel(1969))



total = 0
for module in lines:
    total += getFuel(module)
print(total)