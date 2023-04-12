def pure(a, b):
    return a + b

import time

def impure1():
    return time.time()

def impure2(a):
    print(a)

def impure3():
    with open('file.txt') as f:
        text = f.read()
    return f