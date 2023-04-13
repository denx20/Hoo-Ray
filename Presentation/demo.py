def pure(a, b):
    return a + b

import time

def pure1():
    return 3.14

def pure2(a, b):
    return a + b

def pure3(l: list):
    return sorted(l)

def impure1():
    return time.time()

def impure2(a):
    print(a)

def impure3():
    with open('file.txt') as f:
        text = f.read()
    return text