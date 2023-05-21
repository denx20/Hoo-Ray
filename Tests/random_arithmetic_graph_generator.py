"""Generates a file 'generated.hs' with randomly generated arithmetic operations
"""
import random
import math
import itertools
from pathlib import Path

random.seed('susamogus')
# operations = ['add', 'sub', 'mul', 'divide', 'pow']
operations = ['(+)', '(-)', '(*)', '(/)']

def random_execution_generator(p_var):
    # Creates a list of strings, each with the form e.g. "let c = add a 10"
    # p_var: probability that a given token is a variable

    current_variables = []

    def variable_name_generator():
        # Generates 'a', 'b', 'c', ... 'aa', 'ab', ...
        i = 0
        keyword_list = {'do', 'if', 'of', 'in'}
        def int_to_chars(i):
            if i < 26:
                return chr(i%26 + 97)
            else:
                return int_to_chars(math.floor(i/26) - 1) + chr(i%26 + 97)

        while True:
            # if i == 117 or i == 239:
            #     continue

            yield int_to_chars(i)
            i += 1

    vargen = variable_name_generator()

    for i in itertools.count():
        res_var = next(vargen)
        # print(res_var)

        op = random.choice(operations)
        if i == 0:
            var1 = random.random() * 100
            var2 = random.random() * 100
        else:
            var1 = random.choice(current_variables) if random.random() < p_var else random.random() * 100
            var2 = random.choice(current_variables) if random.random() < p_var else random.random() * 100

        current_variables.append(res_var)
        yield f'{res_var} = {op} {var1} {var2}'




header = '''
main :: IO ()
main = do
    print "started running"
    '''


if __name__ == '__main__':
    lines = list(itertools.islice(random_execution_generator(0.5), 100000))

    lines = [s.replace('do', 'dO') for s in lines]
    lines = [s.replace('if', 'iF') for s in lines]
    lines = [s.replace('in', 'iN') for s in lines]
    lines = [s.replace('of', 'oF') for s in lines]
    lines = [s.replace('let', 'leT') for s in lines]
    lines = [s.replace('case', 'casE') for s in lines]
    lines = [s.replace('data', 'datA') for s in lines]
    lines = [s.replace('else', 'elsE') for s in lines]
    lines = [s.replace('then', 'theN') for s in lines]
    lines = [s.replace('proc', 'proC') for s in lines]
    lines = [s.replace('type', 'typE') for s in lines]

    lines = ['let ' + l for l in lines]
    lines = '\n    '.join(lines)
    result = header + lines + '\n    print "finished running"\n    return ()'

    with open(Path(__file__).parent / 'generated_computation.hs', 'w') as f:
        f.write(result)