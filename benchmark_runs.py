## A code for benchmarking runs over slurm batch
import os
import subprocess
from statistics import stdev

command = "stack exec queue master 127.0.0.1 8000 test/matmul_ss_test.hs"

def trial():
    # Runs the command and captures the output
    output = subprocess.check_output(command.split(" "), stderr=subprocess.STDOUT)
    output = output.decode("utf-8")

    # Should contain a line that reads "Number of running_workers used: 20"
    # Extracts the number of workers from the output
    lines_with_workers = [line for line in output.split("\n") if "Number of running_workers used" in line]

    if len(lines_with_workers) == 0:
        raise Exception("No workers recorded")
    elif len(lines_with_workers) > 1:
        print(lines_with_workers)
        raise Exception("Multiple workers recorded")
    else:
        workers = int(lines_with_workers[0].split(" ")[-1])
        print(f'Workers: {workers}')

    # Should contain a line that reads like "Fri Jul 14 13:51:07 UTC 2023 pid://127.0.0.1:8000:0:8: Total time: 2.164819708s"
    # Extracts the time from the output

    output = output.split("\n")
    lines_with_time = [line for line in output if "Total time" in line]

    if len(lines_with_time) == 0:
        raise Exception("No time recorded")
    elif len(lines_with_time) > 1:
        print(lines_with_time)
        raise Exception("Multiple times recorded")
    else:
        time = lines_with_time[0].split(" ")[-1]
        print(f'Time: {time}')
        return float(time[:-1])

if __name__ == "__main__":
    times = list(map(lambda _: trial(), range(10)))

    print(f'Executed {command}')
    print(f'Average: {sum(times) / len(times)}')
    print(f'Standard deviation: {stdev(times)}')
    print(f'{times}')