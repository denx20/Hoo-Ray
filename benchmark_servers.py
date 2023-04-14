# Spawns N servers and tracks their time
import typer
import os
import signal
import subprocess
import time

app = typer.Typer(
    help="A script for benchmarking Hoo-Ray master-slave servers", add_completion=False, pretty_exceptions_enable=False
)

process_pids = []

def run(num_slaves):
    slave_command = lambda port: f"cabal run queue slave 127.0.0.1 {port}"
    for i in range(num_slaves):
        slave = subprocess.Popen((slave_command (i+8085)).split(' '), stdout=subprocess.DEVNULL)
        process_pids.append(slave.pid)


    master_command = "cabal run queue master 127.0.0.1 8084"
    master_command = subprocess.Popen(master_command.split(' '), stdout=subprocess.PIPE)

    # start n slaves
    while master_command.poll() is None:
        # master is still alive
        time.sleep(1)


    # Just read runtime from the log - no need to time anything in Python

    cleanup(process_pids)


def cleanup(pids):
    print(f"killing processes: {pids}")
    for pid in pids:
        os.kill(pid, signal.SIGINT)
    time.sleep(1)

@app.command("run", help="The main function")
def main(num_slaves: int = typer.Argument(..., help="Number of slaves")):
    try:
        run(num_slaves)
    except KeyboardInterrupt:
        print("???")
        cleanup(process_pids)


if __name__ == '__main__':
    app()
