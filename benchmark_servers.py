# Spawns N servers and tracks their time
import typer
import os
import signal
import subprocess
import time
from enum import Enum

app = typer.Typer(
    help="A script for benchmarking Hoo-Ray master-slave servers",
    add_completion=False,
    pretty_exceptions_enable=False,
)

def run(num_slaves, mode):
    slave_command = lambda port: f"cabal run queue slave 127.0.0.1 {port}"
    for i in range(num_slaves):
        slave = subprocess.Popen(
            (slave_command(i + 8085)).split(" "), stdout=subprocess.DEVNULL
        )

    master_command = (
        "cabal run queue master 127.0.0.1 8084 " + "coarse"
        if mode == Mode.coarse
        else "fine"
    )
    master_command = subprocess.Popen(master_command.split(" "), stdout=subprocess.PIPE)

    # start n slaves
    while master_command.poll() is None:
        # master is still alive
        time.sleep(1)

    # Just read runtime from the log - no need to time anything in Python

class Mode(str, Enum):
    coarse = "coarse"
    fine = "fine"


@app.command("run", help="The main function")
def main(
    num_slaves: int = typer.Argument(..., help="Number of slaves"),
    mode: Mode = Mode.coarse,
):
    try:
        run(num_slaves, mode)
    except KeyboardInterrupt:
        print("???")
        cleanup(process_pids)


if __name__ == "__main__":
    app()
