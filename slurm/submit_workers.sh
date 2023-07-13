#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <number_of_servers>"
    exit 1
fi

number_of_servers="$1"
base_port=8080

echo "Submitting $number_of_servers servers"
for i in $(seq 1 $number_of_servers); do
    port=$((base_port + i))
    sbatch --job-name hooray_$port worker.sh $port
    sleep 0.1  # If you start too many servers too fast the stack database will block
done

echo "Submitted $number_of_servers servers"