#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <number_of_servers>"
    exit 1
fi

number_of_servers="$1"
base_port=8080

for i in $(seq 1 $number_of_servers); do
    port=$((base_port + i))
    (stack exec queue slave 127.0.0.1 $port > /dev/null &)
    echo "Started server $i on port $port"
done
