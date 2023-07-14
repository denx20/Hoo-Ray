#!/bin/bash
#SBATCH --requeue
#SBATCH --chdir=/usr/project/dlab/Users/jaden/ICFP2023/Hoo-Ray
#SBATCH --output=/usr/project/dlab/Users/jaden/ICFP2023/Hoo-Ray/slurm/slurm-outs/%j.out
#SBATCH --mem=1000M
#SBATCH --cpus-per-task=1
#SBATCH --partition=compsci
#SBATCH --exclude=linux[1-10]

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 port"
    exit 1
fi

port="$1"

date
hostname
cd /usr/project/dlab/Users/jaden/ICFP2023/Hoo-Ray

bash /usr/project/dlab/Users/jaden/ICFP2023/Hoo-Ray/kill_all_workers.sh

interface=$(ifconfig | awk '/MULTICAST/ {print $1}' | sed 's/:$//')

# Credit: https://serverfault.com/a/911621
ip_addr=$(ip -f inet addr show $interface | sed -En -e 's/.*inet ([0-9.]+).*/\1/p')

ifconfig

echo "Starting worker on $ip_addr:$port"
stack exec queue slave $ip_addr $port > /dev/null

