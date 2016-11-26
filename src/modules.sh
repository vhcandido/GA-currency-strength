#!/bin/bash

bold=$(tput bold)
normal=$(tput sgr0)
b="\e[1m"
u="\e[4m"
i="\e[3m"
n="\e[0m"

echo -e "${b}ABOUT${n}"
echo -e "\tmodules - a helper script to run the Genetic Algorithm modules"
echo
echo -e "${b}SYNOPSIS${n}"
echo -e "\t${b}modules -f${n} ${u}FILE${n} [${b}-p${n} ${u}PORT${n}]"
echo
echo -e "${b}DESCRIPTION${n}"
echo -e "\tThe following options are acepted"
echo
echo -e "\t${b}-f${n} ${u}FILE${n}"
echo -e "\t\tFile containing population parameters that will be passed to the GA"
echo
echo -e "\t${b}-p${n} ${u}PORT${n}"
echo -e "\t\tNumber of port used for comunication between GA server and client"
echo

if [ $# -eq 0 ]; then
	echo 'Parameters expected, none received'
	echo 'Exiting...'
	exit
fi

# Get population parameters from a file passed as argument
file=$1

# Define comunication port
if [ $# -lt 2 ]; then
	port=1010
else
	port=$2
fi

exit
# Open R server (chromosome backtest)
gnome-terminal -e "bash -ic 'Rscript server.R $port; exec bash'"

# Wait for R server to load required libraries
sleep 5

# Execute the Python client (Genetic Algorithm)
# with population parameters taken from a file passed as argument
python client.py $file $port
