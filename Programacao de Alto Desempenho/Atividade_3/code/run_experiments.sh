#!/bin/bash

DIM=${1:-2048}
GENS=${2:-2000}
REPEATS=2
OUT=results_dim${DIM}_gens${GENS}.csv

echo "np,run,total_time,loop_time,final_live" > $OUT

filter_output() {
    grep -E "^(Final live cells|Total wall-clock time|Loop-only time)" 
}

filter_output_2() {
    grep "^\[0\]" | sed 's/^\[0\] //'
}

run_and_parse() {
    local np=$1
    local r=$2

    clean=$(mpirun -l -np $np ./mpi_gol $DIM $GENS 2>/dev/null | filter_output_2)
    echo "clean=$clean ..."

    total=$(echo "$clean" | grep "Total wall-clock time" | awk '{print $5}')
    loop=$( echo "$clean" | grep "Loop-only time"       | awk '{print $3}')
    final=$(echo "$clean" | grep "Final live cells"     | awk '{print $5}')


    echo "Entrando al segundo for serial np=$np run $r..."
    echo "total=$total..."
    echo "loop=$loop..."
    echo "final=$final..."
    echo "$np,$r,$total,$loop,$final" >> $OUT
}

echo "Running serial..."
for r in $(seq 1 $REPEATS); do
    out=$(./serial_gol $DIM $GENS | filter_output)
    total=$(echo "$out" | grep "Total wall-clock time" | awk '{print $4}')
    loop=$( echo "$out" | grep "Loop-only time"       | awk '{print $3}')
    final=$(echo "$out" | grep "Final live cells"     | awk '{print $4}')
    echo "1,$r,$total,$loop,$final" >> $OUT
done

for np in 2 4 8; do
    for r in $(seq 1 $REPEATS); do
        echo "MPI np=$np run $r..."
        run_and_parse $np $r
    done
done

echo "Saved to $OUT"
